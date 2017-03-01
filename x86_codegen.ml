(*
 *
 * Generate x86 code.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) Kai Chen
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * @end[license]
 *)
open Field_table
open Symbol

open Fj_fir
open Fj_fir_exn
open Fj_fir_pos
open Fj_fir_env
open Fj_fir_type
open Fj_fir_check

open X86_inst
open X86_frame
open X86_frame.X86Frame

module Pos = MakePos (struct let name = "X86_codegen" end)
open Pos

module type CodegenSig =
sig
   val build_prog : Fj_fir.prog -> X86_frame.prog
end

module X86Codegen : CodegenSig =
struct
   (************************************************************************
    * UTILITIES
    ************************************************************************)

   (*
    * Build a block from the instruction list.
    * We'll compute the jumps later.
    *)
   let block_of_insts label insts =
      { block_label = label;
        block_code = Listbuf.to_list insts;
        block_jumps = []
      }

   (*
    * "Block" types correspond to pointers.
    *)
   let is_pointer_type env pos ty =
      match expand_type env pos ty with
         TyUnit
       | TyBool
       | TyChar
       | TyInt
       | TyFloat ->
            false
       | TyNil
       | TyString
       | TyArray _
       | TyFun _
       | TyMethod _
       | TyRecord _
       | TyId _
       | TyNames _ ->
            true


   (*
    * test if i is power of 2
    *
    * Implemented by: turtles
    *)
   let is_power2 i =
      let rec search k =
         if k > i || k = (1 lsl 30) then
            false
         else
            k = i || search (k lsl 1)
      in
         search 1

   (*
    * compute log2 i
    *
    * Implemented by: turtles
    *)
   let log2 i =
      let rec search j =
         let k = 1 lsl j in
            if k = i then
               j
            else
               search (succ j)
      in
         search 0

   (************************************************************************
    * FUNCTION TABLES
    ************************************************************************)

   (*
    * Information we keep for each function.
    *)
   type fun_info =
      { fun_closure  : var;
        fun_vars     : var list;
        fun_pointers : var list;
        fun_reserve  : int
      }

   (*
    * Environment for codegeneration.  We save:
    *    1. The GC info for each function
    *    2. Label offsets
    *)
   type info =
      { info_funs   : fun_info SymbolTable.t;
        info_labels : int SymbolTable.t;
        info_rttd   : (label * int list) SymbolTable.t
      }

   (*
    * Fetch info.
    *)
   let info_mem_fun info f =
      SymbolTable.mem info.info_funs f

   let info_lookup_fun info pos f =
      try SymbolTable.find info.info_funs f with
         Not_found ->
            raise (FirException (pos, UnboundVar f))

   let info_lookup_closure info f =
      try (SymbolTable.find info.info_funs f).fun_closure with
         Not_found ->
            f

   let info_lookup_label info pos label =
      try SymbolTable.find info.info_labels label with
         Not_found ->
            raise (FirException (pos, UnboundLabel label))

   let info_lookup_rttd info pos label =
      try SymbolTable.find info.info_rttd label with
         Not_found ->
            raise (FirException (pos, UnboundLabel label))

   (*
    * Scan the types for record definitions, and save the offset
    * of each label.
    *)
   let rec label_type env pos (renv, lenv) ty =
      match ty with
         TyUnit
       | TyNil
       | TyBool
       | TyChar
       | TyString
       | TyInt
       | TyFloat
       | TyNames _
       | TyId _ ->
            renv, lenv
       | TyArray ty ->
            label_type env pos (renv, lenv) ty
       | TyFun (ty_vars, ty_res) ->
            List.fold_left (label_type env pos) (renv, lenv) (ty_res :: ty_vars)
       | TyMethod (ty_this, ty_vars, ty_res) ->
            List.fold_left (label_type env pos) (renv, lenv) (ty_this :: ty_res :: ty_vars)
       | TyRecord (rclass, fields) ->
            let pos = string_pos "label_type" pos in
            let off =
               match rclass with
                  RecordFrame
                | RecordObject ->
                     1
                | RecordClass
                | RecordMethods ->
                     0
            in

            (* Build the RTTD and label environment *)
            let fields = FieldTable.to_list fields in
            let rttd, lenv, _ =
               List.fold_left (fun (rttd, lenv, off) (v, ty) ->
                     let rttd =
                        if is_pointer_type env pos ty then
                           off :: rttd
                        else
                           rttd
                     in
                     let lenv = SymbolTable.add lenv v off in
                     let off = succ off in
                        rttd, lenv, off) ([], lenv, off) fields
            in

            (* Save the RTTD at each label *)
            let renv =
               match fields with
                  (v, _) :: _ ->
                     let label = new_symbol_string (Symbol.to_string v ^ "_rttd") in
                        SymbolTable.add renv v (label, rttd)
                | [] ->
                     renv
            in
               renv, lenv

   let label_types env tenv =
      SymbolTable.fold (fun rlenv v ty ->
            let pos = string_pos "label_types" (var_exp_pos v) in
               label_type env pos rlenv ty) (SymbolTable.empty, SymbolTable.empty) tenv

   (*
    * Count up the maximum allocation in the body
    * of the function.
    *)
   let sizeof_vobject = sizeof_header + 2 * sizeof_field
   let sizeof_closure = sizeof_header + 2 * sizeof_field
   let sizeof_maxint_string = sizeof_header + 12
   let sizeof_strcat_pair = sizeof_header + 2 * sizeof_field

   let rec alloc_exp pos e =
      match e with
         TailCall _
       | MethodCall _ ->
            0
       | LetAtom (_, _, _, e)
       | LetUnop (_, _, _, _, e)
       | LetBinop (_, _, _, _, _, e)
       | LetSubscript (_, _, _, _, e)
       | SetSubscript (_, _, _, _, e)
       | LetProject (_, _, _, _, e)
       | SetProject (_, _, _, _, e) ->
            alloc_exp pos e
       | IfThenElse (_, e1, e2) ->
            let size1 = alloc_exp pos e1 in
            let size2 = alloc_exp pos e2 in
               max size1 size2
       | IfType (_, _, _, e1, e2) ->
            (* Add size of a vobject allocation to the first branch *)
            let size1 = alloc_exp pos e1 + sizeof_vobject in
            let size2 = alloc_exp pos e2 in
               max size1 size2
       | LetArray (_, _, [AtomInt i], _, e) ->
            (* Array includes a header *)
            alloc_exp pos e + i * sizeof_field + sizeof_header
       | LetArray (_, _, _, _, e) ->
            raise (FirException (pos, StringError "multimensional and variable-sized arrays not implemented"))
       | LetRecord (_, _, _, fields, e) ->
            alloc_exp pos e + (succ (FieldTable.cardinal fields) * sizeof_field) + sizeof_header
       | LetClosure (_, _, _, _, e) ->
            alloc_exp pos e + sizeof_closure
       | LetExt (_, _, s, _, _, e) ->
            let size = alloc_exp pos e in
               (match s with
                   "itoa" -> size + sizeof_maxint_string
                 | "strcat" -> size + sizeof_strcat_pair
                 | _ -> size)
       | LetFuns _
       | LetVar _
       | SetVar _ ->
            raise (FirException (pos, FirLevel2))

   (*
    * For each function, save:
    *    1. the params of the fun
    *    2. the params that are pointers
    *    3. the max allocation (in words)
    *)
   let build_info prog =
      let { prog_types = tenv;
            prog_funs = funs
          } = prog
      in
      let env = env_of_prog prog in
      let renv, lenv = label_types env tenv in
      let fenv =
         SymbolTable.mapi (fun f (_, ty, vars, body) ->
               let pos = string_pos "build_fenv" (var_exp_pos f) in
               let ty_vars, _ = dest_fun_or_method_type env pos ty in
               let pointers =
                  List.fold_left2 (fun pointers v ty ->
                        if is_pointer_type env pos ty then
                           v :: pointers
                        else
                           pointers) [] vars ty_vars
               in
               let size = alloc_exp pos body in
                  { fun_closure = new_symbol_string (Symbol.to_string f ^ "_closure");
                    fun_vars = vars;
                    fun_pointers = pointers;
                    fun_reserve = size
                  }) funs
      in
         { info_labels = lenv;
           info_rttd = renv;
           info_funs = fenv
         }

   (************************************************************************
    * CODE GENERATION
    ************************************************************************)

   (*
    * A global initializer.
    * Implemented by: jyh
    *)
   let build_init info a =
      match a with
         AtomUnit
       | AtomNil ->
            InitNumber 0
       | AtomBool b ->
            InitNumber (if b then 1 else 0)
       | AtomChar c ->
            InitNumber (Char.code c)
       | AtomInt i ->
            InitNumber i
       | AtomFloat _ ->
            raise (Failure "code_atom: floats not implemented")
       | AtomVar v ->
            (* Functions get mapped *)
            InitLabel (info_lookup_closure info v)

   (*
    * Operand for an atom.
    * Implemented by: jyh
    *)
   let build_atom a =
      match a with
         AtomUnit
       | AtomNil ->
            ImmediateNumber 0
       | AtomBool b ->
            ImmediateNumber (if b then 1 else 0)
       | AtomChar c ->
            ImmediateNumber (Char.code c)
       | AtomInt i ->
            ImmediateNumber i
       | AtomFloat _ ->
            raise (Failure "code_atom: floats not implemented")
       | AtomVar v ->
            Register v

   (*
    * Build the assembly for an expression.
    * Implemented by: jyh
    *)
   let rec build_exp info blocks insts e =
      let pos = string_pos "build_exp" (exp_pos e) in
      let insts = Listbuf.add insts (CommentFIR e) in
         match e with
            LetAtom (v, ty, a, e) ->
               build_atom_exp info blocks insts pos v a e
          | LetUnop (v, ty, op, a, e) ->
               build_unop_exp info blocks insts pos v op a e
          | LetBinop (v, ty, op, a1, a2, e) ->
               build_binop_exp info blocks insts pos v op a1 a2 e
          | IfThenElse (a, e1, e2) ->
               build_if_exp info blocks insts pos a e1 e2
          | IfType (a, name, v, e1, e2) ->
               build_iftype_exp info blocks insts pos a name v e1 e2
          | LetSubscript (v, ty, a1, a2, e) ->
               build_subscript_exp info blocks insts pos v a1 a2 e
          | SetSubscript (a1, a2, _, a3, e) ->
               build_set_subscript_exp info blocks insts pos a1 a2 a3 e
          | LetProject (v, ty, a, label, e) ->
               build_project_exp info blocks insts pos v a label e
          | SetProject (a1, label, _, a2, e) ->
               build_set_project_exp info blocks insts pos a1 label a2 e
          | TailCall (f, args) ->
               build_tailcall_exp info blocks insts pos f args
          | MethodCall (f, a, args) ->
               build_methodcall_exp info blocks insts pos f a args
          | LetClosure (v, _, f, a, e) ->
               build_closure_exp info blocks insts pos v f a e
          | LetRecord (v, _, rclass, fields, e) ->
               build_record_exp info blocks insts pos v rclass fields e
          | LetArray (v, _, dimens, a, e) ->
               build_array_exp info blocks insts pos v dimens a e
          | LetExt (v, _, s, _, args, e) ->
               build_ext_exp info blocks insts pos v s args e
          | LetFuns _
          | LetVar _
          | SetVar _ ->
               raise (FirException (pos, FirLevel2))

   (*
    * An atom assignment.
    * Implemented by: jyh
    *)
   and build_atom_exp info blocks insts pos v a e =
      let pos = string_pos "build_atom_exp" pos in
      let insts = Listbuf.add insts (MOV (Register v, build_atom a)) in
         build_exp info blocks insts e

   (*
    * A unary operation.
    * Implemented by: jyh
    *)
   and build_unop_exp info blocks insts pos v op a e =
      let pos = string_pos "build_unop_exp" pos in
      let a = build_atom a in
      let insts = Listbuf.add insts (MOV (Register v, a)) in
      let inst =
         match op with
            UMinusIntOp ->
               NEG (Register v)
          | UNotIntOp
          | UNotBoolOp ->
               (* modified by turtles to flip only the LSB *)
               XOR (Register v, ImmediateNumber 1)
          | UCharOfInt ->
               AND (Register v, ImmediateNumber 255)
          | UIntOfChar ->
               (*
                * Interprete as unsigned characters.
                * This NOP is silly.  The code can be restructured to
                * remove it, but optimization will remove it anyway.
                *)
               NOP
          | UMinusFloatOp
          | UIntOfFloat
          | UFloatOfInt ->
               raise (FirException (pos, StringError "floats not implemented"))
      in
      let insts = Listbuf.add insts inst in
         build_exp info blocks insts e

   (*
    * A comparison.
    *
    * For some architectures, SET only works on real registers.
    * So we first SET ecx and then move the result to dst.
    * Since SET only sets the lowest 8 bits, we need to clear ecx first
    *
    * Implemented by: turtles
    *)
   and build_compare insts dst op a1 a2 =
      let tmp = new_symbol_string "cmp_tmp" in
      let insts = Listbuf.add insts (XOR (Register ecx, Register ecx)) in
      let insts = Listbuf.add insts (MOV (Register tmp, a1)) in
      let insts = Listbuf.add insts (CMP (Register tmp, a2)) in
      let insts = Listbuf.add insts (SET (op, Register ecx)) in
      let insts = Listbuf.add insts (MOV (dst, Register ecx)) in
         insts


   (*
    * A shift.  Variable shifts have to use cl for the shift.
    * Implemented by: jyh
    *)
   and build_shift insts dst op a1 a2 =
      match a2 with
         ImmediateNumber _ ->
            let insts = Listbuf.add insts (MOV (dst, a1)) in
            let insts = Listbuf.add insts (op dst a2) in
               insts
       | _ ->
            let insts = Listbuf.add insts (MOV (dst, a1)) in
            let insts = Listbuf.add insts (MOV (Register ecx, a2)) in
            let insts = Listbuf.add insts (op dst (Register ecx)) in
               insts

   (*
    * A binary operation.
    * This part includes some simple strength reduction.
    *
    * Implemented by: turtles
    *)
   and build_binop_exp info blocks insts pos v op a1 a2 e =
      let pos = string_pos "build_binop_exp" pos in
      let a1 = build_atom a1 in
      let a2 = build_atom a2 in
      let dst = Register v in
      let insts =
         match op with
            (* reduce addition by 0 *)
            AddIntOp ->
               (match a1, a2 with
                  (* if a1 = 0, dst <- a2 *)
                  ImmediateNumber 0, _ ->
                     Listbuf.add insts (MOV (dst, a2))
                  (* if a2 = 0, dst <- a1 *)
                | _, ImmediateNumber 0 ->
                     Listbuf.add insts (MOV (dst, a1))
                  (* otherwise, we do not do any reduction *)
                | _ ->
                     let insts = Listbuf.add insts (MOV (dst, a1)) in
                     let insts = Listbuf.add insts (ADD (dst, a2)) in
                        insts)
            (* reduce subtraction by 0 *)
          | SubIntOp ->
               (match a2 with
                  (* if a2 = 0, dst <- a1 *)
                  ImmediateNumber 0 ->
                     Listbuf.add insts (MOV (dst, a1))
                  (* otherwise, no reduction *)
                | _ ->
                     let insts = Listbuf.add insts (MOV (dst, a1)) in
                     let insts = Listbuf.add insts (SUB (dst, a2)) in
                        insts)
            (* reduce multiply by 0, by 1, or by power of 2 *)
          | MulIntOp ->
               (* dst of IMUL must be register *)
               let unreduced_insts = Listbuf.add insts (MOV (Register ecx, a1)) in
               let unreduced_insts = Listbuf.add unreduced_insts (IMUL (Register ecx, a2)) in
               let unreduced_insts = Listbuf.add unreduced_insts (MOV (dst, Register ecx)) in
                  (match a1, a2 with
                     (* if a1 = 0, or a2 = 0 *)
                     ImmediateNumber 0, _
                   | _, ImmediateNumber 0 ->
                        Listbuf.add insts (MOV (dst, ImmediateNumber 0))
                     (* if a1 = 1 *)
                   | ImmediateNumber 1, _ ->
                        Listbuf.add insts (MOV (dst, a2))
                     (* if a2 = 1 *)
                   | _, ImmediateNumber 1 ->
                        Listbuf.add insts (MOV (dst, a1))
                     (* now test for power of 2 case *)
                   | ImmediateNumber i, a2 ->
                        (* if a1 is power2, shift a2 left by log2 a1 *)
                        if is_power2 i then
                           let x = log2 i in
                           let op = fun r1 r2 -> SHL (r1, r2) in
                              build_shift insts dst op a2 (ImmediateNumber x)
                        else
                           (match a2 with
                              ImmediateNumber i ->
                                 (* if a2 is power2, shift a1 left by log2 a2 *)
                                 if is_power2 i then
                                    let x = log2 i in
                                    let op = fun r1 r2 -> SHL (r1, r2) in
                                       build_shift insts dst op a1 (ImmediateNumber x)
                                 (* no reduction *)
                                 else
                                    unreduced_insts
                              (* no reduction *)
                            | _ ->
                                 unreduced_insts)
                   | a1, ImmediateNumber i ->
                        (* if a2 is power2, shift a1 left by log2 a2 *)
                        if is_power2 i then
                           let x = log2 i in
                           let op = fun r1 r2 -> SHL (r1, r2) in
                              build_shift insts dst op a1 (ImmediateNumber x)
                        (* no reduction *)
                        else
                           unreduced_insts
                     (* no reduction *)
                   | _, _ ->
                        unreduced_insts)
            (* reduce a1=0, a2=1, a2=-1, or a1>0 && a2 power2 *)
          | DivIntOp ->
               let tmp = new_symbol_string "div_op2" in
               let unreduced_insts = Listbuf.add insts (MOV (Register eax, a1)) in
               let unreduced_insts = Listbuf.add unreduced_insts CDQ in
               let unreduced_insts = Listbuf.add unreduced_insts (MOV (Register tmp, a2)) in
               let unreduced_insts = Listbuf.add unreduced_insts (IDIV (Register tmp)) in
               let unreduced_insts = Listbuf.add unreduced_insts (MOV (dst, Register eax)) in
               (match a1, a2 with
                  (* in this case, we know it's divide by 0 exception *)
                  _, ImmediateNumber 0 ->
                     raise (Failure "divide by 0 exception")
                  (* if a1 = 0, dst <- 0
                   * this may get by some divide by 0 exception, but we
                   * can't tell at compile time. *)
                | ImmediateNumber 0, _ ->
                     Listbuf.add insts (MOV (dst, ImmediateNumber 0))
                  (* if a2 = 1, dst <- a1 *)
                | _, ImmediateNumber 1 ->
                     Listbuf.add insts (MOV (dst, a1))
                  (* if a2 = -1, dst <- -a1, this gives fewer instructions *)
                | _, ImmediateNumber -1 ->
                     let insts = Listbuf.add insts (MOV (dst, a1)) in
                     let insts = Listbuf.add insts (NEG dst) in
                        insts
                  (* be careful, idiv->sar reduction only works for positive numbers *)
                | ImmediateNumber i, ImmediateNumber j ->
                     (* use SAR for efficiency *)
                     if i > 0 && is_power2 j then
                        let x = log2 j in
                        let op = fun r1 r2 -> SAR (r1, r2) in
                           build_shift insts dst op a1 (ImmediateNumber x)
                     (* otherwise no reduction *)
                     else
                        unreduced_insts
                  (* otherwise no reduction *)
                | _, _ ->
                     unreduced_insts)
            (* reduce a1=0, a2=1, a2=-1, or a1>0 && a2 power2 *)
          | RemIntOp ->
               let tmp = new_symbol_string "rem_op2" in
               let unreduced_insts = Listbuf.add insts (MOV (Register eax, a1)) in
               let unreduced_insts = Listbuf.add unreduced_insts CDQ in
               let unreduced_insts = Listbuf.add unreduced_insts (MOV (Register tmp, a2)) in
               let unreduced_insts = Listbuf.add unreduced_insts (IDIV (Register tmp)) in
               let unreduced_insts = Listbuf.add unreduced_insts (MOV (dst, Register edx)) in
                  (match a1, a2 with
                     (* in this case, we know it's divide by 0 exception *)
                     _, ImmediateNumber 0 ->
                        raise (Failure "divide by 0 exception")
                     (* if a1 = 0, dst <- 0.
                      * this may get by some divide by 0 exception, but we
                      * can't tell at compile time. *)
                   | ImmediateNumber 0, _ ->
                        Listbuf.add insts (MOV (dst, ImmediateNumber 0))
                     (* if a2 = 1 or a2 = -1, dst <- 0 *)
                   | _, ImmediateNumber 1
                   | _, ImmediateNumber -1 ->
                        Listbuf.add insts (MOV (dst, ImmediateNumber 0))
                     (* if a2 is power2, we can use AND a1, a2-1 *)
                   | ImmediateNumber i, ImmediateNumber j ->
                        if i > 0 && is_power2 j then
                           let insts = Listbuf.add insts (MOV (dst, a1)) in
                           let insts = Listbuf.add insts (AND (dst, ImmediateNumber (pred j))) in
                              insts
                        else
                           unreduced_insts
                     (* no reduction *)
                   | _, _ ->
                        unreduced_insts)
            (* reduce a1 = 0, a2 = 0, a1 = -1, a2 = -1 *)
          | AndIntOp ->
               (match a1, a2 with
                  (* if a1 = 0, or a2 = 0, dst <- 0 *)
                  ImmediateNumber 0, _
                | _, ImmediateNumber 0 ->
                     Listbuf.add insts (MOV (dst, ImmediateNumber 0))
                  (* if a1 = -1, dst <- a2 *)
                | ImmediateNumber -1, _ ->
                     Listbuf.add insts (MOV (dst, a2))
                  (* if a2 = -1, dst <- a1 *)
                | _, ImmediateNumber -1 ->
                     Listbuf.add insts (MOV (dst, a1))
                  (* otherwise, no reduction *)
                | _, _ ->
                     let insts = Listbuf.add insts (MOV (dst, a1)) in
                     let insts = Listbuf.add insts (AND (dst, a2)) in
                        insts)
            (* reduce a1 = 0, a2 = 0, a1 = -1, a2 = -1 *)
          | OrIntOp ->
               (match a1, a2 with
                  (* if a1 = -1 or a2 = -1, dst <- -1 *)
                  ImmediateNumber -1, _
                | _, ImmediateNumber -1 ->
                     Listbuf.add insts (MOV (dst, ImmediateNumber (-1)))
                  (* if a1 = 0, dst <- a2 *)
                | ImmediateNumber 0, _ ->
                     Listbuf.add insts (MOV (dst, a2))
                  (* if a2 = 0, dst <- a1 *)
                | _, ImmediateNumber 0 ->
                     Listbuf.add insts (MOV (dst, a1))
                  (* otherwise, no reduction *)
                | _, _ ->
                     let insts = Listbuf.add insts (MOV (dst, a1)) in
                     let insts = Listbuf.add insts (OR (dst, a2)) in
                        insts)
            (* reduce a1 = 0, a2 = 0 *)
          | LslIntOp ->
               (match a1, a2 with
                  (* if either a1 or a2 is 0, SHL is nop *)
                  ImmediateNumber 0, _
                | _, ImmediateNumber 0 ->
                     Listbuf.add insts (MOV (dst, a1))
                  (* otherwise no reduction *)
                | _, _ ->
                     let op = fun r1 r2 -> SHL (r1, r2) in
                        build_shift insts dst op a1 a2)
            (* reduce a1 = 0, a2 = 0 *)
          | LsrIntOp ->
               (match a1, a2 with
                  (* if either a1 or a2 is 0, SHR is nop *)
                | ImmediateNumber 0, _
                | _, ImmediateNumber 0 ->
                     Listbuf.add insts (MOV (dst, a1))
                  (* otherwise no reduction *)
                | _, _ ->
                     let op = fun r1 r2 -> SHR (r1, r2) in
                        build_shift insts dst op a1 a2)
            (* reduce a1 = 0, a1 = -1, a2 = 0 *)
          | AsrIntOp ->
               (match a1, a2 with
                  (* if a1 = 0, or a1 = -1, or a2 = 0, SAR is nop *)
                | ImmediateNumber 0, _
                | ImmediateNumber (-1), _
                | _, ImmediateNumber 0 ->
                     Listbuf.add insts (MOV (dst, a1))
                  (* otherwise no reduction *)
                | _, _ ->
                     let op = fun r1 r2 -> SAR (r1, r2) in
                        build_shift insts dst op a1 a2)
            (* reduce a1 = 0, a2 = 0 *)
          | XorIntOp ->
               (match a1, a2 with
                  (* if either a1 or a2 is 0, XOR is nop *)
                  ImmediateNumber 0, _ ->
                     Listbuf.add insts (MOV (dst, a2))
                | _, ImmediateNumber 0 ->
                     Listbuf.add insts (MOV (dst, a1))
                  (* otherwise no reduction *)
                | _, _ ->
                     let insts = Listbuf.add insts (MOV (dst, a1)) in
                     let insts = Listbuf.add insts (XOR (dst, a2)) in
                        insts)
          | EqIntOp
          | EqBoolOp ->
               build_compare insts dst EQ a1 a2
          | NeqIntOp
          | NeqBoolOp ->
               build_compare insts dst NEQ a1 a2
          | LeIntOp ->
               build_compare insts dst LE a1 a2
          | LtIntOp ->
               build_compare insts dst LT a1 a2
          | GtIntOp ->
               build_compare insts dst GT a1 a2
          | GeIntOp ->
               build_compare insts dst GE a1 a2
          | AddFloatOp
          | SubFloatOp
          | MulFloatOp
          | DivFloatOp
          | RemFloatOp
          | EqFloatOp
          | NeqFloatOp
          | LeFloatOp
          | LtFloatOp
          | GtFloatOp
          | GeFloatOp ->
               raise (FirException (pos, StringError "floats not implemented"))
          | EqPtrOp
          | NeqPtrOp ->
               raise (FirException (pos, StringError "ptr not implemented"))
      in
         build_exp info blocks insts e

   (*
    * For a condition, test the atom and branch.
    * Implemented by: turtles
    *)
   and build_if_exp info blocks insts pos a e1 e2 =
      let pos = string_pos "build_if_exp" pos in
      let a = build_atom a in

      (* build the branches *)
      let true_label = new_symbol_string "true_case" in
      let false_label = new_symbol_string "false_case" in
      let blocks, true_insts = build_exp info blocks Listbuf.empty e1 in
      let blocks, false_insts = build_exp info blocks Listbuf.empty e2 in
      let true_block = block_of_insts true_label true_insts in
      let false_block = block_of_insts false_label false_insts in
      let blocks = true_block :: false_block :: blocks in

      (* test the atom and branch *)
      let tmp = new_symbol_string "if_tmp" in
      let insts = Listbuf.add insts (MOV (Register tmp, a)) in
      let insts = Listbuf.add insts (CMP (Register tmp, ImmediateNumber 0)) in
      let insts = Listbuf.add insts (JCC (NEQ, true_label)) in
      let insts = Listbuf.add insts (JMP (false_label)) in
         blocks, insts

   (*
    * The iftype requires searching the name table.
    * This function is especially difficult; we have to construct
    * a loop that walks through the name table.
    * Implemented by: jyh
    *)
   and build_iftype_exp info blocks insts pos a name v e1 e2 =
      let pos = string_pos "build_iftype_exp" pos in
      let a = build_atom a in

      (* Build the branches *)
      let label1 = new_symbol_string "true_case" in
      let label2 = new_symbol_string "false_case" in
      let blocks, insts1 = build_exp info blocks Listbuf.empty e1 in
      let blocks, insts2 = build_exp info blocks Listbuf.empty e2 in
      let block1 = block_of_insts label1 insts1 in
      let block2 = block_of_insts label2 insts2 in
      let blocks = block1 :: block2 :: blocks in

      (* The match case *)
      let match_label = new_symbol_string "match" in
      let names_reg = new_symbol_string "names_ptr" in
      let name_reg = new_symbol_string "name" in
      let insts_match = Listbuf.empty in
      let insts_match = Listbuf.add insts_match (MOV  (Register name_reg, MemRegOff (names_reg, sizeof_field))) in
      let insts_match = Listbuf.add insts_match (CMP  (Register name_reg, ImmediateNumber 0)) in
      let insts_match = Listbuf.add insts_match (JCC  (EQ, label1)) in
      let insts_match = Listbuf.add insts_match (LEA  (Register v, MemRegOff (mem_next, sizeof_header))) in
      let insts_match = Listbuf.add insts_match (MOV  (MemRegOff (v, header_off), ImmediateNumber headerof_vobject)) in
      let insts_match = Listbuf.add insts_match (MOV  (MemReg v, Register name_reg)) in
      let insts_match = Listbuf.add insts_match (MOV  (MemRegOff (v, sizeof_field), a)) in
      let insts_match = Listbuf.add insts_match (ADD  (Register mem_next, ImmediateNumber sizeof_vobject)) in
      let insts_match = Listbuf.add insts_match (JMP  label1) in
      let blocks = block_of_insts match_label insts_match :: blocks in

      (* The name loop *)
      let loop_label = new_symbol_string "names_loop" in
      let insts_loop = Listbuf.empty in
      let insts_loop = Listbuf.add insts_loop (MOV  (Register name_reg, MemReg names_reg)) in
      let insts_loop = Listbuf.add insts_loop (CMP  (Register name_reg, ImmediateCLabel name)) in
      let insts_loop = Listbuf.add insts_loop (JCC  (EQ, match_label)) in
      let insts_loop = Listbuf.add insts_loop (CMP  (Register name_reg, ImmediateNumber 0)) in
      let insts_loop = Listbuf.add insts_loop (JCC  (EQ, label2)) in
      let insts_loop = Listbuf.add insts_loop (ADD  (Register names_reg, ImmediateNumber (2 * sizeof_field))) in
      let insts_loop = Listbuf.add insts_loop (JMP  loop_label) in
      let blocks = block_of_insts loop_label insts_loop :: blocks in

      (* Generate the test *)
      let insts = Listbuf.add insts (MOV (Register v, a)) in
      let insts = Listbuf.add insts (MOV (Register names_reg, MemRegOff (v, sizeof_field))) in
      let insts = Listbuf.add insts (MOV (Register names_reg, MemReg names_reg)) in
      let insts = Listbuf.add insts (JMP loop_label) in
         blocks, insts

   (*
    * Array subscripting.
    * Check that the array is not nil, and that the index is in bounds.
    * Implemented by: turtles
    *)
   and build_subscript_exp info blocks insts pos v a1 a2 e =
      let pos = string_pos "build_subscript_exp" pos in
      let a1 = build_atom a1 in
      let a2 = build_atom a2 in
      let v_array = new_symbol_string "let_sub_array" in
      let v_index = new_symbol_string "let_sub_index" in
      let size = new_symbol_string "let_sub_size" in

      (* check that array is not nil *)
      let insts = Listbuf.add insts (MOV (Register v_array, a1)) in
      let insts = Listbuf.add insts (CMP (Register v_array, ImmediateNumber 0)) in
      let insts = Listbuf.add insts (JCC (EQ, seg_fault_label)) in

      (* check if the index is in bounds *)
      let insts = Listbuf.add insts (MOV (Register size, MemRegOff(v_array, header_off))) in
      let insts = Listbuf.add insts (SHR (Register size, ImmediateNumber size_shift)) in
      let insts = Listbuf.add insts (AND (Register size, ImmediateNumber size_mask)) in
      let insts = Listbuf.add insts (MOV (Register v_index, a2)) in
      (* size is in bytes, but index is in words ... so we need to divide size by 4 
       * the following is a hack for efficiency. we rely on the assumption that sizeof_field
       * is a power of 2 *)
      let insts = Listbuf.add insts (SAR (Register size, ImmediateNumber (log2 sizeof_field))) in
      let insts = Listbuf.add insts (CMP (Register size, Register v_index)) in
      let insts = Listbuf.add insts (JCC (ULE, seg_fault_label)) in

      (* fetch array element *)
      let insts = Listbuf.add insts (MOV (Register v, MemRegRegOffMul(v_array, v_index, 0, sizeof_field))) in
         build_exp info blocks insts e


   (*
    * Assign a subscript.
    * Check that the array is not nil, and that the index is in bounds.
    * Implemented by: turtles
    *)
   and build_set_subscript_exp info blocks insts pos a1 a2 a3 e =
      let pos = string_pos "build_set_subscript_exp" pos in
      let a1 = build_atom a1 in
      let a2 = build_atom a2 in
      let a3 = build_atom a3 in
      let v_array = new_symbol_string "set_sub_array" in
      let v_index = new_symbol_string "set_sub_index" in
      let size = new_symbol_string "set_sub_size" in

      (* check that array is not nil *)
      let insts = Listbuf.add insts (MOV (Register v_array, a1)) in
      let insts = Listbuf.add insts (CMP (Register v_array, ImmediateNumber 0)) in
      let insts = Listbuf.add insts (JCC (EQ, seg_fault_label)) in

      (* check if the index is in bounds *)
      let insts = Listbuf.add insts (MOV (Register size, MemRegOff(v_array, header_off))) in
      let insts = Listbuf.add insts (SHR (Register size, ImmediateNumber size_shift)) in
      let insts = Listbuf.add insts (AND (Register size, ImmediateNumber size_mask)) in
      let insts = Listbuf.add insts (MOV (Register v_index, a2)) in
      (* same here, need to divide size by 4 first *)
      let insts = Listbuf.add insts (SAR (Register size, ImmediateNumber (log2 sizeof_field))) in
      let insts = Listbuf.add insts (CMP (Register size, Register v_index)) in
      let insts = Listbuf.add insts (JCC (ULE, seg_fault_label)) in

      (* set array element *)
      (* let tmp = new_symbol_string "set_sub_tmp" in *)
      (* let insts = Listbuf.add insts (MOV (Register tmp, a3)) in *)
      let insts = Listbuf.add insts (MOV (MemRegRegOffMul(v_array, v_index, 0, sizeof_field), a3)) in
         build_exp info blocks insts e

   (*
    * Record projection.
    * Check that the record is not nil.
    * Don't have to worry about bounds checking.
    * Note that you can get the index into the block with
    * the function info_lookup_label.
    *
    * Implemented by: turtles
    *)
   and build_project_exp info blocks insts pos v a label e =
      let pos = string_pos "build_project_exp" pos in
      let a = build_atom a in
      let index = info_lookup_label info pos label in
      let v_record = new_symbol_string "let_proj_record" in

      (* check that the record is not nil *)
      let insts = Listbuf.add insts (MOV (Register v_record, a)) in
      let insts = Listbuf.add insts (CMP (Register v_record, ImmediateNumber 0)) in
      let insts = Listbuf.add insts (JCC (EQ, seg_fault_label)) in

      (* fetch record field *)
      let insts = Listbuf.add insts (MOV (Register v, MemRegOff(v_record, index * sizeof_field))) in
         build_exp info blocks insts e


   (*
    * Record projection.
    * Check that the record is not nil.
    * Implemented by: turtles
    *)
   and build_set_project_exp info blocks insts pos a1 label a2 e =
      let pos = string_pos "build_set_project_exp" pos in
      let a1 = build_atom a1 in
      let a2 = build_atom a2 in
      let index = info_lookup_label info pos label in
      let v_record = new_symbol_string "set_proj_record" in

      (* check that the record is not nil *)
      let insts = Listbuf.add insts (MOV (Register v_record, a1)) in
      let insts = Listbuf.add insts (CMP (Register v_record, ImmediateNumber 0)) in
      let insts = Listbuf.add insts (JCC (EQ, seg_fault_label)) in

      (* set a record field *)
      let tmp = new_symbol_string "set_proj_tmp" in
      let insts = Listbuf.add insts (MOV (Register tmp, a2)) in
      let insts = Listbuf.add insts (MOV (MemRegOff(v_record, index * sizeof_field), Register tmp)) in
         build_exp info blocks insts e

   (*
    * A function call.  If this is not a real function,
    * it has to be a closure.  In this case, we put the arguments
    * in the "standard" places, including the environment as the
    * first arg.
    *)
   and build_tailcall_exp info blocks insts pos f args =
      let pos = string_pos "build_tailcall_exp" pos in
         if info_mem_fun info f then
            build_normal_call info blocks insts pos f args
         else
            build_closure_call info blocks insts pos f args

   (*
    * For a normal tailcall, we generate the MOV instructions
    * to put the args in the right place.
    * Implemented by: turtles
    *)
   and build_normal_call info blocks insts pos f args =
      let pos = string_pos "build_normal_call" pos in
      let {fun_vars = vars} = info_lookup_fun info pos f in
      let args = List.map build_atom args in

      (* allocate a list of tmp registers *)
      let tmp_vars =
         let rec build_list tmp_vars i =
            if i = List.length args then
               tmp_vars
            else
               build_list (new_symbol_string "tmp_var" :: tmp_vars) (succ i)
         in
            build_list [] 0
      in

      (* we first move all arguments to tmp registers *)
      let insts =
         List.fold_left2 (fun insts tmp_var a ->
            Listbuf.add insts (MOV (Register tmp_var, a))) insts tmp_vars args
      in

      (* now move arguments to the right place *)
      let insts =
         List.fold_left2 (fun insts v tmp_var ->
            Listbuf.add insts (MOV (Register v, Register tmp_var))) insts vars tmp_vars
      in


      (* and jump, here we use direct jump *)
      let insts = Listbuf.add insts (JMP f) in
         blocks, insts

   (*
    * A closure requires a call with the "standard" calling convention.
    * MOV all the arguments into the standard places; project the
    * function pointer and its frame, and IJMP to the function.
    * Implemented by: jyh
    *)
   and build_closure_call info blocks insts pos f args =
      let pos = string_pos "build_closure_call" pos in

      (* Project the parts of the closure *)
      let v_fun = new_symbol_string "fun" in
      let v_env = new_symbol_string "env" in
      let insts = Listbuf.add insts (MOV (Register v_fun, MemReg f)) in
      let insts = Listbuf.add insts (MOV (Register v_env, MemRegOff (f, sizeof_field))) in

      (* Move the arguments into place *)
      let args = Register v_env :: List.map build_atom args in
      let len = List.length args in
      let vars = get_stdargs len in
      let insts =
         List.fold_left2 (fun insts v a ->
               Listbuf.add insts (MOV (Register v, a))) insts vars args
      in

      (* Indirect jump *)
      let insts = Listbuf.add insts (IJMP (args, Register v_fun)) in
         blocks, insts

   (*
    * A method call uses the normal calling convention,
    * but the function is a pointer.
    * Implemented by: turtles
    *)
   and build_methodcall_exp info blocks insts pos f a args =
      let pos = string_pos "build_method_call" pos in
      let args = List.map build_atom (a :: args) in
      let vars = get_stdargs (List.length args) in

      (* move arguments into standard places *)
      let insts =
         List.fold_left2 (fun insts v a ->
            Listbuf.add insts (MOV (Register v, a))) insts vars args
      in

      (* a method call is a pointer, so we use IJMP *)
      let insts = Listbuf.add insts (IJMP (args, Register f)) in
         blocks, insts



   (*
    * Build a closure from the fun and the environment.
    * A closure is a record with two fields: the first field
    * is the environment (the frame), and the second field
    * is a function pointer.
    *
    * The first field is the function pointer.
    * The second field is the environment. -kchen
    *
    * Implemented by: turtles
    *)
   and build_closure_exp info blocks insts pos v f a e =
      let pos = string_pos "build_closure_exp" pos in
      let a = build_atom a in
      let { fun_closure = f } = info_lookup_fun info pos f in

      (* the pointer points to the word _after_ the header. see x86_frame.ml *)
      let insts = Listbuf.add insts (LEA (Register v, MemRegOff (mem_next, sizeof_header))) in
      (* set the header *)
      let insts = Listbuf.add insts (MOV (MemRegOff (v, header_off), ImmediateNumber headerof_closure)) in
      (* update mem_next *)
      let insts = Listbuf.add insts (ADD (Register mem_next, ImmediateNumber sizeof_closure)) in
      (* set the function pointer *)
      let insts = Listbuf.add insts (MOV (MemReg v, ImmediateCLabel f)) in
      (* set environment field *)
      let insts = Listbuf.add insts (MOV (MemRegOff (v, sizeof_field), a)) in

         build_exp info blocks insts e

   (*
    * Allocate a record.
    *)
   and build_record_exp info blocks insts pos v rclass fields e =
      let pos = string_pos "build_record_exp" pos in

      (* Get fields *)
      let fields = FieldTable.to_list fields in
      let args = List.map (fun (_, a) -> build_atom a) fields in

      (* Frames and objects have a RTTD *)
      let count = List.length args in
      let header, count, args =
         match rclass with
            RecordFrame
          | RecordObject ->
               (* Get the rttd based on one of the labels *)
               (match fields with
                   (label, _) :: _ ->
                      let count = succ count in
                      let header = headerof_rttd_tuple count in
                      let label, _ = info_lookup_rttd info pos label in
                      let args = ImmediateCLabel label :: args in
                         header, count, args
                 | [] ->
                      (* This record is empty, so it doesn't need a RTTD *)
                      headerof_empty, count, [])
          | RecordClass
          | RecordMethods ->
               headerof_normal_tuple count, count, args
      in

      (* Allocate and assign *)
      let total = count * sizeof_field + sizeof_header in
      let insts = Listbuf.add insts (LEA (Register v, MemRegOff (mem_next, sizeof_header))) in
      let insts = Listbuf.add insts (MOV (MemRegOff (v, header_off), ImmediateNumber header)) in
      let insts = Listbuf.add insts (ADD (Register mem_next, ImmediateNumber total)) in
      let insts, _ =
         List.fold_left (fun (insts, off) a ->
               let insts = Listbuf.add insts (MOV (MemRegOff (v, off), a)) in
                  insts, off + sizeof_field) (insts, 0) args
      in
         build_exp info blocks insts e

   (*
    * Allocate an array.  We only support constant-size, single dimension
    * arrays.
    *)
   and build_array_exp info blocks insts pos v dimens a e =
      let pos = string_pos "build_array_exp" pos in
      let a = build_atom a in

      (* we only support constant-size, single dimension arrays *)
      let size =
         match dimens with
            [AtomInt dimen] -> dimen
          | _ ->
               raise (FirException (pos, StringError "variable-size, multi-dimension arrays not implemented"))
      in

      (* the header *)
      let header = headerof_normal_tuple size in
      (* compute number of bytes the array takes *)
      let total = size * sizeof_field + sizeof_header in

      (* point to the word _after_ the header *)
      let insts = Listbuf.add insts (LEA (Register v, MemRegOff (mem_next, sizeof_header))) in
      (* set header *)
      let insts = Listbuf.add insts (MOV (MemRegOff (v, header_off), ImmediateNumber header)) in
      (* continuation *)
      let insts = Listbuf.add insts (ADD (Register mem_next, ImmediateNumber total)) in

      (* finally, initialize the array *)
      let insts =
         let rec init_array insts count =
            if count = size then
               insts
            else
               init_array (Listbuf.add insts (MOV (MemRegOff (v, count*sizeof_field), a))) (succ count)
         in
         init_array insts 0
      in
         build_exp info blocks insts e


   (*
    * External call.  GCC calling convention on Linux
    * places the arguments on the stack, and returns the
    * result in %eax.
    *)
   and build_ext_exp info blocks insts pos v s args e =
      let pos = string_pos "build_ext_exp" pos in

      (* Push all the args on the stack, in reverse order *)
      let args = List.map build_atom args in
      let insts =
         List.fold_right (fun a insts ->
               Listbuf.add insts (PUSH a)) args insts
      in

      (* Call the function *)
      let insts = Listbuf.add insts (CALL (ImmediateLabel (Symbol.add s))) in

      (* Remove the args from the stack *)
      let insts = Listbuf.add insts (ADD (Register esp, ImmediateNumber (List.length args * sizeof_field))) in
      let insts = Listbuf.add insts (MOV (Register v, Register eax)) in
         build_exp info blocks insts e

   (*
    * Build a function.  We have to be careful here to
    * interact faithfully with the garbage collector.  The function
    * starts off with a (GC (bytes, pointers)) instruction that reserves
    * space on the heap for that many bytes.  The pointers are a list of
    * pointers that are live at the reserve point.  If a GC happens,
    * the garbage collector will update the pointers with new values.
    *)
   let build_fun info blocks f (gflag, ty, vars, body) =
      let pos = string_pos "build_fun" (var_exp_pos f) in

      (* Build the reserve statement *)
      let fun_info = info_lookup_fun info pos f in
      let { fun_closure = f_closure;
            fun_pointers = pointers;
            fun_reserve = size
          } = fun_info
      in

      (* Name of the function body *)
      let s = Symbol.to_string f in
      let v_body = new_symbol_string (s ^ "_body") in

      (* Code the body *)
      let blocks, insts = build_exp info blocks Listbuf.empty body in

      (* Build the block *)
      let blocks = block_of_insts v_body insts :: blocks in

      (* Function header checks the heap *)
      let insts, blocks =
         if size = 0 then
            Listbuf.empty, blocks
         else
            (* Build GC block *)
            let v_gc = new_symbol_string (s ^ "_gc") in
            let pointers = List.map (fun v -> Register v) pointers in
            let insts = Listbuf.empty in
            let insts = Listbuf.add insts (GC (size, pointers)) in
            let insts = Listbuf.add insts (JMP (v_body)) in
            let blocks = block_of_insts v_gc insts :: blocks in

            (* Build the heap checking code *)
            let insts = Listbuf.empty in
            let v_tmp = new_symbol_string "size" in
            let insts = Listbuf.add insts (MOV (Register v_tmp, Register mem_limit)) in
            let insts = Listbuf.add insts (SUB (Register v_tmp, Register mem_next)) in
            let insts = Listbuf.add insts (CMP (Register v_tmp, ImmediateNumber size)) in
            let insts = Listbuf.add insts (JCC (LT, v_gc)) in
               insts, blocks
      in
      let insts = Listbuf.add insts (JMP v_body) in
      let blocks = block_of_insts f insts :: blocks in

      (* Add the closure block *)
      let len = List.length vars in
      let stdargs = get_stdargs len in
      let insts =
         List.fold_left2 (fun insts v1 v2 ->
               Listbuf.add insts (MOV (Register v1, Register v2))) Listbuf.empty vars stdargs
      in
      let insts = Listbuf.add insts (JMP f) in
         block_of_insts f_closure insts :: blocks

   (*
    * Convert a global.
    *)
   let build_init info  (_, init) =
      match init with
         InitString s ->
            GlobalString s
       | InitRecord (_, args) ->
            let args = FieldTable.to_list args in
               GlobalTuple (List.map (fun (_, a) -> build_init info a) args)
       | InitNames names ->
            let args =
               List.fold_left (fun args (v, v_opt) ->
                     let v_opt =
                        match v_opt with
                           Some v ->
                              InitLabel v
                         | None ->
                              InitNumber 0
                     in
                        v_opt :: InitLabel v :: args) [] names
            in
               GlobalTuple (List.rev (InitNumber 0 :: InitNumber 0 :: args))

   (************************************************************************
    * GENERATE ASSEMBLY
    ************************************************************************)

   let build_prog prog =
      let { prog_tynames = tynames;
            prog_funs = funs;
            prog_main = v_main;
            prog_globals = globals
          } = prog
      in

      (* Build an environment *)
      let info = build_info prog in

      (* Translate the globals *)
      let globals = SymbolTable.map (build_init info) globals in

      (* Build the blocks for the functions *)
      let blocks =
         SymbolTable.fold (fun blocks f def ->
               build_fun info blocks f def) [] funs
      in

      (* Save the pointer lists fields *)
      let globals =
         SymbolTable.fold (fun globals _ (v, pointers) ->
               let pointers = List.rev_map (fun i -> InitNumber i) (-1 :: pointers) in
                  SymbolTable.add globals v (GlobalTuple pointers)) globals info.info_rttd
      in

      (* Save the tynames *)
      let globals =
         SymbolTable.fold (fun globals v _ ->
               SymbolTable.add globals v (GlobalString (string_of_symbol v))) globals tynames
      in

      (* Get the name of the closure for the main function *)
      let pos = string_pos "build_prog" (var_exp_pos v_main) in
      let { fun_closure = v_main } = info_lookup_fun info pos v_main in
         { asm_globals = globals;
           asm_blocks = blocks;
           asm_main = v_main
         }
end

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
