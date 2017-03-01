(*
 * Convert from the IR to the IR2.
 * This includes CPS conversion and class elimination.
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
open Symbol
open Field_table

open Fj_fir
open Fj_fir_env
open Fj_fir_exn
open Fj_fir_pos
open Fj_fir_type
open Fj_fir_check
open Fj_fir_state
open Fj_fir_standardize

module Pos = MakePos (struct let name = "Fj_fir_ir" end)
open Pos

(************************************************************************
 * INFO
 ************************************************************************)

(*
 * Default exception function name.
 *)
let f_cont = new_symbol_string "cont"
let f_exnh = new_symbol_string "exnh"

(*
 * Some type definitions.
 *)
let v_exnh = new_symbol_string "ty_exnh"
let ty_exnh = TyId v_exnh

(*
 * For each label, we record whether it is a class method,
 * or class field.  For the methods, we also
 * record the vma type.
 *)
type label =
   ClassMethod
 | ObjectField

(*
 * Program info.
 *)
type info =
   { info_globals    : (ty * init) SymbolTable.t;
     info_tynames    : ty SymbolTable.t;
     info_types      : ty SymbolTable.t;
     info_funs       : fun_info SymbolTable.t;
     info_labels     : label SymbolTable.t;
     info_classes    : field_atoms SymbolTable.t;
     info_methods    : (ty * field_types) SymbolTable.t;
     info_object     : ty
   }

(*
 * Empty info.
 *)
let info_empty v_object =
   (* Add the exception type *)
   let ty_object = TyId v_object in
   let types = SymbolTable.add SymbolTable.empty v_exnh (TyFun ([ty_object], TyUnit)) in
      { info_globals = SymbolTable.empty;
        info_tynames = SymbolTable.empty;
        info_types = types;
        info_funs = SymbolTable.empty;
        info_labels = SymbolTable.empty;
        info_classes = SymbolTable.empty;
        info_methods = SymbolTable.empty;
        info_object = ty_object
      }

(*
 * Add a global.
 *)
let info_add_global info v init =
   { info with info_globals = SymbolTable.add info.info_globals v init }

(*
 * Add a type name.
 *)
let info_add_typename info v ty =
   { info with info_tynames = SymbolTable.add info.info_tynames v ty }

(*
 * Add a type.
 *)
let info_add_type info v ty =
   { info with info_types = SymbolTable.add info.info_types v ty }

(*
 * Add a function.
 *)
let info_add_fun info f def =
   { info with info_funs = SymbolTable.add info.info_funs f def }

(*
 * Add the class info.
 *)
let info_add_class info cname args =
   { info with info_classes = SymbolTable.add info.info_classes cname args }

let info_lookup_class info pos cname =
   try SymbolTable.find info.info_classes cname with
      Not_found ->
         raise (FirException (pos, UnboundVar cname))

let is_class_name info cname =
   SymbolTable.mem info.info_classes cname

(*
 * Add method definitions for classes.
 *)
let info_add_methods info iname methods =
   { info with info_methods = SymbolTable.add info.info_methods iname methods }

let info_lookup_methods info pos iname =
   try SymbolTable.find info.info_methods iname with
      Not_found ->
         raise (FirException (pos, UnboundVar iname))

(*
 * Save info about the label types.
 *)
let info_lookup_label info pos label =
   try SymbolTable.find info.info_labels label with
      Not_found ->
         raise (FirException (pos, StringVarError ("unbound label", label)))

let info_add_labels info linfo fields =
   let labels =
      List.fold_left (fun labels (_, v, _) ->
            SymbolTable.add labels v linfo) info.info_labels fields
   in
      { info with info_labels = labels }

(************************************************************************
 * TYPE CONVERSION
 ************************************************************************)

(*
 * Convert a type definition.
 *)
let rec build_type info ty =
   match ty with
      Fj_ir.TyUnit ->
         TyUnit
    | Fj_ir.TyNil ->
         TyNil
    | Fj_ir.TyBool ->
         TyBool
    | Fj_ir.TyChar ->
         TyChar
    | Fj_ir.TyString ->
         TyString
    | Fj_ir.TyInt ->
         TyInt
    | Fj_ir.TyFloat ->
         TyFloat
    | Fj_ir.TyArray ty ->
         let ty =
            match build_type info ty with
               TyId _ ->
                  info.info_object
             | ty ->
                  ty
         in
            TyArray ty
    | Fj_ir.TyFun (ty_vars, ty_res) ->
         let ty_cont = TyFun ([build_type info ty_res], TyUnit) in
         let ty_vars = List.map (build_type info) ty_vars in
            TyFun (ty_cont :: ty_exnh :: ty_vars, TyUnit)
    | Fj_ir.TyMethod (ty_this, ty_vars, ty_res) ->
         let ty_cont = TyFun ([build_type info ty_res], TyUnit) in
         let ty_this = build_type info ty_this in
         let ty_vars = List.map (build_type info) ty_vars in
            TyMethod (ty_this, ty_cont :: ty_exnh :: ty_vars, TyUnit)
    | Fj_ir.TyObject id ->
         TyId id

(*
 * This is the same, except that it does not add extra
 * cont/exnh args to the toplevel function.
 *)
let build_fun_type info ty =
   match ty with
      Fj_ir.TyFun (ty_vars, ty_res) ->
         let ty_vars = List.map (build_type info) ty_vars in
         let ty_res = build_type info ty_res in
            TyFun (ty_vars, ty_res)
    | _ ->
         build_type info ty

(*
 * Convert a class definition.
 *)
let build_class_def info pos cname cinfo =
   let pos = string_pos "build_class_def" pos in
   let { Fj_ir.class_parents = parents;
         Fj_ir.class_methods = methods;
         Fj_ir.class_fields = fields
       } = cinfo
   in

   (* Get the string name of the clas, so we can make smart names *)
   let s = Symbol.to_string cname in

   (* Collect the coercions *)
   let names = List.map (fun v -> v, None, None) (cname :: parents) in
   let ty_names = TyNames (List.map (fun (v, _, ty) -> v, ty) names) in
   let names = List.map (fun (v, v_vma, _) -> v, v_vma) names in
   let v_names = new_symbol_string (s ^ "_names") in

   let v_ty_names = new_symbol v_names in
   let info = info_add_type info v_ty_names ty_names in
   let ty_names = TyId v_ty_names in

   let info = info_add_global info v_names (ty_names, InitNames names) in

   (* Collect the fields *)
   let fields =
      try FieldMTable.to_list fields with
         Failure s ->
            raise (FirException (pos, StringError s))
   in
   let info = info_add_labels info ObjectField fields in
   let fields = List.map (fun (v1, v2, ty) -> v1, v2, build_type info ty) fields in

   (* Collect the methods *)
   let methods =
      try FieldMTable.to_list methods with
         Failure s ->
            raise (FirException (pos, StringError s))
   in

   (* Define the VMA for the class *)
   let ty_class = FieldTable.empty in
   let ty_class = FieldTable.add ty_class names_var ty_names in
   let ty_class =
      List.fold_left (fun ty_class (_, f, ty) ->
            FieldTable.add ty_class f (build_type info ty)) ty_class methods
   in
   let ty_class = TyRecord (RecordClass, ty_class) in
   let v_ty_class = new_symbol_string (s ^ "_classtype") in
   let info = info_add_type info v_ty_class ty_class in
   let ty_class = TyId v_ty_class in

   (* Method label info *)
   let info = info_add_labels info ClassMethod methods in

   (* Define the class global *)
   let init = FieldTable.empty in
   let init = FieldTable.add init names_var (AtomVar v_names) in
   let init =
      List.fold_left (fun init (_, f, _) ->
            FieldTable.add init f (AtomVar f)) init methods
   in
   let init = InitRecord (RecordClass, init) in
   let v_class = new_symbol_string (s ^ "_class") in
   let info = info_add_global info v_class (ty_class, init) in

   (* Define the object type *)
   let ty_object = FieldTable.empty in
   let ty_object = FieldTable.add ty_object class_var ty_class in
   let ty_object =
      List.fold_left (fun ty_object (_, v, ty) ->
            FieldTable.add ty_object v ty) ty_object fields
   in
   let ty_object = TyRecord (RecordObject, ty_object) in
   let v_ty_object = new_symbol_string (s ^ "_objtype") in
   let info = info_add_type info v_ty_object ty_object in
   let ty_object = TyId v_ty_object in

   (* Add the class initializer *)
   let args = FieldTable.empty in
   let args = FieldTable.add args class_var (AtomVar v_class) in
   let args =
      List.fold_left (fun args (_, v, ty) ->
            FieldTable.add args v (default_atom ty)) args fields
   in
   let info = info_add_class info cname args in

   (* Now we can add the class VMA *)
   let info = info_add_typename info cname (TyId cname) in
      info_add_type info cname ty_object

(*
 * Convert class type definitions.
 *)
let build_class_tydef info v tydef =
   let pos = string_pos "build_class_tydef" (var_exp_pos v) in
      match tydef with
         Fj_ir.TyDefClass (Some cinfo) ->
            build_class_def info pos v cinfo
       | Fj_ir.TyDefClass None ->
            info

(************************************************************************
 * EXPRESSION CONVERSION
 ************************************************************************)

(*
 * Convert an atom.
 *)
let build_atom a =
   match a with
      Fj_ir.AtomUnit ->
         AtomUnit
    | Fj_ir.AtomNil ->
         AtomNil
    | Fj_ir.AtomBool b ->
         AtomBool b
    | Fj_ir.AtomChar c ->
         AtomChar c
    | Fj_ir.AtomInt i ->
         AtomInt i
    | Fj_ir.AtomFloat x ->
         AtomFloat x
    | Fj_ir.AtomVar v ->
         AtomVar v

(*
 * Build an expression.
 *)
let rec build_exp env info e =
   let pos = string_pos "build_exp" (ir_exp_pos e) in
      match e with
         Fj_ir.LetFuns (funs, e) ->
            build_funs_exp env info pos funs e
       | Fj_ir.LetVar (v, ty, a, e) ->
            build_var_exp env info pos v ty a e
       | Fj_ir.LetAtom (v, ty, a, e) ->
            build_atom_exp env info pos v ty a e
       | Fj_ir.LetUnop (v, ty, op, a, e) ->
            build_unop_exp env info pos v ty op a e
       | Fj_ir.LetBinop (v, ty, op, a1, a2, e) ->
            build_binop_exp env info pos v ty op a1 a2 e
       | Fj_ir.LetApply (v, ty, f, args, e) ->
            build_apply_exp env info pos v ty f args e
       | Fj_ir.LetApplyMethod (v, ty, f, a, args, e) ->
            build_apply_method_exp env info pos v ty f a args e
       | Fj_ir.LetExt (v, ty1, s, ty2, args, e) ->
            build_ext_exp env info pos v ty1 s ty2 args e
       | Fj_ir.TailCall (f, args) ->
            build_tailcall_exp env info pos f args
       | Fj_ir.Return (f, a) ->
            build_return_exp env info pos f a
       | Fj_ir.IfThenElse (a, e1, e2) ->
            build_if_exp env info pos a e1 e2
       | Fj_ir.Try (e1, v, e2) ->
            build_try_exp env info pos e1 v e2
       | Fj_ir.Raise a ->
            build_raise_exp env info pos a
       | Fj_ir.TypeCase (a, cases, e) ->
            build_typecase_exp env info pos a cases e
       | Fj_ir.SetVar (v, ty, a, e) ->
            build_setvar_exp env info pos v ty a e
       | Fj_ir.LetSubscript (v, ty, a1, a2, e) ->
            build_subscript_exp env info pos v ty a1 a2 e
       | Fj_ir.SetSubscript (a1, a2, ty, a3, e) ->
            build_set_subscript_exp env info pos a1 a2 ty a3 e
       | Fj_ir.LetProject (v, ty, a, label, e) ->
            build_project_exp env info pos v ty a label e
       | Fj_ir.SetProject (a1, label, ty, a2, e) ->
            build_set_project_exp env info pos a1 label ty a2 e
       | Fj_ir.LetString (v, s, e) ->
            build_string_exp env info pos v s e
       | Fj_ir.LetArray (v, ty, args, a, e) ->
            build_array_exp env info pos v ty args a e
       | Fj_ir.LetObject (v, ty, cname, e) ->
            build_object_exp env info pos v ty cname e

(*
 * Build a function.
 *)
and build_fun env info pos (gflag, ty, vars, body) =
   let pos = string_pos "build_fundef" pos in
   let gflag, ty, vars =
      match gflag with
         Fj_ir.FunLocalClass ->
            (* Local functions do not get cont/exnh arguments *)
            let ty = build_fun_type info ty in
               FunLocalClass, ty, vars
       | Fj_ir.FunGlobalClass ->
            let ty = build_type info ty in
            let vars = f_cont :: f_exnh :: vars in
               FunGlobalClass, ty, vars
       | Fj_ir.FunMethodClass ->
            let ty = build_type info ty in
            let vars =
               match vars with
                  v_this :: vars ->
                     v_this :: f_cont :: f_exnh :: vars
                | [] ->
                     raise (FirException (pos, StringError "method has no arguments"))
            in
               FunGlobalClass, ty, vars
   in

   (* Add function vars *)
   let ty_vars, _ = dest_fun_or_method_type env pos ty in
   let env = List.fold_left2 env_add_var env vars ty_vars in
   let info, body = build_exp env info body in
      info, (gflag, ty, vars, body)

and build_funs_exp env info pos funs e =
   let pos = string_pos "build_funs_exp" pos in
   let info, funs =
      List.fold_left (fun (info, funs) (f, def) ->
            let info, def = build_fun env info pos def in
            let funs = (f, def) :: funs in
               info, funs) (info, []) funs
   in
   let info, e = build_exp env info e in
   let e = LetFuns (List.rev funs, e) in
      info, e

(*
 * A variable declaration.
 *)
and build_var_exp env info pos v ty a e =
   let pos = string_pos "build_var_exp" pos in
   let ty = build_type info ty in
   let a = build_atom a in
   let env = env_add_var env v ty in
   let info, e = build_exp env info e in
      info, LetVar (v, ty, a, e)

(*
 * A variable declaration.
 *)
and build_atom_exp env info pos v ty a e =
   let pos = string_pos "build_atom_exp" pos in
   let ty = build_type info ty in
   let a = build_atom a in
   let env = env_add_var env v ty in
   let info, e = build_exp env info e in
      info, LetAtom (v, ty, a, e)

(*
 * Unary operators.
 *)
and build_unop_exp env info pos v ty op a e =
   let pos = string_pos "build_unop_exp" pos in
   let ty = build_type info ty in
   let a = build_atom a in
   let env = env_add_var env v ty in
   let info, e = build_exp env info e in
   let e =
      match op with
         Fj_ir.UMinusIntOp ->
            LetUnop (v, ty, UMinusIntOp, a, e)
       | Fj_ir.UMinusFloatOp ->
            LetUnop (v, ty, UMinusFloatOp, a, e)
       | Fj_ir.UNotIntOp ->
            LetUnop (v, ty, UNotIntOp, a, e)
       | Fj_ir.UNotBoolOp ->
            LetUnop (v, ty, UNotBoolOp, a, e)
       | Fj_ir.UIntOfChar ->
            LetUnop (v, ty, UCharOfInt, a, e)
       | Fj_ir.UCharOfInt ->
            LetUnop (v, ty, UIntOfChar, a, e)
       | Fj_ir.UIntOfFloat ->
            LetUnop (v, ty, UIntOfFloat, a, e)
       | Fj_ir.UFloatOfInt ->
            LetUnop (v, ty, UFloatOfInt, a, e)
   in
      info, e

(*
 * Binary operators.
 *)
and build_binop_exp env info pos v ty op a1 a2 e =
   let pos = string_pos "build_binop_exp" pos in
   let ty = build_type info ty in
   let a1 = build_atom a1 in
   let a2 = build_atom a2 in
   let env = env_add_var env v ty in
   let info, e = build_exp env info e in
   let op =
      match op with
         (* Integers *)
         Fj_ir.AddIntOp -> AddIntOp
       | Fj_ir.SubIntOp -> SubIntOp
       | Fj_ir.MulIntOp -> MulIntOp
       | Fj_ir.DivIntOp -> DivIntOp
       | Fj_ir.RemIntOp -> RemIntOp
       | Fj_ir.AndIntOp -> AndIntOp
       | Fj_ir.OrIntOp -> OrIntOp
       | Fj_ir.LslIntOp -> LslIntOp
       | Fj_ir.LsrIntOp -> LsrIntOp
       | Fj_ir.AsrIntOp -> AsrIntOp
       | Fj_ir.XorIntOp -> XorIntOp
       | Fj_ir.EqIntOp -> EqIntOp
       | Fj_ir.NeqIntOp -> NeqIntOp
       | Fj_ir.LeIntOp -> LeIntOp
       | Fj_ir.LtIntOp -> LtIntOp
       | Fj_ir.GtIntOp -> GtIntOp
       | Fj_ir.GeIntOp -> GeIntOp

         (* Floats *)
       | Fj_ir.AddFloatOp -> AddFloatOp
       | Fj_ir.SubFloatOp -> SubFloatOp
       | Fj_ir.MulFloatOp -> MulFloatOp
       | Fj_ir.DivFloatOp -> DivFloatOp
       | Fj_ir.RemFloatOp -> RemFloatOp
       | Fj_ir.EqFloatOp -> EqFloatOp
       | Fj_ir.NeqFloatOp -> NeqFloatOp
       | Fj_ir.LeFloatOp -> LeFloatOp
       | Fj_ir.LtFloatOp -> LtFloatOp
       | Fj_ir.GtFloatOp -> GtFloatOp
       | Fj_ir.GeFloatOp -> GeFloatOp

         (* Booleans *)
       | Fj_ir.EqBoolOp  -> EqBoolOp
       | Fj_ir.NeqBoolOp -> NeqBoolOp
   in
      info, LetBinop (v, ty, op, a1, a2, e)

(*
 * CPS convert an application.
 *)
and build_apply_exp env info pos v ty f args e =
   let pos = string_pos "build_apply_exp" pos in
   let ty = build_type info ty in
   let env = env_add_var env v ty in
   let args = List.map build_atom args in

   (* Wrap the rest in a contination function *)
   let f_cont = new_symbol_string (Symbol.to_string f ^ "_cont") in
   let info, e = build_exp env info e in
   let e =
      LetFuns ([f_cont, (FunContClass, TyFun ([ty], TyUnit), [v], e)],
      TailCall (f, AtomVar f_cont :: AtomVar f_exnh :: args))
   in
      info, e

and build_apply_method_exp env info pos v ty f a args e =
   let pos = string_pos "build_apply_exp" pos in
   let ty = build_type info ty in
   let env = env_add_var env v ty in
   let a = build_atom a in
   let args = List.map build_atom args in

   (* Wrap the rest in a contination function *)
   let f_cont = new_symbol_string (Symbol.to_string f ^ "_cont") in
   let info, e = build_exp env info e in
   let e =
      LetFuns ([f_cont, (FunContClass, TyFun ([ty], TyUnit), [v], e)],
      MethodCall (f, a, AtomVar f_cont :: AtomVar f_exnh :: args))
   in
      info, e

(*
 * External call.
 *)
and build_ext_exp env info pos v ty1 s ty2 args e =
   let pos = string_pos "build_ext_exp" pos in
   let ty1 = build_fun_type info ty1 in
   let ty2 = build_fun_type info ty2 in
   let env = env_add_var env v ty1 in
   let args = List.map build_atom args in
   let info, e = build_exp env info e in
      info, LetExt (v, ty1, s, ty2, args, e)

(*
 * Tailcall.
 *)
and build_tailcall_exp env info pos f args =
   let pos = string_pos "build_ext_exp" pos in
   let args = List.map build_atom args in
      info, TailCall (f, args)

(*
 * Instead of returning, call the continuation.
 *)
and build_return_exp env info pos f a =
   let pos = string_pos "build_return_exp" pos in
   let a = build_atom a in
      info, TailCall (f_cont, [a])

(*
 * Conditional.
 *)
and build_if_exp env info pos a e1 e2 =
   let pos = string_pos "build_if_exp" pos in
   let a = build_atom a in
   let info, e1 = build_exp env info e1 in
   let info, e2 = build_exp env info e2 in
      info, IfThenElse (a, e1, e2)

(*
 * Exceptions.
 *)
and build_try_exp env info pos e1 v e2 =
   let pos = string_pos "build_try_exp" pos in
   let env' = env_add_var env v info.info_object in
   let info, e1 = build_exp env info e1 in
   let info, e2 = build_exp env info e2 in

   (* Add a new exception handler *)
   let f_exnh' = new_symbol f_exnh in
   let ty_object = info.info_object in
   let e =
      LetFuns ([f_exnh', (FunContClass, TyFun ([ty_object], TyUnit), [v], e2)],
      LetFuns ([f_exnh, (FunContClass, TyFun ([ty_object], TyUnit), [v], TailCall (f_exnh', [AtomVar v]))],
      e1))
   in
      info, e

(*
 * When an exception is raised, call the exception handler with
 * the argument.
 *)
and build_raise_exp env info pos a =
   let pos = string_pos "build_raise_exp" pos in
   let a = build_atom a in
      info, TailCall (f_exnh, [a])

(*
 * SetVar is unchanged.
 *)
and build_setvar_exp env info pos v ty a e =
   let pos = string_pos "build_setvar_exp" pos in
   let ty = build_type info ty in
   let a = build_atom a in
   let info, e = build_exp env info e in
      info, SetVar (v, ty, a, e)

(*
 * Subscripting.
 *)
and build_subscript_exp env info pos v ty a1 a2 e =
   let pos = string_pos "build_subscript_exp" pos in
   let ty = build_type info ty in
   let a1 = build_atom a1 in
   let a2 = build_atom a2 in
   let env = env_add_var env v ty in
   let info, e = build_exp env info e in
      info, LetSubscript (v, ty, a1, a2, e)

and build_set_subscript_exp env info pos a1 a2 ty a3 e =
   let pos = string_pos "build_set_subscript_exp" pos in
   let ty = build_type info ty in
   let a1 = build_atom a1 in
   let a2 = build_atom a2 in
   let a3 = build_atom a3 in
   let info, e = build_exp env info e in
      info, SetSubscript (a1, a2, ty, a3, e)

(*
 * Convert projections to subscripts.
 *)
and build_project_exp env info pos v ty a label e =
   let pos = string_pos "build_project_exp" pos in
   let ty = build_type info ty in
   let a = build_atom a in
   let env = env_add_var env v ty in
   let info, e = build_exp env info e in
   let e =
      match info_lookup_label info pos label with
         ObjectField ->
            LetProject (v, ty, a, label, e)
       | ClassMethod ->
            let v' = new_symbol v in
            let ty_object = type_of_atom env pos a in
            let ty_fields = dest_object_type env pos ty_object in
            let ty_class = type_of_field env pos ty_fields class_var in
               LetProject (v', ty_class, a, class_var,
               LetProject (v, ty, AtomVar v', label,
               e))
   in
      info, e

and build_set_project_exp env info pos a1 label ty a2 e =
   let pos = string_pos "build_set_project_exp" pos in
   let ty = build_type info ty in
   let a1 = build_atom a1 in
   let a2 = build_atom a2 in
   let info, e = build_exp env info e in
   let e =
      match info_lookup_label info pos label with
         ObjectField ->
            SetProject (a1, label, ty, a2, e)
       | ClassMethod ->
            raise (FirException (pos, StringVarError ("field is immutable", label)))
   in
      info, e

(*
 * String allocation.
 * Add the string to the globals.
 *)
and build_string_exp env info pos v s e =
   let pos = string_pos "build_string_exp" pos in
   let v_string = new_symbol v in
   let info = info_add_global info v_string (TyString, InitString s) in
   let env = env_add_var env v TyString in
   let info, e = build_exp env info e in
      info, LetAtom (v, TyString, AtomVar v_string, e)

(*
 * Array allocation.
 *)
and build_array_exp env info pos v ty args a e =
   let pos = string_pos "build_array_exp" pos in
   let ty = build_type info ty in
   let args = List.map build_atom args in
   let a = build_atom a in
   let env = env_add_var env v ty in
   let info, e = build_exp env info e in
      info, LetArray (v, ty, args, a, e)

(*
 * Object allocation.
 * Look up the class in the environment.
 *)
and build_object_exp env info pos v ty cname e =
   let pos = string_pos "build_object_exp" pos in
   let ty = build_type info ty in
   let env = env_add_var env v ty in
   let info, e = build_exp env info e in
   let args = info_lookup_class info pos cname in
      info, LetRecord (v, ty, RecordObject, args, e)

(*
 * Typecase.
 *)
and build_typecase_exp env info pos a cases e =
   let pos = string_pos "build_typecase_exp" pos in
   let a = build_atom a in
   let info, e = build_exp env info e in
   let rec build_cases info cases =
      match cases with
         (cname, v, body) :: cases ->
            let env = env_add_var env v (TyId cname) in
            let info, body = build_exp env info body in
            let info, e = build_cases info cases in
               info, IfType (a, cname, v, body, e)
       | [] ->
            info, e
   in
      build_cases info cases

(*
 * Build a function.
 *)
let build_fundef env info f def =
   let pos = string_pos "build_fundef" (var_exp_pos f) in
   let info, def = build_fun env info pos def in
      info_add_fun info f def

(*
 * Convert a program.
 *)
let build_prog prog =
   let { Fj_ir.prog_types = types;
         Fj_ir.prog_funs = funs;
         Fj_ir.prog_main = main;
         Fj_ir.prog_object = v_object
       } = prog
   in
   let info = info_empty v_object in

   (* Convert the classes *)
   let info =
      SymbolTable.fold (fun info v tydef ->
            build_class_tydef info v tydef) info types
   in

   (* Initial environment *)
   let env = SymbolTable.fold env_add_type env_empty info.info_types in

   (* Convert all the funs *)
   let info =
      SymbolTable.fold (fun info f def ->
            build_fundef env info f def) info funs
   in

   (* Collect the info into a program *)
   let { info_globals = globals;
         info_tynames = tynames;
         info_types = types;
         info_funs = funs
       } = info
   in
   let prog =
      { prog_types = types;
        prog_tynames = tynames;
        prog_funs = funs;
        prog_main = main;
        prog_object = v_object;
        prog_globals = globals
      }
   in
      standardize_prog prog

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
