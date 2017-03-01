(*
 * Generate IR code from AST expressions and types.
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
open Printf
open Debug
open Symbol
open Field_table

open Fj_ir
open Fj_ir_env
open Fj_ir_exn
open Fj_ir_pos
open Fj_ir_type
open Fj_ir_check
open Fj_ir_state

module Pos = MakePos (struct let name = "Fj_ir_exp" end)
open Pos

(*
 * This is used to define "lvalues": assignable expressions.
 *)
type lvalue =
   LValueVar of var * ty
 | LValueProject of atom * var * ty
 | LValueSubscript of atom * atom * ty

(************************************************************************
 * TYPES
 ************************************************************************)

(*
 * Build a type.
 *)
let rec build_type env pos ty =
   match ty with
      Fj_ast.TypeVoid _ ->
         TyUnit
    | Fj_ast.TypeBool _ ->
         TyBool
    | Fj_ast.TypeChar _ ->
         TyChar
    | Fj_ast.TypeInt _ ->
         TyInt
    | Fj_ast.TypeFloat _ ->
         TyFloat
    | Fj_ast.TypeId (id, _) ->
         env_lookup_type env pos id
    | Fj_ast.TypeArray (ty, _) ->
         TyArray (build_type env pos ty)
    | Fj_ast.TypeFun (ty_args, ty_res, _) ->
         TyFun (List.map (build_type env pos) ty_args, build_type env pos ty_res)

(************************************************************************
 * FIELD SELECTION
 ************************************************************************)

(*
 * Find a field in a class_info.
 *)
let find_class_field pos info v =
   try List.hd (FieldMTable.find_ext info.class_fields v) with
      Not_found ->
         raise (IrException (pos, UnboundVar v))

(*
 * Find the field in an object.
 *)
let find_object_field env pos a label =
   let pos = string_pos "find_object_field" pos in
   let ty = type_of_atom env pos a in
   let cname = dest_object_type env pos ty in
   let info = dest_class_type env pos cname in
      find_class_field pos info label

(*
 * Find a field in the current object.
 *)
let find_current_object_field env pos label =
   let _, info = env_get_current_class env pos in
      find_class_field pos info label

(************************************************************************
 * OVERLOADING
 ************************************************************************)

(*
 * The "metric" for a function application is
 * the number of number and class coercions.
 *)
let compute_apply_metric env pos ty_args1 ty_args2 =
   (* Compare a single argument *)
   let rec compare_arg num_count class_count ty1 ty2 =
      if equal_types env pos ty1 ty2 then
         Some (num_count, class_count)
      else
         match ty1, ty2 with
            TyInt, TyFloat
          | TyFloat, TyInt
          | TyInt, TyChar
          | TyChar, TyInt ->
               Some (succ num_count, class_count)
          | TyObject cname1, TyObject cname2 ->
               if is_parent_class env pos cname1 cname2 || is_parent_class env pos cname2 cname1 then
                  Some (num_count, succ class_count)
               else
                  None
          | TyArray ty1, TyArray ty2 ->
               (* This is Java, but the semantics of arrays are problematic *)
               compare_arg num_count class_count ty1 ty2
          | _ ->
               None
   in

   (* Search all the args *)
   let rec compare num_count class_count ty_args1 ty_args2 =
      match ty_args1, ty_args2 with
         ty1 :: ty_args1, ty2 :: ty_args2 ->
            let ty1 = expand_type env pos ty1 in
            let ty2 = expand_type env pos ty2 in
               (match compare_arg num_count class_count ty1 ty2 with
                   Some (num_count, class_count) ->
                      compare num_count class_count ty_args1 ty_args2
                 | None ->
                      None)
       | [], [] ->
            Some (num_count, class_count)
       | _ ->
            raise (Invalid_argument "compute_apply_metric")
   in
   let len1 = List.length ty_args1 in
   let len2 = List.length ty_args2 in
      if len1 <> len2 then
         None
      else
         compare 0 0 ty_args1 ty_args2

(*
 * Choose the better method.
 *)
let merge_apply_choice best metric f' ty =
   match best, metric with
      Some (num_count1, class_count1, _, _), Some (num_count2, class_count2) ->
         if class_count2 < class_count1 || (class_count2 = class_count1 && num_count1 < num_count2) then
            Some (num_count2, class_count2, f', ty)
         else
            best
    | None, Some (num_count2, class_count2) ->
         Some (num_count2, class_count2, f', ty)
    | _ ->
         best

(*
 * Search for the best function out of a list of functions.
 *)
let find_best_function env pos f args funs =
   (* Collect the types of all the args *)
   let pos = string_pos "find_best_function" pos in
   let ty_args = List.map (type_of_atom env pos) args in

   (* Method searching *)
   let rec search best = function
      (f', ty) :: funs ->
         if is_fun_type env pos ty then
            let ty_args', ty_res = dest_fun_type env pos ty in
            let metric = compute_apply_metric env pos ty_args ty_args' in
            let best = merge_apply_choice best metric f' ty in
               search best funs
         else
            search best funs
    | [] ->
         match best with
            Some (_, _, f', ty) ->
               f', ty
          | None ->
               raise (IrException (pos, OverloadError (f, ty_args, funs)))
   in
      search None funs

(*
 * Search for the best method out of a list of methods.
 * This is like searching for a function, but we ignore the
 * first argument, and choose from a FieldMTable.
 *)
let find_best_method env pos f args methods =
   (* Collect the types of all the args *)
   let pos = string_pos "find_best_method" pos in
   let ty_args = List.map (type_of_atom env pos) args in

   (* Lookup all the choices for the method *)
   let methods =
      try FieldMTable.find_ext methods f with
         Not_found ->
            raise (IrException (pos, UnboundVar f))
   in

   (* Method searching *)
   let rec search best choices = function
      (f', ty) :: methods ->
         if is_method_type env pos ty then
            let ty_this, ty_args', ty_res = dest_method_type env pos ty in
            let choices = (f', ty) :: choices in
            let metric = compute_apply_metric env pos ty_args' ty_args in
            let best = merge_apply_choice best metric f' ty in
               search best choices methods
         else
            search best choices methods
    | [] ->
         match best with
            Some (_, _, f', ty) ->
               f', ty
          | None ->
               raise (IrException (pos, OverloadError (f, ty_args, choices)))
   in
      search None [] methods

(*
 * Find the best method in the current class.
 *)
let find_current_method env pos f args =
   let pos = string_pos "find_current_method" pos in
   let _, info = env_get_current_class env pos in
      find_best_method env pos f args info.class_methods

(*
 * Find the best fit for a method in a class.
 *)
let find_class_method env pos cname f args =
   (* Get the methods for the class *)
   let pos = string_pos "find_class_method" pos in
   let info = dest_class_type env pos cname in
      find_best_method env pos f args info.class_methods

(************************************************************************
 * COERCIONS
 ************************************************************************)

(*
 * Coerce an atom to the given type.
 *)
let coerce_type strict env pos a ty1 cont =
   let pos = string_pos "coerce_type" pos in
   let ty2 = type_of_atom env pos a in
      (* If the types are already equal, don't need to do anything *)
      if equal_types env pos ty1 ty2 then
         cont env a
      else
         (* These are the only other allowed coercions *)
         match expand_type env pos ty1, expand_type env pos ty2 with
            (* Number coercions *)
            TyInt, TyChar ->
               let v = new_symbol_string "int_of_char" in
               let env = env_add_var env v TyInt in
                  LetUnop (v, TyInt, UIntOfChar, a, cont env (AtomVar v))
          | TyChar, TyInt ->
               let v = new_symbol_string "char_of_int" in
               let env = env_add_var env v TyChar in
                  LetUnop (v, TyChar, UCharOfInt, a, cont env (AtomVar v))
          | TyInt, TyFloat ->
               let v = new_symbol_string "int_of_float" in
               let env = env_add_var env v TyInt in
                  LetUnop (v, TyInt, UIntOfFloat, a, cont env (AtomVar v))
          | TyFloat, TyInt ->
               let v = new_symbol_string "float_of_int" in
               let env = env_add_var env v TyFloat in
                  LetUnop (v, TyFloat, UFloatOfInt, a, cont env (AtomVar v))

            (* String coercions *)
          | TyString, TyInt ->
               let v = new_symbol_string "string" in
               let env = env_add_var env v TyString in
                  LetExt (v, TyString, "itoa", TyFun ([TyInt], TyString), [a], cont env (AtomVar v))
          | TyString, TyFloat ->
               let v = new_symbol_string "string" in
               let env = env_add_var env v TyString in
                  LetExt (v, TyString, "ftoa", TyFun ([TyFloat], TyString), [a], cont env (AtomVar v))

            (* Object coercions *)
          | TyObject cname1, TyObject cname2 ->
               let v = new_symbol cname1 in
               let env = env_add_var env v ty1 in
                  if is_parent_class env pos cname1 cname2 then
                     (* No need for a runtime coercion *)
                     LetAtom (v, ty1, a, cont env (AtomVar v))
                  else if is_parent_class env pos cname2 cname1 then
                     (* Insert a runtime coercion *)
                     TypeCase (a, [cname1, v, cont env (AtomVar v)],
                     LetObject (v, ty_object, object_var, Raise (AtomVar v)))
                  else
                     raise (IrException (pos, TypeMismatch (ty1, ty2)))

            (* All other coercions are prohibited *)
          | ty1, ty2 ->
               raise (IrException (pos, TypeMismatch (ty1, ty2)))

(*
 * Simplified forms.
 *)
let coerce_bool env pos a cont =
   coerce_type true env pos a TyBool cont

let coerce_int env pos a cont =
   coerce_type true env pos a TyInt cont

let coerce_ints env pos al cont =
   let rec coerce env al' = function
      a :: al ->
         coerce_int env pos a (fun env a ->
               coerce env (a :: al') al)
    | [] ->
         cont env (List.rev al')
   in
      coerce env [] al

(*
 * Coerce two atoms so the have equal types.
 *)
let coerce_equal env pos a1 a2 cont =
   let ty1 = type_of_atom env pos a1 in
   let ty2 = type_of_atom env pos a2 in
   let ty =
      (* Compute the final type *)
      match expand_type env pos ty1, expand_type env pos ty2 with
         TyString, _
       | _, TyString ->
            TyString
       | TyFloat, _
       | _, TyFloat ->
            TyFloat
       | TyInt, _
       | _, TyInt ->
            TyInt
       | _ ->
            TyBool
   in
      coerce_type true env pos a1 ty (fun env a1 ->
      coerce_type true env pos a2 ty (fun env a2 ->
      cont env ty a1 a2))

(*
 * Make sure we have an array.
 *)
let coerce_array env pos a cont =
   let pos = string_pos "coerce_array" pos in
   let ty = type_of_atom env pos a in
      match expand_type env pos ty with
         TyArray ty ->
            cont env a ty
       | _ ->
            raise (IrException (pos, StringError "not an array"))

(************************************************************************
 * EXPRESSIONS
 ************************************************************************)

(*
 * Build the IR code for a variable.
 *)
let build_var_exp env pos v cont =
   if env_mem_fun env v then
      let f, _ = env_lookup_fun env pos v in
         cont env (AtomVar f)
   else if env_mem_var env v then
      cont env (AtomVar v)
   else
      let label, ty = find_current_object_field env pos v in
      let v' = new_symbol v in
      let env = env_add_var env v' ty in
         LetProject (v', ty, this_atom, label, cont env (AtomVar v'))

(*
 * A unary operation.
 *)
let rec build_unop_exp env pos op e cont =
   let pos = string_pos "build_unop_exp" pos in
      build_exp env e (fun env a ->
            let ty = type_of_atom env pos a in
            let op =
               match op, expand_type env pos ty with
                  Fj_ast.UMinusOp, TyInt ->
                     UMinusIntOp
                | Fj_ast.UMinusOp, TyFloat ->
                     UMinusFloatOp
                | Fj_ast.UBNotOp, TyInt ->
                     UNotIntOp
                | Fj_ast.UNotOp, TyBool ->
                     UNotBoolOp
                | _ ->
                     raise (IrException (pos, TypeError))
            in
            let v = new_symbol_string "unop" in
            let env = env_add_var env v ty in
               LetUnop (v, ty, op, a, cont env (AtomVar v)))

(*
 * A binary operation.
 *)
and build_binop_exp env pos op e1 e2 cont =
   let pos = string_pos "build_binop_exp" pos in
      build_exp env e1 (fun env a1 ->
      build_exp env e2 (fun env a2 ->
      build_binop_atom env pos op a1 a2 cont))

and build_binop_atom env pos op a1 a2 cont =
   let pos = string_pos "build_binop_atom" pos in
      coerce_equal env pos a1 a2 (fun env ty a1 a2 ->
            match op, ty with
               Fj_ast.PlusOp, TyInt ->
                  build_binop env pos TyInt AddIntOp a1 a2 cont
             | Fj_ast.MinusOp, TyInt ->
                  build_binop env pos TyInt SubIntOp a1 a2 cont
             | Fj_ast.TimesOp, TyInt ->
                  build_binop env pos TyInt MulIntOp a1 a2 cont
             | Fj_ast.DivideOp, TyInt ->
                  build_binop env pos TyInt DivIntOp a1 a2 cont
             | Fj_ast.ModOp, TyInt ->
                  build_binop env pos TyInt RemIntOp a1 a2 cont
             | Fj_ast.BAndOp, TyInt ->
                  build_binop env pos TyInt AndIntOp a1 a2 cont
             | Fj_ast.BOrOp, TyInt ->
                  build_binop env pos TyInt OrIntOp a1 a2 cont
             | Fj_ast.BXorOp, TyInt ->
                  build_binop env pos TyInt XorIntOp a1 a2 cont
             | Fj_ast.LslOp, TyInt ->
                  build_binop env pos TyInt LslIntOp a1 a2 cont
             | Fj_ast.LsrOp, TyInt ->
                  build_binop env pos TyInt LsrIntOp a1 a2 cont
             | Fj_ast.AsrOp, TyInt ->
                  build_binop env pos TyInt AsrIntOp a1 a2 cont
             | Fj_ast.EqOp, TyInt ->
                  build_binop env pos TyBool EqIntOp a1 a2 cont
             | Fj_ast.NeqOp, TyInt ->
                  build_binop env pos TyBool NeqIntOp a1 a2 cont
             | Fj_ast.LeOp, TyInt ->
                  build_binop env pos TyBool LeIntOp a1 a2 cont
             | Fj_ast.LtOp, TyInt ->
                  build_binop env pos TyBool LtIntOp a1 a2 cont
             | Fj_ast.GeOp, TyInt ->
                  build_binop env pos TyBool GeIntOp a1 a2 cont
             | Fj_ast.GtOp, TyInt ->
                  build_binop env pos TyBool GtIntOp a1 a2 cont
             | Fj_ast.PlusOp, TyFloat ->
                  build_binop env pos TyFloat AddFloatOp a1 a2 cont
             | Fj_ast.MinusOp, TyFloat ->
                  build_binop env pos TyFloat SubFloatOp a1 a2 cont
             | Fj_ast.TimesOp, TyFloat ->
                  build_binop env pos TyFloat MulFloatOp a1 a2 cont
             | Fj_ast.DivideOp, TyFloat ->
                  build_binop env pos TyFloat DivFloatOp a1 a2 cont
             | Fj_ast.ModOp, TyFloat ->
                  build_binop env pos TyFloat RemFloatOp a1 a2 cont
             | Fj_ast.EqOp, TyFloat ->
                  build_binop env pos TyBool EqFloatOp a1 a2 cont
             | Fj_ast.NeqOp, TyFloat ->
                  build_binop env pos TyBool NeqFloatOp a1 a2 cont
             | Fj_ast.LeOp, TyFloat ->
                  build_binop env pos TyBool LeFloatOp a1 a2 cont
             | Fj_ast.LtOp, TyFloat ->
                  build_binop env pos TyBool LtFloatOp a1 a2 cont
             | Fj_ast.GeOp, TyFloat ->
                  build_binop env pos TyBool GeFloatOp a1 a2 cont
             | Fj_ast.GtOp, TyFloat ->
                  build_binop env pos TyBool GtFloatOp a1 a2 cont
             | Fj_ast.EqOp, TyBool ->
                  build_binop env pos TyBool EqBoolOp a1 a2 cont
             | Fj_ast.NeqOp, TyBool ->
                  build_binop env pos TyBool NeqBoolOp a1 a2 cont
             | Fj_ast.PlusOp, TyString ->
                  let v = new_symbol_string "strcat" in
                  let env = env_add_var env v TyString in
                     LetExt (v, TyString, "strcat", TyFun ([TyString; TyString], TyString), [a1; a2], cont env (AtomVar v))
             | _ ->
                  raise (IrException (pos, TypeError)))

and build_binop env pos ty op a1 a2 cont =
   let pos = string_pos "build_binop" pos in
   let v = new_symbol_string "binop" in
   let env = env_add_var env v ty in
      LetBinop (v, ty, op, a1, a2, cont env (AtomVar v))

(*
 * Assignment.
 * There are several cases:
 *    1. assign to a normal var
 *    2. assign to a field of the current class
 *    3. assign to a field in another class
 *    4. assign to a array element
 *)
and build_assign_exp env pos op e1 e2 cont =
   let pos = string_pos "build_assign_exp" pos in
      build_lvalue env pos e1 (fun env lval1 ->
      build_exp env e2 (fun env a2 ->
            match op with
               Some op ->
                  build_atom_of_lvalue env pos lval1 (fun env a1 ->
                  build_binop_atom env pos op a1 a2 (fun env a3 ->
                  build_assign_lvalue env pos lval1 a3 cont))
             | None ->
                  build_assign_lvalue env pos lval1 a2 cont))

(*
 * A unary-assignment operator.
 *)
and build_uarith_exp env pos op e cont =
   let pos = string_pos "build_uarith_exp" pos in
      build_lvalue env pos e (fun env lval ->
      build_atom_of_lvalue env pos lval (fun env a1 ->
            let ty = type_of_atom env pos a1 in
            let v = new_symbol_string "uarith" in
            let env = env_add_var env v ty in
            let a_var = AtomVar v in
            let op, a_result, a2 =
               match op, ty with
                  Fj_ast.PreIncrOp, TyInt ->
                     AddIntOp, a_var, AtomInt 1
                | Fj_ast.PreDecrOp, TyInt ->
                     SubIntOp, a_var, AtomInt 1
                | Fj_ast.PostIncrOp, TyInt ->
                     AddIntOp, a1, AtomInt 1
                | Fj_ast.PostDecrOp, TyInt ->
                     SubIntOp, a1, AtomInt 1
                | Fj_ast.PreIncrOp, TyFloat ->
                     AddFloatOp, a_var, AtomFloat 1.0
                | Fj_ast.PreDecrOp, TyFloat ->
                     SubFloatOp, a_var, AtomFloat 1.0
                | Fj_ast.PostIncrOp, TyFloat ->
                     AddFloatOp, a1, AtomFloat 1.0
                | Fj_ast.PostDecrOp, TyFloat ->
                     SubFloatOp, a1, AtomFloat 1.0
                | _ ->
                     raise (IrException (pos, TypeError))
            in
               LetBinop (v, ty, op, a1, a2,
               build_assign_lvalue env pos lval a_var (fun env _ ->
                     cont env a_result))))

(*
 * An "lvalue" is an assignable expression.
 *)
and build_lvalue env pos e cont =
   let pos = string_pos "build_lvalue" pos in
      match e with
         Fj_ast.VarExp (v, _) ->
            if env_mem_var env v then
               let ty = env_lookup_var env pos v in
                  cont env (LValueVar (v, ty))
            else
               let label, ty = find_current_object_field env pos v in
                  cont env (LValueProject (this_atom, label, ty))
       | Fj_ast.ProjectExp (e, label, _) ->
            build_exp env e (fun env a ->
                  let label, ty = find_object_field env pos a label in
                     cont env (LValueProject (a, label, ty)))
       | Fj_ast.SubscriptExp (e1, e2, _) ->
            build_exp env e1 (fun env a1 ->
            build_exp env e2 (fun env a2 ->
            coerce_array env pos a1 (fun env a1 ty ->
            coerce_int env pos a2 (fun env a2 ->
                  cont env (LValueSubscript (a1, a2, ty))))))
       | _ ->
            raise (IrException (pos, StringError "not an assignable expression"))

(*
 * Build a value from an lvalue.
 *)
and build_atom_of_lvalue env pos lval cont =
   let pos = string_pos "build_atom_of_lvalue" pos in
      match lval with
         LValueVar (v, _) ->
            cont env (AtomVar v)
       | LValueProject (a, label, ty) ->
            let v = new_symbol_string "proj" in
            let env = env_add_var env v ty in
               LetProject (v, ty, a, label, cont env (AtomVar v))
       | LValueSubscript (a1, a2, ty) ->
            let v = new_symbol_string "subscript" in
            let env = env_add_var env v ty in
               LetSubscript (v, ty, a1, a2, cont env (AtomVar v))

(*
 * Assign an atom to a lvalue.
 *)
and build_assign_lvalue env pos lval a cont =
   let pos = string_pos "build_assign_lvalue" pos in
      match lval with
         LValueVar (v, ty) ->
            coerce_type true env pos a ty (fun env a ->
                  SetVar (v, ty, a, cont env AtomUnit))
       | LValueProject (a', label, ty) ->
            coerce_type true env pos a ty (fun env a ->
                  SetProject (a', label, ty, a, cont env AtomUnit))
       | LValueSubscript (a1, a2, ty) ->
            coerce_type false env pos a ty (fun env a3 ->
                  SetSubscript (a1, a2, ty, a3, cont env AtomUnit))

(*
 * Boolean expression.
 * Evaluation is short-circuit.
 *)
and build_boolop_exp env pos op e1 e2 cont =
   let pos = string_pos "build_boolop_exp" pos in

   (* Place the continuation in a function *)
   let ty_fun = TyFun ([TyBool], TyUnit) in
   let f_cont = new_symbol_string "boolop_cont" in
   let v_cont = new_symbol_string "test" in
   let body_cont =
      let env = env_add_var env v_cont TyBool in
         cont env (AtomVar v_cont)
   in

   (* Call the function with specific args *)
   let body_true = TailCall (f_cont, [AtomBool true]) in
   let body_false = TailCall (f_cont, [AtomBool false]) in

   (* Call the function with the second expr as the arg *)
   let body2 =
      build_exp env e2 (fun env a2 ->
      coerce_bool env pos a2 (fun env a2 ->
            TailCall (f_cont, [a2])))
   in

   (* Evaluate the test *)
   let body =
      (* Evaluate the first expr *)
      build_exp env e1 (fun env a1 ->
      coerce_bool env pos a1 (fun env a1 ->
            match op with
               Fj_ast.LAndOp ->
                  IfThenElse (a1, body2, body_false)
             | Fj_ast.LOrOp ->
                  IfThenElse (a1, body_true, body2)))
   in
      (* Add the function def *)
      LetFuns ([f_cont, (FunLocalClass, ty_fun, [v_cont], body_cont)], body)

(*
 * Array subscripting.
 *)
and build_subscript_exp env pos e1 e2 cont =
   let pos = string_pos "build_subscript_exp" pos in
      build_exp env e1 (fun env a1 ->
      build_exp env e2 (fun env a2 ->
      coerce_array env pos a1 (fun env a1 ty ->
      coerce_int env pos a2 (fun env a2 ->
            let v = new_symbol_string "sub" in
            let env = env_add_var env v ty in
               if is_object_type env pos ty then
                  let name = dest_object_type env pos ty in
                  let v2 = new_symbol_string "object" in
                  let env = env_add_var env v2 ty_object in
                     LetSubscript (v2, ty_object, a1, a2,
                     TypeCase (AtomVar v2, [name, v, cont env (AtomVar v)],
                     LetObject (v2, ty_object, object_var, Raise (AtomVar v2))))
               else
                  LetSubscript (v, ty, a1, a2, cont env (AtomVar v))))))

(*
 * Field projection.
 *)
and build_project_exp env pos e label cont =
   let pos = string_pos "build_project_exp" pos in
      build_exp env e (fun env a ->
            let label, ty = find_object_field env pos a label in
            let v = new_symbol_string "proj" in
            let env = env_add_var env v ty in
               LetProject (v, ty, a, label, cont env (AtomVar v)))

(*
 * Build a conditional.
 * The continuation is wrapped in a function.
 *)
and build_if_exp env pos e1 e2 e3 cont =
   let pos = string_pos "build_if_exp" pos in

   (* Wrap the rest in a function *)
   let ty_fun = TyFun ([], TyUnit) in
   let f_cont = new_symbol_string "if_cont" in
   let f_body = cont env AtomUnit in
   let f_call = TailCall (f_cont, []) in

   (* Build the branches *)
   let body2 =
      build_exp env e2 (fun env _ -> f_call)
   in
   let body3 =
      match e3 with
         Some e3 ->
            build_exp env e3 (fun env _ -> f_call)
       | None ->
            f_call
   in

   (* Build the test *)
   let body =
      build_exp env e1 (fun env a1 ->
      coerce_bool env pos a1 (fun env a1 ->
            IfThenElse (a1, body2, body3)))
   in
      (* Add the function *)
      LetFuns ([f_cont, (FunLocalClass, ty_fun, [], f_body)], body)

(*
 * A while loop.
 *)
and build_while_exp env pos e1 e2 cont =
   let pos = string_pos "build_while_exp" pos in

   (* Wrap the rest in a function *)
   let ty_break = TyFun ([], TyUnit) in
   let break_f = new_symbol_string "break" in
   let break_body = cont env AtomUnit in
   let break_call = TailCall (break_f, []) in
   let env = env_set_break env break_f in

   (* The while loop function *)
   let ty_loop = TyFun ([], TyUnit) in
   let loop_f = new_symbol_string "while" in
   let loop_call = TailCall (loop_f, []) in

   (* This is the code to run if the loop test is true *)
   let true_body =
      build_exp env e2 (fun env a2 ->
            loop_call)
   in
   let loop_body =
      build_exp env e1 (fun env a1 ->
      coerce_bool env pos a1 (fun env a1 ->
            IfThenElse (a1, true_body, break_call)))
   in
      (* Add the functions *)
      LetFuns ([break_f, (FunLocalClass, ty_break, [], break_body);
                loop_f,  (FunLocalClass, ty_loop, [], loop_body)],
      loop_call)

(*
 * Code the for loop as a while loop.
 *)
and build_for_exp env pos e1 e2 e3 e4 cont =
   let pos = string_pos "build_for_exp" pos in
   let body = Fj_ast.SeqExp ([e4; e3], Fj_ast_util.loc_of_exp e3) in
   let loop = Fj_ast.WhileExp (e2, body, Fj_ast_util.loc_of_exp e2) in
   let e = Fj_ast.SeqExp ([e1; loop], Fj_ast_util.loc_of_exp e1) in
      build_exp env e cont

(*
 * Break branches to the most recent break label.
 * The continuation is ignored.
 *)
and build_break_exp env pos cont =
   let pos = string_pos "build_break_exp" pos in
   let f_break = env_get_break env pos in
      TailCall (f_break, [])

(*
 * Code the try.
 * The IR version has no pattern matching.
 *)
and build_try_exp env pos e cases finally cont =
   let pos = string_pos "build_try_exp" pos in

   (* Wrap the rest in a function *)
   let ty_fun = TyFun ([], TyUnit) in
   let f_cont = new_symbol_string "try_cont" in
   let f_body = cont env AtomUnit in
   let f_call = TailCall (f_cont, []) in

   (* Make a typecase for the cases *)
   let cases =
      List.map (fun (cname, v, e) ->
            (* Check that the class is an Exception *)
            if not (is_class_type env pos cname) then
               raise (IrException (pos, StringError "not an Exception type"));
            let body = build_exp env e (fun env _ -> f_call) in
               cname, v, body) cases
   in
   let v_exn = new_symbol_string "exn" in
   let cases = TypeCase (AtomVar v_exn, cases, Raise (AtomVar v_exn)) in

   (* Build the try *)
   let try_body = build_exp env e (fun env _ -> f_call) in
   let try_exp = Try (try_body, v_exn, cases) in
      LetFuns ([f_cont, (FunLocalClass, ty_fun, [], f_body)], try_exp)

(*
 * Raise an exception.
 * Ignore the continuation.
 *)
and build_throw_exp env pos e cont =
   let pos = string_pos "build_raise_exp" pos in
      build_exp env e (fun env a ->
            Raise a)

(*
 * Sequence.
 *)
and build_seq_exp env pos el cont =
   let pos = string_pos "build_seq_exp" pos in

   (* Wrap the rest in a function *)
   let ty_fun = TyFun ([], TyUnit) in
   let f_cont = new_symbol_string "seq_cont" in
   let f_body = cont env AtomUnit in

   (* Build all the expressions *)
   let rec build_seq env' el =
      match el with
         e :: el ->
            build_exp env' e (fun env' _ ->
                  build_seq env' el)
       | [] ->
            TailCall (f_cont, [])
   in
      LetFuns ([f_cont, (FunLocalClass, ty_fun, [], f_body)], build_seq env el)

(*
 * An instanceof is implemented with a TypeCase.
 *)
and build_instanceof_exp env pos e name cont =
   let pos = string_pos "build_instanceof_exp" pos in

   (* Wrap the rest in a function *)
   let ty_fun = TyFun ([TyBool], TyUnit) in
   let f_cont = new_symbol_string "if_cont" in
   let v_cont = new_symbol_string "instanceof" in
   let f_body =
      let env = env_add_var env v_cont TyBool in
         cont env (AtomVar v_cont)
   in

   (* The call forms *)
   let f_true = TailCall (f_cont, [AtomBool true]) in
   let f_false = TailCall (f_cont, [AtomBool false]) in

   (* Make sure we're checking for a class *)
   let _ =
      if not (is_class_type env pos name) then
         raise (IrException (pos, StringError "not a class type"))
   in

   (* Build the typecase *)
   let v_ignore = new_symbol_string "_" in
   let body =
      build_exp env e (fun env a ->
      coerce_type false env pos a ty_object (fun env a ->
            TypeCase (a, [name, v_ignore, f_true], f_false)))
   in
      LetFuns ([f_cont, (FunLocalClass, ty_fun, [v_cont], f_body)], body)

(*
 * Typecast.
 *)
and build_cast_exp env pos ty e cont =
   let pos = string_pos "build_cast_exp" pos in
   let ty = build_type env pos ty in
      build_exp env e (fun env a ->
            coerce_type false env pos a ty cont)

(*
 * New object.
 * Call the best constructor.
 *)
and apply_const env pos f args consts cont =
   let pos = string_pos "apply_const" pos in
   let f', _ = find_best_function env pos f args consts in
   let v = new_symbol_string "const" in
   let env = env_add_var env v TyUnit in
      LetApply (v, TyUnit, f', args, cont env AtomUnit)

and build_new_const_exp env pos name args cont =
   let pos = string_pos "build_new_const_exp" pos in
      build_exp_list env args (fun env args ->
            let consts = (dest_class_type env pos name).class_consts in
            let ty = TyObject name in
            let v = new_symbol name in
            let env = env_add_var env v ty in
            let args = AtomVar v :: args in
               LetObject (v, ty, name,
               apply_const env pos name args consts (fun env _ -> cont env (AtomVar v))))

(*
 * New array.
 *)
and build_new_array_exp env pos ty_name el cont =
   let pos = string_pos "build_new_array_exp" pos in
      build_exp_list env el (fun env args ->
      coerce_ints env pos args (fun env args ->
            let ty = env_lookup_type env pos ty_name in
            let a = default_atom env pos ty in

            (* Wrap the array refs *)
            let ty_array = List.fold_left (fun ty _ -> TyArray ty) ty args in
            let v = new_symbol_string "alloc_array" in
            let env = env_add_var env v ty_array in
               LetArray (v, ty_array, args, a, cont env (AtomVar v))))

(*
 * A function application.
 * There are three cases:
 *    1. the function is a normal var
 *    2. the function is a method of the current class
 *    3. the function is a method of another class
 *)
and build_apply_exp env pos e el cont =
   let pos = string_pos "build_apply_exp" pos in
      build_exp_list env el (fun env args ->
            match e with
               Fj_ast.VarExp (f, _) ->
                  let pos = string_pos "apply_var" pos in
                     if env_mem_var env f then
                        (* Higher-orer function *)
                        let ty = env_lookup_var env pos f in
                        let _, ty_res = dest_fun_type env pos ty in
                        let v' = new_symbol f in
                        let env = env_add_var env v' ty_res in
                           LetApply (v', ty_res, f, args, cont env (AtomVar v'))
                     else if env_mem_fun env f then
                        (* Nested function *)
                        let funs = env_lookup_funs env pos f in
                        let f', ty = find_best_function env pos f args funs in
                        let _, ty_res = dest_fun_type env pos ty in
                        let v' = new_symbol f in
                        let env = env_add_var env v' ty_res in
                           LetApply (v', ty_res, f', args, cont env (AtomVar v'))
                     else
                        (* A method in the current class *)
                        let f_label, ty = find_current_method env pos f args in
                        let _, _, ty_res = dest_method_type env pos ty in

                        (* Project it from the current class *)
                        let f2 = new_symbol f in
                        let v1 = new_symbol f in
                        let env = env_add_var env f2 ty in
                        let env = env_add_var env v1 ty_res in
                           LetProject (f2, ty, AtomVar this_var, f_label,
                           LetApplyMethod (v1, ty_res, f2, AtomVar this_var, args, cont env (AtomVar v1)))

             | Fj_ast.ProjectExp (e, f, _) ->
                  build_exp env e (fun env a ->
                        let ty = type_of_atom env pos a in
                        let pos = string_pos "apply_object" pos in
                        let cname = dest_object_type env pos ty in

                        (* Find the method *)
                        let f_label, ty = find_class_method env pos cname f args in
                        let _, _, ty_res = dest_method_type env pos ty in

                        (* Project the method, get the object, and apply the method *)
                        let f2 = new_symbol f in
                        let v1 = new_symbol f in
                        let env = env_add_var env f2 ty in
                        let env = env_add_var env v1 ty_res in
                           LetProject (f2, ty, a, f_label,
                           LetApplyMethod (v1, ty_res, f2, a, args, cont env (AtomVar v1))))
             | _ ->
                  raise (IrException (pos, StringError "bogus function call")))

(*
 * Return from a function.
 *)
and build_return_exp env pos e cont =
   let pos = string_pos "build_return_exp" pos in
      build_exp env e (fun env a ->
            let f, ty = env_lookup_return env pos in
               coerce_type true env pos a ty (fun env a ->
                     Return (f, a)))

(*
 * Variable definitions.
 *)
and build_vardefs_exp env pos defs cont =
   let pos = string_pos "build_vardefs_exp" pos in
   let rec build env defs =
      match defs with
         (v, ty, e_opt, _) :: defs ->
            let ty = build_type env pos ty in
            let build_var env a =
               let env = env_add_var env v ty in
                  LetVar (v, ty, a, build env defs)
            in
               (match e_opt with
                   Some e ->
                      build_exp env e (fun env a ->
                            build_var env a)
                 | None ->
                      let a = default_atom env pos ty in
                         build_var env a)
       | [] ->
            cont env AtomUnit
   in
      build env defs

(*
 * Function definition.
 *)
and build_fundef env pos (f, vars, ty_res, body, _) cont =
   let pos = string_pos "build_fundef" pos in

   (* Function type *)
   let ty_vars = List.map (fun (_, ty, _) -> build_type env pos ty) vars in
   let ty_res = build_type env pos ty_res in
   let ty_fun = TyFun (ty_vars, ty_res) in

   (* Add the function to the env *)
   let f' = new_symbol f in
   let env = env_add_fun env f f' ty_fun in

   (* Add formal parameters and scope to the env *)
   let vars = List.map (fun (v, _, _) -> v) vars in
   let env' = List.fold_left2 env_add_var env vars ty_vars in
   let env' = env_add_return env' f' ty_res in

   (* Build the function body *)
   let body =
      build_exp env' body (fun env _ ->
            if is_unit_type env pos ty_res then
               Return (f', AtomUnit)
            else
               let v = new_symbol f in
                  LetObject (v, ty_object, object_var, Raise (AtomVar v)))
   in
      (* Define the function *)
      cont env (f', (FunGlobalClass, ty_fun, vars, body))

and build_fundefs_exp env pos funs cont =
   let pos = string_pos "build_fundefs_exp" pos in

   (* Compile all the funs *)
   let rec build env funs' funs =
      match funs with
         def :: funs ->
            build_fundef env pos def (fun env def ->
                  build env (def :: funs') funs)
       | [] ->
            LetFuns (List.rev funs', cont env AtomUnit)
   in
      build env [] funs

(*
 * Build the IR for an AST expression.
 *)
and build_exp env e cont =
   let pos = string_pos "build_exp" (ast_exp_pos e) in
      match e with
         Fj_ast.NilExp _ ->
            cont env AtomNil
       | Fj_ast.BoolExp (b, _) ->
            cont env (AtomBool b)
       | Fj_ast.CharExp (c, _) ->
            cont env (AtomChar c)
       | Fj_ast.IntExp (i, _) ->
            cont env (AtomInt i)
       | Fj_ast.FloatExp (x, _) ->
            cont env (AtomFloat x)
       | Fj_ast.StringExp (s, _) ->
            let v = new_symbol_string "str" in
            let env = env_add_var env v TyString in
               LetString (v, s, cont env (AtomVar v))
       | Fj_ast.VarExp (v, _) ->
            build_var_exp env pos v cont
       | Fj_ast.UnOpExp (op, e, _) ->
            build_unop_exp env pos op e cont
       | Fj_ast.BinOpExp (op, e1, e2, _) ->
            build_binop_exp env pos op e1 e2 cont
       | Fj_ast.AssignExp (op, e1, e2, _) ->
            build_assign_exp env pos op e1 e2 cont
       | Fj_ast.UArithExp (op, e, _) ->
            build_uarith_exp env pos op e cont
       | Fj_ast.BoolOpExp (op, e1, e2, _) ->
            build_boolop_exp env pos op e1 e2 cont
       | Fj_ast.SubscriptExp (e1, e2, _) ->
            build_subscript_exp env pos e1 e2 cont
       | Fj_ast.ProjectExp (e, l, _) ->
            build_project_exp env pos e l cont
       | Fj_ast.IfExp (e1, e2, e3, _) ->
            build_if_exp env pos e1 e2 e3 cont
       | Fj_ast.WhileExp (e1, e2, _) ->
            build_while_exp env pos e1 e2 cont
       | Fj_ast.ForExp (e1, e2, e3, e4, _) ->
            build_for_exp env pos e1 e2 e3 e4 cont
       | Fj_ast.BreakExp _ ->
            build_break_exp env pos cont
       | Fj_ast.TryExp (e, cases, finally, _) ->
            build_try_exp env pos e cases finally cont
       | Fj_ast.ThrowExp (e, _) ->
            build_throw_exp env pos e cont
       | Fj_ast.SeqExp (el, _) ->
            build_seq_exp env pos el cont
       | Fj_ast.InstanceofExp (e, name, _) ->
            build_instanceof_exp env pos e name cont
       | Fj_ast.CastExp (ty, e, _) ->
            build_cast_exp env pos ty e cont
       | Fj_ast.NewConstExp (name, args, _) ->
            build_new_const_exp env pos name args cont
       | Fj_ast.NewArrayExp (ty_name, el, _) ->
            build_new_array_exp env pos ty_name el cont
       | Fj_ast.ApplyExp (e, el, _) ->
            build_apply_exp env pos e el cont
       | Fj_ast.ReturnExp (e, _) ->
            build_return_exp env pos e cont
       | Fj_ast.DefExp (Fj_ast.VarDefs (defs, _), _) ->
            build_vardefs_exp env pos defs cont
       | Fj_ast.DefExp (Fj_ast.FunDefs (funs, _), _) ->
            build_fundefs_exp env pos funs cont
       | Fj_ast.DefExp (Fj_ast.ConstDef _, _) ->
            raise (IrException (pos, StringError "illegal constructor definition"))
       | Fj_ast.DefExp (Fj_ast.ClassDef _, _)
       | Fj_ast.DefExp (Fj_ast.TypeDef _, _) ->
            raise (IrException (pos, StringError "nested class definitions not supported"))

(*
 * Build a list of expressions.
 *)
and build_exp_list env el cont =
   let rec build env args el =
      match el with
         e :: el ->
            build_exp env e (fun env a ->
                  build env (a :: args) el)
       | [] ->
            cont env (List.rev args)
   in
      build env [] el

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
