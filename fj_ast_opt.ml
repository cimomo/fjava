(*
 * Kai Chen
 *
 * Inline fields in classes that are constants.
 *
 * In other words, this is a constant-folding optimization.  We only do
 * fields in classes that we determine to always have the same value.  The
 * reason we perform this analysis here, and not in the FIR, is that it is
 * much easier to approximate this analysis while the class definitions
 * still have some of their original structure.
 *
 * Anyway, here's the general algorithm.  For each class definition in the
 * program, we do the following:
 *
 *    (1)   Make a list of all the instance fields in the class with
 *          initializers and have a basic type (no reference types).
 *
 *    (2)   We scan all the function bodies now and if one of the variables
 *          collected in step (1) appears on the left hand side of an
 *          assignment, in a pre/post-increment/decrement operation, or is
 *          declared (either in some formal parameter list or function
 *          body), then we remove it from the list.
 *
 *          This leaves us with a list of variables that we know are
 *          constants now.
 *
 *    (3)   We mangle the code in the class to propogate the inializers to
 *          the locations where the variables are actually used.
 *
 * Clearly, there are more intelligent ways of doing this.  But this is an
 * easy approximation to code, especially since the AST doesn't have a
 * particularly nice binding structure (as compared to the FIR).
 *)

open Symbol
open Fj_ast
open Fj_ast_exn
open Fj_ast_pos

module Pos = MakePos (struct let name = "Fj_ast_opt" end)
open Pos




(**************************************************************************
 * TYPE DEFINITIONS
 **************************************************************************)



(*
 * This is a table type that maps variables their values.  Used to keep
 * track of the fields in a class that are constants.
 *)
type const_info = exp SymbolTable.t


(*
 * For generality, define a few convinient aliases.
 *)
let info_empty      = SymbolTable.empty
let info_add_def    = SymbolTable.add
let info_find_def   = SymbolTable.find
let info_remove_def = SymbolTable.remove



(**************************************************************************
 * REMOVE NON-CONSTANTS
 *
 * In this stage, 'info' contains mappings for all the fields in a class.
 * We need to remove the ones that are not constant.  The algorithm here is
 * a bit conservative (the details are up above).
 **************************************************************************)



(*
 * Remove any non-constant variables in an expression from the info table.
 * The 'flag' parameter is boolean that should be true if and only if
 * VarExps should be removed from the table.  We can use it to indicate if
 * we're processing the left hand of an assignment.
 *)
let rec remove_exp info flag exp : const_info =
   match exp with
      NilExp _
    | BoolExp _
    | CharExp _
    | IntExp _
    | FloatExp _
    | StringExp _
    | BreakExp _ ->
         info
    | VarExp (v, _) ->
         if flag then
            info_remove_def info v
         else
            info
    | UArithExp (_, e, _) ->
         (* use true since this is an assignment. *)
         remove_exp info true e
    | UnOpExp (_, e, _)
    | ReturnExp (e, _)
    | InstanceofExp (e, _, _)
    | CastExp (_, e, _)
    | ThrowExp (e, _) ->
         remove_exp info flag e
    | ProjectExp (e, v, _) ->
         let info = remove_exp info flag e in
            if flag then
               info_remove_def info v
            else
               info
    | BinOpExp (_, e1, e2, _)
    | BoolOpExp (_, e1, e2, _)
    | SubscriptExp (e1, e2, _)
    | WhileExp (e1, e2, _) ->
         let info = remove_exp info flag e1 in
         let info = remove_exp info flag e2 in
            info
    | ApplyExp (e, e_list, _) ->
         let info = remove_exp info flag e in
         let info = remove_exp_list info flag e_list in
            info
    | AssignExp (_, e_left, e_right, _) ->
         (* We need to use 'true' for e_left! *)
         let info = remove_exp info flag e_right in
         let info = remove_exp info true e_left in
            info
    | IfExp (e1, e2, e_opt, _) ->
         let info = remove_exp info flag e1 in
         let info = remove_exp info flag e2 in
         let info = remove_exp_option info flag e_opt in
            info
    | ForExp (e1, e2, e3, e4, _) ->
         let info = remove_exp info flag e1 in
         let info = remove_exp info flag e2 in
         let info = remove_exp info flag e3 in
         let info = remove_exp info flag e4 in
            info
    | SeqExp (e_list, _)
    | NewConstExp (_, e_list, _)
    | NewArrayExp (_, e_list, _) ->
         remove_exp_list info flag e_list
    | TryExp (e, catch_list, e_opt, _) ->
         let info = remove_exp info flag e in
         let info = remove_exp_catches info flag catch_list in
         let info = remove_exp_option info flag e_opt in
            info
    | DefExp (def, _) ->
         remove_def info flag def


(*
 * Remove any non-constant variables in an expression list.
 *)
and remove_exp_list info flag e_list : const_info =
   let iterator = (fun info e -> remove_exp info flag e) in
      List.fold_left iterator info e_list


(*
 * Remove any non-constant variables from an (exp option).
 *)
and remove_exp_option info flag e_opt : const_info =
   match e_opt with
      None   -> info
    | Some e -> remove_exp info flag e


(*
 * Remove any non-constant variables from a list of catch blocks.
 *)
and remove_exp_catches info flag catch_list : const_info =
   let iterator = fun info (v1, v2, e) ->
      let info = info_remove_def info v1 in
      let info = info_remove_def info v2 in
      let info = remove_exp info flag e in
         info
   in
      List.fold_left iterator info catch_list


(*
 * Remove any non-constant variables from a series of definitions.
 *)
and remove_def info flag def : const_info =
   match def with
      VarDefs (var_def_list, _) ->
         remove_var_def_list info flag var_def_list
    | FunDefs (fundef_list, _) ->
         remove_fundef_list info flag fundef_list
    | ConstDef (_, var_decl_list, e, _) ->
         let info = remove_var_decl_list info var_decl_list in
         let info = remove_exp info flag e in
            info
    | ClassDef (_, _, def_list, _) ->
         remove_def_list info flag def_list
    | TypeDef _ ->
         let pos = string_exp_pos "remove_def" in
            raise (AstException (pos, StringError "unexpected TypeDef"))


(*
 * Remove any non-constant variables in a function definition.
 *)
and remove_fundef info flag (_, var_decls, _, body, _) : const_info =
   let info = remove_var_decl_list info var_decls in
   let info = remove_exp info flag body in
      info


(*
 * Remove any non-constant variables from a list of function definitions.
 *)
and remove_fundef_list info flag fundefs : const_info =
   let iterator = (fun info def -> remove_fundef info flag def) in
      List.fold_left iterator info fundefs


(*
 * Remove any non-constant variables in a list of definitions.
 *)
and remove_def_list info flag def_list : const_info =
   let iterator = (fun info def -> remove_def info flag def) in
      List.fold_left iterator info def_list


(*
 * Remove any non-constant variables in a variable definition.
 * Note that this includes the variable being defined!
 *)
and remove_var_def info flag (v, _, e_opt, _) : const_info =
   let info = info_remove_def info v in
   let info = remove_exp_option info flag e_opt in
      info


(*
 * Remove any non-constant variables in a list of variable definitions.
 *)
and remove_var_def_list info flag var_def_list : const_info =
   let iterator = (fun info var_def -> remove_var_def info flag var_def) in
      List.fold_left iterator info var_def_list


(*
 * Remove v from 'info' since it is being declared.
 *)
and remove_var_decl info (v, _, _) : const_info =
   info_remove_def info v


(*
 * Remove from 'info' the variables being declared.
 *)
and remove_var_decl_list info decl_list : const_info =
   let iterator = (fun info var_decl -> remove_var_decl info var_decl) in
      List.fold_left iterator info decl_list



(**************************************************************************
 * CODE MANGLING
 *
 * Here, we rewrite the code to propogate the definitions that are still in
 * the info table.  The code is similar to the analysis we did above.  We
 * refer to this as 'mangling'.
 **************************************************************************)



(*
 * Mangle an expression.
 * Essentially recurses into the components of each expression.
 * VarExp's are treated specially (that's the entire point).
 *)
let rec mangle_exp info exp : exp =
   match exp with
      NilExp _
    | BoolExp _
    | CharExp _
    | IntExp _
    | FloatExp _
    | StringExp _
    | BreakExp _ ->
         exp
    | VarExp (v, loc) ->
         (* This is the key step. *)
         (try info_find_def info v with
            Not_found -> exp)
    | UArithExp (op, e, loc) ->
         UArithExp (op, mangle_exp info e, loc)
    | UnOpExp (op, e, loc) ->
         UnOpExp (op, mangle_exp info e, loc)
    | BinOpExp (op, e1, e2, loc) ->
         let e1 = mangle_exp info e1 in
         let e2 = mangle_exp info e2 in
            BinOpExp (op, e1, e2, loc)
    | BoolOpExp (op, e1, e2, loc) ->
         let e1 = mangle_exp info e1 in
         let e2 = mangle_exp info e2 in
            BoolOpExp (op, e1, e2, loc)
    | SubscriptExp (e1, e2, loc) ->
         let e1 = mangle_exp info e1 in
         let e2 = mangle_exp info e2 in
            SubscriptExp (e1, e2, loc)
    | ProjectExp (e, v, loc) ->
         (try info_find_def info v with
            Not_found -> ProjectExp (mangle_exp info e, v, loc))
    | ApplyExp (e, e_list, loc) ->
         let e = mangle_exp info e in
         let e_list = mangle_exp_list info e_list in
            ApplyExp (e, e_list, loc)
    | AssignExp (op, e1, e2, loc) ->
         let e1 = mangle_exp info e1 in
         let e2 = mangle_exp info e2 in
            AssignExp (op, e1, e2, loc)
    | IfExp (e1, e2, e3, loc) ->
         let e1 = mangle_exp info e1 in
         let e2 = mangle_exp info e2 in
         let e3 = mangle_exp_option info e3 in
            IfExp (e1, e2, e3, loc)
    | ForExp (e1, e2, e3, e4, loc) ->
         let e1 = mangle_exp info e1 in
         let e2 = mangle_exp info e2 in
         let e3 = mangle_exp info e3 in
         let e4 = mangle_exp info e4 in
            ForExp (e1, e2, e3, e4, loc)
    | WhileExp (e1, e2, loc) ->
         let e1 = mangle_exp info e1 in
         let e2 = mangle_exp info e2 in
            WhileExp (e1, e2, loc)
    | SeqExp (e_list, loc) ->
         SeqExp (mangle_exp_list info e_list, loc)
    | ReturnExp (e, loc) ->
         ReturnExp (mangle_exp info e, loc)
    | NewConstExp (v, e_list, loc) ->
         NewConstExp (v, mangle_exp_list info e_list, loc)
    | NewArrayExp (v, e_list, loc) ->
         NewArrayExp (v, mangle_exp_list info e_list, loc)
    | InstanceofExp (e, v, loc) ->
         InstanceofExp (mangle_exp info e, v, loc)
    | CastExp (ty, e, loc) ->
         CastExp (ty, mangle_exp info e, loc)
    | ThrowExp (e, loc) ->
         ThrowExp (mangle_exp info e, loc)
    | TryExp (e, catch_list, e_opt, loc) ->
         let e = mangle_exp info e in
         let catch_list = mangle_catch_list info catch_list in
         let e_opt = mangle_exp_option info e_opt in
            TryExp (e, catch_list, e_opt, loc)
    | DefExp (def, loc) ->
         DefExp (mangle_def info def, loc)


(*
 * Mangle a list of expressions.
 *)
and mangle_exp_list info e_list : exp list =
   let iterator = (fun e -> mangle_exp info e) in
      List.map iterator e_list


(*
 * Mangle an (exp option).
 *)
and mangle_exp_option info e : exp option =
   match e with
      None     -> None
    | Some e   -> Some (mangle_exp info e)


(*
 * Mangle a list of catch blocks.
 *)
and mangle_catch_list info catch_list : (symbol * symbol * exp) list =
   let iterator = (fun (v1, v2, e) -> (v1, v2, mangle_exp info e)) in
      List.map iterator catch_list


(*
 * Mangle a definition.
 *)
and mangle_def info def : def =
   match def with
      VarDefs (var_defs, loc) ->
         let iterator = fun (v, ty, e_opt, loc) ->
            (v, ty, mangle_exp_option info e_opt, loc)
         in
            VarDefs (List.map iterator var_defs, loc)
    | FunDefs (fundefs, loc) ->
         let iterator = fun (f, args, ty, e, loc) ->
            (f, args, ty, mangle_exp info e, loc)
         in
            FunDefs (List.map iterator fundefs, loc)
    | ConstDef (v, args, body, loc) ->
         ConstDef (v, args, mangle_exp info body, loc)
    | ClassDef (v1, v2, defs, loc) ->
         ClassDef (v1, v2, mangle_def_list info defs, loc)
    | TypeDef _ ->
         def


(*
 * Mangle a list of definitions.
 *)
and mangle_def_list info defs : def list =
   List.map (mangle_def info) defs



(**************************************************************************
 * DRIVER CODE
 **************************************************************************)



(*
 * Add instance fields that have a basic type and an
 * initializer to the info table.
 *)
let add_instance_fields info vdefs : const_info =
   let iterator = fun info (v, ty, init_opt, _) ->
      match init_opt with
         Some (BoolExp (b, loc)) ->
         info_add_def info v (BoolExp (b, loc))
       | Some (CharExp (c, loc)) ->
         info_add_def info v (CharExp (c, loc))
       | Some (IntExp (i, loc)) ->
         info_add_def info v (IntExp (i, loc))
       | Some (FloatExp (x, loc)) ->
            info_add_def info v (FloatExp (x, loc))
       | _ ->
            info
   in
      List.fold_left iterator info vdefs


(*
 * Optimizes a class definition by propogating the values of constant
 * instance fields into functions.  Works according to the algorithm
 * described at the beginning of this file.
 *)
let optimize_classdefs defs : def list =
   (* Collect instance field definitions that have initializers. *)
   let iterator = fun info def ->
      match def with
         VarDefs (vdefs, _) ->
            add_instance_fields info vdefs
       | _ ->
            info
   in
   let info = List.fold_left iterator info_empty defs in

   (* Remove the instance fields that are not constants. *)
   let iterator = fun info def ->
      match def with
         FunDefs (fdefs, _) ->
            remove_fundef_list info false fdefs
       | ConstDef (_, var_decl_list, e, _) ->
            let info = remove_var_decl_list info var_decl_list in
            let info = remove_exp info false e in
               info
       | _ ->
            info
   in
   let info = List.fold_left iterator info defs in

      (* Now propogate the values of the contant fields into functions. *)
      mangle_def_list info defs


(*
 * Optimizes each of the class definitions in a program.
 *)
let optimize_prog prog : prog =
   let iterator = fun def ->
      match def with
         ClassDef (name, extends, defs, loc) ->
            ClassDef (name, extends, optimize_classdefs defs, loc)
       | _ ->
            def
   in
      List.map iterator prog
