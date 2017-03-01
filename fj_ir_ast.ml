(*
 * Translate the AST to IR.
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
open Format
open Symbol
open Location
open Field_table

open Fj_ir
open Fj_ir_env
open Fj_ir_exn
open Fj_ir_exp
open Fj_ir_pos
open Fj_ir_type
open Fj_ir_check
open Fj_ir_state
open Fj_ir_standardize

module Pos = MakePos (struct let name = "Fj_ast_exn_print" end)
open Pos

(************************************************************************
 * OTHER TYPES
 ************************************************************************)

(*
 * This is a collection of all the fields in a class definition.
 *)
type field_info =
   { field_vars : (var * var * ty * Fj_ast.exp option * loc) list;
     field_methods : (var * var * ty * var list * Fj_ast.exp * loc) list;
     field_consts  : (var * ty * var list * Fj_ast.exp * loc) list;
   }

let field_empty =
   { field_vars = [];
     field_methods = [];
     field_consts = []
   }

(*
 * This is the info we collect from the program.
 *)
type prog_info =
   { info_types    : tydef SymbolTable.t;
     info_abstypes : SymbolSet.t;
     info_funs     : (fun_class * ty * var list * exp) SymbolTable.t;
   }

(*
 * Adding to the program info.
 *)
let info_add_tydef info v ty =
   { info with info_types = SymbolTable.add info.info_types v ty }

let info_add_abs_type info v =
   { info with info_abstypes = SymbolSet.add info.info_abstypes v }

let info_add_fun info f entry =
   { info with info_funs = SymbolTable.add info.info_funs f entry }

(************************************************************************
 * INITIAL ENVIRONMENT
 ************************************************************************)

(*
 * Add the FjObject type to the env.
 *)
let info_empty, env_empty =
   let info =
      { info_types    = SymbolTable.empty;
        info_abstypes = SymbolSet.empty;
        info_funs     = SymbolTable.empty
      }
   in
   let ty_object = TyObject object_var in
   let methods = FieldMTable.create () in
   let fields = FieldMTable.create () in

   (* Define parseInt *)
   let ty_parse_int = TyMethod (ty_object, [TyString], TyInt) in
   let parse_int_var = Symbol.add "parseInt" in
   let parse_int_f = new_symbol parse_int_var in
   let v_string = new_symbol_string "string" in
   let v_int = new_symbol_string "int" in
   let parse_int_body =
      LetExt (v_int, TyInt, "atoi", TyFun ([TyString], TyInt), [AtomVar v_string],
      Return (parse_int_f, AtomVar v_int))
   in
   let info = info_add_fun info parse_int_f (FunMethodClass, ty_parse_int, [this_var; v_string], parse_int_body) in
   let methods = FieldMTable.add methods parse_int_var parse_int_f ty_parse_int in

   (* Define parseFloat *)
   let ty_parse_float = TyMethod (ty_object, [TyString], TyFloat) in
   let parse_float_var = Symbol.add "parseFloat" in
   let parse_float_f = new_symbol parse_float_var in
   let v_string = new_symbol_string "string" in
   let v_float = new_symbol_string "float" in
   let parse_float_body =
      LetExt (v_float, TyFloat, "atof", TyFun ([TyString], TyFloat), [AtomVar v_string],
      Return (parse_float_f, AtomVar v_float))
   in
   let info = info_add_fun info parse_float_f (FunMethodClass, ty_parse_float, [this_var; v_string], parse_float_body) in
   let methods = FieldMTable.add methods parse_float_var parse_float_f ty_parse_float in

   (* Define println *)
   let ty_println = TyMethod (ty_object, [TyString], TyUnit) in
   let println_var = Symbol.add "println" in
   let println_f = new_symbol println_var in
   let v_string = new_symbol_string "string" in
   let v_unit = new_symbol_string "_" in
   let println_body =
      LetExt (v_int, TyUnit, "println", TyFun ([TyString], TyUnit), [AtomVar v_string],
      Return (println_f, AtomVar v_int))
   in
   let info = info_add_fun info println_f (FunMethodClass, ty_println, [this_var; v_string], println_body) in
   let methods = FieldMTable.add methods println_var println_f ty_println in

   (* Define FjObject *)
   let ty_object_unit = TyFun ([TyObject object_var], TyUnit) in
   let object_info =
      { class_parents = [];
        class_consts = [object_init_var, ty_object_unit];
        class_methods = methods;
        class_fields = fields
      }
   in
   let object_def = TyDefClass (Some object_info) in

   (* Add to environment *)
   let env = env_add_tydef env_empty object_var object_def in

   (* FjObject constructor does nothing *)
   let object_body = Return (object_init_var, AtomUnit) in
   let info = info_add_fun info object_init_var (FunGlobalClass, ty_object_unit, [this_var], object_body) in

   (* Add to info *)
   let info = info_add_tydef info object_var object_def in
      info, env

(************************************************************************
 * CLASSES, PHASE 1
 ************************************************************************)

(*
 * Override a previous definition of a method.
 *)
let merge_method env methods (f_ext, f_int, ty, loc) =
   let pos = string_pos "merge_method" (loc_pos loc) in

   (* Search for a previous definition *)
   let replace _ ty' =
      if equal_types env pos ty ty' then
         f_int, ty
      else
         (* This is not the right definition *)
         raise Not_found
   in
      (* Either replace a previous definition, or add a new one *)
      try FieldMTable.replace_ext methods f_ext replace with
         Not_found ->
            FieldMTable.add methods f_ext f_int ty

let merge_methods env methods1 methods2 =
   List.fold_left (merge_method env) methods1 methods2

(*
 * The field merge just adds all the fields.
 *)
let merge_field fields (v, v', ty, _, _) =
   FieldMTable.add fields v v' ty

let merge_fields fields1 fields2 =
   List.fold_left merge_field fields1 fields2

(*
 * A variable definition.
 * We just convert the type,
 * we'll save the initializer conversion for later.
 *)
let build_var_def env fields v ty e_opt loc =
   let pos = string_pos "build_var_def" (loc_pos loc) in
   let ty = build_type env pos ty in
   let v' = new_symbol v in
      { fields with field_vars = (v, v', ty, e_opt, loc) :: fields.field_vars }

(*
 * Add a variable definition to the class fields.
 *)
let rec build_var_defs env fields defs =
   match defs with
      (v, ty, e_opt, pos) :: defs ->
         let fields = build_var_def env fields v ty e_opt pos in
            build_var_defs env fields defs
    | [] ->
         fields

(*
 * Add a method definition to the class fields.
 * Build the IR function type.  The method gets
 * an extra "this" parameter.
 *)
let build_method_def env name fields (f, args, ty_res, body, loc) =
   (* Collect all the formal params *)
   let pos = string_pos "build_method_def" (loc_pos loc) in
   let vars, ty_vars =
      List.fold_left (fun (vars, ty_vars) (v, ty, _) ->
            let vars = v :: vars in
            let ty_vars = build_type env pos ty :: ty_vars in
               vars, ty_vars) ([], []) args
   in
   let vars = this_var :: List.rev vars in
   let ty_vars = List.rev ty_vars in

   (* Add the field with the new function type *)
   let f' = new_symbol f in
   let ty_fun = TyMethod (TyObject name, ty_vars, build_type env pos ty_res) in
      { fields with field_methods = (f, f', ty_fun, vars, body, loc) :: fields.field_methods }

let build_method_defs env fields name funs =
   List.fold_left (build_method_def env name) fields funs

(*
 * Add a constructor.
 * It is like a function, but we don't add
 * the "this" parameter.
 *)
let build_const_def env fields name f args body loc =
   (* Collect all the formal params *)
   let pos = string_pos "build_const_def" (loc_pos loc) in
   let vars, ty_vars =
      List.fold_left (fun (vars, ty_vars) (v, ty, _) ->
            let vars = v :: vars in
            let ty_vars = build_type env pos ty :: ty_vars in
               vars, ty_vars) ([], []) args
   in
   let ty_fun = TyFun (TyObject name :: List.rev ty_vars, TyUnit) in
   let f' = new_symbol f in
      { fields with field_consts = (f', ty_fun, this_var :: List.rev vars, body, loc) :: fields.field_consts }

(*
 * Add a default constructor if there are no constructors.
 *)
let add_default_const fields name loc =
   let pos = string_pos "add_default_const" (loc_pos loc) in
      if fields.field_consts = [] then
         let f = new_symbol name in
         let ty_fun = TyFun ([TyObject name], TyUnit) in
         let body = Fj_ast.SeqExp ([], loc) in
            { fields with field_consts = [f, ty_fun, [this_var], body, loc] }
      else
         fields

(*
 * Build the class info.
 *)
let build_class_type env name extends defs loc =
   let pos = string_pos "build_class_type" (loc_pos loc) in

   (* Build all the types in the defs *)
   let fields =
      List.fold_right (fun def fields ->
            match def with
               Fj_ast.VarDefs (defs, _) ->
                  build_var_defs env fields defs
             | Fj_ast.FunDefs (funs, _) ->
                  build_method_defs env fields name funs
             | Fj_ast.ConstDef (f, args, body, pos) ->
                  build_const_def env fields name f args body pos
             | Fj_ast.ClassDef (_, _, _, loc)
             | Fj_ast.TypeDef (_, _, loc) ->
                  raise (IrException (loc_pos loc, StringError "nested class definitions not implemented"))) (**)
         defs field_empty
   in

   (* Look up the parent *)
   let parent = dest_class_type env pos extends in

   (* Add a default constructor if necessary *)
   let fields = add_default_const fields name loc in

   (* Collect class vars and fields *)
   let class_methods = List.map (fun (f, f', ty, _, _, loc) -> f, f', ty, loc) fields.field_methods in
   let class_methods = merge_methods env parent.class_methods class_methods in
   let class_fields = merge_fields parent.class_fields fields.field_vars in
   let class_consts = List.map (fun (f, ty, _, _, _) -> f, ty) fields.field_consts in

   (* Collect all the class info *)
   let class_info =
      { class_parents = extends :: parent.class_parents;
        class_consts = class_consts;
        class_methods = class_methods;
        class_fields = class_fields
      }
   in
      class_info, fields

(************************************************************************
 * CLASSES, PHASE 2
 ************************************************************************)

(*
 * Build the IR for an instance variable declaration.
 * The main result is a collection of initializers for the vars.
 *)
let rec build_field_init env init_var defs =
   match defs with
      (v, v', ty, e_opt, loc) :: defs ->
         let pos = string_pos "build_vars" (loc_pos loc) in
            (match e_opt with
                Some e ->
                   build_exp env e (fun env a ->
                         coerce_type true env pos a ty (fun env a ->
                               SetProject (this_atom, v', ty, a,
                               build_field_init env init_var defs)))
              | None ->
                   build_field_init env init_var defs)
    | [] ->
         (* Initialization works by side-effect *)
         Return (init_var, AtomUnit)

(*
 * The build_vars wrapper function defines the "alloc" function for
 * the class.  We need: the program info; the class info for
 * the current class; the name of the VMA record for the class;
 * and the fields of the class.
 *)
let build_init info env name init_var class_info fields loc =
   let pos = string_pos "build_alloc" (loc_pos loc) in

   (* Build the initializer expression *)
   let e = build_field_init env init_var fields.field_vars in
      (* Define the allocator as a function *)
      info_add_fun info init_var (FunGlobalClass, TyFun ([TyObject name], TyUnit), [this_var], e)

(*
 * Build the IR for a method.  Add the function to the program info.
 * We need: the program info; the class info for the current class;
 * the function info.  We define one element of the VMA record for
 * the current class.
 *)
let build_method env info (f, f', ty, vars, body, loc) =
   let pos = string_pos "build_method" (loc_pos loc) in
   let ty_this, ty_vars, ty_res = dest_method_type env pos ty in

   (* Add the return label with the new function name *)
   let env = env_add_return env f' ty_res in

   (* Add all the vars to the venv *)
   let env = List.fold_left2 env_add_var env vars (ty_this :: ty_vars) in

   (* Build the body *)
   let e =
      build_exp env body (fun env a ->
            if is_unit_type env pos ty_res then
               Return (f', AtomUnit)
            else
               let v = new_symbol f in
                  LetObject (v, ty_object, object_var, Raise (AtomVar v)))
   in

   (* Add the function to the program *)
   let info = info_add_fun info f' (FunMethodClass, ty, vars, e) in
      info

(*
 * Add the code for all of the methods.
 *)
let build_methods info env fields =
   List.fold_left (build_method env) info fields.field_methods

(*
 * Build a constructor.
 * The constructor first calls the initialier,
 * then it calls any other constructors, then
 * it performs the rest of the construction.
 *)
let build_const env cname pname init_var info (f', ty, vars, body, loc) =
   let pos = string_pos "build_const" (loc_pos loc) in
   let ty_obj = TyObject cname in
   let ty_vars, _ = dest_fun_type env pos ty in

   (* Examine the body, and extract a constructor call *)
   let body, id, args =
      match body with
         Fj_ast.SeqExp (Fj_ast.ApplyExp (Fj_ast.VarExp (id, _), args, _) :: rest, pos)
         when Symbol.eq id this_var || Symbol.eq id super_var ->
            let body = Fj_ast.SeqExp (rest, pos) in
               body, id, args
       | _ ->
            body, super_var, []
   in

   (* Build the body and call the parent constructor *)
   let body =
      let name =
         if Symbol.eq id this_var then
            cname
         else
            pname
      in
      let consts = (dest_class_type env pos name).class_consts in
      let env = List.fold_left2 env_add_var env vars ty_vars in
      let env = env_add_var env this_var ty_obj in
         build_exp_list env args (fun env args ->
         apply_const env pos name (this_atom :: args) consts (fun env _ ->
         let body = build_exp env body (fun _ _ -> Return (f', AtomUnit)) in
         (* Call the initializer right after first call to super constructor *)
         if Symbol.eq id super_var then
            let v = new_symbol_string "init" in
               LetApply (v, TyUnit, init_var, [AtomVar this_var], body)
         else
            body
         ))
   in

   (* Add the function to the info *)
   let info = info_add_fun info f' (FunGlobalClass, ty, vars, body) in
      info

let build_consts info env cname pname init_var fields =
   List.fold_left (build_const env cname pname init_var) info fields.field_consts

(*
 * Build a class definition.
 *)
let build_class_def info env name extends defs pos =
   (* Compile the type and get the fields *)
   let class_info, fields = build_class_type env name extends defs pos in

   (* Add the class and its type to the env *)
   let ty_class = TyDefClass (Some class_info) in
   let info = info_add_tydef info name ty_class in
   let env = env_add_tydef env name ty_class in
   let env' = env_set_current_class env name class_info in

   (* Initialization function *)
   let name_str = Symbol.to_string name in
   let init_var = new_symbol_string (name_str ^ ".init") in
   let info = build_init info env' name init_var class_info fields pos in

   (* Build all the methods *)
   let info = build_methods info env' fields in

   (* Constructors *)
   let info = build_consts info env' name extends init_var fields in
      info, env

(*
 * Build the file initialization function.
 *)
let build_init info env filename =
   let loc = create_loc (Symbol.add filename) 0 0 0 0 in
   let filename = Symbol.add filename in
   let main_sym = Symbol.add "main" in
   let v_argv = new_symbol_string "argv" in
   let body =
      Fj_ast.ApplyExp (Fj_ast.ProjectExp (Fj_ast.NewConstExp (filename, [], loc), main_sym, loc),
                       [Fj_ast.VarExp (v_argv, loc)],
                       loc)
   in
   let env = env_add_var env v_argv (TyArray TyString) in
   let body =
      build_exp env body (fun env _ ->
            Return (main_sym, AtomUnit))
   in
   let info = info_add_fun info main_sym (FunGlobalClass, TyFun ([TyArray TyString], TyUnit), [v_argv], body) in
      info, main_sym

(************************************************************************
 * MAIN
 ************************************************************************)

(*
 * Add dummy definitions to the env.
 *)
let build_env env def =
   match def with
      Fj_ast.TypeDef (_, _, loc) ->
         raise (IrException (loc_pos loc, StringError "illegal type definition"))
    | Fj_ast.ClassDef (name, _, _, _) ->
         env_add_tydef env name (TyDefClass None)
    | Fj_ast.VarDefs (_, loc) ->
         raise (IrException (loc_pos loc, StringError "variable definition not allowed at top level"))
    | Fj_ast.FunDefs (_, loc)
    | Fj_ast.ConstDef (_, _, _, loc) ->
         raise (IrException (loc_pos loc, StringError "function definition not allowed at top level"))

let build_env env defs =
   List.fold_left build_env env defs

(*
 * Compile a definition.
 *)
let build_def (info, env) def =
   match def with
      Fj_ast.TypeDef (_, _, loc) ->
         raise (IrException (loc_pos loc, StringError "illegal type definition"))
    | Fj_ast.ClassDef (name, extends, defs, pos) ->
         build_class_def info env name extends defs pos
    | Fj_ast.VarDefs (_, loc) ->
         raise (IrException (loc_pos loc, StringError "variable definition not allowed at top level"))
    | Fj_ast.FunDefs (_, loc)
    | Fj_ast.ConstDef (_, _, _, loc) ->
         raise (IrException (loc_pos loc, StringError "function definition not allowed at top level"))

let build_defs info env defs =
   List.fold_left build_def (info, env) defs

(*
 * Convert the AST definition list to an IR program.
 *)
let build_prog prog filename =
   let env = build_env env_empty prog in
   let info, env = build_defs info_empty env prog in
   let info, main = build_init info env filename in
   let { info_types = types;
         info_abstypes = abstypes;
         info_funs = funs
       } = info
   in
   let prog =
      { prog_types = types;
        prog_abstypes = abstypes;
        prog_funs = funs;
        prog_main = main;
        prog_object = object_var
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
