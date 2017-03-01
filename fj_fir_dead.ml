(*
 * Kai Chen
 *
 * Deadcode elimination.  Specifically, we remove dead function definitions,
 * dead expressions, and dead fields from frames.  We do not remove dead
 * function arguments.  We use a two stage algorithm.
 *
 * In stage one, we collect all the live symbols in the program using a
 * fix-point calculation.  Initially, the only live symbol is the name of
 * the main function.  We analyze each live function in the program for the
 * symbols, labels, and functions that are live in it.  We iterate until
 * the live set does not change.  In stage two, we actually go through
 * everything and mangle types and expressions appropriately.
 *
 * The algorithm is not as aggressive as it could be.  For example, if a
 * label is only assigned into and never read, we still consider it to be
 * live.  We also don't remove redundant globals, e.g., two global strings
 * with the exact same sequence of characters.
 *
 * The code below is fairly straightforward, and the comments should make
 * the implementation details a bit clearer.  There are of course things we
 * could to improve the algorithm, but we only have so much time.
 *)
open Field_table
open Symbol

open Fj_fir
open Fj_fir_exn
open Fj_fir_pos
open Fj_fir_env
open Fj_fir_type
open Fj_fir_state
open Fj_fir_standardize

module Pos = MakePos (struct let name = "Fj_fir_dead" end)
open Pos



(**************************************************************************
 * GENERAL UTILITY CODE
 **************************************************************************)



(*
 * All the liveness information is kept in a SymbolSet.  This includes which
 * variables, function names, labels, and type names are live.  This will
 * only work if the program is standardized!
 *)
type live_info = SymbolSet.t


(*
 * For generality, alias a few functions for working with the live set.
 *)
let live_empty      = SymbolSet.empty
let add_live_sym    = SymbolSet.add
let is_live_sym     = SymbolSet.mem
let equal_live_info = SymbolSet.equal


(*
 * Returns true if and only if a is a live atom.
 *)
let is_live_atom live a : bool =
   match a with
      AtomVar v   -> is_live_sym live v
    | _           -> false



(**************************************************************************
 * LIVENESS ANALYSIS
 **************************************************************************)



(*
 * Add all the type/label names in a type to the live set.
 *)
let rec live_type live ty : live_info =
   match ty with
      TyUnit
    | TyNil
    | TyBool
    | TyChar
    | TyString
    | TyInt
    | TyFloat ->
         live
    | TyArray ty ->
         live_type live ty
    | TyFun (ty_args, ty_res) ->
         let live = live_types live ty_args in
         let live = live_type live ty_res in
            live
    | TyMethod (ty_obj, ty_args, ty_res) ->
         let live = live_type live ty_obj in
         let live = live_types live ty_args in
         let live = live_type live ty_res in
            live
    | TyRecord (rclass, field_types) ->
         (* Only add the types for fields that are live.
          * The class record and names object fields are always live! *)
         let iterator = fun live field_name ty ->
            let index = FieldTable.find_index field_types field_name in
            let is_special =
               (rclass = RecordClass || rclass = RecordObject) && (index = 0)
            in
               if is_live_sym live field_name || is_special then
                  let live = add_live_sym live field_name in
                  let live = live_type live ty in
                     live
               else
                  live
         in
            FieldTable.fold iterator live field_types
    | TyNames names ->
         (* Add everything to the live set. *)
         let iterator = fun live (v, ty_opt) ->
            let live = add_live_sym live v in
               match ty_opt with
                  Some ty ->  live_type live ty
                | None    ->  live
         in
            List.fold_left iterator live names
    | TyId ty_var ->
         add_live_sym live ty_var


(*
 * Adds the type/label names in a list of type names to the live set.
 *)
and live_types live tys : live_info =
   List.fold_left (fun live ty -> live_type live ty) live tys


(*
 * Adds an atom to the live set.
 *)
let live_atom live a : live_info =
   match a with
      AtomVar v -> add_live_sym live v
    | _         -> live


(*
 * Adds a list of atoms to the live set.
 *)
let live_atoms live atoms : live_info =
   List.fold_left (fun live a -> live_atom live a) live atoms


(*
 * Compute the live variables in an expression.
 * Returns the new set of live symbols.
 *)
let rec live_exp live exp : live_info =
   let pos = string_pos "live_exp" (exp_pos exp) in
      match exp with
         LetFuns _ ->
            raise (FirException (pos, StringError "LetFuns encountered"))
       | LetVar (v, ty, a, e) ->
            live_letvar live v ty a e
       | LetAtom (v, ty, a, e) ->
            live_letvar live v ty a e
       | LetUnop (v, ty, op, a, e) ->
            live_letvar live v ty a e
       | LetBinop (v, ty, op, a1, a2, e) ->
            live_letbinop live v ty op a1 a2 e
       | LetExt (v, ty, name, fun_ty, args, e) ->
            live_letext live v ty name fun_ty args e
       | TailCall (f, args) ->
            live_tailcall live f args
       | MethodCall (f, obj, args) ->
            live_tailcall live f (obj :: args)
       | IfThenElse (a, e_true, e_false) ->
            live_ifthenelse live a e_true e_false
       | IfType (a, name, v, e_true, e_false) ->
            live_iftype live a name v e_true e_false
       | SetVar _ ->
            raise (FirException (pos, StringError "SetVar encountered"))
       | LetArray (v, ty, dims, init, e) ->
            live_letarray live v ty dims init e
       | LetSubscript (v, ty, a1, a2, e) ->
            live_letsubscript live v ty a1 a2 e
       | SetSubscript (a1, a2, ty, a3, e) ->
            live_setsubscript live a1 a2 ty a3 e
       | LetRecord (v, ty, rclass, field_atoms, e) ->
            live_letrecord live v ty rclass field_atoms e
       | LetProject (v, ty, a, label, e) ->
            live_letproject live v ty a label e
       | SetProject (a1, label, ty, a2, e) ->
            live_setproject live a1 label ty a2 e
       | LetClosure (v, ty, f, a, e) ->
            live_letclosure live v ty f a e


(*
 * Liveness for LetVar and similar expressions (LetAtom, LetUnop).
 *)
and live_letvar live v ty a e : live_info =
   let live = live_exp live e in
      if is_live_sym live v then
         let live = live_atom live a in
         let live = live_type live ty in
            live
      else
         live


(*
 * Liveness for a LetBinop expression.  If v is not live, then the
 * expression is considered to be dead.  So the program's behavior will
 * change if this is actually a division by zero.
 *)
and live_letbinop live v ty op a1 a2 e : live_info =
   let live = live_exp live e in
      if is_live_sym live v then
         let live = live_atom live a1 in
         let live = live_atom live a2 in
         let live = live_type live ty in
            live
      else
         live


(*
 * Liveness for a LetExt expression.  Only the "println" external call
 * always needs to be considered live since the others are side-effect
 * free (as far as evaluation goes).
 *)
and live_letext live v ty name fun_ty args e : live_info =
   let live = live_exp live e in
      if is_live_sym live v || name = "println" then
         let live = live_atoms live args in
         let live = live_type live fun_ty in
         let live = live_type live ty in
            live
      else
         live


(*
 * Liveness for a TailCall expression.  For now, assume that all the
 * arguments are live.  Don't forget to add the function as being live.
 * (Also used for MethodCalls.)
 *)
and live_tailcall live f args : live_info =
   let live = live_atoms live args in
   let live = add_live_sym live f in
      live


(*
 * Liveness for a IfThenElse expression.
 *)
and live_ifthenelse live a e_true e_false : live_info =
   let live = live_exp live e_true in
   let live = live_exp live e_false in
   let live = live_atom live a in
      live


(*
 * Liveness for a IfType expression.
 *)
and live_iftype live a name v e_true e_false : live_info =
   let live = live_exp live e_true in
   let live = live_exp live e_false in
   let live = live_atom live a in
   let live = add_live_sym live name in
   let live = add_live_sym live v in
      live


(*
 * Liveness for a LetArray expression.
 *)
and live_letarray live v ty dims init e : live_info =
   let live = live_exp live e in
      if is_live_sym live v then
         let live = live_atom live init in
         let live = live_atoms live dims in
         let live = live_type live ty in
            live
      else
         live


(*
 * Liveness for a LetSubscript expression.  If v is not live, then the
 * expression is considered to be dead.  So the program's behavior will
 * change if the index is actually out of bounds.
 *)
and live_letsubscript live v ty a1 a2 e : live_info =
   let live = live_exp live e in
      if is_live_sym live v then
         let live = live_atom live a1 in
         let live = live_atom live a2 in
         let live = live_type live ty in
            live
      else
         live


(*
 * Liveness for a SetSubscript expression.  To be on the safe side, we
 * always consider it to be live.
 *)
and live_setsubscript live a1 a2 ty a3 e : live_info =
   let live = live_exp live e in
   let live = live_atom live a1 in
   let live = live_atom live a2 in
   let live = live_atom live a3 in
   let live = live_type live ty in
      live


(*
 * Liveness for a LetRecord expression.  If the expression is live, only add
 * those initializer atoms for fields which are live.
 *)
and live_letrecord live v ty rclass field_atoms e : live_info =
   let live = live_exp live e in
      if is_live_sym live v then
         let live = live_type live ty in
         let iterator = fun live field_name atom ->
            if is_live_sym live field_name then
               live_atom live atom
            else
               live
         in
            FieldTable.fold iterator live field_atoms
      else
         live


(*
 * Liveness for a LetProject expression.
 *)
and live_letproject live v ty a label e : live_info =
   let live = live_exp live e in
      if is_live_sym live v then
         let live = live_atom live a in
         let live = live_type live ty in
         let live = add_live_sym live label in
            live
      else
         live


(*
 * Liveness for a SetProject expression.  The expression is only live if the
 * label we are assigning into is live.
 *)
and live_setproject live a1 label ty a2 e : live_info =
   let live = live_exp live e in
      if is_live_sym live label then
         let live = live_atom live a1 in
         let live = live_atom live a2 in
         let live = live_type live ty in
            live
      else
         live


(*
 * Liveness for a LetClosure expression.
 *)
and live_letclosure live v ty f a e : live_info =
   let live = live_exp live e in
      if is_live_sym live v then
         let live = live_atom live a in
         let live = live_type live ty in
         let live = add_live_sym live f in
            live
      else
         live



(**************************************************************************
 * LIVENESS FIXPOINT
 **************************************************************************)



(*
 * Add the live symbols in a global initializer to the live symbol set.
 *)
let live_init live init : live_info =
      match init with
         InitString str ->
            (* Nothing to add here. *)
            live
       | InitNames names ->
            (* Assume everything is live. *)
            let iterator = fun live (v, v_opt) ->
               let live = add_live_sym live v in
                  match v_opt with
                     Some v   -> add_live_sym live v
                   | None     -> live
            in
               List.fold_left iterator live names
       | InitRecord (_, field_atoms) ->
            (* Only add initializer atoms for fields that are live. *)
            let iterator = fun live field_name atom ->
               if is_live_sym live field_name then
                  live_atom live atom
               else
                  live
            in
               FieldTable.fold iterator live field_atoms


(*
 * The calculation of live variables in the program is a fix-point
 * calculation.  The live_prog function computes one iteration.
 *)
let live_prog live prog : live_info =

   (* The main function is always live. *)
   let live = add_live_sym live prog.prog_main in

   (* Iterate over all the live functions in the program. *)
   let iterator = fun live f (fclass, ty, vars, body) ->
      if is_live_sym live f then
         let live = live_exp live body in
         let live = live_type live ty in
            live
      else
         live
   in
   let live = SymbolTable.fold iterator live prog.prog_funs in

   (* Iterate over all the live globals. *)
   let iterator = fun live sym (ty, init) ->
      if is_live_sym live sym then
         let live = live_type live ty in
         let live = live_init live init in
            live
      else
         live
   in
   let live = SymbolTable.fold iterator live prog.prog_globals in

   (* Iterate over all the type definitions in the program. *)
   let iterator = fun live sym ty ->
      if is_live_sym live sym then
         live_type live ty
      else
         live
   in
   let live = SymbolTable.fold iterator live prog.prog_types in
   let live = SymbolTable.fold iterator live prog.prog_tynames in

      (* Return the liveness information now. *)
      live


(*
 * Find the fix point of the live symbol set.
 *)
let rec compute_liveness live prog : live_info =
   let new_live = live_prog live prog in
      if equal_live_info new_live live then
         new_live
      else
         compute_liveness new_live prog



(**************************************************************************
 * DEADCODE REMOVAL (UTILITY FUNCTIONS)
 **************************************************************************)



(*
 * Removes occurances of dead type names from types.
 *)
let rec dead_type live ty : ty =
   match ty with
      TyUnit
    | TyNil
    | TyBool
    | TyChar
    | TyString
    | TyInt
    | TyFloat ->
         ty
    | TyArray ty ->
         TyArray (dead_type live ty)
    | TyFun (ty_args, ty_res) ->
         let ty_args = dead_types live ty_args in
         let ty_res = dead_type live ty_res in
            TyFun (ty_args, ty_res)
    | TyMethod (ty_obj, ty_args, ty_res) ->
         let ty_obj = dead_type live ty_obj in
         let ty_args = dead_types live ty_args in
         let ty_res = dead_type live ty_res in
            TyMethod (ty_obj, ty_args, ty_res)
    | TyRecord (rclass, f_types ) ->
         (* Form a new field table with only live field names. *)
         let iterator = fun f_types field_name ty ->
            if is_live_sym live field_name then
               FieldTable.add f_types field_name (dead_type live ty)
            else
               f_types
         in
         let f_types = FieldTable.fold iterator FieldTable.empty f_types in
            TyRecord (rclass, f_types)
    | TyNames names ->
         (* These were considered to be live in liveness analysis . *)
         ty
    | TyId _ ->
         (* Nothing we can do here. *)
         ty


(*
 * Applies deadcode elimination to a list of types.
 *)
and dead_types live tys : ty list =
   List.map (fun ty -> dead_type live ty) tys


(*
 * Removes the deadcode in an expression.  Returns the new expression.
 *)
let rec dead_exp live body : exp =
   let pos = string_pos "dead_exp" (exp_pos body) in
      match body with
         LetFuns _ ->
            raise (FirException (pos, StringError "LetFuns encountered"))
       | LetVar (v, ty, a, e) ->
            dead_letvar live v ty a e
       | LetAtom (v, ty, a, e) ->
            dead_letatom live v ty a e
       | LetUnop (v, ty, op, a, e) ->
            dead_letunop live v ty op a e
       | LetBinop (v, ty, op, a1, a2, e) ->
            dead_letbinop live v ty op a1 a2 e
       | LetExt (v, ty, name, fun_ty, args, e) ->
            dead_letext live v ty name fun_ty args e
       | TailCall _
       | MethodCall _ ->
            (* Nothing to do here. *)
            body
       | IfThenElse (a, e_true, e_false) ->
            IfThenElse (a, dead_exp live e_true, dead_exp live e_false)
       | IfType (a, name, v, e_true, e_false) ->
            IfType (a, name, v, dead_exp live e_true, dead_exp live e_false)
       | SetVar _ ->
            raise (FirException (pos, StringError "SetVar encountered"))
       | LetArray (v, ty, dims, init, e) ->
            dead_letarray live v ty dims init e
       | LetSubscript (v, ty, a1, a2, e) ->
            dead_letsubscript live v ty a1 a2 e
       | SetSubscript (a1, a2, ty, a3, e) ->
            dead_setsubscript live a1 a2 ty a3 e
       | LetRecord (v, ty, rclass, field_atoms, e) ->
            dead_letrecord live v ty rclass field_atoms e
       | LetProject (v, ty, a, label, e) ->
            dead_letproject live v ty a label e
       | SetProject (a1, label, ty, a2, e) ->
            dead_setproject live a1 label ty a2 e
       | LetClosure (v, ty, f, a, e) ->
            dead_letclosure live v ty f a e


(*
 * Deadcode elimination for a LetVar expression.
 *)
and dead_letvar live v ty a e : exp =
   let e = dead_exp live e in
      if is_live_sym live v then
         LetVar (v, dead_type live ty, a, e)
      else
         e


(*
 * Deadcode elimination for a LetAtom expression.
 *)
and dead_letatom live v ty a e : exp =
   let e = dead_exp live e in
      if is_live_sym live v then
         LetAtom (v, dead_type live ty, a, e)
      else
         e


(*
 * Deadcode elimination for a LetUnop expression.
 *)
and dead_letunop live v ty op a e : exp =
   let e = dead_exp live e in
      if is_live_sym live v then
         LetUnop (v, dead_type live ty, op, a, e)
      else
         e


(*
 * Deadcode elimination for a LetBinop expression.  If v is not live, then
 * the expression is considered to be dead.  So the program's behavior will
 * change if this is actually a division by zero.
 *)
and dead_letbinop live v ty op a1 a2 e : exp =
   let e = dead_exp live e in
      if is_live_sym live v then
         LetBinop (v, dead_type live ty, op, a1, a2, e)
      else
         e


(*
 * Deadcode elimination for a LetExt expression.  Only the "println"
 * external call always needs to be considered live since the others are
 * side-effect free (as far as evaluation goes).
 *)
and dead_letext live v ty name fun_ty args e : exp =
   let e = dead_exp live e in
      if is_live_sym live v || name = "println" then
         let ty = dead_type live ty in
         let fun_ty = dead_type live fun_ty in
            LetExt (v, ty, name, fun_ty, args, e)
      else
         e


(*
 * Deadcode elimination for a LetArray expression.
 *)
and dead_letarray live v ty dims init e : exp =
   let e = dead_exp live e in
      if is_live_sym live v then
         LetArray (v, dead_type live ty, dims, init, e)
      else
         e


(*
 * Deadcode elimination for a LetSubscript expression.  If v is not live,
 * then the expression is considered to be dead.  So the program's behavior
 * will change if the index is actually out of bounds.
 *)
and dead_letsubscript live v ty a1 a2 e : exp =
   let e = dead_exp live e in
      if is_live_sym live v then
         LetSubscript (v, dead_type live ty, a1, a2, e)
      else
         e


(*
 * Deadcode elimination for a SetSubscript expression.  To be on the safe
 * side, it is always considered to be live.
 *)
and dead_setsubscript live a1 a2 ty a3 e : exp =
   SetSubscript (a1, a2, dead_type live ty, a3, dead_exp live e)


(*
 * Deadcode elimination for a LetRecord expression.  If the expression is
 * live, only add those initializer atoms for fields which are live.
 *)
and dead_letrecord live v ty rclass f_atoms e : exp =
   let e = dead_exp live e in
      if is_live_sym live v then
         let iterator = fun fields field_name atom ->
            if is_live_sym live field_name then
               FieldTable.add fields field_name atom
            else
               fields
         in
         let f_atoms = FieldTable.fold iterator FieldTable.empty f_atoms in
         let ty = dead_type live ty in
            LetRecord (v, ty, rclass, f_atoms, e)
      else
         e


(*
 * Deadcode elimination for a LetProject expression.
 *)
and dead_letproject live v ty a label e : exp =
   let e = dead_exp live e in
      if is_live_sym live v then
         LetProject (v, dead_type live ty, a, label, e)
      else
         e


(*
 * Deadcode elimination for a SetProject expression.  The expression is only
 * live if the label we're assigning into is live.
 *)
and dead_setproject live a1 label ty a2 e : exp =
   let e = dead_exp live e in
      if is_live_sym live label then
         SetProject (a1, label, dead_type live ty, a2, e)
      else
         e


(*
 * Deadcode elimination for a LetClosure expression.
 *)
and dead_letclosure live v ty f a e : exp =
   let e = dead_exp live e in
      if is_live_sym live v then
         LetClosure (v, dead_type live ty, f, a, e)
      else
         e



(**************************************************************************
 * REMOVE DEADCODE (PRIMARY FUNCTIONS)
 **************************************************************************)



(*
 * Deadcode eliminates an initializer for a global.
 *)
let dead_init live init =
   match init with
      InitString str ->
         (* Nothing to do in this case. *)
         init
    | InitNames names ->
         (* This was all considered live in liveness analysis. *)
         init
    | InitRecord (rclass, f_atoms) ->
         (* Build a new field table with only field names that are live. *)
         let iterator = fun fields field_name atom ->
            if is_live_sym live field_name then
               FieldTable.add fields field_name atom
            else
               fields
         in
         let f_atoms = FieldTable.fold iterator FieldTable.empty f_atoms in
            InitRecord (rclass, f_atoms)


(*
 * Takes the given set of globals and eliminates the unnecessary ones.
 * Deadcode elimination is also applied to the initializers.  The new
 * set of globals is returned.
 *)
let dead_globals live prog_globals : (ty * init) SymbolTable.t =
   let iterator = fun globals sym (ty, init) ->
      if is_live_sym live sym then
         let ty = dead_type live ty in
         let init = dead_init live init in
         let globals = SymbolTable.add globals sym (ty, init) in
            globals
      else
         globals
   in
      SymbolTable.fold iterator SymbolTable.empty prog_globals


(*
 * Takes the given set of functions and returns a new set that only contains
 * the live functions in the program.  Deadcode elimination is also applied
 * to the function bodies.
 *)
let dead_funs live prog_funs : fun_info SymbolTable.t =
   let iterator = fun new_funs f (fclass, ty, vars, body) ->
      if is_live_sym live f then
         let body = dead_exp live body in
         let fun_info = (fclass, ty, vars, body) in
         let new_funs = SymbolTable.add new_funs f fun_info in
            new_funs
      else
         new_funs
   in
      SymbolTable.fold iterator SymbolTable.empty prog_funs


(*
 * Apply deadcode elimination to the program.  Assume that the program is
 * standardized.  The resulting program will still be standardized.
 *)
let rec dead_prog prog : prog =
   (* Compute the set of live variables. *)
   let live = compute_liveness live_empty prog in

   (* Deadcode eliminate the types and type names. *)
   let iterator = fun new_types sym ty ->
      if is_live_sym live sym then
         SymbolTable.add new_types sym (dead_type live ty)
      else
         new_types
   in
   let types = SymbolTable.fold iterator SymbolTable.empty prog.prog_types in
   let names = SymbolTable.fold iterator SymbolTable.empty prog.prog_tynames in

   (* Deadcode eliminate the function definitions and globals. *)
   let funs = dead_funs live prog.prog_funs in
   let globals = dead_globals live prog.prog_globals in

   (* Return the new program now. *)
   let prog = { prog with prog_types  = types;
                         prog_tynames = names;
                         prog_funs    = funs;
                         prog_globals = globals;
              }
   in
      standardize_prog prog
