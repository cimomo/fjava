(*
 * Author: Kai Chen
 *
 * We implement here an approximation of inlining / constant folding /
 * common subexpression elimination.  The basic idea is that we analyze
 * expressions top-down and attempt to propogate definitions as far forward
 * as possible.  For function inlining, we do an initial scan of the program
 * to find the functions that we would like to inline, and then we use this
 * information in the top-down analysis of expressions.
 *
 * To be clear, the CSE implementation here is incredibly dumb.  It may not
 * even be worthy of being called CSE.  The inlining and constant folding
 * implementations are more complete.  Also, we're pretty much implementing
 * all of this as local optimizations.
 *
 * This is the overall description.  Read through this file for the specific
 * details of what we are doing.  Throughout, we assume that the program is
 * standardized and that programs are single threaded.  We will refer to the
 * optimizations above as simplification.  Finally, "icf" stands for
 * "inlining and constant-folding", just in case you're wondering why those
 * three letters keep popping up all over the place.
 *
 * The functions is_nested_too_deep and should_inline effect how deeply
 * nested through a series of conditions we will attempt to inline and which
 * functions are considered for inlining.
 *
 * Miscellaneos note: Aside from aliasing, the other thing we need to be
 * weary of is what happens when we expand a function body.  The algorithm
 * here implicitly depends on the expressions being standardized.
 *)
open Format
open Symbol
open Field_table

open Fj_fir
open Fj_fir_exn
open Fj_fir_pos
open Fj_fir_print
open Fj_fir_standardize

module Pos = MakePos (struct let name = "Fj_fir_inline" end)
open Pos



(**************************************************************************
 * TYPE DEFINITIONS
 **************************************************************************)



(*
 * These are the kinds of expressions that we wish to simplify.
 *
 * Atoms are values, so LetVar and LetAtom just create aliases; we can
 * simply propogate the values to where they are needed.
 *
 * For LetUnop and LetBinop, we wish to perform compile time arithmetic when
 * possible.  We also wish to avoid calculating the exact same unop and
 * binop expressions multiple times.  LetClosure is similar.
 *
 * We can also remove some extraneous LetProjects.  The idea is if we know
 * for certain that some label has been defined as some atom, then for every
 * read operation up to the next assignment, we can just use the atom we
 * used in the definition.  We have to be careful of aliasing, though.
 *)
type icf =
   IcfAtom of atom
 | IcfUnop of unop * atom
 | IcfBinop of binop * atom * atom
 | IcfLetProject of atom * var
 | IcfLetClosure of var * atom


(*
 * We'll want to inline functions and closures.  For IcfFun, we keep track
 * of the function's body and formal parameter list.  For IcfClosure, we
 * keep track of the function being closed, and the atoms that are being
 * partially applied.  The atoms are maintained in the order that the
 * function expects them.
 *)
type icf_fun =
   IcfFun of var list * exp
 | IcfClosure of atom list * var


(*
 * Compare two atoms.  We're mainly interested in when they are equal.
 *)
let compare_atoms a1 a2 : int =
   match a1, a2 with
      AtomUnit, AtomUnit
    | AtomNil, AtomNil ->
         0
    | AtomBool v1, AtomBool v2 ->
         Pervasives.compare v1 v2
    | AtomChar v1, AtomChar v2 ->
         Pervasives.compare v1 v2
    | AtomInt v1,  AtomInt  v2 ->
         Pervasives.compare v1 v2
    | AtomFloat v1, AtomFloat v2 ->
         Pervasives.compare v1 v2
    | AtomVar v1, AtomVar v2 ->
         Symbol.compare v1 v2
    | _ ->
         Pervasives.compare a1 a2


(*
 * Compare two icf values.  We're mainly interested in when they are equal.
 *)
let icf_compare icf1 icf2 : int =
   match icf1, icf2 with
      IcfAtom a1, IcfAtom a2 ->
         compare_atoms a1 a2
    | IcfUnop (op1, a1), IcfUnop (op2, a2) ->
         let i = Pervasives.compare op1 op2 in
            if i = 0 then
               compare_atoms a1 a2
            else
               i
    | IcfBinop (op1, a11, a12), IcfBinop (op2, a21, a22) ->
         let i = Pervasives.compare op1 op2 in
            if i = 0 then
               let j = compare_atoms a11 a21 in
                  if j = 0 then
                     compare_atoms a12 a22
                  else
                     j
            else
               i
    | IcfLetProject (a1, f1), IcfLetProject (a2, f2)
    | IcfLetClosure (f1, a1), IcfLetClosure (f2, a2) ->
         let i = Symbol.compare f1 f2 in
            if i = 0 then
               compare_atoms a1 a2
            else
               i
    | _ ->
         Pervasives.compare icf1 icf2


(*
 * Define a table that maps keys of type icf to some type 'a; we will use
 * the type atom below.  The idea is to use the icf's above as keys, since
 * these are the expressions we wish to simplify.  We map them to atoms that
 * have been defined to have the same value as the expressions.
 *
 * In some sense, this is just a substitution table.
 *)
module IcfCompare =
struct
   type t = icf
   let compare = icf_compare
end

module IcfTable = Mc_map.McMake (IcfCompare)


(*
 * This is the record that we use to hold the information needed to simplify
 * expressions.  icf_defs holds all the expression -> atom mappings, and
 * icf_funs maps function names to their formal parameter lists and bodies.
 * The idea is that if a function is in the table, then we can inline it.
 *
 * icf_rec is the highly entertaining field.  When we pass top-down through
 * a given expression, icf_rec is augmented with the names of the functions
 * that we inline.  In other words, we use it to prevent ourselves from
 * inlining recursive functions infinitely many times.
 *
 * icf_depth is also entertaining.  We use it to keep track of how many
 * conditions deep we are at any given moment.  This is useful in making
 * sure we don't inline nested conditionals to far.
 *)
type icf_info =
   {  icf_defs  : atom IcfTable.t;
      icf_funs  : icf_fun SymbolTable.t;
      icf_rec   : SymbolSet.t;
      icf_depth : int
   }



(**************************************************************************
 * UTILITY CODE
 **************************************************************************)



(*
 * The empty info record.
 *)
let icf_empty : icf_info =
   {  icf_defs  = IcfTable.empty;
      icf_funs  = SymbolTable.empty;
      icf_rec   = SymbolSet.empty;
      icf_depth = 0
   }


(*
 * Add a function to the recursive calls set.
 *)
let add_rec_call info f : icf_info =
   { info with icf_rec = SymbolSet.add info.icf_rec f }


(*
 * Determine if a function is in the recursive call set.
 *)
let is_rec_call info f : bool =
   SymbolSet.mem info.icf_rec f


(*
 * Increment the nesting depth.
 *)
let inc_nesting_depth info : icf_info =
   { info with icf_depth = info.icf_depth + 1 }


(*
 * Return true if and only if we're too deeply nested.
 * Keep this somewhat small to avoid code bloat.
 *)
let is_nested_too_deep info : bool =
   (info.icf_depth > 1)


(*
 * Add an expression definition to the info record.
 *)
let add_exp_def info icf a : icf_info =
   { info with icf_defs = IcfTable.add info.icf_defs icf a }


(*
 * Add a function definition to the info record.
 *)
let add_fun_def info f vars body : icf_info =
   let def = IcfFun (vars, body) in
      { info with icf_funs = SymbolTable.add info.icf_funs f def }


(*
 * Add a closure definition to the info record.  If the function we're
 * closing is already a closure, we should augment the previous definition.
 * v is the variable we're assigning the closure to, and f is the function
 * being partially applied.  a is the atom in the partial application.
 *)
let add_closure_def info v f a : icf_info =
   try
      match SymbolTable.find info.icf_funs f with
         (* f is already a closure. *)
         IcfClosure (atoms, f') ->
            let def = IcfClosure (atoms @ [a], f') in
            let funs = SymbolTable.add info.icf_funs v def in
               { info with icf_funs = funs }
         (* Must have a new closure definition. *)
       | _ ->
            let def = IcfClosure ([a], f) in
            let funs = SymbolTable.add info.icf_funs v def in
               { info with icf_funs = funs }
   with
      (* Must have a new closure definition. *)
      Not_found ->
         let def = IcfClosure ([a], f) in
         let funs = SymbolTable.add info.icf_funs v def in
            { info with icf_funs = funs }


(*
 * Find an expression definition in the info record.
 * Returns None if there is no definition.
 *)
let find_exp_def info icf : atom option =
   try Some (IcfTable.find info.icf_defs icf) with
      Not_found -> None


(*
 * Find a function or closure definition in the info record.
 * Returns None if there is no definition.
 *)
let find_fun_def info f : icf_fun option =
   try Some (SymbolTable.find info.icf_funs f) with
      Not_found -> None


(*
 * Simplifies a variable by seeing if we have a previous definition of it.
 * Otherwise, just returns that variable.
 *)
let simplify_var info v : var =
   match find_exp_def info (IcfAtom (AtomVar v)) with
      Some (AtomVar v')  -> v'
    | _                  -> v


(*
 * Simplifies an atom by seeing if we have a previous definition of it.
 * Otherwise, just returns that atom.
 *)
let simplify_atom info a : atom =
   match find_exp_def info (IcfAtom a) with
      Some a' -> a'
    | None    -> a


(*
 * Simplify a list of atoms.
 *)
let simplify_atoms info atoms : atom list =
   List.map (simplify_atom info) atoms



(**************************************************************************
 * SIMPLIFY EXPRESSIONS
 **************************************************************************)



(*
 * Try to evaluate a unop at compile-time.  Return the atom that is the
 * evaluated value and a flag indicating whether or not we could evaluate
 * (op a).  If the flag is false, the atom returned will be totally bogus.
 * We'll ignore the issue of whether or not compile time arithmetic is the
 * same as runtime arithmetic.
 *)
let eval_unop op a : atom * bool =
   match op, a with
      UMinusIntOp, AtomInt i ->
         AtomInt (- i), true
    | UMinusFloatOp, AtomFloat x ->
         AtomFloat (-. x), true
    | _ ->
         (* We said the atom was going to be bogus in this case. *)
         a, false


(*
 * Try to evaluate a binop at compile-time.  Return the atom that is the
 * evaluated value and a flag indicating whether or not we could evaluate
 * (a1 op a2).  Note that the atom returned will be totally bogus if the
 * flag is false.  We will ignore the issue of whether or not compile time
 * arithmetic is the same as runtime arithmetic.
 *)
let eval_binop op a1 a2 : atom * bool =
   match op, a1, a2 with
      (* Integers. *)
      AddIntOp, AtomInt i, AtomInt j ->
         AtomInt (i + j), true
    | SubIntOp, AtomInt i, AtomInt j ->
         AtomInt (i - j), true
    | MulIntOp, AtomInt i, AtomInt j ->
         AtomInt (i * j), true
    | DivIntOp, AtomInt i, AtomInt j ->
         (* Only print a message for possible divisions by zero. *)
         if j <> 0 then
            AtomInt (i / j), true
         else
            (print_string "possible division by zero in the program\n";
            a1, false)
    | RemIntOp, AtomInt i, AtomInt j ->
         (* Only print a message for possible mods by zero. *)
         if j <> 0 then
            AtomInt (i mod j), true
         else
            (print_string "possible mod by zero in the program\n";
            a1, false)

    | EqIntOp, AtomInt i, AtomInt j ->
         AtomBool (i = j), true
    | NeqIntOp, AtomInt i, AtomInt j ->
         AtomBool (i <> j), true
    | LtIntOp, AtomInt i, AtomInt j ->
         AtomBool (i < j), true
    | LeIntOp, AtomInt i, AtomInt j ->
         AtomBool (i <= j), true
    | GtIntOp, AtomInt i, AtomInt j ->
         AtomBool (i > j), true
    | GeIntOp, AtomInt i, AtomInt j ->
         AtomBool (i >= j), true


      (* Bitwise ops are a bit riskier than the others, so omit them. *)
      (* No float support since the backend does not support them.    *)

      (* Booleans. *)
    | EqBoolOp, AtomBool b1, AtomBool b2 ->
         AtomBool (b1 = b2), true
    | NeqBoolOp, AtomBool b1, AtomBool b2 ->
         AtomBool (b1 <> b2), true

      (* We won't consider any more cases. *)
    | _ ->
         (* We said the atom was going to be bogus in this case. *)
         a1, false


(*
 * Simplify an expression.
 *)
let rec simplify_exp info exp : exp =
   let pos = string_pos "simplify_exp" (exp_pos exp) in
      match exp with
         LetFuns _ ->
            raise (FirException (pos, StringError "LetFuns encountered"))
       | LetVar (v, ty, a, e) ->
            simplify_letvar info v ty a e
       | LetAtom (v, ty, a, e) ->
            simplify_letvar info v ty a e
       | LetUnop (v, ty, op, a, e) ->
            simplify_letunop info v ty op a e
       | LetBinop (v, ty, op, a1, a2, e) ->
            simplify_letbinop info v ty op a1 a2 e
       | LetExt (v, ty, name, fun_ty, args, e) ->
            let args = simplify_atoms info args in
            let e = simplify_exp info e in
               LetExt (v, ty, name, fun_ty, args, e)
       | TailCall (f, args) ->
            simplify_tailcall info f args
       | MethodCall (f, obj, args) ->
            let f = simplify_var info f in
            let obj = simplify_atom info obj in
            let args = simplify_atoms info args in
               MethodCall (f, obj, args)
       | IfThenElse (a, e_true, e_false) ->
            simplify_ifthenelse info a e_true e_false
       | IfType (a, name, v, e_true, e_false) ->
            simplify_iftype info a name v e_true e_false
       | SetVar _ ->
            raise (FirException (pos, StringError "SetVar encountered"))
       | LetArray (v, ty, dims, init, e) ->
            let dims = simplify_atoms info dims in
            let init = simplify_atom info init in
            let e = simplify_exp info e in
               LetArray (v, ty, dims, init, e)
       | LetSubscript (v, ty, a1, a2, e) ->
            let a1 = simplify_atom info a1 in
            let a2 = simplify_atom info a2 in
            let e = simplify_exp info e in
               LetSubscript (v, ty, a1, a2, e)
       | SetSubscript (a1, a2, ty, a3, e) ->
            let a1 = simplify_atom info a1 in
            let a2 = simplify_atom info a2 in
            let a3 = simplify_atom info a3 in
            let e = simplify_exp info e in
               SetSubscript (a1, a2, ty, a3, e)
       | LetRecord (v, ty, rclass, field_atoms, e) ->
            simplify_letrecord info v ty rclass field_atoms e
       | LetProject (v, ty, a, label, e) ->
            simplify_letproject info v ty a label e
       | SetProject (a1, label, ty, a2, e) ->
            simplify_setproject info a1 label ty a2 e
       | LetClosure (v, ty, f, a, e) ->
            simplify_letclosure info v ty f a e


(*
 * Simplify a LetVar or LetAtom expression.  The idea here is pretty simple.
 * These expressions are totally useless since atoms are values.  So we just
 * need to record that v is defined as whatever the simplification of a is.
 * This effectively removes much of the useless variable aliasing introduced
 * during closure conversion.
 *
 * Note that if ty is a reference type, and a' is something like AtomNil,
 * we could run into the following problem.  The AtomNil gets propogates
 * into a subscript or project expressions, where it causes a FIR type
 * error.  So we get for free a conservative way of finding obvious
 * null-pointer dereferences.  Maybe not the most desirable semantics, but
 * it would be simple enough to modify this function to check the ty and
 * only propogate atoms for non-reference types.
 *)
and simplify_letvar info v ty a e : exp =
   let a' = simplify_atom info a in
   let info = add_exp_def info (IcfAtom (AtomVar v)) a' in
      simplify_exp info e


(*
 * Simplify a LetUnop expression by seeing if we've already evaluated this
 * particular unop expression.  Use the previous definition if there is one,
 * otherwise we attempt to evaluate the current expression and add a new
 * definition to info.
 *)
and simplify_letunop info v ty op a e : exp =
   let a = simplify_atom info a in
      match find_exp_def info (IcfUnop (op, a)) with
         Some a' ->
            (* Just add a new binding in info for v. *)
            let info = add_exp_def info (IcfAtom (AtomVar v)) a' in
               simplify_exp info e
       | None ->
            let a', did_eval = eval_unop op a in
               if did_eval then
                  (* v just becomes an alias for a in this case. *)
                  let info = add_exp_def info (IcfAtom (AtomVar v)) a' in
                     simplify_exp info e
               else
                  (* Add a new definition. *)
                  let info = add_exp_def info (IcfUnop (op, a)) (AtomVar v) in
                  let e = simplify_exp info e in
                     LetUnop (v, ty, op, a, e)


(*
 * Simplify a LetBinop expression by seeing if we've already evaluated this
 * particular binop expression.  Use the previous definition if there is one,
 * otherwise we attempt to evaluate the current expression and add a new
 * definition to info.
 *)
and simplify_letbinop info v ty op a1 a2 e : exp =
   let a1 = simplify_atom info a1 in
   let a2 = simplify_atom info a2 in
      match find_exp_def info (IcfBinop (op, a1, a2)) with
         Some a' ->
            (* Just add a new binding in info for v. *)
            let info = add_exp_def info (IcfAtom (AtomVar v)) a' in
               simplify_exp info e
       | None ->
            let a', did_eval = eval_binop op a1 a2 in
               if did_eval then
                  (* v just becomes an alias for a in this case. *)
                  let info = add_exp_def info (IcfAtom (AtomVar v)) a' in
                     simplify_exp info e
               else
                  (* Add a new definition. *)
                  let info = add_exp_def info (IcfBinop (op, a1, a2)) (AtomVar v) in
                  let e = simplify_exp info e in
                     LetBinop (v, ty, op, a1, a2, e)


(*
 * Simplify a TailCall by inlining the function if possible.
 *)
and simplify_tailcall info f args : exp =
   (* Simplify everything. *)
   let f = simplify_var info f in
   let args = simplify_atoms info args in
      (* Test if we should inline the call or not. *)
      if (is_nested_too_deep info) || (is_rec_call info f) then
         (* Don't try to inline. *)
         TailCall (f, args)
      else
         match find_fun_def info f with
            None ->
               (* No definition that we can use to inline. *)
               TailCall (f, args)
          | Some (IcfFun (vars, body)) ->
               (* We have a definition that we can use for inlining. *)
               let info = add_rec_call info f in
               let iterator = fun info v arg ->
                  add_exp_def info (IcfAtom (AtomVar v)) arg
               in
               let info = List.fold_left2 iterator info vars args in
                  simplify_exp info body
          | Some (IcfClosure (atoms, f')) ->
               (* This just becomes a normal tailcall. *)
               simplify_tailcall info f' (atoms @ args)


(*
 * Simplifies an IfThenElse by determining if we can remove one of the
 * branches.  Otherwise, just simplify both branches.
 *)
and simplify_ifthenelse info a e_true e_false : exp =
   let a = simplify_atom info a in
      match a with
         AtomBool true ->
            simplify_exp info e_true
       | AtomBool false ->
            simplify_exp info e_false
       | _ ->
            let info = inc_nesting_depth info in
            let e_true = simplify_exp info e_true in
            let e_false = simplify_exp info e_false in
               IfThenElse (a, e_true, e_false)


(*
 * Simplifies an IfType expression.  Just simplify both branches.
 *)
and simplify_iftype info a name v e_true e_false : exp =
   let a = simplify_atom info a in
   let info = inc_nesting_depth info in
   let e_true = simplify_exp info e_true in
   let e_false = simplify_exp info e_false in
      IfType (a, name, v, e_true, e_false)


(*
 * Simplifies a LetRecord expression.  We need to simplify the initializer
 * atoms, and then record what the fields of this record are defined as so
 * that we can simplify future LetProjects (potentially).
 *)
and simplify_letrecord info v ty rclass field_atoms e : exp =
   (* Simplify the initializer atoms. *)
   let field_atoms = FieldTable.map (simplify_atom info) field_atoms in

   (* Record the definitions of the fields. *)
   let iterator = fun info field atom ->
      add_exp_def info (IcfLetProject (AtomVar v, field)) atom
   in
   let info = FieldTable.fold iterator info field_atoms in

      (* Simplify the rest of the program now. *)
      LetRecord (v, ty, rclass, field_atoms, simplify_exp info e)


(*
 * Simplify a LetProject expression by finding the previous definition of
 * the field we are projecting, if there is one.
 *)
and simplify_letproject info v ty a label e : exp =
   let a = simplify_atom info a in
      match find_exp_def info (IcfLetProject (a, label)) with
         Some a' ->
            (* Just add a new binding in info for v. *)
            let info = add_exp_def info (IcfAtom (AtomVar v)) a' in
               simplify_exp info e
       | None ->
            (* Don't do anything special in this case. *)
            LetProject (v, ty, a, label, simplify_exp info e)


(*
 * Simplify the atoms in a SetProject expression.  This only counts as a new
 * definition if we had a previous one already.  Otherwise, we might have
 * problems with aliasing.
 *)
and simplify_setproject info a1 label ty a2 e : exp =
   let a1 = simplify_atom info a1 in
   let a2 = simplify_atom info a2 in
      match find_exp_def info (IcfLetProject (a1, label)) with
         None ->
            (* No previous definition. *)
            SetProject (a1, label, ty, a2, simplify_exp info e)
       | Some a'  ->
            if 0 = compare_atoms a2 a' then
               (* This assignment is redundant. *)
               simplify_exp info e
            else
               (* Override the previous definition in info. *)
               let info  = add_exp_def info (IcfLetProject (a1, label)) a2 in
               let e = simplify_exp info e in
                  SetProject (a1, label, ty, a2, e)


(*
 * Simplify a LetClosure expression.  We need to simplify the atom, and then
 * see if we've already defined this closure previously.  Use that
 * definition if we have and otherwise create a new one.
 *)
and simplify_letclosure info v ty f a e : exp =
   let a = simplify_atom info a in
      match find_exp_def info (IcfLetClosure (f, a)) with
         Some a' ->
            (* Just add a new binding in info for v. *)
            let info = add_exp_def info (IcfAtom (AtomVar v)) a' in
               simplify_exp info e
       | None ->
            (* Add a new definition. *)
            let info = add_exp_def info (IcfLetClosure (f, a)) (AtomVar v) in
            let info = add_closure_def info v f a in
               LetClosure (v, ty, f, a, simplify_exp info e)



(**************************************************************************
 * INLINING METRIC
 **************************************************************************)



(*
 * This is the record that we will use to store information about functions
 * that we will use in determining what to inline.  We map function names to
 * the number of times they are called and their size.
 *)
type inline_info =
   {  inline_sizes : int SymbolTable.t;
      inline_calls : int SymbolTable.t;
   }


(*
 * The empty inlining information record.
 *)
let inline_empty =
   {  inline_sizes = SymbolTable.empty;
      inline_calls = SymbolTable.empty;
   }


(*
 * Add the size of a function to the inlining info.
 *)
let add_fun_size info f size : inline_info =
   { info with inline_sizes = SymbolTable.add info.inline_sizes f size }


(*
 * Increment the call count of a function.
 *)
let inc_fun_call_count info f : inline_info =
   let i =
      try SymbolTable.find info.inline_calls f
         with Not_found -> 0
   in
      { info with inline_calls = SymbolTable.add info.inline_calls f (i+1) }


(*
 * Returns the size of a function (according to the inlining info).
 * Raises Not_found if there is no entry for f.
 *)
let get_fun_size info f : int =
   SymbolTable.find info.inline_sizes f


(*
 * Returns the number of times a function is called (according to the
 * inlining info).
 *)
let get_fun_call_count info f : int =
   try SymbolTable.find info.inline_calls f with
      Not_found -> 0


(*
 * Scans an expression.  Increments call counts where ever TailCalls and
 * MethodCalls are encountered.  Returns the new info, and the size of the
 * expression that was scanned.
 *)
let rec inline_scan_exp info exp : inline_info * int =
   let pos = string_pos "sizeof_exp" (exp_pos exp) in
      match exp with
         (* Expressions we should never encounter. *)
         LetFuns _ ->
            raise (FirException (pos, StringError "LetFuns encountered"))
       | SetVar _ ->
            raise (FirException (pos, StringError "SetVar encountered"))

         (* These have no size since we will optimize them away. *)
       | LetVar (_, _, _, e)
       | LetAtom (_, _, _, e) ->
            inline_scan_exp info e

         (* These have no size since we might optimize them away. *)
       | TailCall (f, _)
       | MethodCall (f, _, _) ->
            let info = inc_fun_call_count info f in
               info, 0

         (* Branches are bit complex. *)
       | IfThenElse (_, e1, e2)
       | IfType (_, _, _, e1, e2) ->
            let info, size1 = inline_scan_exp info e1 in
            let info, size2 = inline_scan_exp info e2 in
               info, 1 + size1 + size2

         (* Nothing special here. *)
       | LetUnop (_, _, _, _, e)
       | LetBinop (_, _, _, _, _, e)
       | LetExt (_, _, _, _, _, e)
       | LetArray (_, _, _, _, e)
       | LetSubscript (_, _, _, _, e)
       | SetSubscript (_, _, _, _, e)
       | LetRecord (_, _, _, _, e)
       | LetProject (_, _, _, _, e)
       | SetProject (_, _, _, _, e)
       | LetClosure (_, _, _, _, e) ->
            let info, size = inline_scan_exp info e in
               info, size + 1



(**************************************************************************
 * DRIVER CODE
 **************************************************************************)



(*
 * Returns true if and only if the function should be inlined.
 * We want to be really agressive about inlining since we implement
 * all our optimizations as local ones.  Note that we deal with recursive
 * functions elsewhere, so it's okay if we say those should be inlined.
 *)
let should_inline inline_info fclass f : bool =
   true


(*
 * Perform some combination of inlining / constant folding / CSE.
 * Essentially, we just need to go through the program and simplify
 * all the function bodies.
 *)
let inline_prog prog : prog =
   (* Compute the inlining metric(s)for each function in the program. *)
   let iterator = fun info f (fclass, ty, vars, body) ->
      let info, size = inline_scan_exp info body in
      let info = add_fun_size info f size in
         info
   in
   let inline_info = SymbolTable.fold iterator inline_empty prog.prog_funs in

   (* Collect together all the functions that should be inlined. *)
   let iterator = fun icf_info f (fclass, ty, vars, body) ->
      if should_inline inline_info fclass f then
         add_fun_def icf_info f vars body
      else
         icf_info
   in
   let icf_info = SymbolTable.fold iterator icf_empty prog.prog_funs in

   (* Perform the optimization. *)
   let iterator = fun f (fclass, ty, vars, body) ->
      (* We don't want to expand out calls to f within body. *)
      let local_info = add_rec_call icf_info f in
      let body = simplify_exp local_info body in
         (fclass, ty, vars, body)
   in
   let funs = SymbolTable.mapi iterator prog.prog_funs in

   (* Restandardize the program. *)
   let prog = { prog with prog_funs = funs } in
   let prog = standardize_prog prog in
      prog
