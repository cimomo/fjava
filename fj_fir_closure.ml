(*
 * Closure conversion.
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
open Debug
open Symbol
open Field_table

open Fj_fir
open Fj_fir_exn
open Fj_fir_pos
open Fj_fir_env
open Fj_fir_type
open Fj_fir_state
open Fj_fir_standardize

module Pos = MakePos (struct let name = "Fj_fir_closure" end)
open Pos

(************************************************************************
 * TYPES
 ************************************************************************)

(*
 * The frame is used during liveness to remember:
 *    fun: the name of the current function
 *    frame: the name of the current frame
 *    parent: the name of the parent frame
 *)
type frame =
   { frame_class : fun_class;
     frame_fun : var;
     frame_frame : var;
     frame_parent : frame option
   }

(*
 * Live for each function has:
 *    frame: the frame info for this function
 *    uses: live vars
 *    defs: all vars defined in this function
 *    calls: functions used in this function
 *)
type live =
   { live_frame : frame;
     live_uses  : SymbolSet.t;
     live_defs  : SymbolSet.t;
     live_calls : SymbolSet.t
   }

(*
 * Info gives the type of each variable in the program,
 * and the liveness info for each function.  Globals are
 * never considered free.
 *)
type info =
   { info_env     : env;
     info_globals : SymbolSet.t;
     info_venv    : (var * ty) SymbolTable.t;
     info_funs    : live SymbolTable.t
   }

(*
 * Close collects the info for each frame.
 *)
type close_frame = ty FieldTable.t

type close_var =
   SimpleVar of ty
 | FrameVar of var * ty

type close_fun =
   GlobalFun of var * var list
 | ContFun of var * var list
 | LocalFun of var list

type close =
   { close_env    : env;
     close_venv   : close_var SymbolTable.t;
     close_funs   : close_fun SymbolTable.t;
     close_frames : close_frame SymbolTable.t
   }

(************************************************************************
 * ENVIRONMENTS
 ************************************************************************)

(*
 * Get the current frame name.
 *)
let frame_name = function
   Some { frame_frame = name } -> name
 | None -> raise (Invalid_argument "frame_frame")

(*
 * Empty live.
 *)
let live_create frame =
   { live_frame = frame;
     live_uses  = SymbolSet.empty;
     live_defs  = SymbolSet.empty;
     live_calls = SymbolSet.empty
   }

(*
 * Add a use.
 *)
let live_var live v =
   { live with live_uses = SymbolSet.add live.live_uses v }

(*
 * Add a def.
 *)
let live_def info live frame v ty =
   let name = frame_name frame in
   let info = { info with info_venv = SymbolTable.add info.info_venv v (name, ty) } in
   let live = { live with live_defs = SymbolSet.add live.live_defs v } in
      info, live

(*
 * Add a definition list.
 *)
let live_defs info live pos frame vars ty_vars =
   let pos = string_pos "live_defs" pos in
   let len1 = List.length vars in
   let len2 = List.length ty_vars in
      if len1 <> len2 then
         raise (FirException (pos, ArityMismatch (len1, len2)));
      List.fold_left2 (fun (info, live) v ty ->
            live_def info live frame v ty) (info, live) vars ty_vars

(*
 * Add a type definition.
 *)
let info_add_var info frame v ty =
   let name = frame_name frame in
      { info with info_venv = SymbolTable.add info.info_venv v (name, ty) }

(*
 * Add a function definition.
 *)
let info_add_fun info f live =
   { info with info_funs = SymbolTable.add info.info_funs f live }

(*
 * Get the variable type.
 *)
let info_lookup_var info pos v =
   try SymbolTable.find info.info_venv v with
      Not_found ->
         raise (FirException (pos, UnboundVar v))

(*
 * Get the function definition.
 *)
let close_lookup_fun info pos f =
   try SymbolTable.find info.close_funs f with
      Not_found ->
         raise (FirException (pos, UnboundVar f))

(*
 * Get the variable type.
 *)
let close_lookup_var info pos v =
   try SymbolTable.find info.close_venv v with
      Not_found ->
         raise (FirException (pos, UnboundVar v))

let close_lookup_var_type info pos v =
   match close_lookup_var info pos v with
      SimpleVar ty ->
         ty
    | FrameVar (_, ty) ->
         ty

(*
 * Get the frame info.
 *)
let close_lookup_frame info pos v =
   try SymbolTable.find info.close_frames v with
      Not_found ->
         raise (FirException (pos, UnboundVar v))

(************************************************************************
 * LIVENESS
 ************************************************************************)

(*
 * Atom liveness.
 *)
let live_atom live a =
   match a with
      AtomUnit
    | AtomNil
    | AtomBool _
    | AtomChar _
    | AtomInt _
    | AtomFloat _ ->
         live
    | AtomVar v ->
         live_var live v

let live_atoms live args =
   List.fold_left live_atom live args

let live_fields live args =
   FieldTable.fold (fun live _ a -> live_atom live a) live args

(*
 * Compute the live vars in the expression.
 *)
let rec live_exp info live frame e =
   let pos = string_pos "live_exp" (exp_pos e) in
      match e with
         LetFuns (funs, e) ->
            live_funs_exp info live frame pos funs e
       | LetVar (v, ty, a, e)
       | LetAtom (v, ty, a, e)
       | LetUnop (v, ty, _, a, e)
       | LetProject (v, ty, a, _, e) ->
            live_atom_exp info live frame pos v ty a e
       | LetBinop (v, ty, _, a1, a2, e)
       | LetSubscript (v, ty, a1, a2, e) ->
            live_args_exp info live frame pos v ty [a1; a2] e
       | LetExt (v, ty, _, _, args, e) ->
            live_args_exp info live frame pos v ty args e
       | TailCall (f, args) ->
            live_tailcall_exp info live frame pos f args
       | MethodCall (f, a, args) ->
            live_tailcall_exp info live frame pos f (a :: args)
       | IfThenElse (a, e1, e2) ->
            live_if_exp info live frame pos a e1 e2
       | IfType (a, name, v, e1, e2) ->
            live_iftype_exp info live frame pos a name v e1 e2
       | SetVar (v, _, a, e) ->
            live_setvar_exp info live frame pos v a e
       | SetSubscript (a1, a2, _, a3, e) ->
            live_set_exp info live frame pos [a1; a2; a3] e
       | SetProject (a1, _, _, a2, e) ->
            live_set_exp info live frame pos [a1; a2] e
       | LetArray (v, ty, args, a, e) ->
            live_args_exp info live frame pos v ty (a :: args) e
       | LetRecord (v, ty, _, args, e) ->
            live_fields_exp info live frame pos v ty args e
       | LetClosure _ ->
            raise (FirException (pos, StringError "level error"))

(*
 * Liveness for nested function definitions.
 *)
and live_fun_exp info frame pos f gflag ty vars body =
   let pos = string_pos "live_fun_exp" pos in
   let ty_vars, _ = dest_fun_or_method_type info.info_env pos ty in
   let frame =
      match gflag, frame with
         FunGlobalClass, _ ->
            let name = new_symbol_string (Symbol.to_string f ^ "_frame") in
               { frame_class = FunGlobalClass;
                 frame_fun = name;
                 frame_frame = name;
                 frame_parent = frame
               }
       | FunContClass, Some frame ->
            let name = new_symbol_string (Symbol.to_string f ^ "_frame") in
               { frame with frame_class = gflag; frame_fun = name }
       | FunLocalClass, Some frame ->
            { frame with frame_class = gflag }
       | FunContClass, None
       | FunLocalClass, None ->
            raise (FirException (pos, StringVarError ("illegal function", f)))
   in
   let live = live_create frame in
   let frame = Some frame in
   let info, live = live_defs info live pos frame vars ty_vars in
   let info, live = live_exp info live frame body in
      info_add_fun info f live

and live_funs_exp info live frame pos funs e =
   let pos = string_pos "live_funs_exp" pos in
   let info =
      List.fold_left (fun info (f, (_, ty, _, _)) ->
            info_add_var info frame f ty) info funs
   in
   let info =
      List.fold_left (fun info (f, (gflag, ty, vars, body)) ->
            live_fun_exp info frame pos f gflag ty vars body) info funs
   in
      live_exp info live frame e

(*
 * Liveness for a variable definition.
 *)
and live_atom_exp info live frame pos v ty a e =
   let pos = string_pos "live_atom_exp" pos in
   let info, live = live_def info live frame v ty in
   let info, live = live_exp info live frame e in
   let live = live_atom live a in
      info, live

(*
 * Liveness for a variable number of arguments.
 *)
and live_args_exp info live frame pos v ty args e =
   let pos = string_pos "live_args_exp" pos in
   let info, live = live_def info live frame v ty in
   let info, live = live_exp info live frame e in
   let live = live_atoms live args in
      info, live

(*
 * Liveness for a field table.
 *)
and live_fields_exp info live frame pos v ty args e =
   let pos = string_pos "live_fields_exp" pos in
   let info, live = live_def info live frame v ty in
   let info, live = live_exp info live frame e in
   let live = live_fields live args in
      info, live

(*
 * Liveness for a tailcall.
 * This is a special case, we add a def for the tailcall.
 *)
and live_tailcall_exp info live frame pos f args =
   let pos = string_pos "live_tailcall_exp" pos in
   let live = live_atoms live args in
   let live = live_var live f in
      info, live

(*
 * Liveness for a conditional.
 * The uses are the union of both branches, plus any live in the test.
 *)
and live_if_exp info live frame pos a e1 e2 =
   let pos = string_pos "live_if_exp" pos in
   let info, live = live_exp info live frame e1 in
   let info, live = live_exp info live frame e2 in
   let live = live_atom live a in
      info, live

(*
 * Liveness for a type conditional.
 * The uses are the union of both branches, plus any live in the test.
 *)
and live_iftype_exp info live frame pos a name v e1 e2 =
   let pos = string_pos "live_iftype_exp" pos in
   let ty = env_lookup_tyname info.info_env pos name in
   let info, live = live_def info live frame v ty in
   let info, live = live_exp info live frame e1 in
   let info, live = live_exp info live frame e2 in
   let live = live_atom live a in
      info, live

(*
 * Set a variable.
 *)
and live_setvar_exp info live frame pos v a e =
   let pos = string_pos "live_setvar_exp" pos in
   let live = live_var live v in
   let live = live_atom live a in
   let info, live = live_exp info live frame e in
      info, live

(*
 * Liveness for a set subscript operator.
 *)
and live_set_exp info live frame pos args e =
   let pos = string_pos "live_set_exp" pos in
   let live = live_atoms live args in
   let info, live = live_exp info live frame e in
      info, live

(*
 * Compute the live vars of each function.
 *)
let live_prog prog =
   let { prog_funs = funs;
         prog_tynames = names;
         prog_globals = globals
       } = prog
   in

   (* Collect all global names into a set *)
   let globs = SymbolSet.empty in
   let globs =
      SymbolTable.fold (fun globs f _ ->
            SymbolSet.add globs f) globs funs
   in
   let globs =
      SymbolTable.fold (fun globs v _ ->
            SymbolSet.add globs v) globs names
   in
   let globs =
      SymbolTable.fold (fun globs v _ ->
            SymbolSet.add globs v) globs globals
   in

   (* Initial info *)
   let info =
      { info_env = env_of_prog prog;
        info_globals = globs;
        info_venv = SymbolTable.empty;
        info_funs = SymbolTable.empty
      }
   in
      (* Compute liveness for each function *)
      SymbolTable.fold (fun info f (gflag, ty, vars, body) ->
               live_fun_exp info None (var_exp_pos f) f gflag ty vars body) info funs

(************************************************************************
 * LIVENESS FIXPOINT
 ************************************************************************)

(*
 * Solve the dataflow equations.
 *    f.uses = f.uses + union (g in f.calls). (g.uses - f.defs)
 *)
let step funs =
   SymbolTable.fold (fun (funs, changed) f live ->
         let { live_uses = uses;
               live_defs = defs;
               live_calls = calls
             } = live
         in
         let uses' =
            SymbolSet.fold (fun uses g ->
                  try
                     let live = SymbolTable.find funs g in
                        SymbolSet.union uses live.live_uses
                  with
                     Not_found ->
                        uses) uses calls
         in
         let uses' = SymbolSet.diff uses' defs in
            if SymbolSet.equal uses' uses then
               funs, changed
            else
               let live = { live with live_uses = uses' } in
               let funs = SymbolTable.add funs f live in
                  funs, true) (funs, false) funs

(*
 * Repeat until we reach a fixpoint.
 *)
let rec fixpoint funs =
   let funs, changed = step funs in
      if changed then
         fixpoint funs
      else
         funs

(*
 * Clean up the uses.
 * Globals and funs are never free.
 *)
let clean_uses info =
   let { info_funs = funs;
         info_globals = globals
       } = info
   in

   (* Add the funs to the globals *)
   let globals =
      SymbolTable.fold (fun globals f _ ->
            SymbolSet.add globals f) globals funs
   in

   (* Remove all the extra uses *)
   let funs =
      SymbolTable.map (fun live ->
            let { live_uses = uses;
                  live_defs = defs
                } = live
            in
            let calls = SymbolSet.diff uses defs in
            let uses = SymbolSet.diff calls globals in
               { live with live_uses = uses;
                           live_calls = calls
               }) funs
   in
      { info with info_funs = funs;
                  info_globals = globals
      }

(*
 * Solve the dataflow equations.
 *)
let dflow info =
   let info = clean_uses info in
   let funs = fixpoint info.info_funs in
      { info with info_funs = funs }

(************************************************************************
 * FRAMES
 ************************************************************************)

(*
 * List the parent frames.
 *)
let rec frame_parents = function
   Some { frame_frame = name; frame_parent = parent } ->
      name :: frame_parents parent
 | None ->
      []

let frame_list { frame_frame = name; frame_parent = parent } =
   name :: frame_parents parent

(*
 * Add all the escaping vars.
 *)
let frame prog info =
   let { prog_types = tenv } = prog in
   let { info_env = env;
         info_funs = funs;
         info_venv = ienv
       } = info
   in

   (*
    * The initial environment has an simple entry for each var.
    * We'll catch the escaping vars below.
    *)
   let venv =
      env_fold_var (fun venv v ty ->
            SymbolTable.add venv v (SimpleVar ty)) SymbolTable.empty env
   in
   let venv =
      SymbolTable.fold (fun venv v (_, ty) ->
            SymbolTable.add venv v (SimpleVar ty)) venv ienv
   in

   (* Add default frames for all the functions *)
   let frames =
      SymbolTable.fold (fun frames _ { live_frame = { frame_fun = name; frame_parent = parent } } ->
            let frame =
               match parent with
                  Some { frame_frame = frame } ->
                     FieldTable.add FieldTable.empty frame (TyId frame)
                | None ->
                     FieldTable.empty
            in
               SymbolTable.add frames name frame) SymbolTable.empty funs
   in

   (* Add all escaping vars to the frames *)
   let venv, frames =
      SymbolTable.fold (fun (venv, frames) _ live ->
            let { live_frame = { frame_fun = frame1 };
                  live_uses = uses
                } = live
            in
            let venv, frames =
               SymbolSet.fold (fun (venv, frames) v ->
                     let pos = var_exp_pos v in
                     let frame2, ty = info_lookup_var info pos v in
                        if Symbol.eq frame1 frame2 then
                           venv, frames
                        else
                           let frame = SymbolTable.find frames frame2 in
                           let frame = FieldTable.add frame v ty in
                           let frames = SymbolTable.add frames frame2 frame in
                           let venv = SymbolTable.add venv v (FrameVar (frame2, ty)) in
                              venv, frames) (venv, frames) uses
            in
               venv, frames) (venv, frames) funs
   in

   (* Add type definitions for all the frames *)
   let tenv =
      SymbolTable.fold (fun tenv frame fields ->
            SymbolTable.add tenv frame (TyRecord (RecordFrame, fields))) tenv frames
   in

   (* Add all the frames to the venv *)
   let venv =
      SymbolTable.fold (fun venv frame fields ->
            SymbolTable.add venv frame (SimpleVar (TyId frame))) venv frames
   in

   (* Reduce uses by the framed vars *)
   let funs =
      SymbolTable.mapi (fun f live ->
            let { live_frame = frame;
                  live_uses = uses
                } = live
            in
            let { frame_class = gflag;
                  frame_frame = name;
                  frame_parent = parent
                } = frame
            in
               match gflag with
                  FunGlobalClass ->
                     GlobalFun (name, frame_parents parent)
                | FunContClass ->
                     ContFun (name, frame_parents parent)
                | FunLocalClass ->
                     let uses =
                        SymbolSet.fold (fun uses v ->
                              match SymbolTable.find venv v with
                                 SimpleVar _ ->
                                    SymbolSet.add uses v
                               | FrameVar (frame, _) ->
                                    SymbolSet.add uses frame) SymbolSet.empty uses
                     in
                        LocalFun (SymbolSet.to_list uses)) funs
   in
   let info =
      { close_env = env;
        close_venv = venv;
        close_funs = funs;
        close_frames = frames
      }
   in
   let prog =
      { prog with prog_types = tenv }
   in
      prog, info

(************************************************************************
 * CLOSURE
 ************************************************************************)

(*
 * Close a var.  If the var is a function,
 * create a closure.
 *)
let close_var info pos v cont =
   let pos = string_pos "close_var" pos in
      if SymbolTable.mem info.close_funs v then
         match close_lookup_fun info pos v with
            GlobalFun (_, []) ->
               cont v
          | GlobalFun (_, parent :: _)
          | ContFun (parent, _) ->
               let v' = new_symbol v in
               let ty = close_lookup_var_type info pos v in
                  LetClosure (v', ty, v, AtomVar parent, cont v')
          | LocalFun _ ->
               raise (FirException (pos, StringVarError ("local functions can't escape", v)))
      else
         match close_lookup_var info pos v with
            SimpleVar _ ->
               cont v
          | FrameVar (frame, ty) ->
               LetProject (v, ty, AtomVar frame, v, cont v)

(*
 * Close a var for a tailcall.
 *)
let close_fun_var info pos v cont =
   let pos = string_pos "close_var" pos in
      if SymbolTable.mem info.close_funs v then
         match close_lookup_fun info pos v with
            GlobalFun (_, []) ->
               cont v []
          | GlobalFun (_, parent :: _) ->
               cont v [AtomVar parent]
          | ContFun (parent, _) ->
               cont v [AtomVar parent]
          | LocalFun uses ->
               cont v (List.map (fun v -> AtomVar v) uses)
      else
         match close_lookup_var info pos v with
            SimpleVar _ ->
               cont v []
          | FrameVar (frame, ty) ->
               LetProject (v, ty, AtomVar frame, v, cont v [])

(*
 * Close an atom.  Check for function variables.
 *)
let close_atom info pos a cont =
   match a with
      AtomUnit
    | AtomNil
    | AtomBool _
    | AtomChar _
    | AtomInt _
    | AtomFloat _ ->
         cont a
    | AtomVar v ->
         close_var info pos v (fun v -> cont (AtomVar v))

let close_atoms info pos args cont =
   let rec close args' args =
      match args with
         a :: args ->
            close_atom info pos a (fun a ->
                  close (a :: args') args)
       | [] ->
            cont (List.rev args')
   in
      close [] args

let close_fields info pos args cont =
   let args = FieldTable.to_list args in
   let rec close args' args =
      match args with
         (v, a) :: args ->
            close_atom info pos a (fun a ->
                  close (FieldTable.add args' v a) args)
       | [] ->
            cont args'
   in
      close FieldTable.empty args

(*
 * Close an exp.
 *)
let rec close_exp info e =
   let pos = string_pos "close_exp" (exp_pos e) in
      match e with
         LetFuns (funs, e) ->
            close_funs_exp info pos funs e
       | LetVar (v, ty, a, e)
       | LetAtom (v, ty, a, e)
       | SetVar (v, ty, a, e) ->
            close_atom_exp info pos v ty a e
       | LetUnop (v, ty, op, a, e) ->
            close_unop_exp info pos v ty op a e
       | LetBinop (v, ty, op, a1, a2, e) ->
            close_binop_exp info pos v ty op a1 a2 e
       | LetExt (v, ty1, s, ty2, args, e) ->
            close_ext_exp info pos v ty1 s ty2 args e
       | TailCall (f, args) ->
            close_tailcall_exp info pos f args
       | MethodCall (f, a, args) ->
            close_methodcall_exp info pos f a args
       | IfThenElse (a, e1, e2) ->
            close_if_exp info pos a e1 e2
       | IfType (a, cname, v, e1, e2) ->
            close_iftype_exp info pos a cname v e1 e2
       | LetSubscript (v, ty, a1, a2, e) ->
            close_subscript_exp info pos v ty a1 a2 e
       | SetSubscript (a1, a2, ty, a3, e) ->
            close_set_subscript_exp info pos a1 a2 ty a3 e
       | LetProject (v, ty, a, label, e) ->
            close_project_exp info pos v ty a label e
       | SetProject (a1, label, ty, a2, e) ->
            close_set_project_exp info pos a1 label ty a2 e
       | LetArray (v, ty, args, a, e) ->
            close_array_exp info pos v ty args a e
       | LetRecord (v, ty, rclass, args, e) ->
            close_record_exp info pos v ty rclass args e
       | LetClosure _ ->
            raise (FirException (pos, StringError "illegal closure"))

(*
 * Close a function definition.
 * Add all the uses as extra parameters.
 *)
and close_fun info pos f (gflag, ty, vars, body) =
   let pos = string_pos "close_fun" pos in
   let body = close_exp info body in
      match close_lookup_fun info pos f with
         GlobalFun (name, parents) ->
            close_global_fun info pos f name parents ty vars body
       | ContFun (name, parents) ->
            close_cont_fun info pos f name parents ty vars body
       | LocalFun uses ->
            close_local_fun info pos f uses ty vars body

(*
 * Project the frames out of a list of parent frames.
 * Assumes that children come first.
 *
 * Here's the idea.  We need to project the parent frames out of children
 * frames.  BUT, we need to be careful of scoping issues, which is why we
 * construct these expression top-down, instead of bottom-up.  (We found
 * it's easier to keep track of scoping if you go top-down.)
 *
 * Standardization will take care of the naming mess this function creates.
 *
 * NOTE: This was hacked in by --emre to fix a bug in the solution code.
 *)
and project_parent_frames parents body =
   let rec iterator parents =
      match parents with
         p1 :: p2 :: rest ->
            LetProject (p2, TyId p2, AtomVar p1, p2, iterator (p2 :: rest))
       | _ ->
            body
   in
      iterator parents

(*
 * Close a global fun.  Allocate the frame, and project all the nested frames.
 *)
and close_global_fun info pos f frame parents ty vars body =
   let pos = string_pos "close_global_fun" pos in

   (* Add the parent frame to the function *)
   let ty, vars, body =
      match parents with
         parent :: rest ->
            (* Store the parent in the frame *)
            let body = SetProject (AtomVar frame, parent, TyId parent, AtomVar parent, body) in

            (* Fetch all the other parents *)
            let body = project_parent_frames parents body in

            (* Add the parent arg *)
            let vars = parent :: vars in
            let ty_vars, ty_res = dest_fun_type info.close_env (int_pos 3 pos) ty in
            let ty = TyFun (TyId parent :: ty_vars, ty_res) in
               ty, vars, body
       | [] ->
            ty, vars, body
   in

   (* Allocate the frame *)
   let body =
      let fields = close_lookup_frame info (int_pos 4 pos) frame in
      let init =
         FieldTable.mapi (fun v ty ->
               if List.mem v vars then
                  AtomVar v
               else
                  default_atom ty) fields
      in
      let ty_frame = TyId frame in
         LetRecord (frame, ty_frame, RecordFrame, init, body)
   in
      f, (FunGlobalClass, ty, vars, body)

(*
 * Close a continuation.  This is a lot like a global fun, but there is no
 * frame allocation.
 *)
and close_cont_fun info pos f frame parents ty vars body =
   let pos = string_pos "close_cont_fun" pos in

   (* Save the args in the frame *)
   let body =
      List.fold_left (fun body v ->
            match close_lookup_var info pos v with
               SimpleVar _ ->
                  body
             | FrameVar (frame, ty) ->
                  SetProject (AtomVar frame, v, ty, AtomVar v, body)) body vars
   in

   (* Fetch all the parents from the frame *)
   let body = project_parent_frames (frame :: parents) body in

   (* Add the parent to the args *)
   let vars = frame :: vars in
   let ty_vars, ty_res = dest_fun_type info.close_env pos ty in
   let ty = TyFun (TyId frame :: ty_vars, ty_res) in
      f, (FunContClass, ty, vars, body)

(*
 * Close a local fun.
 * Add all the free vars as extra arguments.
 *)
and close_local_fun info pos f uses ty vars body =
   let pos = string_pos "close_local_fun" pos in

   (* Add all the uses to the argument list *)
   let ty_uses = List.map (fun v -> close_lookup_var_type info (int_pos 2 pos) v) uses in
   let ty_vars, ty_res = dest_fun_type info.close_env (int_pos 3 pos) ty in
   let ty = TyFun (ty_uses @ ty_vars, ty_res) in
   let vars = uses @ vars in

   (* Save the args in the frame *)
   let body =
      List.fold_left (fun body v ->
            match close_lookup_var info pos v with
               SimpleVar _ ->
                  body
             | FrameVar (frame, ty) ->
                  SetProject (AtomVar frame, v, ty, AtomVar v, body)) body vars
   in
      f, (FunLocalClass, ty, vars, body)

(*
 * Close the function definitions.
 *)
and close_funs_exp info pos funs e =
   let pos = string_pos "close_funs_exp" pos in
   let funs =
      List.map (fun (f, def) ->
            close_fun info pos f def) funs
   in
   let e = close_exp info e in
      LetFuns (funs, e)

(*
 * Close a let-assignment.
 *)
and close_letvar info pos v ty v' e =
   match close_lookup_var info pos v with
      SimpleVar _ ->
         LetAtom (v, ty, AtomVar v', close_exp info e)
    | FrameVar (frame, _) ->
         SetProject (AtomVar frame, v, ty, AtomVar v', close_exp info e)

(*
 * Close a variable definition.
 *)
and close_atom_exp info pos v ty a e =
   let pos = string_pos "close_atom_exp" pos in
      close_atom info pos a (fun a ->
            let v' = new_symbol v in
               LetAtom (v', ty, a, close_letvar info pos v ty v' e))

(*
 * Operators.
 *)
and close_unop_exp info pos v ty op a e =
   let pos = string_pos "close_unop_exp" pos in
      close_atom info pos a (fun a ->
            let v' = new_symbol v in
               LetUnop (v', ty, op, a, close_letvar info pos v ty v' e))

and close_binop_exp info pos v ty op a1 a2 e =
   let pos = string_pos "close_binop_exp" pos in
      close_atom info pos a1 (fun a1 ->
      close_atom info pos a2 (fun a2 ->
            let v' = new_symbol v in
               LetBinop (v', ty, op, a1, a2, close_letvar info pos v ty v' e)))

and close_ext_exp info pos v ty1 s ty2 args e =
   let pos = string_pos "close_ext_exp" pos in
      close_atoms info pos args (fun args ->
            let v' = new_symbol v in
               LetExt (v', ty1, s, ty2, args, close_letvar info pos v ty1 v' e))

(*
 * Tailcall.  We'll try to be a little smart,
 * and add extra args when possible, rather than generating
 * a closure.
 *)
and close_tailcall_exp info pos f args =
   let pos = string_pos "close_tailcall_exp" pos in
      close_atoms info pos args (fun args ->
      close_fun_var info pos f (fun f args' ->
            TailCall (f, args' @ args)))

and close_methodcall_exp info pos f a args =
   let pos = string_pos "close_methodcall_exp" pos in
      close_atom info pos a (fun a ->
      close_atoms info pos args (fun args ->
      close_fun_var info pos f (fun f args' ->
            MethodCall (f, a, args' @ args))))

(*
 * Conditional.
 *)
and close_if_exp info pos a e1 e2 =
   let pos = string_pos "close_if_exp" pos in
      close_atom info pos a (fun a ->
            IfThenElse (a, close_exp info e1, close_exp info e2))

and close_iftype_exp info pos a cname v e1 e2 =
   let pos = string_pos "close_iftype_exp" pos in
      close_atom info pos a (fun a ->
            IfType (a, cname, v, close_exp info e1, close_exp info e2))

(*
 * Subscripts.
 *)
and close_subscript_exp info pos v ty a1 a2 e =
   let pos = string_pos "close_subscript_exp" pos in
      close_atom info pos a1 (fun a1 ->
      close_atom info pos a2 (fun a2 ->
            let v' = new_symbol v in
               LetSubscript (v', ty, a1, a2, close_letvar info pos v ty v' e)))

and close_set_subscript_exp info pos a1 a2 ty a3 e =
   let pos = string_pos "close_set_subscript_exp" pos in
      close_atom info pos a1 (fun a1 ->
      close_atom info pos a2 (fun a2 ->
      close_atom info pos a3 (fun a3 ->
            SetSubscript (a1, a2, ty, a3, close_exp info e))))

(*
 * Record projections.
 *)
and close_project_exp info pos v ty a label e =
   let pos = string_pos "close_project_exp" pos in
      close_atom info pos a (fun a ->
            let v' = new_symbol v in
               LetProject (v', ty, a, label, close_letvar info pos v ty v' e))

and close_set_project_exp info pos a1 label ty a2 e =
   let pos = string_pos "close_set_project_exp" pos in
      close_atom info pos a1 (fun a1 ->
      close_atom info pos a2 (fun a2 ->
            SetProject (a1, label, ty, a2, close_exp info e)))

(*
 * Allocation.
 *)
and close_array_exp info pos v ty args a e =
   let pos = string_pos "close_array_exp" pos in
      close_atoms info pos args (fun args ->
      close_atom info pos a (fun a ->
            let v' = new_symbol v in
               LetArray (v', ty, args, a, close_letvar info pos v ty v' e)))

and close_record_exp info pos v ty rclass args e =
   let pos = string_pos "close_record_exp" pos in
      close_fields info pos args (fun args ->
            let v' = new_symbol v in
               LetRecord (v', ty, rclass, args, close_letvar info pos v ty v' e))

(************************************************************************
 * PRINTERS
 ************************************************************************)

(*
 * Print a live entry.
 *)
let pp_print_set buf s =
   SymbolSet.iter (fun v ->
         fprintf buf "@ %a" pp_print_symbol v) s

let pp_print_live buf f { live_uses = uses; live_defs = defs } =
   fprintf buf "@ @[<hv 3>%a =@ @[<hv 3>uses: @ %a@]@ @[<hv 3>defs:@ %a@]@]" (**)
      pp_print_symbol f
      pp_print_set uses
      pp_print_set defs

let pp_print_info buf info =
   fprintf buf "@[<hv 3>Liveness:";
   SymbolTable.iter (fun f live ->
         pp_print_live buf f live) info.info_funs;
   pp_close_box buf ()

let print_info info =
   if debug print_fir then
      fprintf err_formatter "%a@." pp_print_info info

(************************************************************************
 * MAIN PROG
 ************************************************************************)

(*
 * Closure conversion for all the functions in the program.
 *)
let close_prog prog =
   let info = live_prog prog in
   let _ = print_info info in
   let info = dflow info in
   let _ = print_info info in
   let prog, close = frame prog info in
   let { prog_funs = funs } = prog in
   let funs =
      SymbolTable.mapi (fun f def ->
            let _, def = close_fun close (var_exp_pos f) f def in
               def) funs
   in
   let prog = { prog with prog_funs = funs } in
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
