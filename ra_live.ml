(*
 * Generate the live sets for abstract assembly instructions.
 *
 * ----------------------------------------------------------------
 *
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
 *)
open Format
open Debug

open Symbol

open Backend
open Symbol_matrix

(*
 * The result of liveness is a list of neighbors for each
 * var, and a set of moves.
 *)
type igraph =
   { igraph_neighbors : SymbolSet.t SymbolTable.t;
     igraph_moves : SymbolMatrix.t
   }

module type LiveSig =
sig
   type prog

   (* Create the liveness graph *)
   val create : prog -> igraph
end

module Live (Backend : BackendSig)
: LiveSig with type prog = Backend.block list =
struct
   (************************************************************************
    * TYPES
    ************************************************************************)

   (*
    * A "program" is a list of blocks.
    *)
   type prog = Backend.block list

   (*
    * A basic block contains a sequence of assembly instructions.
    * it begins with a label, ends with a jump, and contains no
    * other labels.
    *
    * We use faster algorithms for computing defs/uses on
    * basic blocks.
    *)
   type block =
      { (* Save all the block info *)
        block_info  : Backend.block;
        block_label : label;
        block_code  : code list;

        (* Defs and uses *)
        block_defs  : (label * SymbolSet.t) list;
        block_uses  : SymbolSet.t;

        (* Dataflow *)
        block_in  : SymbolSet.t
      }

   (************************************************************************
    * CONTROL FLOW GRAPH                                                   *
    ************************************************************************)

   (*
    * Get the block liveness.
    * Some destinations won't exist (like the seg_fault handler).
    * They have no uses.
    *)
   let block_live blocks label =
      try (SymbolTable.find blocks label).block_in with
         Not_found ->
            SymbolSet.empty

   (*
    * Compute the defs in a basic block.  The defs are the union
    * of all the defined values, captured at the branch points.
    * This was given in the lecture slides.
    *
    * Implemented by: turtles
    *)
   let block_defs code =
      let iterator = fun (dtable, defs) { code_dst   = defs';
                                           code_class = cclass
                                         } ->
            let defs = SymbolSet.add_list defs defs' in
            let dtable =
               match cclass with
                  CodeJump label ->
                     (label, defs) :: dtable
                | CodeNormal
                | CodeMove ->
                     dtable
            in
               dtable, defs
      in
      let dtable, _ = List.fold_left iterator ([], SymbolSet.empty) code in
         dtable


   (*
    * The uses are computed by propagating backward, subtracting defs.
    * This was given in the lecture slides.
    *
    * Implemented by: turtles
    *)
   let block_uses code =
      let rec collect = function
         { code_dst = defs; code_src = uses } :: rest ->
            let uses' = collect rest in
            let uses' = SymbolSet.subtract_list uses' defs in
               SymbolSet.add_list uses' uses
       | [] ->
            SymbolSet.empty
      in
         collect code


   (*
    * Build a block from the assembly instruction list.
    * The first instruction should be a label, and there
    * should be no jumps except for the last instruction.
    *)
   let basic_block block =
      let label = Backend.block_label block in
      let code = Backend.block_code block in
      let defs = block_defs code in
      let uses = block_uses code in
         { block_info  = block;
           block_label = label;
           block_code  = code;
           block_defs  = defs;
           block_uses  = uses;
           block_in    = uses
         }

   (************************************************************************
    * BLOCK DATAFLOW GRAPH
    ************************************************************************)

   (*
    * One iteration of the dataflow function.  This is very similar to the
    * step function in fj_fir_closure.ml The boolean flag changed is false
    * when we reached the fixpoint.
    *
    * The equation we're using is:
    *     in[n] = uses[n] + union (g, dvars) in defs: (in[g] - dvars)
    *
    * Implemented by: turtles
    *)
   let step blocks =
      (* Define a helper function to iterate over all the blocks. *)
      let iterator = fun (blocks, changed) label block ->
            (* Project useful fields from the block. *)
            let { block_defs = block_defs;
                  block_in   = live_in
                } = block
            in

            (* Compute the live-in set for the current block. *)
            let inner_iterator = fun live_in (label', defs') ->
                  let live_in' = block_live blocks label' in
                  let live_in' = SymbolSet.diff live_in' defs' in
                     SymbolSet.union live_in live_in'
            in
            let live_in' = List.fold_left inner_iterator live_in block_defs in

               (* Update the liveness info if necessary. *)
               if SymbolSet.equal live_in live_in' then
                  blocks, changed
               else
                  let block = { block with block_in = live_in' } in
                  let blocks = SymbolTable.add blocks label block in
                     blocks, true
      in
         (* Iterate over all the blocks. *)
         SymbolTable.fold iterator (blocks, false) blocks


   (*
    * Compute the least-fixed point of the dataflow equations:
    *     in[n] = uses[n] + union (g, dvars) in defs: (in[g] - dvars)
    *
    * The dataflow equations are monotone, and the size of the sets is
    * limited by the total number of variables in the program, so we can
    * calculate the least-fixed point by iteration.
    *
    * The input is the table of basic blocks, and this function keeps
    * calling the step function until we reach fixpoint.
    *
    * Implemented by: turtles
    *)
   let rec dataflow blocks =
      let blocks, changed = step blocks in
         if changed then
            dataflow blocks
         else
            blocks


   (************************************************************************
    * INTERFERENCE GRAPH
    ************************************************************************)

   (*
    * Make the graph symmetric.
    *)
   let graph_symmetry graph =
      let neighbors = graph.igraph_neighbors in
      let neighbors =
         SymbolTable.fold (fun neighbors v1 live ->
               SymbolSet.fold (fun neighbors v2 ->
                     SymbolTable.filter_add neighbors v2 (fun vars ->
                           let vars =
                              match vars with
                                 Some vars -> vars
                               | None -> SymbolSet.empty
                           in
                              SymbolSet.add vars v1)) neighbors live) neighbors neighbors
      in

      (* Remove self edges *)
      let neighbors =
         SymbolTable.mapi (fun v live ->
               SymbolSet.remove live v) neighbors
      in
         { graph with igraph_neighbors = neighbors }


   (*
    * Interferance graph operations.
    * For a normal instruction, all the defs interfere with all the live vars.
    * This was given in the lecture slides.
    *
    * Implemented by: turtles
    *)
   let graph_add_normal graph defs live =
      (* Define a helper function to compute the new neighbors. *)
      let iterator = fun neighbors v ->
            (* This returns a new symbol table where the list of neighbors
             * for v is augmented by the given set of live variables. *)
            SymbolTable.filter_add neighbors v (fun vars ->
               let vars =
                  match vars with
                     Some vars  -> vars
                   | None       -> SymbolSet.empty
               in
                  SymbolSet.union live vars)
      in
      let neighbors = List.fold_left iterator graph.igraph_neighbors defs in
         { graph with igraph_neighbors = neighbors }


   (*
    * Interferance graph operations.
    * Moves are special: the defs should be a single variable.
    * The def _does not_ interfere with its uses, although the use
    * almost certainly appears in the given live set, so we need to
    * take that into account.
    *
    * Implemented by: turtles
    *)
   let graph_add_move graph defs uses live =
      (* def does not interfere with its use *)
      let live = SymbolSet.subtract_list live uses in
      let graph = graph_add_normal graph defs live in

      (* def is the dst in MOV, and use is the src in MOV.
       * Matrix operations are imperative, so ignore the return value. *)
      let _ =
         match defs, uses with
            [ def ], [ use ]->
               SymbolMatrix.add graph.igraph_moves def use
          | _ ->
               raise (Failure "ra_live.graph_add_move: invalid MOV")
      in
         graph


   (*
    * Update the interference graph for the instructions within
    * the block.  The live edges are compute from last to first
    * in the instruction order.  Whenever we encounter a jump,
    * we add live_in from the destination.
    *)
   let block_dataflow graph blocks block =
      let { block_code = code } = block in
      let rec collect graph code =
         match code with
            [] ->
               graph, SymbolSet.empty
          | code :: tl ->
               let { code_src   = uses;
                     code_dst   = defs;
                     code_class = cclass
                   } = code
               in
               let graph, live = collect graph tl in
                  match cclass with
                     CodeNormal ->
                        let graph = graph_add_normal graph defs live in
                        let live = SymbolSet.add_list (SymbolSet.subtract_list live defs) uses in
                           graph, live
                   | CodeMove ->
                        let graph = graph_add_move graph defs uses live in
                        let live = SymbolSet.add_list (SymbolSet.subtract_list live defs) uses in
                           graph, live
                   | CodeJump label ->
                        let live = SymbolSet.union live (block_live blocks label) in
                        let graph = graph_add_normal graph defs live in
                        let live = SymbolSet.add_list (SymbolSet.subtract_list live defs) uses in
                           graph, live
      in
      let graph, _ = collect graph code in
         graph

   (*
    * Build the interferance graph from all the blocks.
    *)
   let blocks_dataflow blocks =
      let empty =
         { igraph_neighbors = SymbolTable.empty;
           igraph_moves = SymbolMatrix.create 1021
         }
      in
      let graph =
         SymbolTable.fold (fun graph _ block ->
               block_dataflow graph blocks block) empty blocks
      in
         graph_symmetry graph

   (************************************************************************
    * GLOBAL FUNCTION
    ************************************************************************)

   (*
    * To calculate liveness,
    *    1. create the blocks
    *    2. perform block dataflow
    *    3. map dataflow through the blocks
    *)
   let create blocks =
      let blocks =
         List.fold_left (fun blocks block ->
               let block = basic_block block in
                  SymbolTable.add blocks block.block_label block) SymbolTable.empty blocks
      in
      let blocks = dataflow blocks in
         blocks_dataflow blocks
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
