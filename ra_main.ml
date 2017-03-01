(*
 * The register allocator performs the following steps:
 *    1. Code generation
 *    2. Register assignment
 *    3. Move coalescing
 *
 * This allocator is based directly on Appel.
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
open Ra_live
open Ra_state

module type RegAllocSig =
sig
   type block
   type spset

   val compile : spset -> block list -> spset * block list
end

module RegAlloc (Backend : BackendSig)
: RegAllocSig
  with type block = Backend.block
  with type spset = Backend.spset =
struct
   type block = Backend.block
   type spset = Backend.spset

   (*
    * Modules.
    *)
   module Live = Live (Backend)

   (*
    * Classification for nodes.
    * A node can be:
    *    Precolored: a machine register
    *    SimpWL: on the simplify worklist
    *    SpillWL: on the spill worklist
    *    Spilled: spilled
    *    Coalesced: coalesced (set equal to another node)
    *    Colored: assigned to a machine register
    *    Stack: placed on the allocation stack
    *
    * We want to support two functions efficiently:
    *    reclassify node class: reclassify a node
    *    nodes_in_class class: all the nodes with a given class
    *
    * Reclassify takes constant time.
    *)
   type node_class =
      NodePrecolored
    | NodeInitial
    | NodeSimpWL
    | NodeFreezeWL
    | NodeSpillWL
    | NodeSpilled
    | NodeCoalesced
    | NodeColored
    | NodeStack

   (*
    * Moves are also classified.
    *    Coalesced: move has been coalesced
    *    Constrained: the nodes in the move interfere
    *    Frozen: move is not being considered for coalescing
    *    WL: on the move worklist for consideration
    *    Active: may be considered for coalescing eventually
    *)
   type move_class =
      MoveCoalesced
    | MoveConstrained
    | MoveFrozen
    | MoveWL
    | MoveActive

   (*
    * For each node we keep
    *    node_node: the classification of the node
    *    node_class: the current classification of the node
    *    node_degree: the number of neighbors
    *    node_alias: a new name for the node
    *    node_color: color that is assigned to a node
    *    node_moves: all the moves the node is in
    *)
   type node =
      { node_name : var;
        mutable node_class : node_class;
        mutable node_degree : int;
        mutable node_alias : node option;
        mutable node_color : var option;
        mutable node_moves : move list;
        mutable node_neighbors : node list;

        (* Linked list *)
        mutable node_pred : node option ref;
        node_succ : node option ref
      }

   (*
    * For the move
    *    move_dst: dst of the move
    *    move_src: src of the move
    *    move_class: current classification of the move
    *)
   and move =
      { move_dst : node;
        move_src : node;
        mutable move_class : move_class;

        (* Linked list *)
        mutable move_pred : move option ref;
        move_succ : move option ref
      }

   (*
    * All this stuff is defined in Appel,
    * "Modern Compiler Implementation in ML",
    * 1998, Cambridge University Press,
    * pages 242-243.
    *)
   type ra =
      { (* Node worklists *)
        ra_precolored : node option ref;
        ra_initial : node option ref;
        ra_simp_wl : node option ref;
        ra_freeze_wl : node option ref;
        ra_spill_wl : node option ref;
        ra_spilled : node option ref;
        ra_coalesced : node option ref;
        ra_colored : node option ref;
        ra_stack : node option ref;

        (* Move worklists *)
        mv_coalesced : move option ref;
        mv_constrained : move option ref;
        mv_frozen : move option ref;
        mv_wl : move option ref;
        mv_active : move option ref;

        (* Explicit representation of the graph *)
        ra_edges : SymbolMatrix.t
      }

   (************************************************************************
    * NODE OPERATIONS
    ************************************************************************)

   (*
    * Get the list pointer for a node class.
    *)
   let node_worklist ra_nodes = function
      NodePrecolored -> ra_nodes.ra_precolored
    | NodeInitial -> ra_nodes.ra_initial
    | NodeSimpWL -> ra_nodes.ra_simp_wl
    | NodeFreezeWL -> ra_nodes.ra_freeze_wl
    | NodeSpillWL -> ra_nodes.ra_spill_wl
    | NodeSpilled -> ra_nodes.ra_spilled
    | NodeCoalesced -> ra_nodes.ra_coalesced
    | NodeColored -> ra_nodes.ra_colored
    | NodeStack -> ra_nodes.ra_stack

   (*
    * Test for empty.
    *)
   let node_list_is_empty ra cl =
      let l = node_worklist ra cl in
         match !l with
            Some _ -> false
          | None -> true

   (*
    * Get the head of a list.
    *)
   let node_list_head ra cl =
      let l = node_worklist ra cl in
         match !l with
            Some node -> node
          | None -> raise (Invalid_argument "node_list_head")

   (*
    * Iterate through the node list.
    *)
   let node_iter ra cl f =
      let rec iter = function
         Some node ->
            let next = !(node.node_succ) in
               f node;
               iter next
       | None ->
            ()
      in
         iter (!(node_worklist ra cl))

   let node_fold ra cl f x =
      let rec fold x node =
         match node with
            Some node ->
               let next = !(node.node_succ) in
               let x = f x node in
                  fold x next
          | None ->
               x
      in
         fold x (!(node_worklist ra cl))

   let node_to_list ra cl =
      let rec collect = function
         Some node ->
            node :: collect (!(node.node_succ))
       | None ->
            []
      in
         collect (!(node_worklist ra cl))

   (*
    * Get successor node.
    *)
   let node_succ node =
      match !(node.node_succ) with
         Some node -> node
       | None -> raise (Invalid_argument "node_succ")

   (*
    * Has a succ?
    *)
   let node_has_no_succ node =
      match !(node.node_succ) with
         Some _ -> false
       | None -> true

   (*
    * Create a node in a class.
    *)
   let new_node ra v cl =
      let l = node_worklist ra cl in
      let next = !l in
      let succ = ref next in

      (* Postpone computation of neighbors *)
      let node' =
         { node_name = v;
           node_class = cl;
           node_degree = 0;
           node_alias = None;
           node_color = None;
           node_neighbors = [];
           node_moves = [];
           node_pred = l;
           node_succ = succ
         }
      in
      let _ =
         (* Link it into the list *)
         l := Some node';
         match next with
            Some next -> next.node_pred <- succ
          | None -> ()
      in
         node'

   (*
    * Reclassify the node.
    *)
   let node_reclassify ra node cl =
      (* Delete the node from its current list *)
      let pred = node.node_pred in
      let succ = !(node.node_succ) in
      let _ =
         pred := succ;
         match succ with
            Some next -> next.node_pred <- pred
          | None -> ()
      in

      (* Add to the new list *)
      let l = node_worklist ra cl in
      let next = !l in
         l := Some node;
         assert(node.node_class <> NodePrecolored);
         node.node_class <- cl;
         node.node_pred <- l;
         node.node_succ := next;
         match next with
            Some next -> next.node_pred <- node.node_succ
          | None -> ()

   (*
    * Test for equality.
    *)
   let node_eq node1 node2 =
      Symbol.eq node1.node_name node2.node_name

   (*
    * Get the moves for a node.
    *)
   let node_moves ra node =
      List.filter (fun move ->
            match move.move_class with
               MoveActive
             | MoveWL ->
                  true
             | MoveFrozen
             | MoveCoalesced
             | MoveConstrained ->
                  false) node.node_moves

   (*
    * Get the node's neighbors.
    *)
   let node_neighbors ra node =
      List.filter (fun node ->
            match node.node_class with
               NodeStack
             | NodeCoalesced ->
                  false
             | _ ->
                  true) node.node_neighbors

   (*
    * Spilled neighbors.
    *)
   let node_spill_neighbors ra node =
      List.filter (fun node ->
            match node.node_class with
               NodeSpilled ->
                  true
             | _ ->
                  false) node.node_neighbors

   (*
    * Say if a node is move related.
    *)
   let node_moves node =
      List.filter (fun move ->
            match move.move_class with
               MoveWL
             | MoveActive ->
                  true
             | MoveCoalesced
             | MoveFrozen
             | MoveConstrained ->
                  false) node.node_moves

   let node_is_move_related node =
      List.exists (fun move ->
            match move.move_class with
               MoveWL
             | MoveActive ->
                  true
             | MoveCoalesced
             | MoveFrozen
             | MoveConstrained ->
                  false) node.node_moves

   (*
    * Union of two node lists.
    *)
   let node_list_union nodes1 nodes2 =
      let rec mem node1 = function
         node2 :: nodes2 ->
            node_eq node1 node2 || mem node1 nodes2
       | [] ->
            false
      in
      let rec collect nodes1 nodes2 =
         match nodes1 with
            node1 :: nodes1 ->
               if mem node1 nodes2 then
                  collect nodes1 nodes2
               else
                  node1 :: collect nodes1 nodes2
          | [] ->
               nodes2
      in
         collect nodes1 nodes2

   (*
    * Get string representation (var) of a node.
    *)
   let var_of_node ra node =
      node.node_name

   let string_of_node ra node =
      string_of_symbol (var_of_node ra node)

   (************************************************************************
    * MOVE OPERATIONS
    ************************************************************************)

   (*
    * Get the list pointer for a node class.
    *)
   let move_worklist ra_moves = function
      MoveCoalesced -> ra_moves.mv_coalesced
    | MoveConstrained -> ra_moves.mv_constrained
    | MoveFrozen -> ra_moves.mv_frozen
    | MoveWL -> ra_moves.mv_wl
    | MoveActive -> ra_moves.mv_active

   (*
    * Get the head of a list.
    *)
   let move_list_head ra cl =
      let l = move_worklist ra cl in
         match !l with
            Some move -> move
          | None -> raise (Invalid_argument "move_list_head")

   (*
    * Iterate through the node list.
    *)
   let move_iter ra cl f =
      let rec iter = function
         Some move ->
            let next = !(move.move_succ) in
               f move;
               iter next
       | None ->
            ()
      in
         iter (!(move_worklist ra cl))

   let move_fold ra cl f x =
      let rec fold x move =
         match move with
            Some move ->
               let next = !(move.move_succ) in
               let x = f x move in
                  fold x next
          | None ->
               x
      in
         fold x (!(move_worklist ra cl))

   (*
    * Create a node in a class.
    *)
   let new_move ra_moves dst src cl =
      let l = move_worklist ra_moves cl in
      let next = !l in
      let succ = ref next in
      let node' =
         { move_dst = dst;
           move_src = src;
           move_class = cl;
           move_pred = l;
           move_succ = succ
         }
      in
      let _ =
         (* Link it into the list *)
         l := Some node';
         match next with
            Some next -> next.move_pred <- succ
          | None -> ()
      in
         node'

   (*
    * Reclassify the node.
    *)
   let move_reclassify ra move cl =
      (* Delete the move from its current list *)
      let pred = move.move_pred in
      let succ = !(move.move_succ) in
      let _ =
         pred := succ;
         match succ with
            Some next -> next.move_pred <- pred
          | None -> ()
      in

      (* Add to the new list *)
      let l = move_worklist ra cl in
      let next = !l in
         l := Some move;
         move.move_class <- cl;
         move.move_pred <- l;
         move.move_succ := next;
         match next with
            Some next -> next.move_pred <- move.move_succ
          | None -> ()

   (************************************************************************
    * PRINTING
    ************************************************************************)

   (*
    * Print the register allocator state.
    *)
   let print_ra ra =
      node_iter ra NodePrecolored (fun node ->
            let v = var_of_node ra node in
               match node.node_color with
                  Some v' ->
                     eprintf "Colored: %s->%s@." (string_of_symbol v) (string_of_symbol v')
                | None ->
                     raise (Invalid_argument "Register_alloc.set_colors"));
      node_iter ra NodeInitial (fun node ->
            eprintf "Initial: %s@." (string_of_node ra node));
      node_iter ra NodeSimpWL (fun node ->
            eprintf "Simp WL: %s@." (string_of_node ra node));
      node_iter ra NodeFreezeWL (fun node ->
            eprintf "Freeze WL: %s@." (string_of_node ra node));
      node_iter ra NodeSpillWL (fun node ->
            eprintf "Spill WL: %s@." (string_of_node ra node));
      node_iter ra NodeSpilled (fun node ->
            eprintf "Spilled: %s@." (string_of_node ra node));
      node_iter ra NodeCoalesced (fun node ->
            eprintf "Coalesced: %s@." (string_of_node ra node));
      node_iter ra NodeStack (fun node ->
            eprintf "Stack: %s@." (string_of_node ra node));
      node_iter ra NodeColored (fun node ->
            let v = var_of_node ra node in
               match node.node_color with
                  Some v' ->
                     eprintf "Colored: %s->%s@." (string_of_symbol v) (string_of_symbol v')
                | None ->
                     raise (Invalid_argument "Register_alloc.set_colors"))

   (************************************************************************
    * CLASSIFY NODES
    ************************************************************************)

   (*
    * Precolored nodes.
    *
    * Number of colors assigned to registers.
    *)
   let max_colors = List.length Backend.registers

   (*
    * Classify the nodes.
    * Two parts:
    *   Part 1: create all the nodes
    *   Part 2: add all the edges
    *)
   let classify_nodes ra graph =
      if debug debug_regalloc then
         eprintf "Classify nodes@.";
      let add_edge node1 node2 =
         SymbolMatrix.add ra.ra_edges node1.node_name node2.node_name
      in
      let nodes =
         SymbolTable.mapi (fun v _ ->
               if List.mem v Backend.registers then
                  let node = new_node ra v NodePrecolored in
                     node.node_color <- Some v;
                     node
               else
                  new_node ra v NodeInitial) graph
      in
      let _ =
         (* Now add all the edges *)
         SymbolTable.iter (fun v node ->
               (* Convert neighbor list to a node list *)
               let neighbors = SymbolTable.find graph v in
               let neighbors =
                  SymbolSet.fold (fun neighbors v ->
                        let node = SymbolTable.find nodes v in
                           node :: neighbors) [] neighbors
               in
                  if node.node_class <> NodePrecolored then
                     begin
                        node.node_degree <- List.length neighbors;
                        node.node_neighbors <- neighbors
                     end;
                  List.iter (add_edge node) neighbors) nodes
      in
         nodes

   (*
    * Reclassify all the nodes.
    *)
   let reclassify_nodes ra =
      if !debug_regalloc then
         eprintf "Reclassify nodes@.";
      node_iter ra NodeInitial (fun node ->
            let state =
               if node.node_degree >= max_colors then
                  NodeSpillWL
               else if node_is_move_related node then
                  NodeFreezeWL
               else
                  NodeSimpWL
            in
               node_reclassify ra node state)

   (*
    * Compute on the moves.
    *)
   let classify_moves ra nodes moves =
      if !debug_regalloc then
         eprintf "Classify moves@.";
      SymbolMatrix.iter (fun dst src ->
            let dst_node = SymbolTable.find nodes dst in
            let src_node = SymbolTable.find nodes src in
            let move = new_move ra dst_node src_node MoveWL in
               dst_node.node_moves <- move :: dst_node.node_moves;
               src_node.node_moves <- move :: src_node.node_moves) moves

   (*
    * Classify nodes/moves into worklists.
    *)
   let create igraph =
      let { igraph_neighbors = neighbors;
            igraph_moves = moves
          } = igraph
      in
      let _ =
         if !debug_regalloc then
            eprintf "Register alloc create@."
      in
      let size =
         SymbolTable.fold (fun size _ live ->
               size + SymbolSet.cardinal live) 0 neighbors
      in
      let ra =
         { ra_precolored = ref None;
           ra_initial = ref None;
           ra_simp_wl = ref None;
           ra_freeze_wl = ref None;
           ra_spill_wl = ref None;
           ra_spilled = ref None;
           ra_coalesced = ref None;
           ra_colored = ref None;
           ra_stack = ref None;

           mv_coalesced = ref None;
           mv_constrained = ref None;
           mv_frozen = ref None;
           mv_wl = ref None;
           mv_active = ref None;

           ra_edges = SymbolMatrix.create (size / 3)
         }
      in

      (* Classify all the nodes *)
      let nodes = classify_nodes ra neighbors in

         (* Classify all the moves *)
         classify_moves ra nodes moves;

         (* Reclassify all the nodes *)
         reclassify_nodes ra;

         (* Print it *)
         if !debug_regalloc then
            print_ra ra;

         ra

   (************************************************************************
    * UTILITIES
    ************************************************************************)

   (*
    * Query the graph for an edge.
    *)
   let query ra node1 node2 =
      SymbolMatrix.query ra.ra_edges node1.node_name node2.node_name

   (*
    * Add an edge to the graph.
    *)
   let add_edge ra node1 node2 =
      let name1 = node1.node_name in
      let name2 = node2.node_name in
         if not (query ra node1 node2) && not (Symbol.eq name1 name2) then
            begin
               SymbolMatrix.add ra.ra_edges name1 name2;
               if node1.node_class <> NodePrecolored then
                  begin
                     node1.node_neighbors <- node2 :: node1.node_neighbors;
                     node1.node_degree <- succ node1.node_degree
                  end;
               if node2.node_class <> NodePrecolored then
                  begin
                     node2.node_neighbors <- node1 :: node2.node_neighbors;
                     node2.node_degree <- succ node2.node_degree
                  end
            end

   (*
    * Enable the moves in the nodes.
    *)
   let enable_moves ra nodes =
      List.iter (fun node ->
            List.iter (fun move ->
                  if move.move_class = MoveActive then
                     move_reclassify ra move MoveWL) (node_moves node)) nodes

   (*
    * Decrement the degree of a node.
    * This may enable some previously blocked moves.
    *)
   let decrement_degree ra node =
      if node.node_class <> NodePrecolored then
         let degree = node.node_degree in
            node.node_degree <- pred degree;
            if degree = max_colors then
               begin
                  enable_moves ra (node :: node_neighbors ra node);
                  if node_is_move_related node then
                     node_reclassify ra node NodeFreezeWL
                  else
                     node_reclassify ra node NodeSimpWL
               end

   (*
    * Get the alias for a node.
    *)
   let rec node_alias node =
      match node.node_alias with
         Some node ->
            node_alias node
       | None ->
            node

   (*
    * Add a node to a worklist.
    *)
   let add_worklist ra node =
      if node.node_class <> NodePrecolored
         && not (node_is_move_related node)
         && node.node_degree < max_colors
      then
         node_reclassify ra node NodeSimpWL


   (************************************************************************
    * SIMPLIFY
    ************************************************************************)


   (*
    * Simplify.
    *
    * Implemented by: turtles
    *)
   let simplify ra =
      let node = node_list_head ra NodeSimpWL in
         node_reclassify ra node NodeStack;
         List.iter (decrement_degree ra) (node_neighbors ra node)


   (************************************************************************
    * COALESCE
    ************************************************************************)


   (*
    * Coalesce two nodes.
    * Given in the lecture slides, for the most part.
    *
    * Implemented by: turtles
    *)
   let combine_normal ra u v =
      (* Join/combine the move lists. *)
      u.node_moves <- u.node_moves @ v.node_moves;

      (* Update the interferences of u now. *)
      let iterator = fun t ->
            add_edge ra t u;
            (* We need to counteract the degree increment from add_edge. *)
            decrement_degree ra t
      in
      List.iter iterator (node_neighbors ra v);

      (* Update the alias for v and its status. *)
      node_reclassify ra v NodeCoalesced;
      v.node_alias <- Some u;

      (* Possibly reclassify u. *)
      if u.node_degree >= max_colors && u.node_class = NodeFreezeWL then
         node_reclassify ra u NodeSpillWL


   (*
    * This case is almost the same as combine_normal, but u is precolored.
    * We don't need to combine the moves (according to the labs list).
    * The important difference is we need to call node_alias on the neighbors.
    * (We also shouldn't try to spill u...)
    *
    * Implemented by: turtles
    *)
   let combine_precolored ra u v =
      (* Update the interferences of u now. *)
      let iterator = fun t ->
            (* We need to find the aliases for v's neighbors. *)
            let t = node_alias t in
            add_edge ra t u;
            (* We need to counteract the degree increment from add_edge. *)
            decrement_degree ra t
      in
      List.iter iterator (node_neighbors ra v);

      (* We need to update the aliases and status for v. *)
      node_reclassify ra v NodeCoalesced;
      v.node_alias <- Some u


   (**** Helper functions: conservative tests for coalescing. ****)


   (*
    * Briggs test.
    * Two nodes u, v can be coalesced if the resulting node uv will have
    * fewer than max_colors neighbors of significant degree.
    *
    * Implemented by: turtle
    *)
   let briggs_test ra u v =
      (* Compute the neighbors of the node after coalescing. *)
      let u_neighbors = node_neighbors ra u in
      let v_neighbors = node_neighbors ra v in
      let neighbors = node_list_union u_neighbors v_neighbors in

      (* Count the significant neighbors in the union *)
      let iterator = fun count node ->
               if node.node_degree >= max_colors then
                  succ count
               else
                  count
      in
      let significant_neighbors  = List.fold_left iterator 0 neighbors in
         significant_neighbors < max_colors


   (*
    * George test.
    * Two nodes u, v can be coalesced if, for every neighbor t of v,
    * either t already interferes with v, or t is of insignificant degree.
    *
    * Implemented by: turtles
    *)
   let george_test ra u v =
      let check = fun node ->
            node.node_degree < max_colors || query ra node u
      in
         List.for_all check (node_neighbors ra v)


   (*
    * Coalesce.
    *
    * Implemented by: turtles
    *)
   let coalesce ra =
      (* Pick a move. *)
      let move = move_list_head ra MoveWL in

      (* Get aliases of dst and src for the move. *)
      let x = node_alias move.move_dst in
      let y = node_alias move.move_src in

      (* If one of them is precolored, make sure u is that one. *)
      let u, v =
         if y.node_class = NodePrecolored then
            y, x
         else
            x, y
      in
         (* Equal nodes, so reclassify the move. *)
         if node_eq u v then
            begin
               move_reclassify ra move MoveCoalesced;
               add_worklist ra u
            end

         (* Check for a constrained move. *)
         else if v.node_class = NodePrecolored || query ra u v then
            begin
               move_reclassify ra move MoveConstrained;
               add_worklist ra u;
               add_worklist ra v
            end

         (* If u is precolored and it passes George test, coalesce.
          * Since u is precolored, we do not add it to simplify worklist. *)
         else if u.node_class = NodePrecolored && george_test ra u v then
            begin
               move_reclassify ra move MoveCoalesced;
               combine_precolored ra u v
            end

         (* If u is not precolored and it passes Briggs test, coalesce. *)
         else if u.node_class <> NodePrecolored && briggs_test ra u v then
            begin
               move_reclassify ra move MoveCoalesced;
               combine_normal ra u v;
               add_worklist ra u
            end

         (* Otherwise, reclassify the move as active. *)
         else
            move_reclassify ra move MoveActive


   (*
    * Aggresive coalesce for spilled nodes.
    *)
   let coalesce_spills ra spset =
      if !debug_regalloc then
         eprintf "CoalesceSpills@.";
      move_fold ra MoveFrozen (fun spset move ->
            let u = node_alias move.move_dst in
            let v = node_alias move.move_src in
               if not (node_eq u v) && u.node_class = NodeSpilled && v.node_class = NodeSpilled then
                  let spset = Backend.spset_add spset (var_of_node ra v) (var_of_node ra u) in
                     combine_precolored ra u v;
                     spset
               else
                  spset) spset


   (************************************************************************
    * FREEZE
    ************************************************************************)


   (*
    * Freeze the moves for a node.
    *
    * Implemented by: turtles
    *)
   let freeze_moves ra node =
      (* Get the node that u aliases, and its moves. *)
      let u = node_alias node in
      let moves = node_moves node in

      (* Iterate over all the moves now.  We want to try to simplify
       * a node as a result of this, if possible. *)
      let iterator = fun move ->
         let y = node_alias move.move_src in
         let v =
            if node_eq y u then
               node_alias move.move_dst
            else
               y
         in
            move_reclassify ra move MoveFrozen;
            if not (node_is_move_related v) && v.node_degree < max_colors then
               node_reclassify ra node NodeSimpWL
      in
         List.iter iterator moves


   (*
    * Freeze.
    *)
   let freeze ra =
      if !debug_regalloc then
         eprintf "Freeze@.";
      let node = node_list_head ra NodeFreezeWL in
         node_reclassify ra node NodeSimpWL;
         freeze_moves ra node


   (************************************************************************
    * SPILL
    ************************************************************************)


   (*
    * Select a node for spilling.
    * We pick a variable with max interference to spill.
    *
    * Implemented by: turtles
    *)
   (*
   let spill ra vars =
      (* search for the node with max interference *)
      let rec search node best_node =
         let v = var_of_node ra node in
            if SymbolSet.mem vars v && node.node_degree > best_node.node_degree then
               try search (node_succ node) node with
                  Invalid_argument _ ->
                     node
            else
               try search (node_succ node) best_node with
                  Invalid_argument _ ->
                     best_node
      in
         (* spill the variable with max interference if we find any *)
         if not (node_list_is_empty ra NodeSpillWL) then
            begin
               let node = node_list_head ra NodeSpillWL in
               let node = search node node in
               let v = var_of_node ra node in
                  if SymbolSet.mem vars v then
                     begin
                        node_reclassify ra node NodeSimpWL;
                        freeze_moves ra node
                     end
            end
   *)

   (*
    * This is a naive version of spill,
    * where we pick a variable randomly.
    *
    * Implemented by: turtles
    *)
   let spill ra vars =
      let rec search node =
         let v = var_of_node ra node in
            if SymbolSet.mem vars v then
               begin
                  node_reclassify ra node NodeSimpWL;
                  freeze_moves ra node
               end
            else
               search (node_succ node)
      in
         search (node_list_head ra NodeSpillWL)


   (*
    * Add all the spilled nodes to the spill set.
    *)
   let add_spills spset ra =
      node_fold ra NodeSpilled (fun spset node ->
            let node = node_alias node in
            let v = node.node_name in
               if !debug_regalloc then
                  eprintf "Spilling %s to frame@." (string_of_symbol v);
               Backend.spset_spill spset v) spset


   (************************************************************************
    * SELECT
    ************************************************************************)


   (*
    * Removes the color for w from the list of colors.
    * Given in the lecture slides.
    *
    * Implemented by: turtles
    *)
   let remove_color_normal colors w =
      (* Get the alias for w. *)
      let w' = node_alias w in
         (* If it's colored, remove its color from the list. *)
         match w'.node_class with
            NodeColored
          | NodePrecolored ->
               (match w'.node_color with
                  Some color ->
                     Mc_list_util.remove color colors
                | None ->
                     raise (Invalid_argument "assign_colors"))
          | _ ->
               colors


   (*
    * Assign colors to the nodes in the stack.
    * Given in the lecture slides.
    *
    * Implemented by: turtles
    *)
   let rec assign_colors ra =
      (* Define a helper function to iterate over the node stack. *)
      let iterator = fun node ->
            match List.fold_left remove_color_normal
                     Backend.registers node.node_neighbors
            with
               color :: _ ->
                  node.node_color <- Some color;
                  node_reclassify ra node NodeColored
             | [] ->
                  node_reclassify ra node NodeSpilled
      in
         (* Actually iterate over the node stack now. *)
         node_iter ra NodeStack iterator


   (*
    * Removes the color for w from the list of colors.
    * Given in the lecture slides.
    *
    * Implemented by: turtles
    *)
   let remove_color_spill colors w =
      (* Get the alias for w. *)
      let w' = node_alias w in
         (* If it's colored, remove its color from the list. *)
         match w'.node_class with
            NodeSpilled ->
               (match w'.node_color with
                  Some color ->
                     Mc_list_util.remove color colors
                | _ ->
                     colors)
          | _ ->
               (* Others do not interfere. *)
               colors


   (*
    * Assign colors to the spilled nodes.
    * Given in the lecture slides.
    *
    * Implemented by: turtles
    *)
   let rec assign_spill_colors (ra : ra) (spset : Backend.spset) : spset =
      (* Generate the list of colors to use. *)
      let colors : symbol list =
         let iterator = fun colors node ->
               match List.fold_left remove_color_spill
                        colors node.node_neighbors
               with
                  color :: _ ->
                     node.node_color <- Some color;
                     colors
                | [] ->
                     let color = new_symbol_string "spill_color" in
                        node.node_color <- Some color;
                        color :: colors
         in
            node_fold ra NodeSpilled iterator []
      in

      (* Spill all the new colors now. *)
      let spset : Backend.spset =
         List.fold_left Backend.spset_spill spset colors
      in

      (* Assign all the spills. *)
      let iterator = fun spset node ->
         let v = var_of_node ra node in
            match node.node_color with
               Some v' ->
                  Backend.spset_add spset v v'
             | None ->
                  raise (Invalid_argument "assign_spill_colors")
      in
         node_fold ra NodeSpilled iterator spset


   (*
    * Add all the precolored nodes to the spset.
    *)
   let set_colors spset ra =
      if !debug_regalloc then
         print_ra ra;

      (* Add all the coalesced nodes *)
      let spset =
         node_fold ra NodeCoalesced (fun spset node ->
               let v = var_of_node ra node in
                  match node.node_alias with
                     Some node ->
                        Backend.spset_add spset v (var_of_node ra node)
                   | None ->
                        raise (Invalid_argument "Register_alloc.set_colors")) spset
      in
         (* Add all the colored nodes *)
         node_fold ra NodeColored (fun spset node ->
               let v = var_of_node ra node in
                  match node.node_color with
                     Some v' ->
                        Backend.spset_add spset v v'
                   | None ->
                        raise (Invalid_argument "Register_alloc.set_colors")) spset

   (*
    * Add all the moves to the spset.
    *)
   let add_moves spset ra =
      if !debug_regalloc then
         print_ra ra;

      (* Add all the coalesced nodes *)
      node_fold ra NodeCoalesced (fun spset node ->
            let v = var_of_node ra node in
               match node.node_alias with
                  Some node ->
                     Backend.spset_add spset v (var_of_node ra node)
                | None ->
                     raise (Invalid_argument "Register_alloc.set_colors")) spset


   (************************************************************************
    * MAIN PROGRAM
    ************************************************************************)


   (*
    * Worklists tests.
    *)
   let simp_not_done ra =
      !(ra.ra_simp_wl) <> None

   let move_not_done ra =
      !(ra.mv_wl) <> None

   let freeze_not_done ra =
      !(ra.ra_freeze_wl) <> None

   let spill_not_done ra =
      !(ra.ra_spill_wl) <> None

   let spilled_nodes ra =
      !(ra.ra_spilled) <> None

   (*
    * Main program performs the algorithm on page 244.
    *)
   let rec main spset vars blocks =
      if debug debug_regalloc then
         begin
            eprintf "Register_alloc.main@.";
            Backend.print_blocks stderr blocks
         end;

      (* Generate interference graph *)
      let igraph = Live.create blocks in

      (* Classify nodes into worklists *)
      let ra = create igraph in

      (* Loop until the first spill *)
      let rec loop1 () =
         if simp_not_done ra then
            begin
               simplify ra;
               loop1 ()
            end
         else if move_not_done ra then
            begin
               coalesce ra;
               loop1 ()
            end
         else if freeze_not_done ra then
            begin
               freeze ra;
               loop1 ()
            end
      in

      (* Loop until all worklists are empty *)
      let rec loop2 () =
         if simp_not_done ra then
            begin
               simplify ra;
               loop2 ()
            end
         else if move_not_done ra then
            begin
               coalesce ra;
               loop2 ()
            end
         else if freeze_not_done ra then
            begin
               freeze ra;
               loop2 ()
            end
         else if spill_not_done ra then
            begin
               spill ra vars;
               loop2 ()
            end
      in
      let _ = loop1 () in
      let spset = add_moves spset ra in
      let _ = loop2 () in

         (* Assign colors *)
         assign_colors ra;

         (* Check if there were spills *)
         if spilled_nodes ra then
            (*
             * Use aggressive coalescing on the spills, then
             * rewrite the code to spill them and try again.
             *)
            let spset = coalesce_spills ra spset in
            let spset = assign_spill_colors ra spset in
            let blocks = Backend.subst_blocks spset blocks in
               main spset vars blocks
         else
            (* There are no spills; rewrite the code and return it *)
            let spset = set_colors spset ra in
            let blocks = Backend.subst_blocks spset blocks in
               spset, blocks

   (*
    * Compile a program.
    *)
   let compile spset blocks =
      (* Initial substitution *)
      let blocks = Backend.subst_blocks spset blocks in

      (* Assign registers *)
      let vars = Backend.vars_blocks blocks in
         main spset vars blocks
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
