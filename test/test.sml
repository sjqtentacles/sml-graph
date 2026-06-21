(* Tests for sml-graph.

   Fixed example graphs are defined inline. List-valued results are normalized
   (sorted) where the algorithm's contract only fixes a set / set-of-sets, so
   output is deterministic across compilers. Traversal-order tests assert the
   exact ascending-neighbour-id order documented in graph.sig. *)

structure GraphTests =
struct
  open Harness

  (* Insertion sort over an arbitrary comparison (Basis has no list sort). *)
  fun sortBy cmp xs =
    let
      fun ins (x, []) = [x]
        | ins (x, y :: ys) =
            (case cmp (x, y) of GREATER => y :: ins (x, ys) | _ => x :: y :: ys)
    in
      List.foldr (fn (x, acc) => ins (x, acc)) [] xs
    end

  (* Sort a list of components canonically: each component ascending, the
     whole list ordered by component (lexicographically). *)
  fun normComponents (cs : int list list) : int list list =
    let
      val each = List.map (fn c => sortBy Int.compare c) cs
      fun cmp ([], []) = EQUAL
        | cmp ([], _) = LESS
        | cmp (_, []) = GREATER
        | cmp (x :: xs, y :: ys) =
            (case Int.compare (x, y) of EQUAL => cmp (xs, ys) | o' => o')
    in
      sortBy cmp each
    end

  fun run () =
    let
      val () = section "construction"

      (* Undirected triangle 0-1-2 plus a pendant 2-3. *)
      val g = Graph.fromEdges false 4
                [(0, 1, 1.0), (1, 2, 1.0), (0, 2, 1.0), (2, 3, 1.0)]
      val () = checkInt "numVertices" (4, Graph.numVertices g)
      val () = checkBool "undirected" (false, Graph.isDirected g)
      val () = checkIntList "neighbors of 2 ascending"
                 ([0, 1, 3], List.map #1 (Graph.neighbors g 2))

      val () = section "bfs / dfs (ascending neighbour id)"
      (* Directed graph for traversal:
           0 -> 1, 0 -> 2, 1 -> 3, 2 -> 3, 3 -> 4 *)
      val d = Graph.fromEdges true 5
                [(0, 1, 1.0), (0, 2, 1.0), (1, 3, 1.0), (2, 3, 1.0), (3, 4, 1.0)]
      val () = checkIntList "bfs from 0" ([0, 1, 2, 3, 4], Graph.bfs d 0)
      val () = checkIntList "dfs from 0" ([0, 1, 3, 4, 2], Graph.dfs d 0)

      val () = section "topoSort"
      (* DAG: 5->2, 5->0, 4->0, 4->1, 2->3, 3->1 (classic Wikipedia example) *)
      val dag = Graph.fromEdges true 6
                  [(5, 2, 1.0), (5, 0, 1.0), (4, 0, 1.0), (4, 1, 1.0),
                   (2, 3, 1.0), (3, 1, 1.0)]
      val () =
        (case Graph.topoSort dag of
             NONE => checkBool "DAG has a topo order" (true, false)
           | SOME order =>
               let
                 (* must be a permutation of 0..5 *)
                 val isPerm = (sortBy Int.compare order = [0,1,2,3,4,5])
                 (* every edge u->v: pos u < pos v *)
                 fun pos x =
                   let
                     fun find (i, []) = ~1
                       | find (i, y :: ys) = if x = y then i else find (i + 1, ys)
                   in find (0, order) end
                 val es = [(5,2),(5,0),(4,0),(4,1),(2,3),(3,1)]
                 val valid = List.all (fn (u, v) => pos u < pos v) es
               in
                 checkBool "topo is a permutation" (true, isPerm);
                 checkBool "topo respects all edges" (true, valid)
               end)
      (* Cyclic digraph 0->1->2->0 has no topo order. *)
      val cyc = Graph.fromEdges true 3 [(0,1,1.0),(1,2,1.0),(2,0,1.0)]
      val () = checkBool "cycle yields NONE" (true, Graph.topoSort cyc = NONE)

      val () = section "connectedComponents (undirected)"
      (* 3 components: {0,1,2}, {3,4}, {5} on 6 vertices *)
      val cc = Graph.fromEdges false 6 [(0,1,1.0),(1,2,1.0),(3,4,1.0)]
      val () = checkBool "three components"
                 (true, normComponents (Graph.connectedComponents cc)
                          = [[0,1,2],[3,4],[5]])

      val () = section "stronglyConnected (Tarjan)"
      (* Classic example: three SCCs {0,1,2}, {3,4}, {5,6,7}.
         0->1->2->0 ; 3->4->3 ; 5->6->7->5 ; cross arcs 2->3, 4->5. *)
      val scg = Graph.fromEdges true 8
                  [(0,1,1.0),(1,2,1.0),(2,0,1.0),
                   (3,4,1.0),(4,3,1.0),
                   (5,6,1.0),(6,7,1.0),(7,5,1.0),
                   (2,3,1.0),(4,5,1.0)]
      val () = checkBool "three SCCs grouped correctly"
                 (true, normComponents (Graph.stronglyConnected scg)
                          = [[0,1,2],[3,4],[5,6,7]])
      (* A DAG-like digraph has all singleton SCCs. *)
      val scgDag = Graph.fromEdges true 3 [(0,1,1.0),(1,2,1.0)]
      val () = checkBool "acyclic => singletons"
                 (true, normComponents (Graph.stronglyConnected scgDag)
                          = [[0],[1],[2]])

      val () = section "mst (Prim via sml-pqueue)"
      (* CLRS MST example: 9 vertices a..h + i = 0..8, total MST weight 37.
         a=0 b=1 c=2 d=3 e=4 f=5 g=6 h=7 i=8 *)
      val gm = Graph.fromEdges false 9
                 [(0,1,4.0),(0,7,8.0),(1,2,8.0),(1,7,11.0),(2,3,7.0),
                  (2,8,2.0),(2,5,4.0),(3,4,9.0),(3,5,14.0),(4,5,10.0),
                  (5,6,2.0),(6,7,1.0),(6,8,6.0),(7,8,7.0)]
      val tree = Graph.mst gm
      fun totalWeight es = List.foldl (fn ((_,_,w), s) => s + w) 0.0 es
      val () = checkBool "MST total weight = 37"
                 (true, Real.== (totalWeight tree, 37.0))
      val () = checkInt "MST has n-1 edges (connected)" (8, List.length tree)
      (* Small forest: two disjoint weighted components on 4 vertices. *)
      val gf = Graph.fromEdges false 4 [(0,1,1.0),(0,1,5.0),(2,3,3.0)]
      val forest = Graph.mst gf
      val () = checkBool "forest total weight = 4" (true, Real.== (totalWeight forest, 4.0))
      val () = checkInt "forest edge count = n - components" (2, List.length forest)

      val () = section "maxFlow (Edmonds-Karp)"
      (* CLRS max-flow network (Figure 26.1): s=0 v1=1 v2=2 v3=3 v4=4 t=5,
         max flow value 23. *)
      val gflow = Graph.fromEdges true 6
                    [(0,1,16.0),(0,2,13.0),(1,3,12.0),(2,1,4.0),
                     (3,2,9.0),(2,4,14.0),(4,3,7.0),(3,5,20.0),(4,5,4.0)]
      val () = checkBool "max flow = 23"
                 (true, Real.== (Graph.maxFlow gflow {source=0, sink=5}, 23.0))
      (* Trivial: source = sink => 0. *)
      val () = checkBool "source=sink => 0"
                 (true, Real.== (Graph.maxFlow gflow {source=0, sink=0}, 0.0))
      (* Parallel arcs' capacities sum: two 0->1 arcs (3 + 5 = 8). *)
      val gpar = Graph.fromEdges true 2 [(0,1,3.0),(0,1,5.0)]
      val () = checkBool "parallel arcs sum"
                 (true, Real.== (Graph.maxFlow gpar {source=0, sink=1}, 8.0))
    in
      ()
    end
end
