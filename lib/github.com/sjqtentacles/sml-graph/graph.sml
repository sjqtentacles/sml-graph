(* graph.sml

   Graph implemented over a vector of adjacency lists, one per vertex. Each
   adjacency entry is (neighbour, weight); lists are kept sorted ascending by
   neighbour id so that traversals and algorithms are deterministic (see the
   tie-break note in graph.sig).

   The structure is sealed with the GRAPH signature, so the representation is
   opaque to clients.

   Where a priority queue is needed (Prim's MST) we instantiate the vendored
   sml-pqueue PairingHeap with an ordering on (weight, vertex, vertex) keys. *)

structure Graph :> GRAPH =
struct
  exception Graph of string

  (* adj : (int * real) list vector, index = vertex id *)
  type t = { directed : bool, adj : (int * real) list vector }

  fun numVertices ({adj, ...} : t) = Vector.length adj
  fun isDirected ({directed, ...} : t) = directed

  fun checkV g v =
    if v < 0 orelse v >= numVertices g then
      raise Graph ("vertex out of range: " ^ Int.toString v)
    else ()

  (* Insert (nbr, w) into an ascending-by-nbr list, keeping it sorted; parallel
     edges (same nbr) are retained, ordered after existing equal-nbr entries. *)
  fun insAdj (nbr, w) [] = [(nbr, w)]
    | insAdj (nbr, w) ((n', w') :: rest) =
        if nbr < n' then (nbr, w) :: (n', w') :: rest
        else (n', w') :: insAdj (nbr, w) rest

  fun empty {directed, n} =
    if n < 0 then raise Graph "negative vertex count"
    else { directed = directed, adj = Vector.tabulate (n, fn _ => []) }

  fun addVertex ({directed, adj} : t) =
    let
      val n = Vector.length adj
      val adj' = Vector.tabulate (n + 1, fn i =>
                   if i < n then Vector.sub (adj, i) else [])
    in
      (n, { directed = directed, adj = adj' })
    end

  fun addEdge (g as {directed, adj} : t) {from, to, weight} =
    let
      val () = checkV g from
      val () = checkV g to
      fun upd (i, cur) =
        if i = from then insAdj (to, weight) cur
        else if (not directed) andalso i = to then insAdj (from, weight) cur
        else cur
      val adj' = Vector.mapi upd adj
    in
      { directed = directed, adj = adj' }
    end

  fun fromEdges directed n es =
    List.foldl (fn ((u, v, w), g) => addEdge g {from = u, to = v, weight = w})
               (empty {directed = directed, n = n}) es

  fun neighbors (g as {adj, ...} : t) v =
    (checkV g v; Vector.sub (adj, v))

  fun edges ({directed, adj} : t) =
    let
      val n = Vector.length adj
      fun fromV i =
        List.mapPartial
          (fn (j, w) =>
             if directed then SOME (i, j, w)
             else if i <= j then SOME (i, j, w)
             else NONE)
          (Vector.sub (adj, i))
      val all = List.concat (List.tabulate (n, fromV))
    in
      all
    end

  (* ---- Traversal ---- *)

  fun bfs (g as {adj, ...} : t) s =
    let
      val () = checkV g s
      val n = Vector.length adj
      val seen = Array.array (n, false)
      fun loop ([], acc) = List.rev acc
        | loop (v :: queue, acc) =
            let
              val nbrs = List.map #1 (Vector.sub (adj, v))
              (* enqueue unseen neighbours in ascending order, marking seen *)
              val fresh =
                List.foldl
                  (fn (u, fs) =>
                     if Array.sub (seen, u) then fs
                     else (Array.update (seen, u, true); u :: fs))
                  [] nbrs
              val freshAsc = List.rev fresh
            in
              loop (queue @ freshAsc, List.revAppend (freshAsc, acc))
            end
      val () = Array.update (seen, s, true)
    in
      loop ([s], [s])
    end

  fun dfs (g as {adj, ...} : t) s =
    let
      val () = checkV g s
      val n = Vector.length adj
      val seen = Array.array (n, false)
      val acc = ref []
      fun visit v =
        if Array.sub (seen, v) then ()
        else
          (Array.update (seen, v, true);
           acc := v :: !acc;
           List.app (fn (u, _) => visit u) (Vector.sub (adj, v)))
      val () = visit s
    in
      List.rev (!acc)
    end

  (* ---- topoSort: Kahn's algorithm with ascending tie-break ---- *)

  fun topoSort (g as {directed, adj} : t) =
    let
      val n = Vector.length adj
      val indeg = Array.array (n, 0)
      val () =
        Vector.app
          (fn lst => List.app (fn (j, _) => Array.update (indeg, j, Array.sub (indeg, j) + 1)) lst)
          adj
      (* ready = vertices with indegree 0, kept ascending *)
      fun initReady () =
        List.filter (fn v => Array.sub (indeg, v) = 0) (List.tabulate (n, fn i => i))
      fun insAsc (x, []) = [x]
        | insAsc (x, y :: ys) = if x < y then x :: y :: ys else y :: insAsc (x, ys)
      fun loop ([], acc, count) = (List.rev acc, count)
        | loop (v :: ready, acc, count) =
            let
              val ready' =
                List.foldl
                  (fn ((j, _), rdy) =>
                     let val d = Array.sub (indeg, j) - 1
                     in Array.update (indeg, j, d);
                        if d = 0 then insAsc (j, rdy) else rdy
                     end)
                  ready (Vector.sub (adj, v))
            in
              loop (ready', v :: acc, count + 1)
            end
      val (order, count) = loop (initReady (), [], 0)
    in
      if count = n then SOME order else NONE
    end

  (* ---- connectedComponents (undirected view) ---- *)

  fun connectedComponents ({adj, ...} : t) =
    let
      val n = Vector.length adj
      (* build undirected adjacency (symmetrize) as a seen-based flood fill *)
      val seen = Array.array (n, false)
      (* undirected neighbours: union of out-edges and in-edges *)
      val undAdj = Array.array (n, [] : int list)
      val () =
        Vector.appi
          (fn (i, lst) =>
             List.app
               (fn (j, _) =>
                  (Array.update (undAdj, i, j :: Array.sub (undAdj, i));
                   Array.update (undAdj, j, i :: Array.sub (undAdj, j))))
               lst)
          adj
      fun flood (v, acc) =
        if Array.sub (seen, v) then acc
        else
          (Array.update (seen, v, true);
           List.foldl flood (v :: acc) (Array.sub (undAdj, v)))
      fun sortAsc xs =
        let fun ins (x, []) = [x]
              | ins (x, y :: ys) = if x < y then x :: y :: ys else y :: ins (x, ys)
        in List.foldr (fn (x, a) => ins (x, a)) [] xs end
      fun loop (v, comps) =
        if v >= n then List.rev comps
        else if Array.sub (seen, v) then loop (v + 1, comps)
        else loop (v + 1, sortAsc (flood (v, [])) :: comps)
    in
      loop (0, [])
    end

  (* ---- stronglyConnected: Tarjan's SCC ---- *)

  fun stronglyConnected ({adj, ...} : t) =
    let
      val n = Vector.length adj
      val index = Array.array (n, ~1)
      val low = Array.array (n, 0)
      val onStack = Array.array (n, false)
      val counter = ref 0
      val stack = ref ([] : int list)
      val sccs = ref ([] : int list list)

      fun sortAsc xs =
        let fun ins (x, []) = [x]
              | ins (x, y :: ys) = if x < y then x :: y :: ys else y :: ins (x, ys)
        in List.foldr (fn (x, a) => ins (x, a)) [] xs end

      fun strongConnect v =
        let
          val () = Array.update (index, v, !counter)
          val () = Array.update (low, v, !counter)
          val () = counter := !counter + 1
          val () = stack := v :: !stack
          val () = Array.update (onStack, v, true)
          val () =
            List.app
              (fn (w, _) =>
                 if Array.sub (index, w) = ~1 then
                   (strongConnect w;
                    Array.update (low, v, Int.min (Array.sub (low, v), Array.sub (low, w))))
                 else if Array.sub (onStack, w) then
                   Array.update (low, v, Int.min (Array.sub (low, v), Array.sub (index, w)))
                 else ())
              (Vector.sub (adj, v))
        in
          if Array.sub (low, v) = Array.sub (index, v) then
            let
              fun pop acc =
                case !stack of
                    [] => acc
                  | w :: rest =>
                      (stack := rest;
                       Array.update (onStack, w, false);
                       if w = v then w :: acc else pop (w :: acc))
              val comp = pop []
            in
              sccs := sortAsc comp :: !sccs
            end
          else ()
        end
      val () =
        List.app (fn v => if Array.sub (index, v) = ~1 then strongConnect v else ())
                 (List.tabulate (n, fn i => i))
      (* order components by smallest vertex (each already ascending) *)
      fun cmpComp (a :: _, b :: _) = Int.compare (a, b)
        | cmpComp _ = EQUAL
      fun sortComps xs =
        let fun ins (x, []) = [x]
              | ins (x, y :: ys) =
                  (case cmpComp (x, y) of GREATER => y :: ins (x, ys) | _ => x :: y :: ys)
        in List.foldr (fn (x, a) => ins (x, a)) [] xs end
    in
      sortComps (!sccs)
    end

  (* ---- mst: Prim's algorithm using the vendored priority queue ---- *)

  (* PQ element: (weight, from, to). Ordered by weight, then endpoints, so the
     queue is a proper min-queue and ties break deterministically. *)
  structure EdgeOrder : ORDERED =
  struct
    type t = real * int * int
    fun compare ((w1, a1, b1), (w2, a2, b2)) =
      case Real.compare (w1, w2) of
          EQUAL =>
            (case Int.compare (a1, a2) of
                 EQUAL => Int.compare (b1, b2)
               | o' => o')
        | o' => o'
  end
  structure EdgePQ = PairingHeap (EdgeOrder)

  fun mst ({adj, ...} : t) =
    let
      val n = Vector.length adj
      val inTree = Array.array (n, false)
      val chosen = ref ([] : (int * int * real) list)

      (* push all edges out of v into the queue *)
      fun pushEdges (v, q) =
        List.foldl
          (fn ((u, w), acc) =>
             if Array.sub (inTree, u) then acc
             else EdgePQ.insert ((w, v, u), acc))
          q (Vector.sub (adj, v))

      fun grow (q) =
        case EdgePQ.deleteMin q of
            NONE => ()
          | SOME ((w, a, b), q') =>
              if Array.sub (inTree, b) then grow q'
              else
                let
                  val (lo, hi) = if a <= b then (a, b) else (b, a)
                in
                  Array.update (inTree, b, true);
                  chosen := (lo, hi, w) :: !chosen;
                  grow (pushEdges (b, q'))
                end

      (* MST forest: start a Prim run from each unvisited vertex, ascending *)
      fun startAll v =
        if v >= n then ()
        else if Array.sub (inTree, v) then startAll (v + 1)
        else
          (Array.update (inTree, v, true);
           grow (pushEdges (v, EdgePQ.empty));
           startAll (v + 1))
      val () = startAll 0

      (* order chosen edges by (from, to) *)
      fun cmpE ((a1, b1, _), (a2, b2, _)) =
        case Int.compare (a1, a2) of EQUAL => Int.compare (b1, b2) | o' => o'
      fun sortE xs =
        let fun ins (x, []) = [x]
              | ins (x, y :: ys) =
                  (case cmpE (x, y) of GREATER => y :: ins (x, ys) | _ => x :: y :: ys)
        in List.foldr (fn (x, a) => ins (x, a)) [] xs end
    in
      sortE (!chosen)
    end

  (* ---- maxFlow: Edmonds-Karp ---- *)

  fun maxFlow (g as {adj, ...} : t) {source, sink} =
    let
      val () = checkV g source
      val () = checkV g sink
      val n = Vector.length adj
      (* residual capacity matrix; capacities of parallel arcs sum *)
      val cap = Array2.array (n, n, 0.0)
      val () =
        Vector.appi
          (fn (u, lst) =>
             List.app
               (fn (v, w) =>
                  Array2.update (cap, u, v, Array2.sub (cap, u, v) + w))
               lst)
          adj

      fun bfsAugment () =
        let
          val parent = Array.array (n, ~1)
          val () = Array.update (parent, source, source)
          fun loop [] = false
            | loop (u :: queue) =
                if u = sink then true
                else
                  let
                    val next = ref []
                    val () =
                      let
                        fun scan v =
                          if v >= n then ()
                          else
                            (if Array.sub (parent, v) = ~1
                                andalso Array2.sub (cap, u, v) > 0.0
                             then (Array.update (parent, v, u); next := v :: !next)
                             else ();
                             scan (v + 1))
                      in scan 0 end
                  in
                    loop (queue @ List.rev (!next))
                  end
          val reached = loop [source]
        in
          if not reached then NONE
          else
            let
              (* find bottleneck along parent chain *)
              fun bottleneck (v, acc) =
                if v = source then acc
                else
                  let val u = Array.sub (parent, v)
                  in bottleneck (u, Real.min (acc, Array2.sub (cap, u, v))) end
              val f = bottleneck (sink, Real.posInf)
              fun augment v =
                if v = source then ()
                else
                  let val u = Array.sub (parent, v)
                  in Array2.update (cap, u, v, Array2.sub (cap, u, v) - f);
                     Array2.update (cap, v, u, Array2.sub (cap, v, u) + f);
                     augment u
                  end
              val () = augment sink
            in
              SOME f
            end
        end

      fun loop total =
        case bfsAugment () of
            NONE => total
          | SOME f => loop (total + f)
    in
      if source = sink then 0.0 else loop 0.0
    end
end
