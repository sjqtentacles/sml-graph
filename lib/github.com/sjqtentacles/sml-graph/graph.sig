(* graph.sig

   General-purpose graph algorithms over integer-vertex graphs.

   Representation and conventions
   ------------------------------
   A graph has a fixed vertex set `{0, 1, ..., n-1}` (the order `n` is chosen at
   construction time) and a set of edges, each carrying a `real` weight. A graph
   is either *directed* or *undirected*; this is fixed at construction and
   reported by `isDirected`.

     - In an undirected graph, adding the edge (u, v, w) records adjacency in
       both directions; u and v are neighbours of one another.
     - In a directed graph, edge (u, v, w) is an arc u -> v only.

   Adjacency lists are kept sorted ascending by neighbour vertex id, and the
   traversal/algorithm routines visit neighbours in that ascending order. This
   is the documented tie-break rule: whenever a choice between equally-eligible
   vertices arises (e.g. the next neighbour to explore in BFS/DFS, the next
   root to start a new DFS/component from), the smallest vertex id wins. This
   makes all list-valued results deterministic and identical across compilers.

   Weights default to 1.0 for the unweighted constructors. Parallel edges
   between the same ordered pair are permitted and retained (they matter for
   `maxFlow`, where capacities on parallel arcs add up).

   Errors: constructors and mutators raise `Graph` when given an out-of-range
   vertex id (not in `0..n-1`). Algorithms that take a source/sink likewise
   raise `Graph` on an out-of-range vertex. *)

signature GRAPH =
sig
  type t

  exception Graph of string

  (* ---- Construction ---- *)

  (* `empty {directed, n}` is the graph on vertices 0..n-1 with no edges. *)
  val empty       : {directed : bool, n : int} -> t

  (* Number of vertices / whether the graph is directed. *)
  val numVertices : t -> int
  val isDirected  : t -> bool

  (* Grow the vertex set by one, returning the new vertex's id (= old n) and
     the larger graph. *)
  val addVertex   : t -> int * t

  (* Add a weighted edge. In an undirected graph both directions are recorded.
     Raises `Graph` if an endpoint is out of range. *)
  val addEdge     : t -> {from : int, to : int, weight : real} -> t

  (* `fromEdges directed n edges` builds the graph on 0..n-1 with the given
     weighted edges. Undirected when `directed` is false. *)
  val fromEdges   : bool -> int -> (int * int * real) list -> t

  (* ---- Inspection ---- *)

  (* Neighbours of a vertex (successors, for a directed graph), ascending,
     paired with the edge weight. *)
  val neighbors   : t -> int -> (int * real) list
  (* All edges as (from, to, weight). For an undirected graph each edge is
     reported once, with from <= to. *)
  val edges       : t -> (int * int * real) list

  (* ---- Traversal (ascending-neighbour-id order) ---- *)

  (* Vertices in the order first visited by a breadth-first / depth-first
     search from the source. Only the source's reachable set is returned. *)
  val bfs         : t -> int -> int list
  val dfs         : t -> int -> int list

  (* ---- Ordering / structure ---- *)

  (* Topological order of a directed acyclic graph (every arc u -> v has u
     before v in the result), or NONE if the graph has a directed cycle.
     The order is the deterministic one produced by Kahn's algorithm with the
     ascending tie-break. *)
  val topoSort    : t -> int list option

  (* Connected components of an undirected graph. Each component is a list of
     vertices in ascending order; the components are ordered by their smallest
     vertex. (Defined for undirected graphs; for a directed graph it operates
     on the underlying undirected graph.) *)
  val connectedComponents : t -> int list list

  (* Strongly-connected components of a directed graph (Tarjan's algorithm).
     Each component is ascending; components are ordered by smallest vertex. *)
  val stronglyConnected   : t -> int list list

  (* Minimum spanning tree (forest, if disconnected) of an undirected weighted
     graph, as a list of chosen edges (from, to, weight) with from <= to.
     Edges are returned ordered by (from, to). Implemented with Prim's
     algorithm driven by the vendored sml-pqueue priority queue. *)
  val mst         : t -> (int * int * real) list

  (* Maximum flow from source to sink in a directed graph whose edge weights
     are non-negative capacities (Edmonds-Karp: BFS augmenting paths).
     Parallel arcs' capacities sum. *)
  val maxFlow     : t -> {source : int, sink : int} -> real

  (* ---- Shortest paths ---- *)

  (* Conventions shared by the shortest-path routines: distances are `real`
     and an unreachable vertex has distance `Real.posInf`. A `pred` array gives,
     for each vertex, the previous vertex on a shortest path from the source,
     or `~1` for the source itself and for any unreachable vertex. The arrays
     returned are fresh, indexed by vertex id, and of length `numVertices`.
     Equal-cost ties are broken deterministically by the ascending-id rule, so
     the predecessor arrays are identical across compilers. *)

  (* Single-source shortest paths from `src` over NON-NEGATIVE edge weights,
     using the vendored sml-pqueue (Dijkstra). Raises `Graph` if `src` is out
     of range, or if a negative edge weight is encountered while relaxing. *)
  val dijkstra     : t -> int -> {dist : real array, pred : int array}

  (* Single-source shortest paths from `src` allowing negative edge weights
     (Bellman-Ford). Returns `NONE` iff there is a negative-weight cycle
     reachable from `src` (in which case no well-defined shortest paths exist);
     otherwise `SOME {dist, pred}` with the usual posInf/`~1` conventions.
     Raises `Graph` if `src` is out of range. *)
  val bellmanFord  : t -> int -> {dist : real array, pred : int array} option

  (* All-pairs shortest paths (Floyd-Warshall). Result `m` is an `n*n` matrix
     with `m[i][j]` the shortest distance from i to j (`0.0` on the diagonal in
     the absence of a negative self-loop, `Real.posInf` when j is unreachable
     from i). If the graph has a negative-weight cycle, some diagonal entry
     `m[i][i]` is negative; callers wanting cycle-safety should use `johnson`. *)
  val floydWarshall : t -> real array array

  (* All-pairs shortest paths via Johnson's algorithm (Bellman-Ford reweighting
     followed by a Dijkstra from each vertex), suited to sparse graphs. Returns
     `NONE` iff the graph contains a negative-weight cycle; otherwise `SOME m`
     with the same `n*n` distance-matrix layout as `floydWarshall`. *)
  val johnson      : t -> real array array option

  (* A shortest path from `from` to `to`, as the list of vertices on the path
     (inclusive of both endpoints) paired with its total cost. Handles negative
     edge weights (Bellman-Ford based). Returns `NONE` when `to` is unreachable
     from `from`, or when a negative-weight cycle reachable from `from` makes
     the distance ill-defined. Raises `Graph` if an endpoint is out of range. *)
  val shortestPath : t -> {from : int, to : int} -> (int list * real) option
end
