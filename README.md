# sml-graph

[![CI](https://github.com/sjqtentacles/sml-graph/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-graph/actions/workflows/ci.yml)

General-purpose graph algorithms for Standard ML over integer-vertex graphs:
traversal, topological sort, connected and strongly-connected components,
minimum spanning tree, maximum flow, and shortest paths (Dijkstra,
Bellman-Ford, Floyd-Warshall, Johnson).

Part of the `sjqtentacles` monorepo of SML libraries. It builds on
[`sml-pqueue`](https://github.com/sjqtentacles/sml-pqueue) (vendored), whose
pairing heap drives both Prim's MST and Dijkstra's shortest paths.

## Portability

Pure Standard ML using only the Basis library (plus the vendored `sml-pqueue`)
-- no FFI, no threads. Verified on **MLton** and **Poly/ML**, with identical,
deterministic output across both.

## Building and testing

```sh
make test        # build + run the suite under MLton (default)
make test-poly   # run the suite under Poly/ML
make all-tests   # run under both
make example     # build + run the demo
make clean
```

## Representation and conventions

A graph has a fixed vertex set `{0, 1, ..., n-1}` and `real`-weighted edges,
stored as a vector of per-vertex adjacency lists. A graph is **directed** or
**undirected**, fixed at construction:

- undirected edge `(u, v, w)` records adjacency both ways;
- directed edge `(u, v, w)` is the arc `u -> v` only.

Adjacency lists are kept **sorted ascending by neighbour id**, and every
routine visits neighbours (and picks the next root/component) in that ascending
order. This **ascending-id tie-break** makes all list-valued results
deterministic and identical across compilers. Parallel edges are retained
(their capacities sum in `maxFlow`).

The `Graph` structure is the sole export and is sealed opaquely (`:>`).

## Usage

```sml
(* Undirected weighted graph on vertices 0..3. *)
val g = Graph.fromEdges false 4
          [(0, 1, 1.0), (1, 2, 1.0), (0, 2, 1.0), (2, 3, 1.0)]

val order  = Graph.bfs g 0                     (* breadth-first order      *)
val comps  = Graph.connectedComponents g       (* [[0,1,2,3]]              *)
val tree   = Graph.mst g                        (* MST edges (Prim)         *)

(* Directed graph: topological sort / SCCs / max flow. *)
val d = Graph.fromEdges true 3 [(0,1,1.0),(1,2,1.0)]
val topo = Graph.topoSort d                     (* SOME [0,1,2]             *)
val sccs = Graph.stronglyConnected d            (* [[0],[1],[2]]            *)
val flow = Graph.maxFlow d {source = 0, sink = 2}

(* Shortest paths. Distances are reals, with Real.posInf for unreachable
   vertices and a pred array (~1 for the source / unreachable). *)
val {dist, pred} = Graph.dijkstra d 0           (* single-source, non-neg   *)
val path = Graph.shortestPath d {from = 0, to = 2}  (* SOME ([0,1,2], 2.0)  *)

(* Negative edges (Bellman-Ford): NONE iff a reachable negative cycle. *)
val bf  = Graph.bellmanFord d 0                 (* SOME {dist, pred}        *)

(* All-pairs: Floyd-Warshall (dense) or Johnson (sparse, NONE on neg cycle). *)
val apsp  = Graph.floydWarshall d               (* n*n real matrix          *)
val apsp' = Graph.johnson d                      (* SOME (n*n matrix)        *)
```

## API summary

| Function | Description |
| --- | --- |
| `empty : {directed:bool, n:int} -> t` | Edgeless graph on `0..n-1`. |
| `fromEdges : bool -> int -> (int*int*real) list -> t` | Build from weighted edges. |
| `addVertex : t -> int * t` | Append a vertex; return its id. |
| `addEdge : t -> {from:int, to:int, weight:real} -> t` | Add a weighted edge. |
| `numVertices`, `isDirected` | Basic queries. |
| `neighbors : t -> int -> (int*real) list` | Successors (ascending). |
| `edges : t -> (int*int*real) list` | All edges (undirected: `from <= to`). |
| `bfs`, `dfs : t -> int -> int list` | Traversal order from a source. |
| `topoSort : t -> int list option` | Kahn's order, or `NONE` on a cycle. |
| `connectedComponents : t -> int list list` | Undirected components. |
| `stronglyConnected : t -> int list list` | Tarjan's SCCs (directed). |
| `mst : t -> (int*int*real) list` | Prim's MST/forest via `sml-pqueue`. |
| `maxFlow : t -> {source:int, sink:int} -> real` | Edmonds-Karp max flow. |
| `dijkstra : t -> int -> {dist:real array, pred:int array}` | Single-source shortest paths, non-negative weights, via `sml-pqueue`. |
| `bellmanFord : t -> int -> {dist:real array, pred:int array} option` | Single-source with negative edges; `NONE` on a reachable negative cycle. |
| `floydWarshall : t -> real array array` | All-pairs shortest paths (dense). |
| `johnson : t -> real array array option` | All-pairs for sparse graphs; `NONE` on a negative cycle. |
| `shortestPath : t -> {from:int, to:int} -> (int list * real) option` | Reconstructed path + cost (handles negative edges). |

## Example

[`examples/demo.sml`](examples/demo.sml) builds a fixed directed weighted DAG
and a fixed undirected weighted graph, then runs traversal, topological sort,
Dijkstra, shortest-path reconstruction, connected components, and an MST. Edge
weights are integers, so distances are exact and print via `Real.round`
(unreachable vertices show `inf`); list results are deterministic by the
ascending-id tie-break. Run it with:

```
$ make example
Directed weighted graph (6 vertices):
  bfs from 0 : 0 1 2 3 4
  dfs from 0 : 0 1 3 4 2
  topoSort   : 0 2 1 3 4 5
  dijkstra distances from 0: [0 3 1 4 7 inf]
  shortestPath 0 -> 4: 0 2 1 3 4  (cost 7)

Undirected weighted graph (5 vertices):
  connectedComponents : 0 1 2 3 4
  minimum spanning tree: 0-1(1), 1-2(2), 2-3(3), 3-4(1)
```

## Installing with smlpkg

```sh
smlpkg add github.com/sjqtentacles/sml-graph
smlpkg sync
```

Then reference the library basis from your own `.mlb`:

```
lib/github.com/sjqtentacles/sml-graph/sml-graph.mlb
```

For Poly/ML, `use` the sources listed in `sources.mlb` in order (the vendored
`sml-pqueue` first, then `graph.sig` and `graph.sml`).

## License

MIT. See [LICENSE](LICENSE).
