(* demo.sml - graph algorithms on two small fixed graphs. All edge weights are
   integers, so every distance is an exact integer and is printed via Real.round
   (never Real.toString); unreachable distances print as "inf" (tested with
   Real.isFinite, never Real.==). Vertex/edge lists are deterministic by the
   library's documented ascending-id tie-break. Output is identical on every
   run and on both compilers. *)

structure G = Graph

fun ri (r : real) = Int.toString (Real.round r)
fun showDist d = if Real.isFinite d then ri d else "inf"
fun ints xs = String.concatWith " " (List.map Int.toString xs)
fun showEdges es =
  String.concatWith ", "
    (List.map (fn (u, v, w) => Int.toString u ^ "-" ^ Int.toString v ^ "(" ^ ri w ^ ")") es)

(* A fixed directed, weighted DAG on vertices 0..5 (vertex 5 is isolated). *)
val dg = G.fromEdges true 6
  [ (0, 1, 4.0), (0, 2, 1.0), (2, 1, 2.0), (1, 3, 1.0), (2, 3, 5.0), (3, 4, 3.0) ]

val () = print "Directed weighted graph (6 vertices):\n"
val () = print ("  bfs from 0 : " ^ ints (G.bfs dg 0) ^ "\n")
val () = print ("  dfs from 0 : " ^ ints (G.dfs dg 0) ^ "\n")
val () = print ("  topoSort   : "
                ^ (case G.topoSort dg of
                       SOME order => ints order
                     | NONE => "<has a cycle>") ^ "\n")

val {dist, ...} = G.dijkstra dg 0
val ds = List.tabulate (G.numVertices dg, fn i => showDist (Array.sub (dist, i)))
val () = print ("  dijkstra distances from 0: [" ^ String.concatWith " " ds ^ "]\n")

val () =
  print ("  shortestPath 0 -> 4: "
         ^ (case G.shortestPath dg {from = 0, to = 4} of
                SOME (path, cost) => ints path ^ "  (cost " ^ ri cost ^ ")"
              | NONE => "<unreachable>") ^ "\n")

(* A fixed undirected, weighted graph on vertices 0..4. *)
val ug = G.fromEdges false 5
  [ (0, 1, 1.0), (0, 2, 4.0), (1, 2, 2.0), (1, 3, 5.0), (2, 3, 3.0), (3, 4, 1.0) ]

val () = print "\nUndirected weighted graph (5 vertices):\n"
val () = print ("  connectedComponents : "
                ^ String.concatWith " | " (List.map ints (G.connectedComponents ug)) ^ "\n")
val () = print ("  minimum spanning tree: " ^ showEdges (G.mst ug) ^ "\n")
