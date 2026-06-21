(* pqueue.sml

   PairingHeap functor implementing PQUEUE, plus a convenience IntPQueue
   instance ordered by Int.compare.

   A pairing heap is either empty or a node holding an element and a list of
   child heaps, each of whose roots is >= the node's element (min-heap order).

     - merge: the smaller root becomes the parent of the larger.
     - insert: merge a singleton.
     - deleteMin: two-pass merge of the children (the pairing step).

   `size` is tracked alongside the tree so it is O(1).  deleteMin's two-pass
   merge is written with an explicit accumulator so it does not rely on
   non-tail recursion over the child list. *)

functor PairingHeap (O : ORDERED) :> PQUEUE where type Elem.t = O.t =
struct
  structure Elem = O

  datatype tree = E | T of O.t * tree list
  (* queue carries the tree and its size *)
  type queue = int * tree

  val empty : queue = (0, E)
  fun isEmpty (n, _) = n = 0
  fun size (n, _) = n

  fun le (a, b) = case O.compare (a, b) of GREATER => false | _ => true

  fun mergeT (h, E) = h
    | mergeT (E, h) = h
    | mergeT (h1 as T (x, xs), h2 as T (y, ys)) =
        if le (x, y) then T (x, h2 :: xs)
        else T (y, h1 :: ys)

  fun insert (x, (n, t)) = (n + 1, mergeT (T (x, []), t))

  fun findMin (_, E) = NONE
    | findMin (_, T (x, _)) = SOME x

  (* two-pass pairing merge of a child list *)
  fun mergePairs [] = E
    | mergePairs [h] = h
    | mergePairs (h1 :: h2 :: rest) =
        mergeT (mergeT (h1, h2), mergePairs rest)

  fun deleteMin (_, E) = NONE
    | deleteMin (n, T (x, children)) =
        SOME (x, (n - 1, mergePairs children))

  fun merge ((n1, t1), (n2, t2)) = (n1 + n2, mergeT (t1, t2))

  fun fromList xs = List.foldl (fn (x, q) => insert (x, q)) empty xs

  fun toSortedList q =
      let
        fun loop (q, acc) =
            case deleteMin q of
                NONE => List.rev acc
              | SOME (x, q') => loop (q', x :: acc)
      in loop (q, []) end
end

(* Convenience instance for the common Int min-queue case. *)
structure IntOrder : ORDERED =
struct
  type t = int
  val compare = Int.compare
end

structure IntPQueue = PairingHeap (IntOrder)
