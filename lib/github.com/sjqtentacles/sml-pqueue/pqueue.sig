(* pqueue.sig

   A purely functional priority queue (min-heap) parameterised over an element
   ordering, implemented as a pairing heap.

   The queue is a min-queue: `findMin`/`deleteMin` return the element that is
   smallest under `Elem.compare`. A max-queue (or any other priority order) is
   obtained simply by instantiating the functor with a different ordering --
   see the README. Duplicate elements (equal under `compare`) are kept; the
   queue is a multiset.

   All operations are persistent: updates return a new queue and leave the
   argument unchanged. *)

signature ORDERED =
sig
  type t
  val compare : t * t -> order
end

signature PQUEUE =
sig
  structure Elem : ORDERED
  type queue

  val empty     : queue
  val isEmpty   : queue -> bool
  val size      : queue -> int

  val insert    : Elem.t * queue -> queue

  (* Smallest element under Elem.compare, or NONE if empty. *)
  val findMin   : queue -> Elem.t option
  (* Smallest element paired with the queue minus that element, or NONE. *)
  val deleteMin : queue -> (Elem.t * queue) option

  (* Meld two queues. *)
  val merge     : queue * queue -> queue

  val fromList    : Elem.t list -> queue
  (* Elements in ascending (priority) order. *)
  val toSortedList : queue -> Elem.t list
end
