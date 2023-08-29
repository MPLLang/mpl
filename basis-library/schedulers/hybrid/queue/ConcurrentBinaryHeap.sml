structure ConcurrentBinaryHeap :>
sig
  type 'a t

  val new: {capacity: int, cmp: 'a * 'a -> order} -> 'a t

  (* nondestructive *)
  val size: 'a t -> int
  val capacity: 'a t -> int
  val full: 'a t -> bool
  val empty: 'a t -> bool

  val peekMin: 'a t -> 'a option
  val peekNearMax: 'a t -> 'a option
  val peekNearMaxAllowBackoff: 'a t -> 'a option

  (* destructive *)
  val popMin: 'a t -> 'a option
  val popNearMax: 'a t -> 'a option

  val push: 'a t -> 'a -> bool
end =
struct
  datatype 'a t =
    H of
      { data: 'a option array
      , cmp: 'a * 'a -> order
      , size: int ref
      , lock: SpinLock.t
      }

  (*fun arraySub x = Array.unsafeSub x
  fun arrayUpdate x = Array.unsafeUpdate x*)

  fun arraySub (a, i) =
    if i < 0 orelse i >= Array.length a then
      raise Subscript
    else
      MLton.HM.arraySubNoBarrier (a, i)

  fun arrayUpdate (a, i, x) =
    if i < 0 orelse i >= Array.length a then
      raise Subscript
    else
      MLton.HM.arrayUpdateNoBarrier (a, i, x)

  fun new {capacity, cmp} =
    let
      val data = Array.array (capacity, NONE)
    in
      H { data = data
        , cmp = cmp
        , size = ref 0
        , lock = SpinLock.new ()
        }
    end

  fun size (H {size, ...} : 'a t) = !size
  fun capacity (H {data, ...} : 'a t) = Array.length data
  fun full q = (size q = capacity q)
  fun empty q = (size q = 0)

  val myWorkerId = MLton.Parallel.processorNumber
  fun die strfn =
    ( print ("ERROR: " ^ Int.toString (myWorkerId ()) ^ ": " ^ strfn () ^ "\n")
    ; OS.Process.exit OS.Process.failure
    )

  (* =======================================================================
   * parent/child indices:
   *   0 
   *   | \
   *   1   2
   *   |\  |\
   *   3 4 5 6
   *   ...
   *)

  fun parent i =
    if i = 0 then NONE
    else SOME ((i+1) div 2 - 1)

  fun leftChild i = 2*i+1
  fun rightChild i = 2*i+2

  (* ======================================================================= *)

  (* if returns true, then lock is held and !size > 0.
   * if returns false, then lock is NOT held and !size = 0.
   *)
  fun takeLockAndNotEmpty (H {size, lock, ...}) =
    if !size = 0 then
      false
    else
      ( SpinLock.lock lock
      ; if !size > 0 then true
        else (SpinLock.unlock lock; false)
      )

  (* ======================================================================= *)

  fun peekMin (H {data, size, lock, ...}) =
    if !size = 0 then NONE else
    let
      val _ = SpinLock.lock lock
      val result = arraySub (data, 0)
    in
      SpinLock.unlock lock;
      result
    end

  
  fun peekNearMax (h as H {data, size, lock, ...}) =
    if not (takeLockAndNotEmpty h) then NONE else
    let
      val result = arraySub (data, !size-1)
    in
      SpinLock.unlock lock;
      result
    end

  
  fun peekNearMaxAllowBackoff (h as H {data, size, lock, ...}) =
    if !size = 0 then NONE
    else if not (SpinLock.trylock lock) then NONE
    else
      let
        val n = !size
      in
        if n = 0 then
          (SpinLock.unlock lock; NONE)
        else
          let
            val result = arraySub (data, n-1)
          in
            SpinLock.unlock lock;
            result
          end
      end


  fun popNearMax (h as H {data, size, lock, cmp}) =
    if not (takeLockAndNotEmpty h) then NONE else
    let
      val n = !size
      val result = arraySub (data, n-1)
    in
      size := n-1;
      arrayUpdate (data, n-1, NONE);
      SpinLock.unlock lock;
      result
    end


  (* ====================================================================== *)

  fun forceGetValOf data i =
    Option.valOf (arraySub (data, i))
    handle _ => die (fn _ => "ConcurrentBinaryHeap.forceGetValOf: bug")


  (* Take x at index i and sift it up until it's in the correct position,
   * updating the data array appropriately.
   *
   * Note: you do NOT need to set data[i] := x before calling this function.
   * siftUp will set data[...] := x at the appropriate place.
   *)
  fun siftUp cmp data (i, x) =
    case parent i of
      NONE =>
        arrayUpdate (data, i, SOME x)

    | SOME pi =>
        let
          val p = forceGetValOf data pi
        in
          case cmp (x, p) of
            LESS =>
              ( arrayUpdate (data, i, SOME p)
              ; siftUp cmp data (pi, x)
              )

          | _ =>
              arrayUpdate (data, i, SOME x)
        end


  (* Take x at index i and sift it down until it's in the correct position,
   * updating the data array appropriately.
   *
   * n is the current number of elements in data
   *
   * Note: similar to siftUp, you do NOT need to set data[i] := x before
   * calling this function.
   *)
  fun siftDown cmp (data, n) (i, x) =
    if leftChild i >= n then
      (* no chilren, so we're done *)
      arrayUpdate (data, i, SOME x)

    else if rightChild i >= n then
      (* only left child exists *)
      let
        val li = leftChild i
        val l = forceGetValOf data li
      in
        siftDownTowardsChild cmp (data, n) (i, x) (li, l)
      end
    
    else
      (* both children exist *)
      let
        val (li, ri) = (leftChild i, rightChild i)
        val (l, r) = (forceGetValOf data li, forceGetValOf data ri)
        val (ci, c) =
          if cmp (l, r) = LESS then (li, l) else (ri, r)
      in
        siftDownTowardsChild cmp (data, n) (i, x) (ci, c)
      end


  and siftDownTowardsChild cmp (data, n) (i, x) (ci, c) =
    case cmp (x, c) of
      GREATER =>
        ( arrayUpdate (data, i, SOME c)
        ; siftDown cmp (data, n) (ci, x)
        )
    
    | _ =>
        arrayUpdate (data, i, SOME x)


  (* ====================================================================== *)

  fun popMin (h as H {data, size, lock, cmp}) =
    if not (takeLockAndNotEmpty h) then NONE else
    let
      val n = !size
      val result = arraySub (data, 0)
      val newRoot = forceGetValOf data (n-1)
    in
      arrayUpdate (data, n-1, NONE);
      size := n-1;
      siftDown cmp (data, n-1) (0, newRoot);
      SpinLock.unlock lock;
      result
    end

  
  fun push (H {data, size, lock, cmp}) x =
    if !size >= Array.length data then false else
    let
      val _ = SpinLock.lock lock
      val n = !size
    in
      if n >= Array.length data then
        (SpinLock.unlock lock; false)
      else
        ( size := n+1
        ; siftUp cmp data (n, x)
        ; SpinLock.unlock lock
        ; true
        )
    end

end
