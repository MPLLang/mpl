structure Util:
sig
  val getTime: (unit -> 'a) -> ('a * Time.time)
  val hash64: Word64.word -> Word64.word

  val pow2: int -> int

  (* boundPow2 n == smallest power of 2 that is less-or-equal-to n *)
  val boundPow2: int -> int

  val foreach: 'a ArraySlice.slice -> (int * 'a -> unit) -> unit

  (* if the array is short, then convert it to a string. otherwise only
   * show the first few elements and the last element *)
  val summarizeArray: int -> ('a -> string) -> 'a array -> string
  val summarizeArraySlice: int -> ('a -> string) -> 'a ArraySlice.slice -> string

  (* `for (lo, hi) f` do f(i) sequentially for each lo <= i < hi *)
  val for: (int * int) -> (int -> unit) -> unit
end =
struct

  fun getTime f =
    let
      val t0 = Time.now ()
      val result = f ()
      val t1 = Time.now ()
    in
      (result, Time.- (t1, t0))
    end

  fun hash64 u =
    let
      open Word64
      infix 2 >> << xorb andb
      val v = u * 0w3935559000370003845 + 0w2691343689449507681
      val v = v xorb (v << 0w21)
      val v = v xorb (v << 0w37)
      val v = v xorb (v >> 0w4)
      val v = v * 0w4768777513237032717
      val v = v xorb (v << 0w20)
      val v = v xorb (v >> 0w41)
      val v = v xorb (v << 0w5)
    in
      v
    end

  fun pow2 i = if (i<1) then 1 else 2*pow2(i-1)

  fun searchPow2 n m = if m >= n then m else searchPow2 n (2*m)
  fun boundPow2 n = searchPow2 n 1

  fun for (lo, hi) f =
    if lo >= hi then () else (f lo; for (lo+1, hi) f)

  fun foreach s f =
    ForkJoin.parfor 4096 (0, ArraySlice.length s)
    (fn i => f (i, ArraySlice.sub (s, i)))

  fun summarizeArraySlice count toString xs =
    let
      val n = ArraySlice.length xs
      fun elem i = ArraySlice.sub (xs, i)

      val strs =
        if count <= 0 then raise Fail "summarizeArray needs count > 0"
        else if count <= 2 orelse n <= count then
          List.tabulate (n, toString o elem)
        else
          List.tabulate (count-1, toString o elem) @
          ["...", toString (elem (n-1))]
    in
      "[" ^ (String.concatWith ", " strs) ^ "]"
    end

  fun summarizeArray count toString xs =
    summarizeArraySlice count toString (ArraySlice.full xs)

end
