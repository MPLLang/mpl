structure Util:
sig
  val getTime: (unit -> 'a) -> ('a * Time.time)
  val reportTime: (unit -> 'a) -> 'a

  val closeEnough: real * real -> bool

  val die: string -> 'a

  val repeat: int * (unit -> 'a)  -> ('a)

  val hash64: Word64.word -> Word64.word
  val hash64_2: Word64.word -> Word64.word
  val hash32: Word32.word -> Word32.word
  val hash32_2: Word32.word -> Word32.word
  val hash32_3: Word32.word -> Word32.word
  val hash: int -> int

  val ceilDiv: int -> int -> int

  val pow2: int -> int

  (* this actually computes 1 + floor(log_2(n)), i.e. the number of
   * bits required to represent n in binary *)
  val log2: int -> int

  (* boundPow2 n == smallest power of 2 that is less-or-equal-to n *)
  val boundPow2: int -> int

  val foreach: 'a ArraySlice.slice -> (int * 'a -> unit) -> unit

  (* if the array is short, then convert it to a string. otherwise only
   * show the first few elements and the last element *)
  val summarizeArray: int -> ('a -> string) -> 'a array -> string
  val summarizeArraySlice: int -> ('a -> string) -> 'a ArraySlice.slice -> string

  (* `for (lo, hi) f` do f(i) sequentially for each lo <= i < hi
   * forBackwards goes from hi-1 down to lo *)
  val for: (int * int) -> (int -> unit) -> unit
  val forBackwards: (int * int) -> (int -> unit) -> unit

  (* `loop (lo, hi) b f`
   * for lo <= i < hi, iteratively do  b = f (b, i)  *)
  val loop: (int * int) -> 'a -> ('a * int -> 'a) -> 'a

  val all: (int * int) -> (int -> bool) -> bool
  val exists: (int * int) -> (int -> bool) -> bool

  val copyListIntoArray: 'a list -> 'a array -> int -> int

  val revMap: ('a -> 'b) -> 'a list -> 'b list

  val intToString: int -> string

  val equalLists: ('a * 'a -> bool) -> 'a list * 'a list -> bool

end =
struct

  fun ceilDiv n k = 1 + (n-1) div k

  fun digitToChar d = Char.chr (d + 48)

  fun intToString x =
    let
      (** For binary precision p, number of decimal digits needed is upper
        * bounded by:
        *     1 + log_{10}(2^p) = 1 + p * log_{10}(2)
        *                      ~= 1 + p * 0.30103
        *                       < 1 + p * 0.33333
        *                       = 1 + p / 3
        * Just for a little extra sanity, we'll do ceiling-div.
        *)
      val maxNumChars = 1 + ceilDiv (valOf Int.precision) 3
      val buf = ForkJoin.alloc maxNumChars

      val orig = x

      fun loop q i =
        let
          val i = i-1
          val d = ~(Int.rem (q, 10))
          val _ = Array.update (buf, i, digitToChar d)
          val q = Int.quot (q, 10)
        in
          if q <> 0 then
            loop q i
          else if orig < 0 then
            (Array.update (buf, i-1, #"~"); i-1)
          else
            i
        end

      val start = loop (if orig < 0 then orig else ~orig) maxNumChars
    in
      CharVector.tabulate (maxNumChars-start, fn i => Array.sub (buf, start+i))
    end

  fun die msg =
    ( TextIO.output (TextIO.stdErr, msg ^ "\n")
    ; TextIO.flushOut TextIO.stdErr
    ; OS.Process.exit OS.Process.failure
    )

  fun getTime f =
    let
      val t0 = Time.now ()
      val result = f ()
      val t1 = Time.now ()
    in
      (result, Time.- (t1, t0))
    end

  fun reportTime f =
    let
      val (result, tm) = getTime f
    in
      print ("time " ^ Time.fmt 4 tm ^ "s\n");
      result
    end

  fun closeEnough (x, y) =
    Real.abs (x - y) <= 0.000001

  (* NOTE: this actually computes 1 + floor(log_2(n)), i.e. the number of
   * bits required to represent n in binary *)
  fun log2 n = if (n < 1) then 0 else 1 + log2(n div 2)

  fun pow2 i = if (i<1) then 1 else 2*pow2(i-1)

  fun searchPow2 n m = if m >= n then m else searchPow2 n (2*m)
  fun boundPow2 n = searchPow2 n 1

  fun loop (lo, hi) b f =
    if lo >= hi then b else loop (lo+1, hi) (f (b, lo)) f

  fun forBackwards (i, j) f =
    if i >= j then () else (f (j-1); forBackwards (i, j-1) f)

  fun for (lo, hi) f =
    if lo >= hi then () else (f lo; for (lo+1, hi) f)

  fun foreach s f =
    ForkJoin.parfor 4096 (0, ArraySlice.length s)
    (fn i => f (i, ArraySlice.sub (s, i)))

  fun all (lo, hi) f =
    let
      fun allFrom i =
        (i >= hi) orelse (f i andalso allFrom (i+1))
    in
      allFrom lo
    end

  fun exists (lo, hi) f =
    let
      fun existsFrom i =
        i < hi andalso (f i orelse existsFrom (i+1))
    in
      existsFrom lo
    end

  fun copyListIntoArray xs arr i =
    case xs of
      [] => i
    | x :: xs =>
        ( Array.update (arr, i, x)
        ; copyListIntoArray xs arr (i+1)
        )

  fun repeat (n, f) =
    let
      fun rep_help 1 = f()
      |   rep_help n = ((rep_help (n-1)); f())

      val ns = if (n>0) then n else 1
    in
      rep_help ns
    end

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

  fun revMap f xs =
    let
      fun loop acc xs =
        case xs of
          [] => acc
        | x :: rest => loop (f x :: acc) rest
    in
      loop [] xs
    end

  (* // from numerical recipes
   * uint64_t hash64(uint64_t u)
   * {
   *   uint64_t v = u * 3935559000370003845ul + 2691343689449507681ul;
   *   v ^= v >> 21;
   *   v ^= v << 37;
   *   v ^= v >>  4;
   *   v *= 4768777513237032717ul;
   *   v ^= v << 20;
   *   v ^= v >> 41;
   *   v ^= v <<  5;
   *   return v;
   * }
   *)

  fun hash64 u =
    let
      open Word64
      infix 2 >> << xorb andb
      val v = u * 0w3935559000370003845 + 0w2691343689449507681
      val v = v xorb (v >> 0w21)
      val v = v xorb (v << 0w37)
      val v = v xorb (v >> 0w4)
      val v = v * 0w4768777513237032717
      val v = v xorb (v << 0w20)
      val v = v xorb (v >> 0w41)
      val v = v xorb (v << 0w5)
    in
      v
    end

  (* uint32_t hash32(uint32_t a) {
   *   a = (a+0x7ed55d16) + (a<<12);
   *   a = (a^0xc761c23c) ^ (a>>19);
   *   a = (a+0x165667b1) + (a<<5);
   *   a = (a+0xd3a2646c) ^ (a<<9);
   *   a = (a+0xfd7046c5) + (a<<3);
   *   a = (a^0xb55a4f09) ^ (a>>16);
   *   return a;
   * }
   *)

  fun hash32 a =
    let
      open Word32
      infix 2 >> << xorb
      val a = (a +    0wx7ed55d16) +    (a << 0w12)
      val a = (a xorb 0wxc761c23c) xorb (a >> 0w19)
      val a = (a +    0wx165667b1) +    (a << 0w5)
      val a = (a +    0wxd3a2646c) xorb (a << 0w9)
      val a = (a +    0wxfd7046c5) +    (a << 0w3)
      val a = (a xorb 0wxb55a4f09) xorb (a >> 0w16)
    in
      a
    end

  (* uint32_t hash32_2(uint32_t a) {
   *   uint32_t z = (a + 0x6D2B79F5UL);
   *   z = (z ^ (z >> 15)) * (z | 1UL);
   *   z ^= z + (z ^ (z >> 7)) * (z | 61UL);
   *   return z ^ (z >> 14);
   * }
   *)

  fun hash32_2 a =
    let
      open Word32
      infix 2 >> << xorb orb
      val z = (a + 0wx6D2B79F5)
      val z = (z xorb (z >> 0w15)) * (z orb 0w1)
      val z = z xorb (z + (z xorb (z >> 0w7)) * (z orb 0w61))
    in
      z xorb (z >> 0w14)
    end

  (* inline uint32_t hash32_3(uint32_t a) {
   *   uint32_t z = a + 0x9e3779b9;
   *   z ^= z >> 15; // 16 for murmur3
   *   z *= 0x85ebca6b;
   *   z ^= z >> 13;
   *   z *= 0xc2b2ae3d; // 0xc2b2ae35 for murmur3
   *   return z ^= z >> 16;
   * }
   *)

  fun hash32_3 a =
    let
      open Word32
      infix 2 >> << xorb orb
      val z = a + 0wx9e3779b9
      val z = z xorb (z >> 0w15)  (* 16 for murmur3 *)
      val z = z * 0wx85ebca6b
      val z = z xorb (z >> 0w13)
      val z = z * 0wxc2b2ae3d     (* 0wxc2b2ae35 for murmur3 *)
      val z = z xorb (z >> 0w16)
    in
      z
    end

  (* // a slightly cheaper, but possibly not as good version
   * // based on splitmix64
   * inline uint64_t hash64_2(uint64_t x) {
   *   x = (x ^ (x >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
   *   x = (x ^ (x >> 27)) * UINT64_C(0x94d049bb133111eb);
   *   x = x ^ (x >> 31);
   *   return x;
   * }
   *)

  fun hash64_2 x =
    let
      open Word64
      infix 2 >> << xorb orb
      val x = (x xorb (x >> 0w30)) * 0wxbf58476d1ce4e5b9
      val x = (x xorb (x >> 0w27)) * 0wx94d049bb133111eb
      val x = x xorb (x >> 0w31)
    in
      x
    end

  (* This chooses which hash function to use for generic integers, since
   * integers are configurable at compile time. *)
  val hash : int -> int =
    case Int.precision of
      NONE =>    (Word64.toInt  o hash64 o Word64.fromInt)
    | SOME 32 => (Word32.toIntX o hash32 o Word32.fromInt)
    | SOME 64 => (Word64.toIntX o hash64 o Word64.fromInt)
    | SOME p => (fn x =>
        let
          val wp1 = Word.fromInt (p-1)
          open Word64
          infix 2 >> << andb
          val v = hash64 (fromInt x)
          val v = v andb ((0w1 << wp1) - 0w1)
        in
          toInt v
        end)


  fun equalLists eq ([], []) = true
    | equalLists eq (x :: xs, y :: ys) =
        eq (x, y) andalso equalLists eq (xs, ys)
    | equalLists _ _ = false

end
