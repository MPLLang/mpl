structure Rat :>
sig
  type t
  type i = IntInf.int

  (* make(n, d) ~> n/d *)
  val make: i * i -> t
  val view: t -> i * i

  val normalize: t -> t

  val * : t * t -> t
  val - : t * t -> t
  val + : t * t -> t
  val div: t * t -> t

  val max: t * t -> t

  val sign: t -> int
  val compare: t * t -> order

  val approx: t -> Real64.real

  val toString: t -> string
end =
struct

  type i = IntInf.int
  type t = i * i

  fun make (n, d) = (n, d)
  fun view (n, d) = (n, d)

  fun gcd (a, b) =
    if b = 0 then a else gcd (b, IntInf.mod (a, b))

  fun normalize (n, d) =
    if n = 0 then
      (0, 1)
    else
      let
        val same = IntInf.sameSign (n, d)

        val na = IntInf.abs n
        val da = IntInf.abs d

        val g = gcd (na, da)

        val n' = IntInf.div (na, g)
        val d' = IntInf.div (da, g)
      in
        if same then (n', d') else (IntInf.~ n', d')
      end

  fun mul ((a, b): t, (c, d)) = (a * c, b * d)

  fun add ((a, b): t, (c, d)) =
    (a * d + b * c, b * d)

  fun sub ((a, b): t, (c, d)) =
    (a * d - b * c, b * d)

  fun divv ((a, b): t, (c, d)) = (a * d, b * c)


  fun sign (n, d) =
    if n = 0 then 0 else if IntInf.sameSign (n, d) then 1 else ~1


  fun compare (r1, r2) =
    let
      val diff = sub (r1, r2)
      val s = sign diff
    in
      if s < 0 then LESS else if s = 0 then EQUAL else GREATER
    end


  fun max (r1, r2) =
    case compare (r1, r2) of
      LESS => r2
    | _ => r1


  fun itor x =
    Real64.fromLargeInt (IntInf.toLarge x)


  (* =========================================================================
   * approximate a rational with Real64
   *
   * TODO: not sure what the best way to do this is. Kinda just threw something
   * together. It's probably kinda messed up in a subtle way.
   *)

  local
    fun loopApprox acc (r, d) =
      let
        val r' = itor r
        val d' = itor d
      in
        if Real64.isFinite r' andalso Real64.isFinite d' then
          acc + r' / d'
        else
          let
            val d2 = d div 2
          in
            if r > d2 then
              loopApprox (acc + 0.5) (normalize (r - d2, d))
            else
              (* no idea how good this is... *)
              loopApprox acc (normalize (r div 2, d div 2))
          end
      end
  in
    fun approx (n, d) =
      let
        val (n, d) = normalize (n, d)
        val s = Real64.fromInt (IntInf.sign n)
        val n = IntInf.abs n

        (* abs(n/d) = m + r/d
         * where: m is a natural number
         *   and: r/d is a proper fraction
         *)
        val (m, r) = IntInf.divMod (n, d)
        val m = itor m
      in
        if not (Real64.isFinite m) then s * m
        else s * (m + loopApprox 0.0 (normalize (r, d)))
      end
  end

  (* ======================================================================= *)

  fun toString (n, d) =
    IntInf.toString n ^ "/" ^ IntInf.toString d

  (* ======================================================================= *)

  val op* = mul
  val op+ = add
  val op- = sub
  val op div = divv
end
