signature COMPLEX =
sig
  structure R: REAL
  type r = R.real

  type t

  val toString: t -> string

  val make: (r * r) -> t
  val view: t -> (r * r)

  val defaultReal: Real.real -> t
  val defaultImag: Real.real -> t

  val real: r -> t
  val imag: r -> t
  val rotateBy: r -> t (* rotateBy x = e^(ix) *)

  val zeroThreshold: r
  val realIsZero: r -> bool

  val isZero: t -> bool
  val isNonZero: t -> bool

  val zero: t
  val i: t

  val magnitude: t -> r

  val ~ : t -> t
  val - : t * t -> t
  val + : t * t -> t
  val * : t * t -> t

  val scale: r * t -> t
end


functor MkComplex(R: REAL): COMPLEX =
struct
  structure R = R
  open R
  type r = real

  val fromLarge = fromLarge IEEEReal.TO_NEAREST

  datatype t = C of {re: real, im: real}

  val rtos = fmt (StringCvt.FIX (SOME 8))

  fun toString (C {re, im}) =
    let
      val (front, re) = if Int.< (R.sign re, 0) then ("-", R.~ re) else ("", re)
      val (middle, im) =
        if Int.< (R.sign im, 0) then ("-", R.~ im) else ("+", im)
    in
      front ^ rtos re ^ middle ^ rtos im ^ "i"
    end

  fun make (re, im) = C {re = re, im = im}

  fun view (C {re, im}) = (re, im)

  val zeroThreshold = fromLarge 0.00000001
  fun realIsZero x = R.abs x < zeroThreshold

  fun magnitude (C {re, im}) =
    R.Math.sqrt (R.+ (R.* (re, re), R.* (im, im)))

  fun isZero (C {re, im}) = realIsZero re andalso realIsZero im

  fun isNonZero c =
    not (isZero c)

  fun rotateBy r =
    C {re = Math.cos r, im = Math.sin r}

  fun real r =
    C {re = r, im = fromLarge 0.0}
  fun imag i =
    C {re = fromLarge 0.0, im = i}

  fun defaultReal r =
    real (fromLarge r)
  fun defaultImag r =
    imag (fromLarge r)

  val zero = C {re = fromLarge 0.0, im = fromLarge 0.0}
  val i = C {re = fromLarge 0.0, im = fromLarge 1.0}

  fun neg (C {re, im}) =
    C {re = ~re, im = ~im}

  fun add (C x, C y) =
    C {re = #re x + #re y, im = #im x + #im y}

  fun sub (C x, C y) =
    C {re = #re x - #re y, im = #im x - #im y}

  fun mul (C {re = a, im = b}, C {re = c, im = d}) =
    C {re = a * c - b * d, im = a * d + b * c}

  fun scale (r, C {re, im}) =
    C {re = r * re, im = r * im}

  val ~ = neg
  val op- = sub
  val op+ = add
  val op* = mul
end
