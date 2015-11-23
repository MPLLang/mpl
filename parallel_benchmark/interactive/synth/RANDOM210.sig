signature RANDOM210 =
sig
  type rand
  val fromInt : int -> rand

  val next : rand -> rand
  val split : rand -> rand * (rand * rand)
  val split3 : rand -> rand * (rand * rand * rand)
  val splitTab : rand * int -> rand * (int -> rand)

  val bool : rand -> bool
  val biasedBool : (int * int) -> rand -> bool
  val int : rand -> int
  val boundedInt : (int * int) -> rand -> int
  val real : rand -> real
  val boundedReal : (real * real) -> rand -> real
  val char : rand -> char
  val boundedChar : (char * char) -> rand -> char
end
