(* Standard Mersenne Twister pseudorandom number generator. 
   (MT19937 with the improved initialization scheme.)
   Based on the BSD-licensed code by the algorithm's authors; this
   implementation also BSD-licensed (see mersenne-twister.sml for
   details).

   Important note: This is a good source of randomness but not
   suitable for cryptography, because observing 624 consecutive
   ouputs allows an attacker to determine the internal state and
   predict future outputs. For cryptographic uses, the outputs
   should be passed through a secure one-way hash function.

   For a description of the algorithm, see M. Matsumoto and T.
   Nishimura, "Mersenne Twister: A 623-Dimensionally Equidistributed
   Uniform Pseudo-random Number Generator," ACM Transactions on
   Modeling and Computer Simulation, Vol. 8, No. 1, January 1998, pp
   3-30. *)
signature MERSENNETWISTER =
sig
    exception MersenneTwister of string

    (* Mersenne twister state; imperative *)
    type mt
    val init32 : Word32.word -> mt
    val init : Word32.word Vector.vector -> mt

    val rand32 : mt -> Word32.word

    (* Some utilities. These are nonstandard, and read an
       unspecified number of times from the MT stream. *)

    (* Use input string as 32-bit words, with zero padding. *)
    val initstring : string -> mt
        
    (* random_nat mt max
       Produce a random natural number [0, max). This has
       the correct distribution, but note that it
       can take unbounded time to return.
       (Expected worst case is two MT rand32 calls.) *)
    val random_nat : mt -> int -> int

    (* Produce a random boolean with equal probability. *)
    val random_bool : mt -> bool

    (* Shuffle an array. After calling, the array has
       the same elements but in random order. *)
    val shuffle : mt -> 'a Array.array -> unit

end
