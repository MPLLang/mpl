(* Original C source copyright (c) 1997 - 2002, Makoto Matsumoto and
   Takuji Nishimura. 
   Standard ML port copyright (c) 2008, Tom Murphy VII. 
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote 
        products derived from this software without specific prior written 
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
   COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
   STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
   OF THE POSSIBILITY OF SUCH DAMAGE.

   (For my part, I don't actually care if you preserve this copyright
    notice and disclaimer in binary redistributions of this code.
    Depending on what you think the copyright status of this
    straightfoward port from C to SML is (it doesn't literally contain
    any of their code, just a translation of the algorithm), you might
    have to in order to comply with Matsumoto and Nishimura's
    license.) 
*)
structure MersenneTwister :> MERSENNETWISTER =
struct
    type w32 = Word32.word
    (* of length 624 *)
    type mt = w32 Array.array * int ref

    exception MersenneTwister of string

    val N = 624
    val M = 397
    val MATRIX_A = 0wx9908b0df : w32
    val UPPER_MASK = 0wx80000000 : w32
    val LOWER_MASK = 0wx7fffffff : w32

    val && = Word32.andb
    val || = Word32.orb
    val ^^ = Word32.xorb
    val >> = Word32.>>
    val << = Word32.<<
    infix && || ^^ << >>

    fun init32 (s : w32) =
        let
            val mt = Array.array (N, 0w0)
            fun loop (prev, n) =
                if n = N 
                then ()
                else
                    let 
                        val v = 0w1812433253 * 
                                (prev ^^ (prev >> 0w30)) + 
                                (Word32.fromInt n)
                    in
                        Array.update(mt, n, v);
                        loop (v, n + 1)
                    end
        in
            Array.update(mt, 0, s);
            loop (s, 1);
            (mt, ref N)
        end
    
    fun init (init_key : w32 Vector.vector) =
        let
            val (mt, mti) = init32 0w19650218
            val k = Int.max(Vector.length init_key, N)
            fun loop1 (0, i, _) = i
              | loop1 (k, i, j) =
                let 
                    val () = Array.update
                        (mt, i,
                         (Array.sub(mt, i) ^^
                          ((Array.sub(mt, i - 1) ^^
                            (Array.sub(mt, i - 1) >> 0w30))) *
                          0w1664525) +
                         Vector.sub(init_key, j) + Word32.fromInt j)
                    val i = i + 1
                    val j = j + 1
                    val i = if i >= N
                            then (Array.update(mt, 0, Array.sub(mt, N - 1)); 1)
                            else i
                    val j = if j >= Vector.length init_key then 0 else j
                in
                    loop1 (k - 1, i, j)
                end

            fun loop2 (0, i) = ()
              | loop2 (k, i) =
                let
                    val () = Array.update(mt, i,
                                          (Array.sub(mt, i) ^^
                                           ((Array.sub(mt, i - 1) ^^
                                             (Array.sub(mt, i - 1) >> 0w30))) *
                                           0w1566083941) - Word32.fromInt i)
                    val i = i + 1
                    val i = if i >= N
                            then (Array.update(mt, 0, Array.sub(mt, N - 1)); 1)
                            else i
                in
                    loop2 (k - 1, i)
                end

            val i = loop1 (k, 1, 0)
            val () = loop2 (N - 1, i)
        in
            Array.update(mt, 0, 0wx80000000);
            (mt, mti)
        end

    fun rand32 (mt, mti) =
        let
            fun mag01 0w0 = 0w0
              | mag01 _ = MATRIX_A

            val () = if !mti >= N
                     then
                         let
                             fun loop1 kk =
                                 if kk < N - M
                                 then
                                     let val y = (Array.sub(mt, kk) &&
                                                  UPPER_MASK) ||
                                         (Array.sub(mt, kk + 1) &&
                                          LOWER_MASK)
                                     in
                                         Array.update(mt, kk,
                                                      Array.sub(mt, kk + M) ^^
                                                      (y >> 0w1) ^^
                                                      mag01 (y && 0w1));
                                         loop1 (kk + 1)
                                     end
                                 else ()

                             fun loop2 kk =
                                 if kk < N - 1
                                 then let val y = (Array.sub(mt, kk) &&
                                                   UPPER_MASK) ||
                                     (Array.sub(mt, kk + 1) &&
                                      LOWER_MASK)
                                      in
                                          Array.update
                                          (mt, kk,
                                           Array.sub(mt, kk + (M - N)) ^^
                                           (y >> 0w1) ^^
                                           mag01 (y && 0w1));
                                          loop2 (kk + 1)
                                      end
                                 else ()
                             val () = loop1 0
                             val () = loop2 (N - M)
                                 
                             val y = (Array.sub(mt, N - 1) && UPPER_MASK) ||
                                     (Array.sub(mt, 0) && LOWER_MASK)
                             val () = Array.update(mt, N - 1,
                                                   Array.sub(mt, M - 1) ^^
                                                   (y >> 0w1) ^^
                                                   mag01 (y && 0w1))
                         in
                             mti := 0
                         end
                     else ()

            val y = Array.sub(mt, !mti)
            val () = mti := !mti + 1
            val y = y ^^ (y >> 0w11)
            val y = y ^^ ((y << 0w7) && 0wx9d2c5680)
            val y = y ^^ ((y << 0w15) && 0wxefc60000)
            val y = y ^^ (y >> 0w18)
        in
            y
        end

    (* Utilities (c) 2009 and onwards Tom Murphy VII *)
    fun initstring s =
        let
            (* Always pad at least one zero, since init requires
               a non-empty vector. *)
            val nwords = size s div 4 + 1
            fun wordat i =
                if i >= size s 
                then 0w0
                else Word32.fromInt (ord (CharVector.sub(s, i)))
        in
            init
            (Vector.tabulate 
             (nwords,
              (fn w =>
               (wordat (w * 4) << 0w24) ||
               (wordat (w * 4 + 1) << 0w16) ||
               (wordat (w * 4 + 2) << 0w8) ||
               wordat (w * 4 + 3))))
        end

    fun random_bool mt = Word32.andb(rand32 mt, 0w8) = 0w8

    fun random_nat mt max =
        if max <= 0
        then raise MersenneTwister "in random_nat, max must be >0"
        else
            let
                (* We only need to take as many random bits as cover
                   the max *)
                fun getmask m =
                    let val newmask = Word32.>>(m, 0w1)
                    in
                        if Word32.toInt newmask < max
                        then m
                        else getmask newmask
                    end
                (* Never consider negative mask. *)
                val mask = getmask 0wx7FFFFFFF

                (* keep going until we get a non-rejected sample *)
                fun loop () =
                    let val sample = Word32.toInt (Word32.andb(mask, rand32 mt))
                    in
                        if sample < max
                        then sample
                        else loop()
                    end
            in
                loop ()
            end

    fun util_for lo hi f =
      if lo > hi then ()
      else (ignore (f lo); util_for (lo + 1) hi f)

    fun shuffle mt arr =
        let
            val radix = Array.length arr
            fun swap (i, j) =
                let
                    val t = Array.sub(arr, i)
                in
                    Array.update(arr, i, Array.sub(arr, j));
                    Array.update(arr, j, t)
                end
        in
            util_for 0 (radix - 1)
            (fn i =>
             swap (i, random_nat mt radix))
        end
end
