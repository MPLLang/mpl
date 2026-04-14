structure Signal:
sig
  type sound = NewWaveIO.sound
  val delay: real -> real -> sound -> sound
  val allPass: real -> real -> sound -> sound
  val reverb: sound -> sound
end =
struct

  type sound = NewWaveIO.sound

  structure A = Array
  structure AS = ArraySlice

(*
  structure A =
  struct
    open A
    val update = Unsafe.Array.update
    val sub = Unsafe.Array.sub
  end

  structure AS =
  struct
    open AS
    fun update (s, i, x) =
      let val (a, start, _) = base s
      in A.update (a, start+i, x)
      end
    fun sub (s, i) =
      let val (a, start, _) = base s
      in A.sub (a, start+i)
      end
  end
*)

  fun delaySequential D a data =
    let
      val n = Seq.length data
      val output = ForkJoin.alloc n
    in
      Util.for (0, n) (fn i =>
        if i < D then
          A.update (output, i, Seq.nth data i)
        else
          A.update (output, i, Seq.nth data i + a * A.sub (output, i - D))
      );

      AS.full output
    end

  fun pow (a: real) n =
    if n <= 1 then
      a
    else if n mod 2 = 0 then
      pow (a*a) (n div 2)
    else
      a * pow (a*a) (n div 2)

  (* Granularity parameters *)
  val blockWidth = CommandLineArgs.parseInt "comb-width" 600
  val blockHeight = CommandLineArgs.parseInt "comb-height" 50
  val combGran = CommandLineArgs.parseInt "comb-threshold" 10000
  (*val _ = print ("comb-width " ^ Int.toString blockWidth ^ "\n")
  val _ = print ("comb-height " ^ Int.toString blockHeight ^ "\n")
  val _ = print ("comb-threshold " ^ Int.toString combGran ^ "\n")*)

  (* Imagine laying out the data as a matrix, where sample s[i*D + j] is
   * at row i, column j.
   *)
  fun delay' D alpha data =
    if Seq.length data <= combGran then
      delaySequential D alpha data
    else
    let
      val n = Seq.length data
      (* val _ = print ("delay' " ^ Int.toString D ^ " " ^ Int.toString n ^ "\n") *)
      val output = ForkJoin.alloc n

      val numCols = D
      val numRows = Util.ceilDiv n D

      fun getOutput i j =
        A.sub (output, i*numCols + j)

      fun setOutput i j x =
        let val idx = i*numCols + j
        in if idx < n then A.update (output, idx, x) else ()
        end

      fun input i j =
        let val idx = i*numCols + j
        in if idx >= n then 0.0 else AS.sub (data, idx)
        end

      val powAlpha = pow alpha blockHeight

      val numColumnStrips = Util.ceilDiv numCols blockWidth
      val numRowStrips = Util.ceilDiv numRows blockHeight

      fun doColumnStrip c =
        let
          val jlo = blockWidth * c
          val jhi = Int.min (numCols, jlo + blockWidth)
          val width = jhi - jlo
          val summaries =
            AS.full (ForkJoin.alloc (width * numRowStrips))

          fun doBlock b =
            let
              val ilo = blockHeight * b
              val ihi = Int.min (numRows, ilo + blockHeight)
              val ss = Seq.subseq summaries (width * b, width)
            in
              Util.for (0, width) (fn j => AS.update (ss, j, input ilo (jlo+j)));

              Util.for (ilo+1, ihi) (fn i =>
                Util.for (0, width) (fn j =>
                  AS.update (ss, j, input i (jlo+j) + alpha * AS.sub (ss, j))
                )
              )
            end

          val _ = ForkJoin.parfor 1 (0, numRowStrips) doBlock
          val summaries' = delay' width powAlpha summaries

          fun fillOutputBlock b =
            let
              val ilo = blockHeight * b
              val ihi = Int.min (numRows, ilo + blockHeight)
            in
              if b = 0 then
                Util.for (jlo, jhi) (fn j => setOutput 0 j (input 0 j))
              else
                let
                  val ss = Seq.subseq summaries' (width * (b-1), width)
                in
                  Util.for (0, width) (fn j =>
                    setOutput ilo (jlo+j) (input ilo (jlo+j) + alpha * AS.sub (ss, j)))
                end;

              Util.for (ilo+1, ihi) (fn i =>
                Util.for (jlo, jhi) (fn j =>
                  setOutput i j (input i j + alpha * getOutput (i-1) j)
                )
              )
            end
        in
          ForkJoin.parfor 1 (0, numRowStrips) fillOutputBlock
        end
    in
      ForkJoin.parfor 1 (0, numColumnStrips) doColumnStrip;

      AS.full output
    end

  fun delay ds alpha ({sr, data}: sound) =
    let
      val D = Real.round (ds * Real.fromInt sr)
    in
      {sr = sr, data = delay' D alpha data}
    end

  fun allPass' D a data =
    let
      val combed = delay' D a data

      fun output j =
        let
          val k = j - D
        in
          (1.0 - a*a) * (if k < 0 then 0.0 else Seq.nth combed k)
          - (a * Seq.nth data j)
        end
    in
      Seq.tabulate output (Seq.length data)
    end

  fun allPass ds a (snd as {sr, data}: sound) =
    let
      (* convert to samples *)
      val D = Real.round (ds * Real.fromInt sr)
    in
      { sr = sr
      , data = allPass' D a data
      }
    end

  val par = ForkJoin.par

  fun par4 (a, b, c, d) =
    let
      val ((ar, br), (cr, dr)) =
        par (fn _ => par (a, b), fn _ => par (c, d))
    in
      (ar, br, cr, dr)
    end

  fun shiftBy n s i =
    if i < n then
      0.0
    else if i < Seq.length s + n then
      Seq.nth s (i-n)
    else
      0.0

  fun reverb ({sr, data=dry}: sound) =
    let
      val N = Seq.length dry

      (* Originally, I tuned the comb and allPass parameters
       * based on numbers of samples at 44.1 kHz, which I chose
       * to be relatively prime to one another. But now, to
       * handle any sample rate, we need to convert these numbers
       * of samples. Does it really matter if the sample delays are
       * relatively prime? I'm not sure. For sample rates other
       * than 44.1 kHz, they almost certainly won't be now. *)

      val srr = Real.fromInt sr
      fun secondsToSamples sec = Real.round (sec * srr)
      fun secondsAt441 samples = Real.fromInt samples / 44100.0
      fun adjust x =
        if sr = 44100 then x else secondsToSamples (secondsAt441 x)

      val D1 = adjust 1931
      val D2 = adjust 2213
      val D3 = adjust 1747
      val D4 = adjust 1559

      val DA1 = adjust 167
      val DA2 = adjust 191

      val DE1 = adjust 1013
      val DE2 = adjust 1102
      val DE3 = adjust 1300

      val DF = adjust 1500

      (* ==========================================
       * Fused reflections near 50ms
       * (at 44.1kHz, 50ms is about 2200 samples)
       *
       * The basic design is taken from
       *   The Computer Music Tutorial (1996), page 481
       *   Author: Curtis Roads
       *
       * The basic design is 4 comb filters (in parallel)
       * which are then fed into two allpass filters, in series.
       *)

      val (c1, c2, c3, c4) =
        par4 (fn _ => delay' D1 0.7 dry,
              fn _ => delay' D2 0.7 dry,
              fn _ => delay' D3 0.7 dry,
              fn _ => delay' D4 0.7 dry)

      fun combs i =
        (Seq.nth c1 i +
         Seq.nth c2 i +
         Seq.nth c3 i +
         Seq.nth c4 i)

      val fused = Seq.tabulate combs N
      val fused = allPass' DA1 0.6 fused
      val fused = allPass' DA2 0.6 fused

      (* ==========================================
       * wet signal = dry + early + fused
       *
       * early reflections are single echos of
       * the dry sound that occur after around
       * 25ms delay
       *
       * the fused reflections start emerging after
       * approximately 35ms
       *)

      val wet = Seq.tabulate (fn i =>
          shiftBy 0 dry i
          + 0.6 * (shiftBy DE1 dry i)
          + 0.5 * (shiftBy DE2 dry i)
          + 0.4 * (shiftBy DE3 dry i)
          + 0.75 * (shiftBy DF fused i))
        (N + DF)

    in
      NewWaveIO.compress 2.0 {sr=sr, data=wet}
    end

end
