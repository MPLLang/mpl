structure NewWaveIO:
sig
  (* A sound is a sequence of samples at the given
   * sample rate, sr (measured in Hz).
   * Each sample is in range [-1.0, +1.0]. *)
  type sound = {sr: int, data: real Seq.t}

  val readSound: string -> sound
  val writeSound: sound -> string -> unit

  (* Essentially mu-law compression. Normalizes to [-1,+1] and compresses
   * the dynamic range slightly. The boost parameter should be >= 1. *)
  val compress: real -> sound -> sound
end =
struct

  type sound = {sr: int, data: real Seq.t}

  structure AS = ArraySlice

  fun err msg =
    raise Fail ("NewWaveIO: " ^ msg)

  fun compress boost (snd as {sr, data}: sound) =
    if boost < 1.0 then
      err "Compression boost parameter must be at least 1"
    else
      let
        (* maximum amplitude *)
        val maxA =
          SeqBasis.reduce 10000 Real.max 1.0 (0, Seq.length data)
            (fn i => Real.abs (Seq.nth data i))

        (* a little buffer of intensity to avoid distortion *)
        val maxA' = 1.05 * maxA

        val scale = Math.ln (1.0 + boost)

        fun transfer x =
          let
            (* normalized *)
            val x' = Real.abs (x / maxA')
          in
            (* compressed *)
            Real.copySign (Math.ln (1.0 + boost * x') / scale, x)
          end
      in
        { sr = sr
        , data = Seq.map transfer data
        }
      end

  fun readSound path =
    let
      val bytes = ReadFile.contentsBinSeq path

      fun findChunk chunkId offset =
        if offset > (Seq.length bytes - 8) then
          err "unexpected end of file"
        else if Parse.r32b bytes offset = chunkId then
          offset (* found it! *)
        else
          let
            val chunkSize = Word32.toInt (Parse.r32l bytes (offset+4))
            val chunkName =
              CharVector.tabulate (4, fn i =>
                Char.chr (Word8.toInt (Seq.nth bytes (offset+i))))
          in
            if chunkSize < 0 then
              err ("error parsing chunk size of '" ^ chunkName ^ "' chunk")
            else
              findChunk chunkId (offset + 8 + chunkSize)
          end

      (* =======================================================
       * RIFF header, 12 bytes
       *)

      val _ =
        if Seq.length bytes >= 12 then ()
        else err "not enough bytes for RIFF header"

      val offset = 0
      val riff = 0wx52494646 (* ascii "RIFF", big endian *)
      val _ =
        if Parse.r32b bytes offset = riff then ()
        else err "expected 'RIFF' chunk ID"

      (* the chunkSize should be the size of the "rest of the file" *)
      val offset = 4
      val chunkSize = Word32.toInt (Parse.r32l bytes offset)

      val totalFileSize = 8 + chunkSize
      val _ =
        if Seq.length bytes >= totalFileSize then ()
        else err ("expected " ^ Int.toString totalFileSize ^
                  " bytes but the file is only " ^
                  Int.toString (Seq.length bytes))

      val offset = 8
      val wave = 0wx57415645 (* ascii "WAVE" big endian *)
      val _ =
        if Parse.r32b bytes offset = wave then ()
        else err "expected 'WAVE' format"

      val offset = 12

      (* =======================================================
       * fmt subchunk, should be at least 8+16 bytes total for PCM
       *)

      val fmtId = 0wx666d7420 (* ascii "fmt " big endian *)
      val fmtChunkStart = findChunk fmtId offset
      val offset = fmtChunkStart

      val _ =
        if Parse.r32b bytes offset = fmtId then ()
        else err "expected 'fmt ' chunk ID"

      val offset = offset+4
      val fmtChunkSize = Word32.toInt (Parse.r32l bytes offset)
      val _ =
        if fmtChunkSize >= 16 then ()
        else err "expected 'fmt ' chunk to be at least 16 bytes"

      val offset = offset+4
      val audioFormat = Word16.toInt (Parse.r16l bytes offset)
      val _ =
        if audioFormat = 1 then ()
        else err ("expected PCM audio format, but found 0x"
                  ^ Int.fmt StringCvt.HEX audioFormat)

      val offset = offset+2
      val numChannels = Word16.toInt (Parse.r16l bytes offset)

      val offset = offset+2
      val sampleRate = Word32.toInt (Parse.r32l bytes offset)

      val offset = offset+4
      val byteRate = Word32.toInt (Parse.r32l bytes offset)

      val offset = offset+4
      val blockAlign = Word16.toInt (Parse.r16l bytes offset)

      val offset = offset+2
      val bitsPerSample = Word16.toInt (Parse.r16l bytes offset)
      val bytesPerSample = bitsPerSample div 8

      val offset = fmtChunkStart+8+fmtChunkSize

      (* =======================================================
       * data subchunk, should be the rest of the file
       *)

      val dataId = 0wx64617461 (* ascii "data" big endian *)
      val dataChunkStart = findChunk dataId offset
      val offset = dataChunkStart

      val _ =
        if Parse.r32b bytes offset = dataId then ()
        else err "expected 'data' chunk ID"

      val offset = offset + 4
      val dataSize = Word32.toInt (Parse.r32l bytes offset)
      val _ =
        if dataChunkStart + 8 + dataSize <= totalFileSize then ()
        else err ("badly formatted data chunk: unexpected end-of-file")

      val dataStart = dataChunkStart + 8

      val numSamples = (dataSize div numChannels) div bytesPerSample

      fun readSample8 pos =
        Real.fromInt (Word8.toInt (Seq.nth bytes pos) - 128) / 256.0
      fun readSample16 pos =
        Real.fromInt (Word16.toIntX (Parse.r16l bytes pos)) / 32768.0

      val readSample =
        case bytesPerSample of
          1 => readSample8
        | 2 => readSample16
        | _ => err "only 8-bit and 16-bit samples supported at the moment"

      (* jth sample of ith channel *)
      fun readChannel i j =
        readSample (dataStart + j * (numChannels * bytesPerSample)
                              + i * bytesPerSample)

      val rawData =
        AS.full (SeqBasis.tabulate 1000 (0, numSamples) (fn j =>
            Util.loop (0, numChannels) 0.0 (fn (s, i) => s + readChannel i j)))

      val rawResult = {sr = sampleRate, data = rawData}
    in
      if numChannels = 1 then
        rawResult
      else
        ( TextIO.output (TextIO.stdErr,
            "[WARN] mixing " ^ Int.toString numChannels
            ^ " channels down to mono\n")
        ; compress 1.0 rawResult
        )
    end

  (* ====================================================================== *)

  fun writeSound ({sr, data}: sound) path =
    let
      val srw = Word32.fromInt sr

      val file = BinIO.openOut path

      val w32b = ExtraBinIO.w32b file
      val w32l = ExtraBinIO.w32l file
      val w16l = ExtraBinIO.w16l file

      val totalBytes =
        44 + (Seq.length data * 2)

      val riffId = 0wx52494646 (* ascii "RIFF", big endian *)
      val fmtId = 0wx666d7420 (* ascii "fmt " big endian *)
      val wave = 0wx57415645 (* ascii "WAVE" big endian *)
      val dataId = 0wx64617461 (* ascii "data" big endian *)
    in
      (* ============================
       * RIFF header, 12 bytes *)
      w32b riffId;
      w32l (Word32.fromInt (totalBytes - 8));
      w32b wave;

      (* ============================
       * fmt subchunk, 24 bytes *)
      w32b fmtId;
      w32l 0w16;    (* 16 remaining bytes in subchunk *)
      w16l 0w1;     (* audio format PCM = 1 *)
      w16l 0w1;     (* 1 channel (mono) *)
      w32l srw;     (* sample rate *)
      w32l (srw * 0w2); (* "byte rate" = sampleRate * numChannels * bytesPerSample *)
      w16l 0w2;     (* "block align" = numChannels * bytesPerSample *)
      w16l 0w16;    (* bits per sample *)

      (* ============================
       * data subchunk: rest of file *)
      w32b dataId;
      w32l (Word32.fromInt (2 * Seq.length data)); (* number of data bytes *)

      Util.for (0, Seq.length data) (fn i =>
        let
          val s = Seq.nth data i
          val s =
            if s < ~1.0 then ~1.0
            else if s > 1.0 then 1.0
            else s
          val s = Real.round (s * 32767.0)
          val s = if s < 0 then s + 65536 else s
        in
          w16l (Word16.fromInt s)
        end);

      BinIO.closeOut file
    end
end
