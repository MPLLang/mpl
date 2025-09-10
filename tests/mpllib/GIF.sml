structure GIF:
sig
  type pixel = Color.pixel
  type image = {height: int, width: int, data: pixel Seq.t}

  (* A GIF color palette is a table of up to 256 colors, and
   * function for remapping the colors of an image. *)
  structure Palette:
  sig
    type t = {colors: pixel Seq.t, remap: image -> int Seq.t}

    (* Selects a set of "well-spaced" colors sampled from the image.
     * The first argument is a list of required colors, that must be
     * included in the palette. Second number is desired palette size.
     *)
    val summarize: pixel list -> int -> image -> t

    (* Same as `summarize`, except now just an arbitrary sampling function
     * is given instead of an image.
     *)
    val summarizeBySampling: pixel list -> int -> (int -> pixel) -> t

    (* Selects a quantized color palette. *)
    val quantized: (int * int * int) -> t

    val remapColor: t -> pixel -> int
  end

  structure LZW:
  sig
    (* First step of compression. Remap an image with the given color
     * palette, and then generate the LZW-compressed code stream.
     * This inserts clear- and EOI codes.
     *
     * arguments are
     *   1. number of colors, and
     *   2. color indices (from palette remap)
     *)
    val codeStream: int -> int Seq.t -> int Seq.t

    (* Second step of compression: pack the code stream into bits with
     * flexible bit-lengths. This step also inserts sub-block sizes.
     * The first argument is the number of colors. *)
    val packCodeStream: int -> int Seq.t -> Word8.word Seq.t
  end

  (* Write many images as an animation. All images must be the same dimension. *)
  val writeMany: string  (* output path *)
              -> int     (* microsecond delay between images *)
              -> Palette.t
              -> {width: int, height: int, numImages: int, getImage: int -> int Seq.t}
              -> unit

  val write: string -> image -> unit
end =
struct

  structure AS = ArraySlice

  type pixel = Color.pixel
  type image = {height: int, width: int, data: pixel Seq.t}

  fun err msg =
    raise Fail ("GIF: " ^ msg)

  fun stderr msg =
    (TextIO.output (TextIO.stdErr, msg); TextIO.output (TextIO.stdErr, "\n"))

  fun ceilLog2 n =
    if n <= 0 then
      err "ceilLog2: expected input at least 1"
    else
      (* Util.log2(x) computes 1 + floor(log_2(x)) *)
      Util.log2 (n-1)

  structure Palette =
  struct

    type t = {colors: pixel Seq.t, remap: image -> int Seq.t}

    fun remapColor ({remap, ...}: t) px =
      Seq.nth (remap {width=1, height=1, data=Seq.fromList [px]}) 0

    fun makeQuantized (rqq, gqq, bqq) =
      let
        fun bucketSize numBuckets =
          Real.floor (1.0 + 255.0 / Real.fromInt numBuckets)
        fun bucketShift numBuckets =
          Word8.fromInt ((255 - (numBuckets-1)*(bucketSize numBuckets)) div 2)

        fun qi nb = fn c => Word8.toInt c div (bucketSize nb)
        fun qc nb = fn i => bucketShift nb + Word8.fromInt (i * bucketSize nb)

        fun makeQ nb =
          { numBuckets = nb
          , channelToIdx = qi nb
          , channelFromIdx = qc nb
          }

        val R = makeQ rqq
        val G = makeQ gqq
        val B = makeQ bqq
        val numQuantized = (* this should be at most 256 *)
          List.foldl op* 1 (List.map #numBuckets [R, G, B])

        fun channelIndices {red, green, blue} =
          (#channelToIdx R red, #channelToIdx G green, #channelToIdx B blue)

        fun packChannelIndices (r, g, b) =
          b +
          g * (#numBuckets B) +
          r * (#numBuckets B) * (#numBuckets G)

        fun colorOfQuantizeIdx i =
          let
            val b = i mod (#numBuckets B)
            val g = (i div (#numBuckets B)) mod (#numBuckets G)
            val r = (i div (#numBuckets B) div (#numBuckets G)) mod (#numBuckets R)
          in
            { red = #channelFromIdx R r
            , green = #channelFromIdx G g
            , blue = #channelFromIdx B b
            }
          end
      in
        (numQuantized, channelIndices, packChannelIndices, colorOfQuantizeIdx)
      end

    fun quantized qpackage =
      let
        val (numQuantized, channelIndices, pack, colorOfQuantizeIdx) =
          makeQuantized qpackage
      in
        { colors = Seq.tabulate colorOfQuantizeIdx numQuantized
        , remap = fn ({data, ...}: image) =>
            AS.full (SeqBasis.tabulate 1000 (0, Seq.length data) (fn i =>
              pack (channelIndices (Seq.nth data i))))
        }
      end

    fun summarizeBySampling requiredColors paletteSize (sample: int -> Color.pixel) =
      if paletteSize <= 0 then
        err "summarize: palette size must be at least 1"
      else if paletteSize > 256 then
        err "summarize: max palette size is 256"
      else if List.length requiredColors > paletteSize then
        err "summarize: Too many required colors"
      else
      let
        val dist = Color.sqDistance

        val dimBits = 3
        val dim = Util.pow2 dimBits
        val numBuckets = dim*dim*dim
        fun chanIdx chan =
          Word8.toInt (Word8.>> (chan, Word.fromInt (8 - dimBits)))
        fun chanIdxs {red, green, blue} =
          (chanIdx red, chanIdx green, chanIdx blue)
        fun pack (r, g, b) = (dim*dim)*r + dim*g + b

        val table = Array.array (numBuckets, [])
        val sz = ref 0
        fun tableSize () = !sz

        fun insert color =
          let
            val i = pack (chanIdxs color)
            val inBucket = Array.sub (table, i)
          in
            Array.update (table, i, color :: inBucket);
            sz := !sz + 1
          end

        fun bounds x = (Int.max (0, x-1), Int.min (dim, x+2))

        fun minDist color =
          let
            val (r, g, b) = chanIdxs color
          in
            Util.loop (bounds r) (valOf Int.maxInt) (fn (md, r) =>
            Util.loop (bounds g) md (fn (md, g) =>
            Util.loop (bounds b) md (fn (md, b) =>
              List.foldl (fn (c, md) => Int.min (md, dist (c, color)))
                md
                (Array.sub (table, pack (r, g, b)) ))))
          end

        val candidatesSize = 20

        fun chooseColorsLoop i =
          if tableSize () = paletteSize then () else
          let
            fun minDist' px = (px, minDist px)
            fun chooseMax ((c1, dc1), (c2, dc2)) =
              if dc1 > dc2 then (c1, dc1) else (c2, dc2)
            val (c, _) =
              Util.loop (0, candidatesSize) (Color.black, ~1)
              (fn (best, j) => chooseMax (best, minDist' (sample (i+j))))
          in
            insert c;
            chooseColorsLoop (i + candidatesSize)
          end

        (* First, demand that there are a few simple colors in there! *)
        val _ = List.app insert requiredColors

        (* Now, loop until full *)
        val _ = chooseColorsLoop 0

        (* Compact the table *)
        val buckets = AS.full table
        val bucketSizes = Seq.map List.length buckets
        val bucketOffsets =
          AS.full (SeqBasis.scan 100 op+ 0 (0, numBuckets) (Seq.nth bucketSizes))
        val palette = ForkJoin.alloc paletteSize
        val _ =
          Util.for (0, numBuckets) (fn i =>
            ignore (Util.copyListIntoArray
              (Seq.nth buckets i)
              palette
              (Seq.nth bucketOffsets i)))
        val palette = AS.full palette

        (* remap by lookup into compacted table *)
        fun remapOne color =
          let
            val (r, g, b) = chanIdxs color

            fun chooseMin ((c1, dc1), (c2, dc2)) =
              if dc1 <= dc2 then (c1, dc1) else (c2, dc2)

            val (i, _) =
              Util.loop (bounds r) (~1, valOf Int.maxInt) (fn (md, r) =>
              Util.loop (bounds g) md (fn (md, g) =>
              Util.loop (bounds b) md (fn (md, b) =>
                let
                  val bucketIdx = pack (r, g, b)
                  val bStart = Seq.nth bucketOffsets bucketIdx
                  val bEnd = Seq.nth bucketOffsets (bucketIdx+1)
                in
                  Util.loop (bStart, bEnd) md (fn (md, i) =>
                    chooseMin (md, (i, dist (color, Seq.nth palette i))))
                end)))
          in
            Int.max (0, i)
          end

        fun remap {width, height, data} =
          AS.full (SeqBasis.tabulate 100 (0, Seq.length data)
                    (remapOne o Seq.nth data))
      in
        {colors = palette, remap = remap}
      end

    fun summarize cs sz ({data, width, height}: image) =
      let
        val n = Seq.length data
        fun sample i = Seq.nth data (Util.hash i mod n)
      in
        summarizeBySampling cs sz sample
      end

  end

  (* =================================================================== *)

  structure CodeTable:
  sig
    type t
    type idx = int
    type code = int

    val new: int -> t (* `new numColors` *)
    val clear: t -> unit
    val insert: (code * idx) -> t -> bool (* returns false when full *)
    val maybeLookup: (code * idx) -> t -> code option
  end =
  struct
    type idx = int
    type code = int

    exception Invalid

    type t =
      { nextCode: int ref
      , numColors: int
      , table: (idx * code) list array
      }

    fun new numColors =
      { nextCode = ref (Util.boundPow2 numColors + 2)
      , numColors = numColors
      , table = Array.array (4096, [])
      }

    fun clear {nextCode, numColors, table} =
      ( Util.for (0, Array.length table) (fn i => Array.update (table, i, []))
      ; nextCode := Util.boundPow2 numColors + 2
      )

    fun insert (code, idx) ({nextCode, numColors, table}: t) =
      if !nextCode = 4095 then
        false (* GIF limits the maximum code number to 4095 *)
      else
        ( Array.update (table, code, (idx, !nextCode) :: Array.sub (table, code))
        ; nextCode := !nextCode + 1
        ; true
        )

    fun maybeLookup (code, idx) ({table, numColors, ...}: t) =
      case List.find (fn (i, c) => i = idx) (Array.sub (table, code)) of
        SOME (_, c) => SOME c
      | NONE => NONE

  end

  (* =================================================================== *)

  structure DynArray =
  struct
    type 'a t = 'a array * int

    fun new () =
      (ForkJoin.alloc 100, 0)

    fun push x (data, nextIdx) =
      if nextIdx < Array.length data then
        (Array.update (data, nextIdx, x); (data, nextIdx+1))
      else
        let
          val data' = ForkJoin.alloc (2 * Array.length data)
        in
          Util.for (0, Array.length data) (fn i =>
            Array.update (data', i, Array.sub (data, i)));
          Array.update (data', nextIdx, x);
          (data', nextIdx+1)
        end

    fun toSeq (data, nextIdx) =
      AS.slice (data, 0, SOME nextIdx)
  end

(*
  structure DynArrayList =
  struct
    type 'a t = int * 'a array * ('a array list)

    val chunkSize = 256

    fun new () =
      (0, ForkJoin.alloc chunkSize, [])

    fun push x (nextIdx, chunk, tail) =
      ( Array.update (chunk, nextIdx, x)
      ; if nextIdx+1 < chunkSize then
          (nextIdx+1, chunk, tail)
        else
          (0, ForkJoin.alloc chunkSize, chunk :: tail)
      )

    fun toSeq (nextIdx, chunk, tail) =
      let
        val totalSize = nextIdx + (chunkSize * List.length tail)
        val result = ForkJoin.alloc totalSize

        fun writeChunks cs i =
          case cs of
            [] => ()
          | c :: cs' =>
              ( Array.copy {src = c, dst = result, di = i - Array.length c}
              ; writeChunks cs' (i - Array.length c)
              )
      in
        Util.for (0, nextIdx) (fn i =>
          Array.update (result, totalSize - nextIdx + i, Array.sub (chunk, i)));
        writeChunks tail (totalSize - nextIdx);
        AS.full result
      end
  end
*)

(*
  structure DynList =
  struct
    type 'a t = 'a list
    fun new () = []
    fun push x list = x :: list
    fun toSeq xs = Seq.rev (Seq.fromList xs)
  end
*)

  (* =================================================================== *)


  structure LZW =
  struct

    structure T = CodeTable
    structure DS = DynArray

    fun codeStream numColors colorIndices =
      let
        fun colorIdx i = Seq.nth colorIndices i

        val clear = Util.boundPow2 numColors
        val eoi = clear + 1

        val table = T.new numColors

        fun finish stream =
          DS.toSeq (DS.push eoi stream)

        (* The buffer is implicit, represented instead by currentCode
         * i is the next index into `colorIndices` *)
        fun loop stream currentCode i =
          if i >= Seq.length colorIndices then
            finish (DS.push currentCode stream)
          else
            case T.maybeLookup (currentCode, colorIdx i) table of
              SOME code => loop stream code (i+1)
            | NONE =>
                if T.insert (currentCode, colorIdx i) table then
                  loop (DS.push currentCode stream) (colorIdx i) (i+1)
                else
                  ( T.clear table
                  ; loop (DS.push clear (DS.push currentCode stream))
                      (colorIdx i) (i+1)
                  )
      in
        if Seq.length colorIndices = 0 then
          err "empty color index sequence"
        else
          loop (DS.push clear (DS.new ())) (colorIdx 0) 1
      end

    (* val codeStream = fn image => fn palette =>
      let
        val (result, tm) = Util.getTime (fn _ => codeStream image palette)
      in
        print ("computed codeStream in " ^ Time.fmt 4 tm ^ "s\n");
        result
      end *)

    fun packCodeStream numColors codes =
      let
        val n = Seq.length codes
        fun code i = Seq.nth codes i
        val clear = Util.boundPow2 numColors
        val eoi = clear+1
        val minCodeSize = ceilLog2 numColors
        val firstCodeWidth = minCodeSize+1

        (* Begin by calculating the bit width of each code. Since we know bit
         * widths are reset at each clear code, we can parallelize by splitting
         * the codestream into segments delimited by clear codes and processing
         * the segments in parallel.
         *
         * Within a segment, the width is increased every time we generated
         * a new code with power-of-two width. Every symbol in the code stream
         * corresponds to a newly generated code.
         *)

        val clears =
          AS.full (SeqBasis.filter 2000 (0, n) (fn i => i) (fn i => code i = clear))
        val numClears = Seq.length clears

        val widths = ForkJoin.alloc n
        val _ = Array.update (widths, 0, firstCodeWidth)
        val _ = ForkJoin.parfor 1 (0, numClears) (fn c =>
          let
            val i = 1 + Seq.nth clears c
            val j = if c = numClears-1 then n else 1 + Seq.nth clears (c+1)

            (* max code in table, up to (but not including) index k *)
            fun currentMaxCode k =
              k - i  (* num outputs since the table was cleared *)
              + eoi  (* the max code immediately after clearing the table *)
          in
            Util.loop (i, j) (firstCodeWidth, Util.pow2 firstCodeWidth)
            (fn ((currWidth, cwPow2), k) =>
              ( Array.update (widths, k, currWidth)
              ; if currentMaxCode (k+1) <> cwPow2 then
                  (currWidth, cwPow2)
                else
                  (currWidth+1, cwPow2*2)
              ));
            ()
          end)
        val widths = AS.full widths

        val totalBitWidth = Seq.reduce op+ 0 widths
        val packedSize = Util.ceilDiv totalBitWidth 8

        val packed = ForkJoin.alloc packedSize

        fun flushBuffer (oi, buffer, used) =
          if used < 8 then
            (oi, buffer, used)
          else
            ( Array.update (packed, oi, Word8.fromLarge buffer)
            ; flushBuffer (oi+1, LargeWord.>> (buffer, 0w8), used-8)
            )

        (* Input index range [ci,cj)
         * Output index range [oi, oj)
         * `buffer` is a partially filled byte that has not yet been written
         * to the packed. `used` (0 to 7) is how much of that byte is
         * used. *)
        fun pack (oi, oj) (ci, cj) (buffer: LargeWord.word) (used: int) =
          if ci >= cj then
            (if oi >= oj then
              ()
            else if oi = oj-1 then
              Array.update (packed, oi, Word8.fromLarge buffer)
            else
              err "cannot fill rest of packed region")
          else
            let
              val thisCode = code ci
              val thisWidth = Seq.nth widths ci
              val buffer' =
                LargeWord.orb (buffer,
                  LargeWord.<< (LargeWord.fromInt thisCode, Word.fromInt used))
              val used' = used + thisWidth
              val (oi', buffer'', used'') = flushBuffer (oi, buffer', used')
            in
              pack (oi', oj) (ci+1, cj) buffer'' used''
            end

        val _ = pack (0, packedSize) (0, n) 0w0 0
        val packed = AS.full packed
        val numBlocks = Util.ceilDiv packedSize 255
        val output = ForkJoin.alloc (packedSize + numBlocks + 1)
      in
        ForkJoin.parfor 10 (0, numBlocks) (fn i =>
          let
            val size = if i < numBlocks-1 then 255 else packedSize - 255*i
          in
            Array.update (output, 256*i, Word8.fromInt size);
            Util.for (0, size) (fn j =>
              Array.update (output, 256*i + 1 + j, Seq.nth packed (255*i + j)))
          end);

        Array.update (output, packedSize + numBlocks, 0w0);

        AS.full output
      end
  end

  (* ====================================================================== *)

  fun checkToWord16 thing x =
    if x >= 0 andalso x <= 65535 then
      Word16.fromInt x
    else
      err (thing ^ " must be non-negative and less than 2^16");

  fun packScreenDescriptorByte
        { colorTableFlag: bool
        , colorResolution: int
        , sortFlag: bool
        , colorTableSize: int
        } =
    let
      open Word8
      infix 2 << orb andb
    in
      ((if colorTableFlag then 0w1 else 0w0) << 0w7)
      orb
      ((fromInt colorResolution andb 0wx7) << 0w4)
      orb
      ((if sortFlag then 0w1 else 0w0) << 0w3)
      orb
      (fromInt colorTableSize andb 0wx7)
    end

  fun writeMany path delay palette {width, height, numImages, getImage} =
    if numImages <= 0 then
      err "Must be at least one image"
    else
    let
      val width16 = checkToWord16 "width" width
      val height16 = checkToWord16 "height" height

      val numberOfColors = Seq.length (#colors palette)

      val _ =
        if numberOfColors <= 256 then ()
        else err "Must have at most 256 colors in the palette"

      val (imageData, tm) = Util.getTime (fn _ =>
        AS.full (SeqBasis.tabulate 1 (0, numImages) (fn i =>
          let
            val img = getImage i
          in
            if Seq.length img <> height * width then
              err "Not all images are the right dimensions"
            else
              LZW.packCodeStream numberOfColors
                (LZW.codeStream numberOfColors img)
          end)))

      (* val _ = print ("compressed image data in " ^ Time.fmt 4 tm ^ "s\n") *)

      val file = BinIO.openOut path
      val w8 = ExtraBinIO.w8 file
      val w32b = ExtraBinIO.w32b file
      val w32l = ExtraBinIO.w32l file
      val w16l = ExtraBinIO.w16l file
      val wrgb = ExtraBinIO.wrgb file
    in
      (* ==========================
       * "GIF89a" header: 6 bytes
       *)

      List.app (w8 o Word8.fromInt) [0x47, 0x49, 0x46, 0x38, 0x39, 0x61];

      (* ===================================
       * logical screen descriptor: 7 bytes
       *)

      w16l width16;
      w16l height16;

      w8 (packScreenDescriptorByte
        { colorTableFlag  = true
        , colorResolution = 1
        , sortFlag        = false
        , colorTableSize  = (ceilLog2 numberOfColors) - 1
        });

      w8 0w0; (* background color index. just use 0 for now. *)

      w8 0w0; (* pixel aspect ratio ?? *)

      (* ===================================
       * global color table
       *)

      Util.for (0, numberOfColors) (fn i =>
        wrgb (Seq.nth (#colors palette) i));

      Util.for (numberOfColors, Util.boundPow2 numberOfColors) (fn i =>
        wrgb Color.black);

      (* ==================================
       * application extension, for looping
       * OPTIONAL. skip it if there is only one image.
       *)

      if numImages = 1 then () else
      List.app (w8 o Word8.fromInt)
      [ 0x21, 0xFF, 0x0B, 0x4E, 0x45, 0x54, 0x53, 0x43, 0x41, 0x50, 0x45, 0x32
      , 0x2E, 0x30, 0x03, 0x01, 0x00, 0x00, 0x00
      ];

      (* ==================================
       * IMAGE DATA
       *)

      Util.for (0, numImages) (fn i =>
        let
          val bytes = Seq.nth imageData i
        in
          (* ==========================
           * graphics control extension.
           * OPTIONAL. only needed if
           * doing animation.
           *)

          if numImages = 1 then () else
          ( List.app (w8 o Word8.fromInt) [ 0x21, 0xF9, 0x04, 0x04 ]
          ; w16l (Word16.fromInt delay)
          ; w8 0w0
          ; w8 0w0
          );

          (* ==========================
           * image descriptor
           *)

          w8 0wx2C; (* image separator *)

          w16l 0w0;  (* image left *)
          w16l 0w0;  (* image top *)

          w16l width16;  (* image width *)
          w16l height16; (* image height *)

          w8 0w0;   (* packed local color table descriptor (NONE FOR NOW) *)

          (* ===========================
           * compressed image data
           *)

          w8 (Word8.fromInt (ceilLog2 numberOfColors));
          Util.for (0, Seq.length bytes) (fn i =>
            w8 (Seq.nth bytes i))
        end);

      (* ================================
       * trailer
       *)

      w8 0wx3B;

      BinIO.closeOut file
    end

  fun write path img =
    let
      val palette = Palette.summarize [] 128 img
      val img' = #remap palette img
    in
      writeMany path 0 palette
        { width = #width img
        , height = #height img
        , numImages = 1
        , getImage = (fn _ => img')
        }
    end
end
