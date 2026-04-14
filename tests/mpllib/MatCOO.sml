(* MatCOO(I, R):
 *   - indices of type I.int
 *   - values of type R.real
 *
 * For example, the following defines matrices where the row and column indices
 * will be arrays of C type int32_t*, and values of C type float*
 *
 *   structure M = MatCOO(structure I = Int32
 *                        structure R = Real32)
 *
 * We can also use int64_t and doubles (or any other desired combination):
 *
 *   structure M = MatCOO(structure I = Int64
 *                        structure R = Real64)
 *)

functor MatCOO
  (structure I: INTEGER
   structure R:
   sig
     include REAL
     structure W: WORD
     val castFromWord: W.word -> real
     val castToWord: real -> W.word
   end) =
struct
  structure I = I
  structure R = R

  (* SoA format for storing every nonzero value = mat[row,column], i.e.:
   *   (row, column, value) = (row_indices[i], col_indices[i], values[i])
   *
   * so, number of nonzeros (nnz)
   * = length(row_indices)
   * = length(col_indices)
   * = length(values)
   *
   * we assume row_indices is sorted
   *)
  datatype mat =
    Mat of
      { width: int
      , height: int
      , row_indices: I.int Seq.t
      , col_indices: I.int Seq.t
      , values: R.real Seq.t
      }

  type t = mat

  fun width (Mat m) = #width m
  fun height (Mat m) = #height m

  fun nnz (Mat m) =
    Seq.length (#row_indices m)

  fun row_lo (mat as Mat m) =
    if nnz mat = 0 then I.fromInt 0 else Seq.first (#row_indices m)

  fun row_hi (mat as Mat m) =
    if nnz mat = 0 then I.fromInt 0
    else I.+ (I.fromInt 1, Seq.last (#row_indices m))

  fun row_spread mat =
    I.toInt (row_hi mat) - I.toInt (row_lo mat)

  fun split_seq s k =
    (Seq.take s k, Seq.drop s k)

  fun undo_split_seq s1 s2 =
    let
      val (a1, i1, n1) = ArraySlice.base s1
      val (a2, i2, n2) = ArraySlice.base s2
    in
      if MLton.eq (a1, a2) andalso i1 + n1 = i2 then
        Seq.subseq (ArraySlice.full a1) (i1, n1 + n2)
      else
        raise Fail
          ("undo_split_seq: arguments are not adjacent: " ^ Int.toString i1
           ^ " " ^ Int.toString n1 ^ " " ^ Int.toString i2 ^ " "
           ^ Int.toString n2)
    end


  fun split_nnz k (mat as Mat m) =
    let
      val (r1, r2) = split_seq (#row_indices m) k
      val (c1, c2) = split_seq (#col_indices m) k
      val (v1, v2) = split_seq (#values m) k

      val m1 = Mat
        { width = #width m
        , height = #height m
        , row_indices = r1
        , col_indices = c1
        , values = v1
        }

      val m2 = Mat
        { width = #width m
        , height = #height m
        , row_indices = r2
        , col_indices = c2
        , values = v2
        }
    in
      (m1, m2)
    end


  (* split m -> (m1, m2)
   * where m = m1 + m2
   * and nnz(m1) ~= frac * nnz(m)
   * and nnz(m2) ~= (1-frac) * nnz(m)
   *)
  fun split frac mat =
    let
      val half = Real.floor (frac * Real.fromInt (nnz mat))
      val half =
        if nnz mat <= 1 then half
        else if half = 0 then half + 1
        else if half = nnz mat then half - 1
        else half
    in
      split_nnz half mat
    end


  (* might fail if the matrices were not created by a split *)
  fun undo_split mat1 mat2 =
    if nnz mat1 = 0 then
      mat2
    else if nnz mat2 = 0 then
      mat1
    else
      let
        val Mat m1 = mat1
        val Mat m2 = mat2
      in
        if #width m1 <> #width m2 orelse #height m1 <> #height m2 then
          raise Fail "undo_split: dimension mismatch"
        else
          Mat
            { width = #width m1
            , height = #height m1
            , row_indices = undo_split_seq (#row_indices m1) (#row_indices m2)
            , col_indices = undo_split_seq (#col_indices m1) (#col_indices m2)
            , values = undo_split_seq (#values m1) (#values m2)
            }
      end


  fun dump_info msg (Mat m) =
    let
      val info = String.concatWith " "
        [ msg
        , Seq.toString I.toString (#row_indices m)
        , Seq.toString I.toString (#col_indices m)
        , Seq.toString (R.fmt (StringCvt.FIX (SOME 1))) (#values m)
        ]
    in
      print (info ^ "\n")
    end


  fun upd (a, i, x) =
    ( (*print ("upd " ^ Int.toString i ^ "\n");*)Array.update (a, i, x))


  (* =======================================================================
   * write_mxv: serial and parallel versions
   *
   * The first and last row of the input `mat` might overlap with other
   * parallel tasks, so these need to be returned separately and combined.
   *
   * All middle rows are "owned" by the call to write_mxv
   *)

  datatype write_mxv_result =
    SingleRowValue of R.real
  | FirstLastRowValue of R.real * R.real


  (* requires `row_lo mat < row_hi mat`, i.e., at least one row *)
  fun write_mxv_serial mat vec output : write_mxv_result =
    let
      val Mat {row_indices, col_indices, values, ...} = mat
      val n = nnz mat
    in
      if row_lo mat = I.- (row_hi mat, I.fromInt 1) then
        (* no writes to output; only a single result row value *)
        let
          (* val _ = dump_info "write_mxv_serial (single row)" mat *)
          val result = Util.loop (0, n) (R.fromInt 0) (fn (acc, i) =>
            R.+ (acc, R.*
              (Seq.nth vec (I.toInt (Seq.nth col_indices i)), Seq.nth values i)))
        in
          SingleRowValue result
        end
      else
        let
          (* val _ = dump_info "write_mxv_serial (multi rows)" mat *)
          fun single_row_loop r (i, acc) =
            if i >= n orelse Seq.nth row_indices i <> r then
              (i, acc)
            else
              let
                val acc' = R.+ (acc, R.*
                  ( Seq.nth vec (I.toInt (Seq.nth col_indices i))
                  , Seq.nth values i
                  ))
              in
                single_row_loop r (i + 1, acc')
              end

          val last_row = I.- (row_hi mat, I.fromInt 1)

          fun advance_to_next_nonempty_row i' r =
            let
              val next_r = Seq.nth row_indices i'
            in
              Util.for (1 + I.toInt r, I.toInt next_r) (fn rr =>
                upd (output, rr, R.fromInt 0));
              next_r
            end

          fun middle_loop r i =
            if r = last_row then
              i
            else
              let
                val (i', row_value) = single_row_loop r (i, R.fromInt 0)
              in
                upd (output, I.toInt r, row_value);
                middle_loop (advance_to_next_nonempty_row i' r) i'
              end

          val (i, first_row_value) =
            single_row_loop (row_lo mat) (0, R.fromInt 0)
          val i = middle_loop (advance_to_next_nonempty_row i (row_lo mat)) i
          val (_, last_row_value) = single_row_loop last_row (i, R.fromInt 0)
        in
          FirstLastRowValue (first_row_value, last_row_value)
        end
    end

  val nnzGrain = 5000


  fun write_mxv_combine_results (m1, m2) (result1, result2) output =
    if I.- (row_hi m1, I.fromInt 1) = row_lo m2 then
      case (result1, result2) of
        (SingleRowValue r1, SingleRowValue r2) => SingleRowValue (R.+ (r1, r2))
      | (SingleRowValue r1, FirstLastRowValue (f2, l2)) =>
          FirstLastRowValue (R.+ (r1, f2), l2)
      | (FirstLastRowValue (f1, l1), SingleRowValue r2) =>
          FirstLastRowValue (f1, R.+ (l1, r2))
      | (FirstLastRowValue (f1, l1), FirstLastRowValue (f2, l2)) =>
          (* overlap *)
          ( (*print "fill in middle overlap\n"
            ;*) upd (output, I.toInt (row_lo m2), R.+ (l1, f2))
          ; FirstLastRowValue (f1, l2)
          )
    else
      let
        fun finish_l1 v =
          upd (output, I.toInt (row_hi m1) - 1, v)
        fun finish_f2 v =
          upd (output, I.toInt (row_lo m2), v)
        fun fill_middle () =
          ForkJoin.parfor 5000 (I.toInt (row_hi m1), I.toInt (row_lo m2))
            (fn r => upd (output, r, R.fromInt 0))
      in
        (* print "fill in middle, no overlap\n"; *)
        case (result1, result2) of
          (SingleRowValue r1, SingleRowValue r2) =>
            (fill_middle (); FirstLastRowValue (r1, r2))

        | (SingleRowValue r1, FirstLastRowValue (f2, l2)) =>
            (fill_middle (); finish_f2 f2; FirstLastRowValue (r1, l2))

        | (FirstLastRowValue (f1, l1), SingleRowValue r2) =>
            (finish_l1 l1; fill_middle (); FirstLastRowValue (f1, r2))

        | (FirstLastRowValue (f1, l1), FirstLastRowValue (f2, l2)) =>
            ( finish_l1 l1
            ; fill_middle ()
            ; finish_f2 f2
            ; FirstLastRowValue (f1, l2)
            )
      end


  (* requires `row_lo mat < row_hi mat`, i.e., at least one row *)
  fun write_mxv mat vec output : write_mxv_result =
    if nnz mat <= nnzGrain then
      write_mxv_serial mat vec output
    else
      let
        val (m1, m2) = split 0.5 mat
        val (result1, result2) =
          ForkJoin.par (fn _ => write_mxv m1 vec output, fn _ =>
            write_mxv m2 vec output)

      in
        write_mxv_combine_results (m1, m2) (result1, result2) output
      end


  fun mxv (mat: mat) (vec: R.real Seq.t) =
    if nnz mat = 0 then
      Seq.tabulate (fn _ => R.fromInt 0) (Seq.length vec)
    else
      let
        val output: R.real array = ForkJoin.alloc (Seq.length vec)
        val result = write_mxv mat vec output
      in
        (* print "top level: fill in front\n"; *)
        ForkJoin.parfor 5000 (0, I.toInt (row_lo mat)) (fn r =>
          upd (output, r, R.fromInt 0));
        (* print "top-level: fill in middle\n"; *)
        case result of
          SingleRowValue r => upd (output, I.toInt (row_lo mat), r)
        | FirstLastRowValue (f, l) =>
            ( upd (output, I.toInt (row_lo mat), f)
            ; upd (output, I.toInt (row_hi mat) - 1, l)
            );
        (* print "top-level: fill in back\n"; *)
        ForkJoin.parfor 5000 (I.toInt (row_hi mat), Seq.length vec) (fn r =>
          upd (output, r, R.fromInt 0));
        ArraySlice.full output
      end

  (* ======================================================================= *)
  (* ======================================================================= *)
  (* =======================================================================
   * to/from file
   *)

  structure DS = DelayedSeq

  fun fromMatrixMarketFile path chars =
    let
      val lines = ParseFile.tokens (fn c => c = #"\n") chars
      val numLines = DS.length lines
      fun line i : char DS.t = DS.nth lines i
      fun lineStr i : string =
        let val ln = line i
        in CharVector.tabulate (DS.length ln, DS.nth ln)
        end

      fun lineIsComment i =
        let val ln = line i
        in DS.length ln > 0 andalso DS.nth ln 0 = #"%"
        end

      val _ =
        if
          numLines > 0
          andalso
          ParseFile.eqStr "%%MatrixMarket matrix coordinate real general"
            (line 0)
        then ()
        else raise Fail ("MatCOO.fromFile: not sure how to parse file: " ^ path)

      val firstNonCommentLineNum =
        case FindFirst.findFirst 1000 (1, numLines) (not o lineIsComment) of
          SOME i => i
        | NONE => raise Fail ("MatCOO.fromFile: missing contents?")

      fun fail () =
        raise Fail
          ("MatCOO.fromFile: error parsing line "
           ^ Int.toString (1 + firstNonCommentLineNum)
           ^ ": expected <num-rows> <num-cols> <num-values>")
      val (numRows, numCols, numValues) =
        let
          val lineNum = firstNonCommentLineNum
          val lnChars = DS.toArraySeq (line lineNum)
          val toks = ParseFile.tokens Char.isSpace lnChars
          val nr = valOf (ParseFile.parseInt (DS.nth toks 0))
          val nc = valOf (ParseFile.parseInt (DS.nth toks 1))
          val nv = valOf (ParseFile.parseInt (DS.nth toks 2))
        in
          (nr, nc, nv)
        end
        handle _ => fail ()

      val _ = print ("num rows    " ^ Int.toString numRows ^ "\n")
      val _ = print ("num cols    " ^ Int.toString numCols ^ "\n")
      val _ = print ("num nonzero " ^ Int.toString numValues ^ "\n")
      val _ = print ("parsing elements (may take a while...)\n")

      val row_indices = ForkJoin.alloc numValues
      val col_indices = ForkJoin.alloc numValues
      val values = ForkJoin.alloc numValues

      fun fail lineNum =
        raise Fail
          ("MatCOO.fromFile: error parsing line " ^ Int.toString (1 + lineNum)
           ^ ": expected <row> <col> <value>")

      (* TODO: this is very slow *)
      fun parseValue i =
        let
          val lineNum = firstNonCommentLineNum + 1 + i
          val lnChars = DS.toArraySeq (line lineNum)
          val toks = ParseFile.tokens Char.isSpace lnChars
          val r = I.fromInt (valOf (ParseFile.parseInt (DS.nth toks 0)))
          val c = I.fromInt (valOf (ParseFile.parseInt (DS.nth toks 1)))
          val v = R.fromLarge IEEEReal.TO_NEAREST (valOf (ParseFile.parseReal
            (DS.nth toks 2)))

        (* val ln = line lineNum
        val chars = CharVector.tabulate (DS.length ln, DS.nth ln)
        val toks = String.tokens Char.isSpace chars
        val r = I.fromInt (valOf (Int.fromString (List.nth (toks, 0))))
        val c = I.fromInt (valOf (Int.fromString (List.nth (toks, 1))))
        val v = R.fromLarge IEEEReal.TO_NEAREST (valOf (Real.fromString
          (List.nth (toks, 2)))) *)
        in
          (* if i mod 500000 = 0 then
            print ("finished row " ^ Int.toString i ^ "\n")
          else
            (); *)

          (* coordinates are stored in .mtx files using 1-indexing, but we
           * want 0-indexing
           *)
          Array.update (row_indices, i, I.- (r, I.fromInt 1));
          Array.update (col_indices, i, I.- (c, I.fromInt 1));
          Array.update (values, i, v)
        end
        handle _ => fail (firstNonCommentLineNum + 1 + i)

      val _ = ForkJoin.parfor 1000 (0, numValues) parseValue
      val _ = print ("finished parsing elements\n")
      val _ = print ("formatting...\n")

      val getSorted =
        let
          val idx = Seq.tabulate (fn i => i) numValues
        in
          StableSort.sortInPlace
            (fn (i, j) =>
               I.compare
                 (Array.sub (row_indices, i), Array.sub (row_indices, j))) idx;
          fn i => Seq.nth idx i
        end

      val row_indices =
        Seq.tabulate (fn i => Array.sub (row_indices, getSorted i)) numValues
      val col_indices =
        Seq.tabulate (fn i => Array.sub (col_indices, getSorted i)) numValues
      val values =
        Seq.tabulate (fn i => Array.sub (values, getSorted i)) numValues

      val _ = print ("done parsing " ^ path ^ "\n")
    in
      Mat
        { width = numCols
        , height = numRows
        , row_indices = row_indices
        , col_indices = col_indices
        , values = values
        }
    end


  (* MatrixCoordinateRealBin\n
   * [8 bits unsigned: real value precision, either 32 or 64]
   * [64 bits unsigned: number of rows]
   * [64 bits unsigned: number of columns]
   * [64 bits unsigned: number of elements]
   * [element]
   * [element]
   * ...
   *
   * each element is as follows, where X is the real value precision (32 or 64)
   * [64 bits unsigned: row index][64 bits unsigned: col index][X bits: value]
   *)
  fun writeBinFile mat path =
    let
      val file = TextIO.openOut path
      val _ = TextIO.output (file, "MatrixCoordinateRealBin\n")
      val _ = TextIO.closeOut file

      val file = BinIO.openAppend path

      fun w8 (w: Word8.word) = BinIO.output1 (file, w)

      fun w32 (w: Word64.word) =
        let
          open Word64
          infix 2 >> andb
        in
          w8 (Word8.fromLarge (w >> 0w24));
          w8 (Word8.fromLarge (w >> 0w16));
          w8 (Word8.fromLarge (w >> 0w8));
          w8 (Word8.fromLarge w)
        end

      fun w64 (w: Word64.word) =
        let
          open Word64
          infix 2 >> andb
        in
          (* this will only work if Word64 = LargeWord, which is good. *)
          w8 (Word8.fromLarge (w >> 0w56));
          w8 (Word8.fromLarge (w >> 0w48));
          w8 (Word8.fromLarge (w >> 0w40));
          w8 (Word8.fromLarge (w >> 0w32));
          w8 (Word8.fromLarge (w >> 0w24));
          w8 (Word8.fromLarge (w >> 0w16));
          w8 (Word8.fromLarge (w >> 0w8));
          w8 (Word8.fromLarge w)
        end

      fun wr64 (r: R.real) =
        w64 (R.W.toLarge (R.castToWord r))
      fun wr32 (r: R.real) =
        w32 (R.W.toLarge (R.castToWord r))

      val (wr, rsize) =
        case R.W.wordSize of
          32 => (wr32, 0w32)
        | 64 => (wr64, 0w64)
        | _ =>
            raise Fail
              "MatCOO.writeBinFile: only 32-bit and 64-bit reals supported"
    in
      w8 rsize;
      w64 (Word64.fromInt (height mat));
      w64 (Word64.fromInt (width mat));
      w64 (Word64.fromInt (nnz mat));
      Util.for (0, nnz mat) (fn i =>
        let
          val Mat {row_indices, col_indices, values, ...} = mat
          val r = Seq.nth row_indices i
          val c = Seq.nth col_indices i
          val v = Seq.nth values i
        in
          w64 (Word64.fromInt (I.toInt r));
          w64 (Word64.fromInt (I.toInt c));
          wr v
        end);
      BinIO.closeOut file
    end


  fun fromBinFile path bytes =
    let
      val header = "MatrixCoordinateRealBin\n"
      val header' =
        if Seq.length bytes < String.size header then
          raise Fail ("MatCOO.fromBinFile: missing or incomplete header")
        else
          CharVector.tabulate (String.size header, fn i =>
            Char.chr (Word8.toInt (Seq.nth bytes i)))
      val _ =
        if header = header' then
          ()
        else
          raise Fail
            ("MatCOO.fromBinFile: expected MatrixCoordinateRealBin header")

      val bytes = Seq.drop bytes (String.size header)

      fun r64 off =
        let
          infix 2 << orb
          val op<< = Word64.<<
          val op orb = Word64.orb

          val w = Word8.toLarge (Seq.nth bytes off)
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 1)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 2)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 3)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 4)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 5)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 6)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 7)))
        in
          w
        end

      fun r32 off =
        let
          infix 2 << orb
          val op<< = Word64.<<
          val op orb = Word64.orb

          val w = Word8.toLarge (Seq.nth bytes off)
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 1)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 2)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off + 3)))
        in
          w
        end

      fun r8 off = Seq.nth bytes off
      fun rr32 off =
        R.castFromWord (R.W.fromLarge (r32 off))
      fun rr64 off =
        R.castFromWord (R.W.fromLarge (r64 off))

      (* ====================================================================
       * parse binary contents
       *)

      val rsize = Word8.toInt (r8 0)

      fun rsizeFail () =
        raise Fail
          ("MatCOO.fromBinFile: found " ^ Int.toString rsize
           ^ "-bit reals, but expected " ^ Int.toString R.W.wordSize ^ "-bit")

      val (rr, rbytes) =
        if rsize = R.W.wordSize then
          case rsize of
            32 => (rr32, 4)
          | 64 => (rr64, 8)
          | _ => rsizeFail ()
        else
          rsizeFail ()

      val elemSize = 8 + 8 + rbytes
      val elemStartOff = 1 + 8 + 8 + 8

      val height = Word64.toInt (r64 (1 + 0))
      val width = Word64.toInt (r64 (1 + 8))
      val numValues = Word64.toInt (r64 (1 + 8 + 8))

      val row_indices = ForkJoin.alloc numValues
      val col_indices = ForkJoin.alloc numValues
      val values = ForkJoin.alloc numValues
    in
      ForkJoin.parfor 5000 (0, numValues) (fn i =>
        let
          val off = elemStartOff + i * elemSize
          val r = I.fromInt (Word64.toInt (r64 off))
          val c = I.fromInt (Word64.toInt (r64 (off + 8)))
          val v = rr (off + 8 + 8)
        in
          Array.update (row_indices, i, r);
          Array.update (col_indices, i, c);
          Array.update (values, i, v)
        end);

      Mat
        { width = width
        , height = height
        , row_indices = ArraySlice.full row_indices
        , col_indices = ArraySlice.full col_indices
        , values = ArraySlice.full values
        }
    end


  fun fromFile path =
    let
      val file = TextIO.openIn path
      val _ = print ("loading " ^ path ^ "\n")

      val h1 = "%%MatrixMarket"
      val h2 = "MatrixCoordinateRealBin\n"

      val actualHeader = TextIO.inputN
        (file, Int.max (String.size h1, String.size h2))
    in
      TextIO.closeIn file;

      if String.isPrefix h1 actualHeader then
        fromMatrixMarketFile path (ReadFile.contentsSeq path)
      else if String.isPrefix h2 actualHeader then
        fromBinFile path (ReadFile.contentsBinSeq path)
      else
        raise Fail ("unknown header " ^ actualHeader)
    end

end
