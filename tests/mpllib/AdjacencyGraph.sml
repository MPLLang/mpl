functor AdjacencyGraph (Vertex: INTEGER) =
struct

  structure A = Array
  structure AS = ArraySlice

  structure Vertex =
  struct
    type t = Vertex.int
    open Vertex
    val maxVal = toInt (valOf maxInt)
  end

  structure VertexSubset =
  struct
    datatype h = SPARSE of Vertex.t Seq.t | DENSE of int Seq.t
    type t = h * int
    exception BadRep

    fun empty thresh = (SPARSE (Seq.empty()), thresh)

    fun size (vs, thresh) =
      case vs of
        SPARSE s => Seq.length s
      | DENSE s => Seq.reduce op+ 0 s

    fun plugOnes s positions =
      (Seq.foreach positions (fn (i, v) => AS.update (s, Vertex.toInt v, 1)))

    fun append (vs, threshold) s n =
      case vs of
        SPARSE ss =>
          if (Seq.length ss) + (Seq.length s) > threshold then
            let
              val dense_rep = Seq.tabulate (fn x => 0) n
              val _ = plugOnes dense_rep ss
              val _ = plugOnes dense_rep s
            in
              (DENSE (dense_rep), threshold)
            end
          else (SPARSE(Seq.append (ss, s)), threshold)
      | DENSE ss => (plugOnes ss s; (DENSE ss, threshold))

    fun sparse_to_dense vs n =
      case vs of
        SPARSE s =>
          let
            val dense_rep = Seq.tabulate (fn x => 0) n
            val _ = Seq.foreach s (fn (i, v) => AS.update (dense_rep, Vertex.toInt v, 1))
          in
            DENSE (dense_rep)
          end
      | DENSE _ => raise BadRep

    fun dense_to_sparse vs =
      case vs of
        SPARSE _ => raise BadRep
      | DENSE s =>
          let
            val (offsets, total) = Seq.scan op+ 0 s
            val sparse = ForkJoin.alloc total
            val _ = Seq.foreach s (fn (i, v) =>
              if (v=1) then A.update (sparse, Seq.nth offsets i, Vertex.fromInt i)
              else if (v = 0) then ()
              else raise BadRep
              )
          in
            SPARSE (AS.full sparse)
          end

    fun from_sparse_rep s threshold n =
      if (Seq.length s) < threshold then (SPARSE (s), threshold)
      else (sparse_to_dense (SPARSE (s)) n, threshold)

    fun from_dense_rep s countopt threshold =
      let
        val count =
          case countopt of
            SOME x => x
          | NONE => Seq.reduce op+ 0 s
        val d = DENSE(s)
      in
        if count < threshold then (dense_to_sparse(d), threshold)
        else (d, threshold)
      end
  end

  type vertex = Vertex.t
  fun vertexNth s v = Seq.nth s (Vertex.toInt v)
  fun vToWord v = Word64.fromInt (Vertex.toInt v)

  (* offsets, degrees, compact neighbors *)
  type graph = (int Seq.t) * (int Seq.t) * (vertex Seq.t)

  fun degree G v =
    let val (offsets, degrees, _) = G
    in (vertexNth degrees v)
    end

  fun neighbors G v =
    let
      val (offsets, _, nbrs) = G
    in
      Seq.subseq nbrs (vertexNth offsets v, degree G v)
    end

  fun numVertices G =
    let val (_, degrees, _) = G
    in Seq.length degrees
    end

  fun numEdges G =
    let val (_, _, nbrs) = G
    in Seq.length nbrs
    end

  fun computeDegrees (N, M, offsets) =
    AS.full (SeqBasis.tabulate 10000 (0, N) (fn i =>
      let
        val off = Seq.nth offsets i
        val nextOff = if i+1 < N then Seq.nth offsets (i+1) else M
        val deg = nextOff - off
      in
        if deg < 0 then
          raise Fail ("AdjacencyGraph.computeDegrees: vertex " ^ Int.toString i
                      ^ " has negative degree")
        else
          deg
      end))

  fun parse chars =
    let
      fun isNewline i = (Seq.nth chars i = #"\n")

      (* Computing newline positions takes up about half the time of parsing...
       * Can we do this faster? *)
      val nlPos =
        AS.full (SeqBasis.filter 10000 (0, Seq.length chars) (fn i => i) isNewline)
      val numLines = Seq.length nlPos + 1
      fun lineStart i =
        if i = 0 then 0 else 1 + Seq.nth nlPos (i-1)
      fun lineEnd i =
        if i = Seq.length nlPos then Seq.length chars else Seq.nth nlPos i
      fun line i = Seq.subseq chars (lineStart i, lineEnd i - lineStart i)

      val _ =
        if numLines >= 3 then ()
        else raise Fail ("AdjacencyGraph: missing or incomplete header")

      val _ =
        if Parse.parseString (line 0) = "AdjacencyGraph" then ()
        else raise Fail ("expected AdjacencyGraph header")

      fun tryParse thing lineNum =
        let
          fun whoops () =
            raise Fail ("AdjacencyGraph: line "
                        ^ Int.toString (lineNum+1)
                        ^ ": error while parsing " ^ thing)
        in
          case (Parse.parseInt (line lineNum) handle _ => whoops ()) of
            SOME x => if x >= 0 then x else whoops ()
          | NONE => whoops ()
        end

      val numVertices = tryParse "num vertices" 1
      val numEdges = tryParse "num edges" 2

      val _ =
        if numLines >= numVertices + numEdges + 3 then ()
        else raise Fail ("AdjacencyGraph: not enough offsets and/or edges to parse")

      val offsets = AS.full (SeqBasis.tabulate 1000 (0, numVertices)
        (fn i => tryParse "edge offset" (3+i)))

      val neighbors = AS.full (SeqBasis.tabulate 1000 (0, numEdges)
        (fn i => Vertex.fromInt (tryParse "neighbor" (3+numVertices+i))))
    in
      (offsets, computeDegrees (numVertices, numEdges, offsets), neighbors)
    end

  fun writeAsBinaryFormat g filename =
    let
      val (offsets, _, nbrs) = g

      val file = TextIO.openOut filename
      val _ = TextIO.output (file, "AdjacencyGraphBin\n")
      val _ = TextIO.closeOut file

      val file = BinIO.openAppend filename
      fun w8 (w: Word8.word) = BinIO.output1 (file, w)
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
      fun wi (x: int) = w64 (Word64.fromInt x)
      fun wv (v: vertex) = w64 (vToWord v)
    in
      wi (numVertices g);
      wi (numEdges g);
      Util.for (0, numVertices g) (fn i => wi (Seq.nth offsets i));
      Util.for (0, numEdges g) (fn i => wv (Seq.nth nbrs i));
      BinIO.closeOut file
    end

  fun parseBin bytes =
    let
      val header = "AdjacencyGraphBin\n"
      val header' =
        if Seq.length bytes < String.size header then
          raise Fail ("AdjacencyGraphBin: missing or incomplete header")
        else
          CharVector.tabulate (String.size header, fn i =>
            Char.chr (Word8.toInt (Seq.nth bytes i)))
      val _ =
        if header = header' then ()
        else raise Fail ("expected AdjacencyGraphBin header")

      val bytes = Seq.drop bytes (String.size header)

      (* this will only work if Word64 = LargeWord, which is good. *)
      fun r64 i =
        let
          infix 2 << orb
          val op<< = Word64.<<
          val op orb = Word64.orb

          val off = i*8
          val w = Word8.toLarge (Seq.nth bytes off)
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off+1)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off+2)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off+3)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off+4)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off+5)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off+6)))
          val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (off+7)))
        in
          w
        end

      fun ri i = Word64.toInt (r64 i)
      fun rv i = Vertex.fromInt (ri i)

      val numVertices = ri 0
      val numEdges = ri 1

      val offsets =
        AS.full (SeqBasis.tabulate 10000 (0, numVertices) (fn i => ri (i+2)))
      val nbrs =
        AS.full (SeqBasis.tabulate 10000 (0, numEdges) (fn i => rv (i+2+numVertices)))
    in
      (offsets, computeDegrees (numVertices, numEdges, offsets), nbrs)
    end

  fun parseFile path =
    let
      val file = TextIO.openIn path

      val h1 = "AdjacencyGraph\n"
      val h2 = "AdjacencyGraphBin\n"

      val actualHeader =
        TextIO.inputN (file, Int.max (String.size h1, String.size h2))
    in
      TextIO.closeIn file;

      if String.isPrefix h1 actualHeader then
        let
          val (c, tm) = Util.getTime (fn _ => ReadFile.contentsSeq path)
          val _ = print ("read file in " ^ Time.fmt 4 tm ^ "s\n")
          val (graph, tm) = Util.getTime (fn _ => parse c)
          val _ = print ("parsed graph in " ^ Time.fmt 4 tm ^ "s\n")
        in
          graph
        end
      else if String.isPrefix h2 actualHeader then
        let
          val (c, tm) = Util.getTime (fn _ => ReadFile.contentsBinSeq path)
          val _ = print ("read file in " ^ Time.fmt 4 tm ^ "s\n")
          val (graph, tm) = Util.getTime (fn _ => parseBin c)
          val _ = print ("parsed graph in " ^ Time.fmt 4 tm ^ "s\n")
        in
          graph
        end
      else
        raise Fail ("unknown header " ^ actualHeader)
    end

  (* Useful as a sanity check for symmetrized graphs --
   * (every symmetrized graph has edge parity 0, but not all graphs with
   * edge parity 0 are symmetrized!) *)
  fun parityCheck g =
    let
      val (offsets, _, _) = g
      val n = numVertices g

      fun canonical (u, v) =
        if Vertex.< (u, v) then (u, v) else (v, u)
      fun xorEdges ((u1, v1), (u2, v2)) =
        (Word64.xorb (u1, u2), Word64.xorb (v1, v2))
      fun packEdge (u, v) = (vToWord u, vToWord v)

      val (p1, p2) = SeqBasis.reduce 100 xorEdges (0w0, 0w0) (0, n) (fn i =>
        let
          val u = Vertex.fromInt i
          val offset = Seq.nth offsets i
        in
          SeqBasis.reduce 1000 xorEdges (0w0, 0w0) (0, degree g u) (fn j =>
            packEdge (canonical (u, Seq.nth (neighbors g u) j)))
        end)

    in
      p1 = 0w0 andalso p2 = 0w0
    end

  fun fromSortedEdges sorted =
    let
      fun edgeInts (u, v) = (Vertex.toInt u, Vertex.toInt v)
      val m = Seq.length sorted
      val n =
        1 + SeqBasis.reduce 10000 Int.max ~1 (0, m)
            (Int.max o edgeInts o Seq.nth sorted)

      fun k i = Vertex.toInt (#1 (Seq.nth sorted i))

      val ends = Seq.tabulate (fn i => if i = n then m else 0) (n+1)
      val _ = ForkJoin.parfor 10000 (0, m) (fn i =>
        if i = m-1 then
          AS.update (ends, k i, m)
        else if k i <> k (i+1) then
          AS.update (ends, k i, i+1)
        else ())
      val (offsets, _) = Seq.scan Int.max 0 ends

      fun off i = Seq.nth offsets (i+1) - Seq.nth offsets i
      val degrees = Seq.tabulate off n

      val nbrs = Seq.map #2 sorted
    in
      (offsets, degrees, nbrs)
    end

  fun dedupEdges edges =
    let
      val sorted =
        Mergesort.sort (fn ((u1,v1), (u2,v2)) =>
          case Vertex.compare (u1, u2) of
            EQUAL => Vertex.compare (v1, v2)
          | other => other) edges
    in
      AS.full (SeqBasis.filter 5000 (0, Seq.length sorted) (Seq.nth sorted)
        (fn i => i = 0 orelse Seq.nth sorted (i-1) <> Seq.nth sorted i))
    end

  fun randSymmGraph n d =
    let
      val m = Real.ceil (Real.fromInt n * Real.fromInt d / 2.0)

      fun makeEdge i =
        let
          val u = (2 * i) div d
          val v = Util.hash i mod (n-1)
        in
          (Vertex.fromInt u, Vertex.fromInt (if v < u then v else v+1))
        end

      val bothWays = ForkJoin.alloc (2*m)
      val _ = ForkJoin.parfor 1000 (0, m) (fn i =>
        let
          val (u, v) = makeEdge i
        in
          A.update (bothWays, 2*i, (u,v));
          A.update (bothWays, 2*i+1, (v,u))
        end)
    in
      fromSortedEdges (dedupEdges (AS.full bothWays))
    end

end
