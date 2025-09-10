structure DelaunayTriangulation :
sig
  val triangulate: Geometry2D.point Seq.t -> Topology2D.mesh
end =
struct

  val showDelaunayRoundStats =
    CommandLineArgs.parseFlag "show-delaunay-round-stats"

  structure G = Geometry2D
  structure T = Topology2D
  structure NN = NearestNeighbors
  structure A = Array
  structure AS = ArraySlice

  type vertex = T.vertex
  type simplex = T.simplex

  val BOUNDARY_SIZE = 10

  fun generateBoundary pts =
    let
      val p0 = Seq.nth pts 0
      val minCorner = Seq.reduce G.Point.minCoords p0 pts
      val maxCorner = Seq.reduce G.Point.maxCoords p0 pts
      val diagonal = G.Point.sub (maxCorner, minCorner)
      val size = G.Vector.length diagonal
      val stretch = 10.0
      val radius = stretch*size
      val center = G.Vector.add (minCorner, G.Vector.scaleBy 0.5 diagonal)
    in
      T.boundaryCircle {center=center, radius=radius, size=BOUNDARY_SIZE}
    end


  fun writeMax a i x =
    let
      fun loop old =
        if x <= old then () else
        let
          val old' = Concurrency.casArray (a, i) (old, x)
        in
          if old' = old then ()
          else loop old'
        end
    in
      loop (A.sub (a, i))
    end


  fun reserveForInsert reserved mesh start (pt, id) =
    let
      val (cavity, perimeter) =
        T.findCavityAndPerimeter mesh start pt
    in
      List.app (fn v => writeMax reserved v id) perimeter;
      (cavity, perimeter)
    end


  fun resetAndCheckPerimeter reserved perimeter id =
    List.foldl
      (fn (v, allMine) =>
        if A.sub (reserved, v) = id then
          (A.update (reserved, v, ~1); allMine)
        else
          false)
      true
      perimeter


  fun triangulate inputPts =
    let
      val maxBatch = Util.ceilDiv (Seq.length inputPts) 100
      val totalNumVertices = BOUNDARY_SIZE + Seq.length inputPts
      val reserved =
        SeqBasis.tabulate 10000 (0, totalNumVertices) (fn _ => ~1)

      fun batchInsert mesh (nn: NN.t) pointsToInsert =
        let
          val n = T.numVertices mesh
          val m = Seq.length pointsToInsert

          val attempts =
            AS.full (SeqBasis.tabulate 100 (0, m) (fn i =>
              let
                val pt = Seq.nth pointsToInsert i
                val start: simplex =
                  (T.triangleOfVertex mesh (NN.nearestNeighbor nn pt), 0)
              in
                reserveForInsert reserved mesh start
                  (Seq.nth pointsToInsert i, i)
              end))

          val winnerFlags =
            AS.full (SeqBasis.tabulate 1000 (0, m) (fn i =>
              let
                val (_, perimeter) = Seq.nth attempts i
              in
                resetAndCheckPerimeter reserved perimeter i
              end))

          val winners =
            AS.full (SeqBasis.tabFilter 1000 (0, m) (fn i =>
              if Seq.nth winnerFlags i then
                SOME (#1 (Seq.nth attempts i), Seq.nth pointsToInsert i)
              else
                NONE))

          val losers =
            AS.full (SeqBasis.filter 1000 (0, m)
              (Seq.nth pointsToInsert)
              (not o Seq.nth winnerFlags))

          val mesh' = T.ripAndTent winners mesh
        in
          (mesh', losers)
        end

      val nnRebuildMultiplier = 10

      fun shouldRebuild numNextRebuild totalRemaining =
        let
          val n = Seq.length inputPts
          val numDone = n - totalRemaining
        in
          numDone >= numNextRebuild
          andalso
          numDone <= n div nnRebuildMultiplier
        end


      fun loop numRounds mesh (nn: NN.t) numNextRebuild losers remaining =
        if Seq.length losers + Seq.length remaining = 0 then
          (numRounds, mesh)
        else if shouldRebuild numNextRebuild (Seq.length losers + Seq.length remaining) then
          let
            val nn = NN.makeTree 16 (T.getPoints mesh)
            val numNextRebuild = numNextRebuild * nnRebuildMultiplier
          in
            if not showDelaunayRoundStats then () else
            print ("rebuilt nn; next rebuild at " ^ Int.toString numNextRebuild ^ "\n");

            loop numRounds mesh nn numNextRebuild losers remaining
          end
        else
          let
            val numRetry = Seq.length losers
            val totalRemaining = numRetry + Seq.length remaining
            val numDone = Seq.length inputPts - totalRemaining
            val desiredSize =
              Int.min (maxBatch, Int.min (1 + numDone div 50, Seq.length inputPts - numDone))
            val numAdditional =
              Int.max (0, Int.min (desiredSize - numRetry, Seq.length remaining))
            val thisBatchSize = numAdditional + numRetry

            val newcomers = Seq.take remaining numAdditional
            val remaining = Seq.drop remaining numAdditional
            val (mesh, losers) =
              batchInsert mesh nn (Seq.append (losers, newcomers))

            val rate =
              Real.round (100.0 * (Real.fromInt (thisBatchSize - Seq.length losers)
                                   / Real.fromInt thisBatchSize))

            val _ =
              if not showDelaunayRoundStats then () else
              print ("round " ^ Int.toString numRounds
                    ^ "\tdone " ^ Int.toString numDone
                    ^ "\tremaining " ^ Int.toString totalRemaining
                    ^ "\tdesired " ^ Int.toString desiredSize
                    ^ "\tretrying " ^ Int.toString numRetry
                    ^ "\tfresh " ^ Int.toString numAdditional
                    ^ "\tsuccess-rate " ^ Int.toString rate ^ "%\n")
          in
            loop (numRounds+1) mesh nn numNextRebuild losers remaining
          end

      val initialMesh = generateBoundary inputPts
      val initialNN = NN.makeTree 16 (T.getPoints initialMesh)
      val numNextRebuild = 100

      val (numRounds, finalMesh) =
        loop 0 initialMesh initialNN numNextRebuild (Seq.empty()) inputPts

      val _ = print ("num rounds " ^ Int.toString numRounds ^ "\n")
    in
      finalMesh
    end

end
