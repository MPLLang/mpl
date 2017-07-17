structure Main =
struct

  open MainUtils
  open Random

  val maxVal = ref 100000000
  val sorts = ref [] : string list ref
  val ordered = ref false

  local
    fun processSortArgs nil = ()
      | processSortArgs (args as (arg::rest)) =

      if arg = "--sorts" then
        processStringListArg processSortArgs "sorts" (setOption sorts) rest

      else if arg = "--ordered" then
        (ordered := true; processSortArgs rest)

      else processArgs processSortArgs args
  in
    val () = processSortArgs (CommandLine.arguments ())
      handle Argument s => print ("sort: " ^ s ^ "\n")
  end

  structure V = Vector
  structure VS = VectorSlice
  structure A = Array
  structure AS = ArraySlice

  structure ISort = InsertionSort (VectorSliceSortArgSeq)
  structure SSort = SelectionSort (VectorSliceSortArgSeq)
  structure QSort = QuickSort (VectorSliceSortArgSeq)
  structure MSeq = MergeSeq (VectorSliceSortArgSeq)
  structure MSortSeq = MergeSort (structure A =
                                    VectorSliceSortArgSeq
                                  structure M1 =
                                    MSeq
                                  structure M2 =
                                    MSeq)
  structure MPar = MergePar (VectorSliceSortArgParNoDelay)
  structure MSortPar = MergeSort (structure A =
                                    VectorSliceSortArgParNoDelay
                                  structure M1 =
                                    MSeq
                                  structure M2 =
                                    MPar)

  structure ISortInplace = InsertionSortInplace (ArraySliceSortArgSeq)
  structure MSeqInplace = MergeSeqInplace (ArraySliceSortArgSeq)
  structure MSortSeqInplace = MergeSortInplace (structure A =
                                                  ArraySliceSortArgSeq
                                                structure M1 =
                                                  MSeqInplace
                                                structure M2 =
                                                  MSeqInplace)
  structure MParInplace = MergeParInplace (ArraySliceSortArgPar)
  structure MSortParInplace = MergeSortInplace (structure A =
                                                  ArraySliceSortArgPar
                                                structure M1 =
                                                  MSeqInplace
                                                structure M2 =
                                                  MParInplace)

  structure SSortList = SelectionSort (ListSortArgSeq)
  structure QSortList = QuickSort (ListSortArgSeq)
  structure QSortParList = QuickSort (ListSortArgParNoDelay)
  (* structure QSortFutListNoDelay = QuickSortFutures (ListSortArgParNoDelay) *)
  (* structure QSortFutListMaybeDelay = QuickSortFutures (ListSortArgParMaybeDelay) *)
  structure MSeqList = MergeSeq (ListSortArgSeq)
  structure MSortSeqList = MergeSort (structure A =
                                        ListSortArgSeq
                                      structure M1 =
                                        MSeqList
                                      structure M2 =
                                        MSeqList)
  structure MParList = MergePar (ListSortArgParNoDelay)
  structure MSortParList = MergeSort (structure A =
                                        ListSortArgParNoDelay
                                      structure M1 =
                                        MSeqList
                                      structure M2 =
                                        MParList)
  structure SSortTree = SelectionSort (TreeSortArgSeq)
  structure QSortTree = QuickSort (TreeSortArgSeq)
  structure QSortParTree = QuickSort (TreeSortArgParNoDelay)
  structure MSeqTree = MergeSeq (TreeSortArgSeq)
  structure MSortSeqTree = MergeSort (structure A =
                                        TreeSortArgSeq
                                      structure M1 =
                                        MSeqTree
                                      structure M2 =
                                        MSeqTree)
  structure MParTree = MergePar (TreeSortArgParNoDelay)
  structure MSortParTree = MergeSort (structure A =
                                        TreeSortArgParNoDelay
                                      structure M1 =
                                        MSeqTree
                                      structure M2 =
                                        MParTree)
  structure PrintParTree = Print (structure A = TreeSortArgSeq)
  (* structure MFutTreeNoDelay = MergeFutures (TreeSortArgParNoDelay) *)
  (* structure MFutTreeMaybeDelay = MergeFutures (TreeSortArgParMaybeDelay) *)
  (* structure MSortParFutTreeNoDelay = MergeSort (structure A = *)
  (*                                          TreeSortArgParNoDelay *)
  (*                                        structure M1 = *)
  (*                                          MSeqTree *)
  (*                                        structure M2 = *)
  (*                                          MFutTreeNoDelay) *)
  (* structure MSortParFutTreeMaybeDelay = MergeSort (structure A = *)
  (*                                          TreeSortArgParMaybeDelay *)
  (*                                        structure M1 = *)
  (*                                          MSeqTree *)
  (*                                        structure M2 = *)
  (*                                          MFutTreeMaybeDelay) *)
  (* structure MSortSeqFutTreeNoDelay = MergeSort (structure A = *)
  (*                                          TreeSortArgSeq *)
  (*                                        structure M1 = *)
  (*                                          MSeqTree *)
  (*                                        structure M2 = *)
  (*                                          MFutTreeNoDelay) *)
  (* structure MSortSeqFutTreeMaybeDelay = MergeSort (structure A = *)
  (*                                          TreeSortArgSeq *)
  (*                                        structure M1 = *)
  (*                                          MSeqTree *)
  (*                                        structure M2 = *)
  (*                                          MFutTreeMaybeDelay) *)
  (* structure MSortFutTreeNoDelay = MergeSortFutures (structure A = *)
  (*                                          TreeSortArgParNoDelay) *)
  (* structure MSortFutTreeMaybeDelay = MergeSortFutures (structure A = *)
  (*                                          TreeSortArgParMaybeDelay) *)
  (* structure MSortFut2NoDelay = MergeSortFutures2 (VectorSliceSortArgParNoDelay) *)
  (* structure MSortFut2MaybeDelay = MergeSortFutures2 (VectorSliceSortArgParMaybeDelay) *)


  fun printVector v = print (VS.foldr (fn (x, s) => Real.toString x
                                                    ^ ", " ^ s) "\n" v)

  fun run name size seqCutoff parCutoff =
    let
      val isort = ISort.sort { cutoff = 0, fallback = fn x => x }
      val ssort = SSort.sort { cutoff = 0, fallback = fn x => x }
      val qsort_seq = QSort.sort { cutoff = seqCutoff, fallback = ssort }
      val qsort = QSort.sort { cutoff = parCutoff, fallback = qsort_seq }
      val msort_seq = MSortSeq.sort { cutoff = seqCutoff, fallback = ssort }
      val msort = MSortPar.sort { cutoff = parCutoff, fallback = msort_seq }
      val isorti = ISortInplace.sort { cutoff = 0, fallback = fn _ => () }
      val msorti_seq = MSortSeqInplace.sort { cutoff = seqCutoff, fallback = isorti }
      val msorti = MSortParInplace.sort { cutoff = parCutoff, fallback = msorti_seq }
      val ssortl = SSortList.sort { cutoff = 0, fallback = fn x => x }
      val qsortl_seq = QSortList.sort { cutoff = seqCutoff, fallback = ssortl }
      val qsortl = QSortParList.sort { cutoff = parCutoff, fallback = qsortl_seq }
      (* val qsortfln = QSortFutListNoDelay.sort { cutoff = parCutoff, fallback = qsortl_seq } *)
      (* val qsortflm = QSortFutListMaybeDelay.sort { cutoff = parCutoff, fallback = qsortl_seq } *)
      val msortl_seq = MSortSeqList.sort { cutoff = seqCutoff, fallback = ssortl }
      val msortl = MSortParList.sort { cutoff = parCutoff, fallback = msortl_seq }
      val ssortt = SSortTree.sort { cutoff = 0, fallback = fn x => x }
      val qsortt_seq = QSortTree.sort { cutoff = seqCutoff, fallback = ssortt }
      val qsortt = QSortParTree.sort { cutoff = parCutoff, fallback = qsortt_seq }
      val msortt_seq = MSortSeqTree.sort { cutoff = seqCutoff, fallback = ssortt }
      val msortt = MSortParTree.sort { cutoff = parCutoff, fallback = msortt_seq }
      val print_msortt = PrintParTree.sort { cutoff = parCutoff, fallback = msortt }
      (* val msortsftn = MSortSeqFutTreeNoDelay.sort { cutoff = parCutoff, fallback = msortt_seq } *)
      (* val msortsftm = MSortSeqFutTreeMaybeDelay.sort { cutoff = parCutoff, fallback = msortt_seq } *)
      (* val msortpftn = MSortParFutTreeNoDelay.sort { cutoff = parCutoff, fallback = msortt_seq } *)
      (* val msortpftm = MSortParFutTreeMaybeDelay.sort { cutoff = parCutoff, fallback = msortt_seq } *)
      (* val msortftn = MSortFutTreeNoDelay.sort { cutoff = parCutoff, fallback = msortt_seq } *)
      (* val msortftm = MSortFutTreeMaybeDelay.sort { cutoff = parCutoff, fallback = msortt_seq } *)
      (* val msortfvn = MSortFut2NoDelay.sort { cutoff = parCutoff, fallback = msort_seq } *)
      (* val msortfvm = MSortFut2MaybeDelay.sort { cutoff = parCutoff, fallback = msort_seq } *)

      local
        fun wrapInplace sort v =
            let
              val a = AS.full (A.tabulate (VS.length v, fn i => VS.sub (v, i)))
              val a' = AS.full (A.tabulate (AS.length a, fn _ => ~1.0))
              val () = sort (true, a, a')
            in
              VS.full (AS.vector a)
          end

        fun wrapList sort v =
            let
              val l = VectorSliceSortArgSeq.toList v
              val l = sort l
            in
              VectorSliceSortArgSeq.fromList NONE l
            end

        fun wrapTree sort v =
            let
              val l = TreeSortArgSeq.fromList (SOME parCutoff) (VectorSliceSortArgSeq.toList v)
(*
              val () = TreeSortArgSeq.printArg l
              val () = print "\n\n"
*)
              val l = sort l
(*
              val () = print "\n\n"
              val () = TreeSortArgSeq.printArg l
              val () = print "\n"
*)
            in
              VectorSliceSortArgSeq.fromList NONE (TreeSortArgSeq.toList l)
            end

      in
      val sort = if name = "isort" then isort
                 else if name = "ssort" then ssort
                 else if name = "qsort_seq" then qsort_seq
                 else if name = "qsort" then qsort
                 else if name = "msort_seq" then msort_seq
                 else if name = "msort" then msort
                 else if name = "isorti" then wrapInplace isorti
                 else if name = "msorti_seq" then wrapInplace msorti_seq
                 else if name = "msorti" then wrapInplace msorti
                 else if name = "ssortl" then wrapList ssortl
                 else if name = "qsortl_seq" then wrapList qsortl_seq
                 else if name = "qsortl" then wrapList qsortl
                 (* else if name = "qsortfln" then wrapList qsortfln *)
                 (* else if name = "qsortflm" then wrapList qsortflm *)
                 else if name = "msortl_seq" then wrapList msortl_seq
                 else if name = "msortl" then wrapList msortl
                 else if name = "ssortt" then wrapTree ssortt
                 else if name = "qsortt_seq" then wrapTree qsortt_seq
                 else if name = "qsortt" then wrapTree qsortt
                 else if name = "msortt_seq" then wrapTree msortt_seq
                 else if name = "msortt" then wrapTree msortt
                 (* else if name = "msortpftn" then wrapTree msortpftn *)
                 (* else if name = "msortpftm" then wrapTree msortpftm *)
                 (* else if name = "msortsftn" then wrapTree msortsftn *)
                 (* else if name = "msortsftm" then wrapTree msortsftm *)
                 (* else if name = "msortftn" then wrapTree msortftn *)
                 (* else if name = "msortftm" then wrapTree msortftm *)
                 (* else if name = "msortfvn" then msortfvn *)
                 (* else if name = "msortfvm" then msortfvm *)
                 else raise Argument ("unknown sort option: " ^ name)
      end

      fun checkResult a =
        let
          val == = Real.== infix 4 ==
          fun increasing i =
            if i = (VS.length a - 1) then print "success\n"
            else if (if !ordered then VS.sub (a, i) == VS.sub (a, i + 1) - 1.0
                     else VS.sub (a, i) <= VS.sub (a, i + 1))
                 then increasing (i + 1)
                 else print "FAILED!\n"
        in
          if VS.length a = size then increasing 0
          else print "FAILED!\n"
        end

      fun build () =
          let
            val a = if !ordered then
                      VS.full (V.tabulate
                                   (size, fn i => Real.fromInt (size - i)))
                    else
                      VS.full (V.tabulate
                                   (size, fn _ => randomReal (Real.fromInt (!maxVal))))
          in
            if !printResult then
              printVector a
            else ();
            a
          end

      fun post _ a =
          let in
            if !check then checkResult a
            else print "done\n";
            if !printResult then printVector a
            else ()
          end

      val { space, time, successfulSteals, failedSteals, suspends } = repeatTest name build sort post
    in
      print (String.concat [(*"#",*)
                            name, " ",
                            MLton.Parallel.Basic.policyName, " ",
                            Int.toString MLton.Parallel.Basic.numberOfProcessors, " ",
                            Int.toString size, " ",
                            Int.toString seqCutoff, " ",
                            Int.toString parCutoff, " ",
                            Word64.fmt StringCvt.DEC space, " ",
                            LargeInt.toString time, " ",
                            Int.toString successfulSteals, " ",
                            Int.toString failedSteals, " ",
                            Int.toString suspends, "\n"])
    end

  val () = print "#format: sort policy procs size seqCutoff parCutoff average-space(B) average-time(ms) successful-steals failed-steals suspends\n"

  fun doit () = table4 run (!sorts) (!sizes) (!seqCutoffs) (!parCutoffs)

  val () = doit ()

end
