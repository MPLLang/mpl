datatype linkedList = Node of (linkedList ref * int)
                    | End

(*val x = node(ref (nil2) * ref(nil2) * 4)*)
exception IndexError

val n = 10000

fun elem i = i

val y = SeqBasis.foldl (fn (lis, x) => Node(ref(lis), x)) End (0, n) elem

(*fun printList lis =
  case lis of
    End => print "end\n"
  | Node(a, b) => (print (Int.toString b ^ " "); printList (!a))

fun delete (idx:int) (lis:linkedList) =
  case lis of
    End => raise IndexError
  | Node(a, b) =>
      if idx  = 0  then
        (!a, Node(a, b))
      else if idx = 1 then
        let
          val delElement = !a
          val delNext = case delElement of
                          End => raise IndexError
                        | Node(c, d) => !c
          val _ = (a := delNext)
        in
          (Node(a, b), delElement)
        end
      else
        delete (idx - 1) (!a)*)

(*val x = delete (n-1) y*)
