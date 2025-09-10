signature RANGE_TREE =
sig
  type rt
  type point = int * int
  type weight = int

  val build : (point * weight) Seq.t -> int -> rt
  val query : rt -> point -> point -> weight
  val print : rt -> unit
end

structure RangeTree : RANGE_TREE =
struct
  type point = int * int
  type weight = int

  structure IRTree : Aug =
  struct
    type key = point
    type value = weight
    type aug = weight
    val compare = fn (p1 : point, p2 : point) => Int.compare (#2 p1, #2 p2)
    val g = fn (x, y) => y
    val f = fn (x, y) => x + y
    val id = 0
    val balance = WB 0.28
    fun debug (k, v, a) = " "
  end

  structure InnerRangeTree = PAM(IRTree)

  structure ORTree : Aug =
  struct
    type key = point
    type value = weight
    type aug = InnerRangeTree.am
    val compare = fn (p1 : key, p2 : key) => Int.compare (#1 p1, #1 p2)
    val g = fn (x, y) => InnerRangeTree.singleton x y
    val f = fn (x, y) => InnerRangeTree.union x y (Int.+)
    val id = InnerRangeTree.empty ()
    val balance = WB 0.28
    fun debug (k, v, a) = (InnerRangeTree.print_tree a ""; Int.toString v)
  end

  structure OuterRangeTree = PAM(ORTree)
  type rt = OuterRangeTree.am

  fun build s n = OuterRangeTree.build s 0 n

  fun query r p1 p2 =
    let
      fun g' ri = InnerRangeTree.aug_range ri p1 p2
    in
      OuterRangeTree.aug_project g' (Int.+) r p1 p2
    end

  fun print r = OuterRangeTree.print_tree r " "
end

