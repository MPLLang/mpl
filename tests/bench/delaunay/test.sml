structure CLA = CommandLineArgs
structure T = Topology2D
structure DT = DelaunayTriangulation

val (filename, testPtStr) =
  case CLA.positional () of
    [x, y] => (x, y)
  | _ => Util.die "usage: ./foo <meshfile> <test point>"

val testType = CLA.parseString "test" "split"

val testPoint =
  case List.mapPartial Real.fromString (String.tokens (fn c => c = #",") testPtStr) of
    [x,y] => (x,y)
  | _ => Util.die ("bad test point")

val (mesh, tm) = Util.getTime (fn _ => T.parseFile filename)
val _ = print ("num vertices: " ^ Int.toString (T.numVertices mesh) ^ "\n")
val _ = print ("num edges: " ^ Int.toString (T.numTriangles mesh) ^ "\n")

val _ = print ("\n" ^ T.toString mesh ^ "\n\n")

val start: T.simplex = (0, 0)

fun simpToString (t, i) =
  "triangle " ^ Int.toString t ^ " orientation " ^ Int.toString i ^ ": " ^
  let
    val T.Tri {vertices=(a,b,c), ...} = T.tdata mesh t
  in
    String.concatWith " " (List.map Int.toString
      (if i = 0 then [a,b,c]
       else if i = 1 then [b,c,a]
       else [c,a,b]))
  end

fun triToString t =
  "triangle " ^ Int.toString t ^ ": " ^
  let
    val T.Tri {vertices=(a,b,c), ...} = T.tdata mesh t
  in
    String.concatWith " " (List.map Int.toString [a,b,c])
  end

val _ = Util.for (0, T.numVertices mesh) (fn v =>
  let
    val s = T.find mesh v start
  in
    print ("found " ^ Int.toString v ^ ": " ^ simpToString s ^ "\n")
  end)


(* ======================================================================== *)

fun testSplit () =
  let
    val _ = print ("===================================\nTESTING SPLIT\n")

    val ((center, tris), verts) = T.findCavityAndPerimeter mesh start testPoint
    val _ =
      print ("CAVITY CENTER IS:\n  " ^ triToString center ^ "\n")
    val _ =
      print ("CAVITY MEMBERS ARE:\n"
        ^ String.concatWith "\n" (List.map (fn x => "  " ^ simpToString x) tris) ^ "\n")
    val _ =
      print ("CAVITY PERIMETER VERTICES ARE:\n  "
        ^ String.concatWith " " (List.map (fn x => Int.toString x) verts) ^ "\n")


    val mesh' = T.split mesh center testPoint
    val _ =
      print ("===================================\nAFTER SPLIT:\n" ^ T.toString mesh' ^ "\n")
  in
    ()
  end

(* ======================================================================== *)

fun testFlip () =
  let
    val _ = print ("===================================\nTESTING FLIP\n")

    val simp = T.findPoint mesh testPoint start
    val _ =
      print ("SIMPLEX CONTAINING POINT:\n  " ^ simpToString simp ^ "\n")

    val mesh' = T.flip mesh simp
    val _ =
      print ("===================================\nAFTER FLIP:\n" ^ T.toString mesh' ^ "\n")
  in
    ()
  end

(* ======================================================================== *)

fun testRipAndTent () =
  let
    val _ = print ("===================================\nTESTING RIP-AND-TENT\n")

    val (cavity as (center, tris), verts) =
      T.findCavityAndPerimeter mesh start testPoint

    val _ =
      print ("CAVITY CENTER IS:\n  " ^ triToString center ^ "\n")
    val _ =
      print ("CAVITY MEMBERS ARE:\n"
        ^ String.concatWith "\n" (List.map (fn x => "  " ^ simpToString x) tris) ^ "\n")
    val _ =
      print ("CAVITY PERIMETER VERTICES ARE:\n  "
        ^ String.concatWith " " (List.map (fn x => Int.toString x) verts) ^ "\n")

    val mesh' = T.ripAndTentOne (cavity, testPoint) mesh
    val _ =
      print ("===================================\nAFTER FLIP:\n" ^ T.toString mesh' ^ "\n")
  in
    ()
  end

(* ======================================================================== *)

val _ =
  case testType of
    "split" => testSplit ()
  | "flip" => testFlip ()
  | "rip-and-tent" => testRipAndTent ()
  | _ => Util.die ("unknown test type")
