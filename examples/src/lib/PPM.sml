(* Basic support for the netpbm .ppm file format. *)
structure PPM:
sig
  type channel = Word8.word
  type pixel = {red: channel, green: channel, blue: channel}

  (* sequence of rows, each row must be the same length *)
  type image = pixel Seq.t Seq.t

  val width: image -> int
  val height: image -> int

  type box = {topleft: int * int, botright: int * int}

  val subimage: box -> image -> image

  (* `replace box image subimage` copies subimage into the image at the
   * specified box *)
  val replace: box -> image -> image -> image

  (* read the given .ppm file *)
  val read: string -> image

  (* output this image to the given .ppm file *)
  val write: string -> image -> unit

end =
struct

  type 'a seq = 'a Seq.t

  type channel = Word8.word
  type pixel = {red: channel, green: channel, blue: channel}
  type image = pixel Seq.t Seq.t
  type box = {topleft: int * int, botright: int * int}

  fun height image = Seq.length image
  fun width image =
    if height image = 0 then 0
    else Seq.length (Seq.nth image 0)

  fun subimage {topleft=(i1,j1), botright=(i2,j2)} image =
    let
      fun elem i j = Seq.nth (Seq.nth image i) j
    in
      Seq.tabulate
      (fn i => Seq.tabulate (fn j => elem (i1+i) (j1+j)) (j2-j1))
      (i2-i1)
    end

  fun replace {topleft=(i1,j1), botright=(i2,j2)} image subimage =
    let
      fun elem i j =
        if i1 <= i andalso i < i2 andalso
           j1 <= j andalso j < j2
        then Seq.nth (Seq.nth subimage (i-i1)) (j-j1)
        else Seq.nth (Seq.nth image i) j
    in
      Seq.tabulate
      (fn i => Seq.tabulate (fn j => elem i j) (width image))
      (height image)
    end

  (* utilities... *)

  fun niceify str =
    if String.size str <= 10 then str
    else String.substring (str, 0, 7) ^ "..."

  (* ============================== P3 format ============================== *)

  fun parse3 contents =
    let
      val tokens = Seq.fromList (String.tokens Char.isSpace contents)
      fun tok i = Seq.nth tokens i
      val numToks = Seq.length tokens

      val filetype = tok 0
      val _ =
        if filetype = "P3" then ()
        else raise Fail "should not happen"

      fun intTok thingName i =
        let
          fun err () =
            raise Fail ("error parsing .ppm file: cannot parse "
                        ^ thingName ^ " from '" ^ niceify (tok i) ^ "'")
        in
          case Int.fromString (tok i) of
            NONE => err ()
          | SOME x => if x >= 0 then x else err ()
        end

      val width = intTok "width" 1
      val height = intTok "height" 2
      val resolution = intTok "max color value" 3

      val numPixels = width * height
      val numChannels = 3 * width * height

      val _ =
        if numToks = numChannels + 4 then ()
        else raise Fail ("error parsing .ppm file: too few color channels")

      fun normalize (c : int) =
        Real.ceil ((Real.fromInt c / Real.fromInt resolution) * 255.0)

      fun chan i =
        let
          val c = intTok "channel" (i + 4)
          val _ =
            if c <= resolution then ()
            else raise Fail ("error parsing .ppm file: channel value "
                             ^ Int.toString c ^ " greater than resolution "
                             ^ Int.toString resolution)
        in
          Word8.fromInt (normalize c)
        end

      fun pixel i =
        {red = chan (3*i), green = chan (3*i + 1), blue = chan (3*i + 2)}

      fun row i =
        Seq.tabulate (fn j => pixel (width*i + j)) width
    in
      Seq.tabulate row height
    end

  (* ============================== P6 format ============================== *)

  fun parse6 contents =
    let
      val filetype = String.substring (contents, 0, 2)
      val _ =
        if filetype = "P6" then ()
        else raise Fail "should not happen"

      fun findFirst p i =
        if i >= String.size contents then
          NONE
        else if p (String.sub (contents, i)) then
          SOME i
        else
          findFirst p (i+1)

      fun findToken start =
        case findFirst (not o Char.isSpace) start of
          NONE => NONE
        | SOME i =>
            case findFirst Char.isSpace i of
              NONE => SOME (i, String.size contents)
            | SOME j => SOME (i, j)

      (* start must be on a space *)
      fun chompToken start =
        case findToken start of
          NONE => NONE
        | SOME (i, j) => SOME (String.substring (contents, i, j-i), j)

      fun chompInt thingName i =
        case chompToken i of
          NONE => raise Fail ("error parsing .ppm file: missing " ^ thingName)
        | SOME (s, j) =>
            case Int.fromString s of
              NONE => raise Fail ("error parsing .ppm file: cannot parse "
                                  ^ thingName ^ " from '" ^ niceify s ^ "'")
            | SOME x =>
                if x >= 0 then (x, j)
                else raise Fail ("error parsing .ppm file: cannot parse "
                                 ^ thingName ^ " from '" ^ niceify s ^ "'")

      val cursor = 2
      val _ =
        if String.size contents > 2 andalso
           Char.isSpace (String.sub (contents, 2))
        then ()
        else raise Fail "error parsing .ppm file: unexpected format"

      val (width, cursor) = chompInt "width" cursor
      val (height, cursor) = chompInt "height" cursor
      val (resolution, cursor) = chompInt "max color value" cursor

      val numChannels = 3 * width * height

      val _ =
        if resolution = 255 then ()
        else raise Fail "error parsing .ppm file: P6 max color value must be 255"

      val cursor =
        case findFirst (not o Char.isSpace) cursor of
          SOME i => i
        | NONE => raise Fail "error parsing .ppm file: missing contents"

      val _ =
        if String.size contents - cursor >= numChannels then ()
        else raise Fail "error parsing .ppm file: too few color channels"

      fun chan i =
        Word8.fromInt (Char.ord (String.sub (contents, cursor + i)))

      fun pixel i =
        {red = chan (3*i), green = chan (3*i + 1), blue = chan (3*i + 2)}

      fun row i =
        Seq.tabulate (fn j => pixel (width*i + j)) width
    in
      Seq.tabulate row height
    end

  (* ================================= read ================================= *)

  fun read filepath =
    let
      val file = TextIO.openIn filepath
      val contents = TextIO.inputAll file
      val _ = TextIO.closeIn file
    in
      case String.substring (contents, 0, 2) of
        "P3" => parse3 contents
      | "P6" => parse6 contents
      | _ => raise Fail "error parsing .ppm file: unknown or unsupported format"
    end

  (* ================================ write ================================ *)
  (* for now, only writes to format P6 *)

  fun write filepath image =
    let
      val allRowsSameWidth =
        Seq.reduce (fn (a, b) => a andalso b) true
        (Seq.map (fn row => Seq.length row = width image) image)
      val _ =
        if allRowsSameWidth then ()
        else raise Fail ("error writing " ^ filepath ^ ": jagged image")

      val file = TextIO.openOut filepath

      fun dump str = TextIO.output (file, str)
      fun dumpChan c = TextIO.output1 (file, Char.chr (Word8.toInt c))
      fun dumpPx i j =
        let
          val {red, green, blue} = Seq.nth (Seq.nth image i) j
        in
          (dumpChan red;
           dumpChan green;
           dumpChan blue)
        end

      fun dumpLoop i j =
        if i >= height image then ()
        else if j >= width image then
          dumpLoop (i+1) 0
        else
          (dumpPx i j; dumpLoop i (j+1))
    in
      dump "P6 ";
      dump (Int.toString (width image) ^ " ");
      dump (Int.toString (height image) ^ " ");
      dump "255 ";
      dumpLoop 0 0
    end

end
