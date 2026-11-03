(* Basic support for the netpbm .ppm file format. *)
structure PPM:
sig
  type channel = Color.channel
  type pixel = Color.pixel

  (* flat sequence; pixel (i, j) is at data[i*width + j] *)
  type image = {height: int, width: int, data: pixel Seq.t}
  type box = {topleft: int * int, botright: int * int}

  val elem: image -> (int * int) -> pixel

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

  type channel = Color.channel
  type pixel = Color.pixel
  type image = {height: int, width: int, data: pixel Seq.t}
  type box = {topleft: int * int, botright: int * int}

  fun elem ({height, width, data}: image) (i, j) =
    if i < 0 orelse i >= height orelse j < 0 orelse j >= width then
      raise Subscript
    else
      Seq.nth data (i*width + j)

  fun subimage {topleft=(i1,j1), botright=(i2,j2)} image =
    let
      val w = j2-j1
      val h = i2-i1

      fun newElem k =
        let
          val i = k div w
          val j = k mod w
        in
          elem image (i1 + i, j1 + j)
        end
    in
      { width = w
      , height = h
      , data = Seq.tabulate newElem (w * h)
      }
    end

  fun replace {topleft=(i1,j1), botright=(i2,j2)} image subimage =
    let
      fun newElem k =
        let
          val i = k div (#width image)
          val j = k mod (#width image)
        in
          if i1 <= i andalso i < i2 andalso
             j1 <= j andalso j < j2
          then elem subimage (i-i1, j-j1)
          else elem image (i, j)
        end
    in
      { width = #width image
      , height = #height image
      , data = Seq.tabulate newElem (#width image * #height image)
      }
    end

  (* utilities... *)

  fun niceify str =
    if String.size str <= 10 then str
    else String.substring (str, 0, 7) ^ "..."

  (* ============================== P3 format ============================== *)

  fun parse3 contents =
    let
      (* val tokens = Seq.fromList (String.tokens Char.isSpace contents) *)
      (* val numToks = Seq.length tokens *)
      val (numToks, tokRange) = Tokenize.tokenRanges Char.isSpace contents
      fun tok i =
        let
          val (lo, hi) = tokRange i
        in
          Seq.subseq contents (lo, hi-lo)
        end
      fun strTok i =
        Parse.parseString (tok i)

      val filetype = strTok 0
      val _ =
        if filetype = "P3" then ()
        else raise Fail "should not happen"

      fun intTok thingName i =
        let
          fun err () =
            raise Fail ("error parsing .ppm file: cannot parse "
                        ^ thingName ^ " from '"
                        ^ niceify (strTok i) ^ "'")
        in
          case Parse.parseInt (tok i) of
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
    in
      { width = width
      , height = height
      , data = Seq.tabulate pixel (width * height)
      }
    end

  (* ============================== P6 format ============================== *)

  fun parse6 contents =
    let
      val filetype = Parse.parseString (Seq.subseq contents (0, 2))
      val _ =
        if filetype = "P6" then ()
        else raise Fail "should not happen"

      fun findFirst p i =
        if i >= Seq.length contents then
          NONE
        else if p (Seq.nth contents i) then
          SOME i
        else
          findFirst p (i+1)

      fun findToken start =
        case findFirst (not o Char.isSpace) start of
          NONE => NONE
        | SOME i =>
            case findFirst Char.isSpace i of
              NONE => SOME (i, Seq.length contents)
            | SOME j => SOME (i, j)

      (* start must be on a space *)
      fun chompToken start =
        case findToken start of
          NONE => NONE
        | SOME (i, j) => SOME (Seq.subseq contents (i, j-i), j)

      fun chompInt thingName i =
        case chompToken i of
          NONE => raise Fail ("error parsing .ppm file: missing " ^ thingName)
        | SOME (s, j) =>
            case Parse.parseInt s of
              NONE => raise Fail ("error parsing .ppm file: cannot parse "
                                  ^ thingName ^ " from '"
                                  ^ niceify (Parse.parseString s) ^ "'")
            | SOME x =>
                if x >= 0 then (x, j)
                else raise Fail ("error parsing .ppm file: cannot parse "
                                 ^ thingName ^ " from '"
                                 ^ niceify (Parse.parseString s) ^ "'")

      val cursor = 2
      val _ =
        if Seq.length contents > 2 andalso
           Char.isSpace (Seq.nth contents 2)
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
        if Seq.length contents - cursor >= numChannels then ()
        else raise Fail "error parsing .ppm file: too few color channels"

      fun chan i =
        Word8.fromInt (Char.ord (Seq.nth contents (cursor + i)))

      fun pixel i =
        {red = chan (3*i), green = chan (3*i + 1), blue = chan (3*i + 2)}
    in
      { width = width
      , height = height
      , data = Seq.tabulate pixel (width * height)
      }
    end

  (* ================================= read ================================= *)

  fun read filepath =
    let
      val contents = ReadFile.contentsSeq filepath
    in
      case Parse.parseString (Seq.subseq contents (0, 2)) of
        "P3" => parse3 contents
      | "P6" => parse6 contents
      | _ => raise Fail "error parsing .ppm file: unknown or unsupported format"
    end

  (* ================================ write ================================ *)
  (* for now, only writes to format P6 *)

  fun write filepath image =
    let
      val file = TextIO.openOut filepath

      fun dump str = TextIO.output (file, str)
      fun dumpChan c = TextIO.output1 (file, Char.chr (Word8.toInt c))
      fun dumpPx i j =
        let
          val {red, green, blue} = elem image (i, j)
        in
          (dumpChan red;
           dumpChan green;
           dumpChan blue)
        end

      fun dumpLoop i j =
        if i >= #height image then ()
        else if j >= #width image then
          dumpLoop (i+1) 0
        else
          (dumpPx i j; dumpLoop i (j+1))
    in
      dump "P6 ";
      dump (Int.toString (#width image) ^ " ");
      dump (Int.toString (#height image) ^ " ");
      dump "255 ";
      dumpLoop 0 0
    end

end
