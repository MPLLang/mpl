(** SAM_NOTE: copy/pasted... some repetition here with Parse. *)
structure ParseFile =
struct

  structure RF = ReadFile
  structure Seq = ArraySequence
  structure DS = DelayedSeq

  fun tokens (f: char -> bool) (cs: char Seq.t) : (char DS.t) DS.t =
    let
      val n = Seq.length cs
      val s = DS.tabulate (Seq.nth cs) n
      val indices = DS.tabulate (fn i => i) (n+1)
      fun check i =
        if (i = n) then not (f(DS.nth s (n-1)))
        else if (i = 0) then not (f(DS.nth s 0))
        else let val i1 = f (DS.nth s i)
                 val i2 = f (DS.nth s (i-1))
             in (i1 andalso not i2) orelse (i2 andalso not i1) end
      val ids = DS.filter check indices
      val res = DS.tabulate (fn i =>
        let val (start, e) = (DS.nth ids (2*i), DS.nth ids (2*i+1))
        in DS.tabulate (fn i => Seq.nth cs (start+i)) (e - start)
        end)
        ((DS.length ids) div 2)
    in
      res
    end

  fun eqStr str (chars : char DS.t) =
    let
      val n = String.size str
      fun checkFrom i =
        i >= n orelse
        (String.sub (str, i) = DS.nth chars i andalso checkFrom (i+1))
    in
      DS.length chars = n
      andalso
      checkFrom 0
    end

  fun parseDigit char =
    let
      val code = Char.ord char
      val code0 = Char.ord #"0"
      val code9 = Char.ord #"9"
    in
      if code < code0 orelse code9 < code then
        NONE
      else
        SOME (code - code0)
    end

  (* This implementation doesn't work with mpl :(
   * Need to fix the basis library... *)
  (*
  fun parseReal chars =
    let
      val str = CharVector.tabulate (DS.length chars, DS.nth chars)
    in
      Real.fromString str
    end
  *)

  fun parseInt (chars : char DS.t) =
    let
      val n = DS.length chars
      fun c i = DS.nth chars i

      fun build x i =
        if i >= n then SOME x else
        case c i of
          #"," => build x (i+1)
        | #"_" => build x (i+1)
        | cc =>
            case parseDigit cc of
              NONE => NONE
            | SOME dig => build (x * 10 + dig) (i+1)
    in
      if n = 0 then NONE
      else if (c 0 = #"-" orelse c 0 = #"~") then
        Option.map (fn x => x * ~1) (build 0 1)
      else if (c 0 = #"+") then
        build 0 1
      else
        build 0 0
    end

  fun parseReal (chars : char DS.t) =
    let
      val n = DS.length chars
      fun c i = DS.nth chars i

      fun buildAfterE x i =
        let
          val chars' = DS.subseq chars (i, n-i)
        in
          Option.map (fn e => x * Math.pow (10.0, Real.fromInt e))
            (parseInt chars')
        end

      fun buildAfterPoint m x i =
        if i >= n then SOME x else
        case c i of
          #"," => buildAfterPoint m x (i+1)
        | #"_" => buildAfterPoint m x (i+1)
        | #"." => NONE
        | #"e" => buildAfterE x (i+1)
        | #"E" => buildAfterE x (i+1)
        | cc =>
            case parseDigit cc of
              NONE => NONE
            | SOME dig => buildAfterPoint (m * 0.1) (x + m * (Real.fromInt dig)) (i+1)

      fun buildBeforePoint x i =
        if i >= n then SOME x else
        case c i of
          #"," => buildBeforePoint x (i+1)
        | #"_" => buildBeforePoint x (i+1)
        | #"." => buildAfterPoint 0.1 x (i+1)
        | #"e" => buildAfterE x (i+1)
        | #"E" => buildAfterE x (i+1)
        | cc =>
            case parseDigit cc of
              NONE => NONE
            | SOME dig => buildBeforePoint (x * 10.0 + Real.fromInt dig) (i+1)
    in
      if n = 0 then NONE
      else if (c 0 = #"-" orelse c 0 = #"~") then
        Option.map (fn x => x * ~1.0) (buildBeforePoint 0.0 1)
      else
        buildBeforePoint 0.0 0
    end

  fun readSequencePoint2d filename =
    let
      val toks = tokens Char.isSpace (RF.contentsSeq filename)
      fun tok i = DS.nth toks i
      val _ =
        if eqStr "pbbs_sequencePoint2d" (tok 0) then ()
        else raise Fail (filename ^ " wrong file type")

      val n = DS.length toks - 1

      fun r i = Option.valOf (parseReal (tok (1 + i)))

      fun pt i =
        (r (2*i), r (2*i+1))
        handle e => raise Fail ("error parsing point " ^ Int.toString i ^ " (" ^ exnMessage e ^ ")")

      val result = Seq.tabulate pt (n div 2)
    in
      result
    end

  fun readSequenceInt filename =
    let
      val toks = tokens Char.isSpace (RF.contentsSeq filename)
      fun tok i = DS.nth toks i
      val _ =
        if eqStr "pbbs_sequenceInt" (tok 0) then ()
        else raise Fail (filename ^ " wrong file type")

      val n = DS.length toks - 1

      fun p i =
        Option.valOf (parseInt (tok (1 + i)))
        handle e => raise Fail ("error parsing integer " ^ Int.toString i)
    in
      Seq.tabulate p n
    end

  fun readSequenceReal filename =
    let
      val toks = tokens Char.isSpace (RF.contentsSeq filename)
      fun tok i = DS.nth toks i
      val _ =
        if eqStr "pbbs_sequenceDouble" (tok 0) then ()
        else raise Fail (filename ^ " wrong file type")

      val n = DS.length toks - 1

      fun p i =
        Option.valOf (parseReal (tok (1 + i)))
        handle e => raise Fail ("error parsing double value " ^ Int.toString i)
    in
      Seq.tabulate p n
    end

end
