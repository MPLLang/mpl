structure Color =
struct
  type channel = Word8.word
  type pixel = {red: channel, green: channel, blue: channel}

  val white: pixel = {red=0w255, green=0w255, blue=0w255}
  val black: pixel = {red=0w0, green=0w0, blue=0w0}
  val red: pixel = {red=0w255, green=0w0, blue=0w0}
  val blue: pixel = {red=0w0, green=0w0, blue=0w255}

  fun packInt ({red, green, blue}: pixel) =
    let
      fun b x = Word8.toInt x
    in
      (65536 * b red) + (256 * b green) + b blue
    end

  fun compare (p1, p2) = Int.compare (packInt p1, packInt p2)

  fun equal (p1, p2) = (compare (p1, p2) = EQUAL)

  (* Based on the "low-cost approximation" given at
   * https://www.compuphase.com/cmetric.htm *)
  fun approxHumanPerceptionDistance
      ({red=r1, green=g1, blue=b1}, {red=r2, green=g2, blue=b2}) =
    let
      fun c x = Word8.toInt x
      val (r1, g1, b1, r2, g2, b2) = (c r1, c g1, c b1, c r2, c g2, c b2)

      val rmean = (r1 + r2) div 2
      val r = r1 - r2
      val g = g1 - g2
      val b = b1 - b2
    in
      Math.sqrt (Real.fromInt
        ((((512+rmean)*r*r) div 256) + 4*g*g + (((767-rmean)*b*b) div 256)))
    end

  local
    fun c x = Word8.toInt x
    fun sq x = x * x
  in
  fun sqDistance ({red=r1, green=g1, blue=b1}, {red=r2, green=g2, blue=b2}) =
    sq (c r2 - c r1) + sq (c g2 - c g1) + sq (c b2 - c b1)
  end

  fun distance (p1, p2) = Math.sqrt (Real.fromInt (sqDistance (p1, p2)))

  fun to256 rchannel =
    Word8.fromInt (Real.ceil (rchannel * 255.0))

  fun from256 channel =
    Real.fromInt (Word8.toInt channel) / 255.0


  (** hue in range [0,360)
    *   0-------60-------120-------180-------240-------300-------360
    *   red   yellow    green      cyan      blue      purple    red
    *
    * saturation in range [0,1]
    *    0--------------1
    *   grayscale   vibrant
    *
    * value in range [0,1]
    *    0--------------1
    *   dark           light
    *)
  fun hsv {h: real, s: real, v: real}: pixel =
    let
      val H = h
      val S = s
      val V = v

      (* from https://en.wikipedia.org/wiki/HSL_and_HSV#HSV_to_RGB *)
      val C = V * S
      val H' = H / 60.0
      val X = C * (1.0 - Real.abs (Real.rem (H', 2.0) - 1.0))

      val (R1, G1, B1) =
        if H' < 1.0 then      (C,   X,   0.0)
        else if H' < 2.0 then (X,   C,   0.0)
        else if H' < 3.0 then (0.0, C,   X)
        else if H' < 4.0 then (0.0, X,   C)
        else if H' < 5.0 then (X,   0.0, C)
        else                  (C,   0.0, X)

      val m = V - C
    in
      {red = to256 (R1 + m), green = to256 (G1 + m), blue = to256 (B1 + m)}
    end

  (* ======================================================================= *)

  type color = {red: real, green: real, blue: real, alpha: real}

  (** hue in range [0,360)
    *   0-------60-------120-------180-------240-------300-------360
    *   red   yellow    green      cyan      blue      purple    red
    *
    * saturation in range [0,1]
    *    0--------------1
    *   grayscale   vibrant
    *
    * value in range [0,1]
    *    0--------------1
    *   dark           light
    *
    * alpha in range [0,1]
    *    0--------------1
    *   transparent    opaque
    *)
  fun hsva {h: real, s: real, v: real, a: real}: color =
    let
      val H = h
      val S = s
      val V = v

      (* from https://en.wikipedia.org/wiki/HSL_and_HSV#HSV_to_RGB *)
      val C = V * S
      val H' = H / 60.0
      val X = C * (1.0 - Real.abs (Real.rem (H', 2.0) - 1.0))

      val (R1, G1, B1) =
        if H' < 1.0 then      (C,   X,   0.0)
        else if H' < 2.0 then (X,   C,   0.0)
        else if H' < 3.0 then (0.0, C,   X)
        else if H' < 4.0 then (0.0, X,   C)
        else if H' < 5.0 then (X,   0.0, C)
        else                  (C,   0.0, X)

      val m = V - C
    in
      {red = R1 + m, green = G1 + m, blue = B1 + m, alpha = a}
    end

  fun overlayColor {fg: color, bg: color} =
    let
      val alpha = 1.0 - (1.0 - #alpha fg) * (1.0 - #alpha bg)
    in
      if alpha < 1e~6 then
        (* essentially fully transparent, color doesn't matter *)
        { red = 0.0, green = 0.0, blue = 0.0, alpha = alpha }
      else
        let
          val red =
            #red fg * #alpha fg / alpha
            + #red bg * #alpha bg * (1.0 - #alpha fg) / alpha

          val green =
            #green fg * #alpha fg / alpha
            + #green bg * #alpha bg * (1.0 - #alpha fg) / alpha

          val blue =
            #blue fg * #alpha fg / alpha
            + #blue bg * #alpha bg * (1.0 - #alpha fg) / alpha
        in
          {red=red, green=green, blue=blue, alpha=alpha}
        end
    end

  (** Converts a color with transparency to a concrete pixel. Assumes white
    * background.
    *)
  fun colorToPixel (color: color) : pixel =
    let
      val white = {red = 1.0, blue = 1.0, green = 1.0, alpha = 1.0}
      (* alpha will be 1 *)
      val {red, green, blue, ...} = overlayColor {fg = color, bg = white}
    in
      { red = to256 red, green = to256 green, blue = to256 blue}
    end

  fun pixelToColor {red, green, blue} =
    {red = from256 red, green = from256 green, blue = from256 blue, alpha = 1.0}

end
