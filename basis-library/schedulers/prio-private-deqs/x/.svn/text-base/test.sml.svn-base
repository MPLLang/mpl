
local val seed = ref 300
in
    fun randint () =
	let
	    val r = ((!seed * 12345) + 777) mod 31337
	in
	    seed := r;
	    r
	end

    fun randpct () =
	(Real.fromInt (randint ()) / 31337.0)

end

fun f ["gauss"] =
    let
	(* fake gaussian: ln (y / y - 1) *)
	   val d = MLX.opendisplay NONE
	   val rt = MLX.defaultrootwindow d
	       
	   val w = MLX.createsimplewindow d rt 5 5 512 512 1 0wx000000 0wx111111

	   val gc = MLX.creategc d (MLX.wd w)

	   val _ = MLX.mapwindow d w
	   val _ = MLX.raisewindow d w
	   val _ = MLX.flush d

	   val arr = Array.array(512, 0)

	   fun r 0 = ()
	     | r n = 
	       let 
		   val p = (randpct () * 0.9999) + 0.00001
(* 		   val _ = print ("0: " ^ Real.toString p ^ "\n")*)
		   val p = Math.ln (p / (1.0 - p))
(*		   val _ = print ("1: " ^ Real.toString p ^ "\n") *)
		   val i = Real.trunc (50.0 * p + 256.0)
(*		   val _ = print ("2: " ^ Int.toString i ^ "\n") *)
	       in
		   if i >= 0 andalso i < 512 then
		       Array.update(arr, i, Array.sub(arr, i) + 1)
		   else ();
		   r (n - 1)
	       end


	   fun drw 0 mx = ()
	     | drw n mx =
	       let in
		   MLX.drawpoint d (MLX.wd w) gc n (512 - 
						    Real.trunc(512.0 * 
							       (Real.fromInt (Array.sub(arr, n))) / mx));
		   drw (n - 1) mx
	       end

	   fun gmax 0 = 0
	     | gmax n = Int.max(gmax(n-1), Array.sub(arr,n))


	   fun go n =
	       let in
		   Array.modify (fn _ => 0) arr;
		   r n;
		   MLX.setforeground d gc 0wx111111;
		   MLX.fillrectangle d (MLX.wd w) gc 0 0 512 512;
		   MLX.setforeground d gc 0wxFFFFFF;
		   drw 511 (Real.fromInt (gmax 511));
		   print ("n = " ^ Int.toString n ^ "\n");
		   MLX.flush d;
		   MLX.usleep 60000;
		   go (n + 400)
	       end
    in
	go 100;
	Posix.Process.sleep (Time.fromSeconds 10);
	()
    end
  | f ["soko"] =
       let
	   val d = MLX.opendisplay NONE
	   val rt = MLX.defaultrootwindow d
	       
	   val w = MLX.createsimplewindow d rt 5 5 512 512 1 0wx000000 0wx111111
	       
	   val gc = MLX.creategc d (MLX.wd w)
	       
	   val _ = MLX.mapwindow d w
	   val _ = MLX.raisewindow d w
	   val _ = MLX.flush d

	   val _ = MLX.selectinput d w (MLX.Mask.make [MLX.Mask.exposure,
						       MLX.Mask.keyrelease,
						       MLX.Mask.keypress])

	   fun for lo hi f = if lo > hi then () else (f lo; for (lo+1) hi f)

	   datatype sokomono = Empty | Wall | Box

	   val xs = 30

	   val ys = 30

	   val manx = ref 4
	   val many = ref 6


	   val a = Array.array (xs * ys, Empty)

	   fun rnd 0 = ()
	     | rnd n = (Array.update (a, (randint () mod 30) * 30 + (randint () mod 30), 
				      if (randint () mod 2 = 1) then Wall else Box);
			rnd (n - 1))

	   val _ = rnd 60

	   fun drawtile Empty x y =
	       let in
		   MLX.setforeground d gc 0wx111133;
		   MLX.fillrectangle d (MLX.wd w) gc (x+1) (y+1) 14 14
	       end
	     | drawtile Wall x y =
	       let in
		   MLX.setforeground d gc 0wxBB33BB;
		   MLX.fillrectangle d (MLX.wd w) gc (x+1) (y+1) 14 14
	       end
	     | drawtile Box x y =
	       let in
		   MLX.setforeground d gc 0wxEE2244;
		   MLX.fillrectangle d (MLX.wd w) gc (x+4) (y+4) 8 8
	       end

	   fun drawman x y =
	       let in
		   MLX.setforeground d gc 0wxFFBB33;
		   MLX.fillrectangle d (MLX.wd w) gc (x + 4) (y + 4) 8 8;
		   MLX.setforeground d gc 0wx000000;
		   MLX.drawpoint d (MLX.wd w) gc (x + 6) (y + 5);
		   MLX.drawpoint d (MLX.wd w) gc (x + 6) (y + 6);
		   MLX.drawpoint d (MLX.wd w) gc (x + 9) (y + 5);
		   MLX.drawpoint d (MLX.wd w) gc (x + 9) (y + 6)
	       end

	   fun drawboard a manx many =
	       let
	       in
		   for 0 29
		   (fn j =>
		    for 0 29
		    (fn i =>
			drawtile (Array.sub(a, j*ys + i)) (i*16 + 4) (j*16 + 4)));

		   drawman (manx*16 + 4) (many*16 + 4)

	       end

	   fun go _ =
	       (case MLX.nextevent d of
		    (_, _, _, MLX.Key (true, _, _, _, _, _, _, _, _, _, k, _)) =>
			let 
			    val kk = Word32.toInt k
			in
			    print ("KEYEVENT! " ^ (Int.toString kk) ^ "\n");
			    case kk of
				95 => print "UP.\n"
			      | 100 => print "DOWN.\n"
			      | 97 => print "LEFT.\n"
			      | 98 => print "RIGHT.\n"
			      | _ => print "DUMB KEY.\n"
			end
		  | _ => ();
			go 0) handle _ => go 0

	   val _ = Posix.Process.sleep (Time.fromSeconds 1)

	   val _ = drawboard a (!manx) (!many)

	   val _ = go 0

       in
	   ()

       end
  | f ["draw"] = 
    let
	val d = MLX.opendisplay NONE
	val rt = MLX.defaultrootwindow d

	val w = MLX.createsimplewindow d rt 5 5 512 512 1 0wx000000 0wx111111
	    
	val gc = MLX.creategc d (MLX.wd w)
	    
	val _ = MLX.mapwindow d w
	val _ = MLX.raisewindow d w
	val _ = MLX.flush d

	val _ = Posix.Process.sleep (Time.fromSeconds 1)

	val _ = MLX.selectinput d w (MLX.Mask.make [MLX.Mask.exposure,
						    MLX.Mask.keyrelease,
						    MLX.Mask.keypress,
						    MLX.Mask.buttonpress,
						    MLX.Mask.buttonrelease,
						    MLX.Mask.pointermotion])

	fun block x y =
	    let in
		MLX.fillrectangle d (MLX.wd w) gc x y 8 8
	    end

	fun forever drw oldx oldy =
	    let in
(*		print "Calling nextevent..."; *)
		(case MLX.nextevent d of
		     (_, _, _, MLX.Key (b, _, _, _, _, _, _, _, _, _, k, _)) =>
			 let 
			     val kk = Word32.toInt k
			 in
			     MLX.setforeground d gc (if b then 0wxFFFFFF else 0wx111111);
			     print ("KEYEVENT! " ^ (Word32.toString k) ^ "\n");
			     block (32+(8*(kk mod 16))) (32+((kk div 16)*8));
			     MLX.flush d;
			     forever drw oldx oldy
			 end
		   (* window, root, subwindow, time, ptrx, ptry, ptrrx, ptrry, state, ishint, same_screen *)
		   | (_, _, _, MLX.Motion (_, _, _, _, x, y, _, _, _, _, _)) =>
			 if drw then
			     let
			     in
				 print ("MOTION! " ^ (Int.toString x) ^ ", " ^ Int.toString y ^ "\n");
				 MLX.setforeground d gc 0wxFF8899;
				 MLX.drawline d (MLX.wd w) gc oldx oldy x y;
				 MLX.flush d;
(*				 MLX.setforeground d gc 0wx9933EE;
				 MLX.fillrectangle d (MLX.wd w) gc (x-1) (y-1) 2 2;
				 MLX.flush d; *)
				 forever true x y
			     end
			 else forever false x y
		   | (_, _, _, MLX.Button (dir, _, _, _, _, x, y, _, _, _, b, _)) =>
			 let
			 in
			     MLX.drawtext d (MLX.wd w) gc NONE 20 20 "BUTTON!";

			     print ("BUTTON! " ^ Int.toString x ^ ", " ^ Int.toString y ^ " - " ^
				    (if dir then "true" else "false") ^ "\n");
			     MLX.setforeground d gc 0wx2222FF;
			     MLX.fillrectangle d (MLX.wd w) gc (x-2) (y-2) 4 4;
			     MLX.flush d;
			     forever dir x y
			 end
		   | _ => forever drw oldx oldy)
	    end handle MLX.X s => (print ("exn: " ^ s ^ "\n"); forever drw oldx oldy)
		
	fun mb () =
	    (case MLX.nextevent d of
		 (_, _, _, MLX.Motion (_, _, _, _, x, y, _, _, _, _, _)) => forever false x y
	       | _ => mb ()) handle _ => mb ()

	val _ = mb ()

	val _ = MLX.freegc d gc    
	val _ = MLX.closedisplay d
    in () end
  | f ["bounce"] =
    let
	val d = MLX.opendisplay NONE
	val rt = MLX.defaultrootwindow d
	val w = MLX.createsimplewindow d rt 5 5 524 524 1 0wx000000 0wx111111
	val gc = MLX.creategc d (MLX.wd w)
	val _ = MLX.mapwindow d w
	val _ = MLX.raisewindow d w
	val _ = MLX.setforeground d gc 0wxFFFFFF
	val _ = MLX.flush d

	fun go ((x, y, dx, dy,c)::l) ll =
	    let
		val dy = dy - 0.05;
		fun bouncein s d min max = 
		    if (s > max andalso d > 0.0) then (max, ~0.95 *  d)
		    else if (s < min andalso d < 0.0) then (min, ~0.95 * d)
			 else (s + d, d)

		val (nx, dx) = bouncein x dx 0.0 (512.0 - 6.0)
		val (ny, dy) = bouncein y dy 0.0 (512.0 - 6.0)
	    in
		MLX.setforeground d gc 0wx111111;
		MLX.fillrectangle d (MLX.wd w) gc (Real.trunc x) (512 - Real.trunc y) 8 8;
		MLX.setforeground d gc c;
		MLX.fillrectangle d (MLX.wd w) gc (Real.trunc nx) (512 - Real.trunc ny) 8 8;
		go l ((nx, ny, dx, dy, c)::ll)
	    end
	  | go nil ll = 
	    let in
		MLX.flush d;
		MLX.usleep 1000;
		go ll nil
	    end

	val l = [(20.0, 20.0, 1.7, 6.0, 0wx9922EE : Word32.word),
		 (80.0, 20.0, 1.3, 4.1, 0wx330099),
		 (20.0, 120.0, ~1.1, ~1.2, 0wx88EE33),
		 (300.0, 412.0, ~2.0, ~3.0, 0wxFFFF44)]
	    
	fun a (r1, r2, r3, r4, c) = (r1 + 10.0, r2 - 10.0, r3 * r3,  r4 * 1.31, 
				     Word32.xorb (c, 0wx9030F0))
	fun b (r1, r2, r3, r4, c) = (r2, 512.0 - r1, r4, ~r3, Word32.xorb (c, 0wxABCDEF))

	fun c (r1, r2, r3, r4, c) = (r2 * r2, r1 / 2.0, ~r4, r3 * 2.0, 
				     Word32.orb(0wxFF, Word32.<<(c,0w3)))

	val _ = go (l @ (map a l) @ (map b l)
		    @ (map c l) @ (map (c o a) l) 
		    @ (map (a o c) l)
		    @ (map (a o b) l) @ (map (b o a) l)) nil

	val _ = MLX.freegc d gc    
	val _ = MLX.closedisplay d

    in () end
  | f _ = print "On the commandline, try: gauss, soko, bounce, or draw\n"

val _ = (f (CommandLine.arguments())) handle MLX.X s => (print ("unhandled: " ^ s ^ "\n"))