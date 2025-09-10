(*
 * The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * Contributed by Paolo Ribeca
 *
 * (Very loosely based on previous version Ocaml #3,
 *  which had been contributed by
 *   Christophe TROESTLER
 *  and enhanced by
 *   Christian Szegedy and Yaron Minsky)
 *
 * fix compile errors by using Bytes instead of String, by Tony Tavener
 *)

module T = Domainslib.Task

let niter = 50
let limit = 4.

let num_domains = int_of_string (Array.get Sys.argv 1)
let w = int_of_string (Array.get Sys.argv 2)

let worker w h_lo h_hi =
  let buf =
    Bytes.create ((w / 8 + (if w mod 8 > 0 then 1 else 0)) * (h_hi - h_lo))
  and ptr = ref 0 in
  let fw = float w /. 2. in
  let fh = fw in
  let red_w = w - 1 and red_h_hi = h_hi - 1 and byte = ref 0 in
  for y = h_lo to red_h_hi do
    let ci = float y /. fh -. 1. in
    for x = 0 to red_w do
      let cr = float x /. fw -. 1.5
      and zr = ref 0. and zi = ref 0. and trmti = ref 0. and n = ref 0 in
      begin try
        while true do
          Domain.Sync.poll ();
          zi := 2. *. !zr *. !zi +. ci;
          zr := !trmti +. cr;
          let tr = !zr *. !zr and ti = !zi *. !zi in
          if tr +. ti > limit then begin
            byte := !byte lsl 1;
            raise Exit
          end else if incr n; !n = niter then begin
            byte := (!byte lsl 1) lor 0x01;
            raise Exit
          end else
            trmti := tr -. ti
        done
      with Exit -> ()
      end;
      if x mod 8 = 7 then begin
        Bytes.set buf !ptr (Char.chr !byte);
        incr ptr;
        byte := 0
      end
    done;
    let rem = w mod 8 in
    if rem != 0 then begin
      Bytes.set buf !ptr (Char.chr (!byte lsl (8 - rem)));
      incr ptr;
      byte := 0
    end
  done;
  buf

let _ =
  let pool = T.setup_pool ~num_domains:(num_domains - 1) in
  let rows = w / num_domains and rem = w mod num_domains in
  Printf.printf "P4\n%i %i\n%!" w w;
  let work i () =
    worker w (i * rows + min i rem) ((i+1) * rows + min (i+1) rem)
  in
  let doms =
    Array.init (num_domains - 1) (fun i -> T.async pool (work i)) in
  let r = work (num_domains-1) () in
  Array.iter (fun d -> Printf.printf "%a%!" output_bytes (T.await pool d)) doms;
  Printf.printf "%a%!" output_bytes r;
  T.teardown_pool pool
