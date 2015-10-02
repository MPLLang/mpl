open Printf
open XBase
open Env
open Params

let executable_directory =
  XCmd.parse_string "executable-directory"
let virtual_run = XCmd.mem_flag "virtual-run"
let num_runs = XCmd.parse_or_default_int "num-runs" 1

(*------------------------------------------------------------------*)
(* runners *)
let serial_run executable serial_args parallel_args =
  let runner = mk_prog "./run.pl" in
  let exeargs = concat [((mk string "executable" executable) &
			   (mk string "arguments" serial_args));
			((mk string "executable" executable) &
			   (mk string "arguments" parallel_args))] in
  let executables_series = mk_list string
				   "arguments"
				   [serial_args; parallel_args] in

  let executable_directory = mk string
				"executable-directory"
				executable_directory in

  let heap_models = mk string "heap-model" "spoonhower" in
  let num_processors = mk int "number-processors" 1 in
  let fib_values = mk_list int "arguments2" [30; 31; 32; 33; 34;
					     35; 36; 37; 38; 39] in

  (* Do runs *)
  let results_file = "wsbench.results" in
  Mk_runs.(
    call [Output results_file;
	  Virtual virtual_run;
	  Runs num_runs;
	  Args (runner &
		  exeargs &
		    fib_values &
		      heap_models &
			executable_directory &
			  num_processors);]);

  if virtual_run
  then ()
  else
    begin
      let formatter =
	Env.format(
	    Env.([("arguments2",
		   Format_custom (fun n -> sprintf "%s" n))])) in

      (* Chart results *)
      let charts1 =
	Mk_bar_plot.(get_charts (
			 [Bar_plot_opt Bar_plot.(
			     [X_titles_dir Horizontal;
			      Y_axis [Axis.Lower (Some 0.)] ]);
			  Formatter formatter;
			  Charts mk_unit;
			  Series executables_series;
			  Y_label "Runtime (ms)";
			  X_label "Fib Value";
			  X fib_values;
			  Input "wsbench.results";
			 ]
			 @ (y_as_mean "runtime")
		       )) in
      Chart.build "ws_barplots.pdf" charts1
    end

(*-------------------------------------------------------------------*)

let _ = serial_run "fib/fib_sml.ws6" "serial" "parallel"
