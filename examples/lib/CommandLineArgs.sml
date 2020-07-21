structure CommandLineArgs :
sig
  (* each takes a key K and a default value D, looks for -K V in the
   * command-line arguments, and returns V if it finds it, or D otherwise. *)
  val parseString : string -> string -> string
  val parseInt : string -> int -> int
  val parseBool : string -> bool -> bool

  (* parseFlag K returns true if --K given on command-line *)
  val parseFlag : string -> bool

  val positional : unit -> string list
end =
struct

  fun die msg =
    ( TextIO.output (TextIO.stdErr, msg ^ "\n")
    ; TextIO.flushOut TextIO.stdErr
    ; OS.Process.exit OS.Process.failure
    )

  fun positional () =
    List.filter (not o String.isPrefix "-") (CommandLine.arguments ())

  fun search key args =
    case args of
      [] => NONE
    | x :: args' =>
        if key = x
        then SOME args'
        else search key args'

  fun parseString key default =
    case search ("-" ^ key) (CommandLine.arguments ()) of
      NONE => default
    | SOME [] => die ("Missing argument of \"-" ^ key ^ "\" ")
    | SOME (s :: _) => s

  fun parseInt key default =
    case search ("-" ^ key) (CommandLine.arguments ()) of
      NONE => default
    | SOME [] => die ("Missing argument of \"-" ^ key ^ "\" ")
    | SOME (s :: _) =>
        case Int.fromString s of
          NONE => die ("Cannot parse integer from \"-" ^ key ^ " " ^ s ^ "\"")
        | SOME x => x

  fun parseBool key default =
    case search ("-" ^ key) (CommandLine.arguments ()) of
      NONE => default
    | SOME [] => die ("Missing argument of \"-" ^ key ^ "\" ")
    | SOME ("true" :: _) => true
    | SOME ("false" :: _) => false
    | SOME (s :: _) => die ("Cannot parse bool from \"-" ^ key ^ " " ^ s ^ "\"")

  fun parseFlag key =
    case search ("--" ^ key) (CommandLine.arguments ()) of
      NONE => false
    | SOME _ => true

end
