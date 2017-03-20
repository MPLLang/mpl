structure Log :>
sig
  datatype label =
    EnterLaunch
  | ExitLaunch
  | EnterAlgo
  | ExitAlgo
  | EnterWait
  | ExitWait
  | Communicate
  | Interrupt
  | AlgoPhase
  | LocalityStart of Int64.int
  | LocalityStop of Int64.int
  | Other

  (* (time (ms), processor number, event label). Time in microseconds *)
  type event = Int64.int * Int64.int * label

  (* `writeEvents` (path, es) writes the event list `es` out to the file
   * indicated by `path`. The event which occurred earliest must be an
   * EnterAlgo, and the latest must be an ExitAlgo. Each sublist corresponding
   * to a single processor must be sorted by time, but not the whole list.
   * (I.e. one way to construct an event list is to have each processor
   * locally construct its own event list sorted by time, and then append
   * all of them.) *)
  val writeEvents : string * (event list) -> unit
end =
struct

  datatype label =
    EnterLaunch
  | ExitLaunch
  | EnterAlgo
  | ExitAlgo
  | EnterWait
  | ExitWait
  | Communicate
  | Interrupt
  | AlgoPhase
  | LocalityStart of Int64.int
  | LocalityStop of Int64.int
  | Other

  type event = Int64.int * Int64.int * label

  fun labelCode label : Int64.int =
    case label of
      EnterLaunch     => 0
    | ExitLaunch      => 1
    | EnterAlgo       => 2
    | ExitAlgo        => 3
    | EnterWait       => 4
    | ExitWait        => 5
    | Communicate     => 6
    | Interrupt       => 7
    | AlgoPhase       => 8
    | LocalityStart _ => 9
    | LocalityStop _  => 10
    | Other           => 100

  fun pow (x : Int64.int, e : int) : Int64.int =
    if e = 0 then 1 else Int64.* (x, pow (x, e-1))

  fun int64ToBytes (x : Int64.int) =
    List.tabulate (8, fn i =>
      let val x' = Int64.div (x, pow (256, i))
      in Word8.fromLargeInt (Int64.toLarge (Int64.mod (x', 256)))
      end)

  fun labelToBytes label =
    (int64ToBytes (labelCode label) @
    (case label of
      LocalityStart x => int64ToBytes x
    | LocalityStop x => int64ToBytes x
    | _ => []))

  fun eventToBytes (time, proc, label) =
    List.concat [int64ToBytes time, int64ToBytes proc, labelToBytes label]

  fun writeEvents (filepath, events) =
    let val file = BinIO.openOut filepath
        fun output event =
          BinIO.output (file, Word8Vector.fromList (eventToBytes event))
    in ( List.app output events
       ; BinIO.closeOut file
       )
    end

  val example : event list =
    [ (100, 0, EnterAlgo)
    , (150, 0, EnterWait)
    , (120, 1, EnterWait)
    , (200, 0, ExitWait)
    , (300, 0, ExitAlgo)
    , (150, 1, ExitWait)
    ]
end
