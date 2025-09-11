structure Heartbeat :>
sig
  type token_count

  (* interval between heartbeats in microseconds *)
  val interval : LargeInt.int

  (* tokens given out at each heartbeat*)
  val tokensPerBeat : token_count

  val relayerThreshold : int

  (* tokens in spare token pool *)
  val currentSpare : unit -> token_count

  (* consume tokens from spare token pool. will panic if insufficient funds *)
  val consumeSpare : token_count -> unit

  (* add tokens to spare token pool *)
  val addSpare : token_count -> token_count

  (* default cost of of spawning a thread *)
  val spawnCost : token_count

  (* half of current spare token count *)
  val halfOfCurrent : unit -> token_count

  val zero : token_count

  val enoughToSpawn : unit -> bool
end =
struct
  type token_count = Word32.word

  type gcstate = MLton.Pointer.t
  val gcstate = _prim "GC_state": unit -> gcstate;
  val getHeartbeatMicroseconds = _import "GC_getHeartbeatMicroseconds" runtime private: gcstate -> Word32.word;
  val getTokensPerHeartbeat =
    _import "GC_getHeartbeatTokens" runtime private: gcstate -> token_count;
  val tryConsumeSpare =
    _import "GC_tryConsumeSpareHeartbeats" runtime private: gcstate * token_count -> bool;
  val addSpareFFI =
    _import "GC_addSpareHeartbeats" runtime private: gcstate * Word32.word -> Word32.word;
  val getRelayerThreshold =
    _import "GC_getHeartbeatRelayerThreshold" runtime private: gcstate -> Word32.word;

  fun die strfn =
    ( print (Int.toString (MLton.Parallel.processorNumber ()) ^ ": " ^ strfn () ^ "\n")
    ; OS.Process.exit OS.Process.failure
    )

  val interval = (LargeInt.fromInt o Word32.toInt o getHeartbeatMicroseconds o gcstate) ()

  val relayerThreshold = (Word32.toInt o getRelayerThreshold o gcstate) ()

  val currentSpare = _prim "Heartbeat_tokens": unit -> Word32.word;

  val tokensPerBeat = getTokensPerHeartbeat (gcstate ())

  fun consumeSpare tokens =
    if tryConsumeSpare (gcstate (), tokens) then ()
    else
      die (fn _ => "tried to consume " ^ Word32.toString tokens ^ " tokens, but didn't have enouth")

  fun addSpare tokens =
    addSpareFFI (gcstate (), tokens)

  val spawnCost = 0w1
  val zero = 0w0

  fun halfOfCurrent () =
    Word32.>> (currentSpare (), 0w1)

  fun __inline_always__ enoughToSpawn () =
    __inline_always__ currentSpare () >= spawnCost
end
