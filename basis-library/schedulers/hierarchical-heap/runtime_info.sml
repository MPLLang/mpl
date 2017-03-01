structure RuntimeInfo :> RUNTIME_INFO =
struct
    type t = Word8Array.array

    val size: int =
        Word.toInt((_import "HeapManagement_getRuntimeInfoSize"
                            runtime private: unit -> word;) ())

    (* RAM_NOTE: DRY from basic.sml *)
    fun die message =
        (TextIO.output (TextIO.stdErr, message);
         TextIO.flushOut TextIO.stdErr;
         MLtonProcess.exit MLtonProcess.Status.failure)

    local
        val initialize = _import "HeapManagement_initializeRuntimeInfo" runtime private: Word8Array.array -> unit;
    in
        val new: unit -> t =
         fn () =>
            let
                val runtimeInfo = Word8Array.array (size, Word8.fromInt(0))
            in
                initialize runtimeInfo;
                runtimeInfo
            end
    end

    val get: unit -> Word8Array.array = _import "HeapManagement_getRuntimeInfo" runtime private: unit -> Word8Array.array;

    val set: Word8Array.array -> unit = _import "HeapManagement_setRuntimeInfo" runtime private: Word8Array.array -> unit;
end
