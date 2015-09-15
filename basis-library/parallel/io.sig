signature MLTON_PARALLEL_IO =
sig

val input1 : TextIO.StreamIO.instream -> (char * TextIO.StreamIO.instream) option

end
