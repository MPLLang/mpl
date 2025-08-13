(* signature SPID = ID *)
signature SPID = 
   sig
      include ID
      val getTokenSplitPolicy: t -> Word32.word
      val setTokenSplitPolicy: t * Word32.word -> unit
   end
