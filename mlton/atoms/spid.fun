structure Spid : SPID =
struct
  structure S = Id (val noname = "spid")
  open S

  val tokenSplitPolicies = ref [] : (t * Word32.word) list ref
  
  (* MVP, potentially very slow implementation *)
  (* push a new spid-token policy pair to the front of tokenSplitPolicies *)
  fun setTokenSplitPolicy (id: t, pol: Word32.word): unit =
    tokenSplitPolicies := (id, pol) :: !tokenSplitPolicies
  (* search for a matching spid, then return the token policy attached *)
  fun getTokenSplitPolicyOpt (id: t): Word32.word option =
    let fun loop (ids: (t * Word32.word) list): Word32.word option =
            case ids of
                [] => NONE
              | ((id', pol) :: ids') => if equals (id, id') then
                                          SOME pol
                                        else
                                          loop ids'
    in
      loop (!tokenSplitPolicies)
    end

  fun getTokenSplitPolicy (id: t): Word32.word =
      case getTokenSplitPolicyOpt id of
          NONE => Error.bug ("Spid.getTokenSplitPolicy: none found for " ^ toString id)
        | SOME pol => pol
  

  (* override spid.new() to copy token policies *)
  val new =
      (fn i => let val i' = new i
                   val () = case getTokenSplitPolicyOpt i of
                                NONE => ()
                              | SOME pol => setTokenSplitPolicy (i', pol)
               in
                 i'
               end): t -> t (* new id with the same originalName *)

  (*type TokenPolicy = Word32.word
  val tokenPolicyPlist : t -> PropertyList.t = S.plist
  val {get = getTSP: t -> TokenPolicy,
       set = setTSP: t * TokenPolicy -> unit, ...} =
      Property.getSet (S.plist, Property.initRaise ("Spid.tokenPolicy", S.layout))
  val getTokenSplitPolicy = fn x => (print ("getTokenSplitPolicy " ^ toString x ^ " => " ^ Word32.toString (getTSP x) ^ "\n"); getTSP x)
  val setTokenSplitPolicy = fn x => (print ("setTokenSplitPolicy (" ^ toString (#1 x) ^ ", " ^ Word32.toString (#2 x) ^ ")\n"); setTSP x)*)
end
