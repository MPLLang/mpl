structure Spid : SPID =
struct
  structure S = Id (val noname = "spid")
  open S

  val tokenSplitPolicies = ref [] : (S.t * Word32.word) list ref
  
  (* MVP, potentially very slow implementation *)
  (* push a new spid-token policy pair to the front of tokenSplitPolicies *)
  fun setTokenSplitPolicy (id: S.t, pol: Word32.word): unit =
    tokenSplitPolicies := (id, pol) :: !tokenSplitPolicies
  (* search for a matching spid, then return the token policy attached *)
  fun getTokenSplitPolicy (id: S.t): Word32.word =
    let fun loop (ids: (S.t * Word32.word) list) =
            case ids of
                [] => Error.bug ("Spid.getTokenSplitPolicy: none found for " ^ S.toString id)
              | ((id', pol) :: ids') => if S.equals (id, id') then
                                          pol
                                        else
                                          loop ids'
    in
      loop (!tokenSplitPolicies)
    end
  

  (*type TokenPolicy = Word32.word
  val tokenPolicyPlist : t -> PropertyList.t = S.plist
  val {get = getTSP: t -> TokenPolicy,
       set = setTSP: t * TokenPolicy -> unit, ...} =
      Property.getSet (S.plist, Property.initRaise ("Spid.tokenPolicy", S.layout))
  val getTokenSplitPolicy = fn x => (print ("getTokenSplitPolicy " ^ S.toString x ^ " => " ^ Word32.toString (getTSP x) ^ "\n"); getTSP x)
  val setTokenSplitPolicy = fn x => (print ("setTokenSplitPolicy (" ^ S.toString (#1 x) ^ ", " ^ Word32.toString (#2 x) ^ ")\n"); setTSP x)*)
end
