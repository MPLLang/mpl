structure StateKey : ORD_KEY =
struct
type State.state
val compare = State.compare
end

structure StateSet : ORD_SET = SplaySetFn (StateKey)
