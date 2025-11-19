# MPL Work-Stealing Scheduler (Spork)

The Spork scheduler implements efficient work-stealing parallelism with automatic granularity control for MPL.

## Overview

The Spork scheduler is MPL's core parallel execution engine, providing fork-join parallelism with work-stealing and hierarchical memory management. It sits in the basis library, bridging SML code and the parallel runtime system.

**Key features**:
- **Work-stealing**: ABP (Arora-Blumofe-Plaxton) deques for load balancing
- **Fork-join parallelism**: Structured parallel tasks with automatic synchronization
- **Granularity control**: Automatic grain size selection via heartbeats
- **Hierarchical memory**: Integrates with MPL's hierarchical heap GC
- **Disentanglement checking**: Prevents unsafe parallel access to mutable data

**Location**: [basis-library/schedulers/spork/](.)

## Key Concepts

### Work-Stealing

**Work-stealing model**:
- Each processor has a **deque** (double-ended queue) of tasks
- Worker **pushes** new tasks onto its own deque (bottom)
- Worker **pops** tasks from its own deque (bottom) - LIFO for cache locality
- Idle workers **steal** tasks from other workers' deques (top) - FIFO

**Benefits**:
- **Load balancing**: Idle processors automatically find work
- **Low overhead**: Successful steals are rare in practice
- **Provable bounds**: Theoretical guarantees on performance

**ABP Deque** ([DequeABP.sml](DequeABP.sml)): Lock-free work-stealing deque using Arora-Blumofe-Plaxton algorithm.

### Fork-Join Parallelism

**Fork-join structure** ([ForkJoin.sml](ForkJoin.sml)):

```sml
val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
```

**Execution**:
1. **Fork**: Create two parallel tasks (left and right)
2. **Execute**: Run left task locally, push right task to deque
3. **Join**: Wait for right task to complete (steal if necessary)
4. **Return**: Combine results from both tasks

**Spork primitive** (runtime spork):
- Compiler primitive for creating parallel tasks
- Three policies: Fair, Keep, Give (token policies for promotion)
- Integrated with GC for hierarchical heap management

### Granularity Control

**Problem**: Too fine parallelism has high overhead. Too coarse wastes parallelism.

**Solution**: **Heartbeats** ([Heartbeat.sml](Heartbeat.sml)) - dynamic granularity control:

**Heartbeat mechanism**:
- Each task has a **heartbeat counter**
- Counter decrements on each fork
- When counter reaches zero: execute sequentially (no more forking)
- Heartbeats are **transferable** (via token policies)

**Token policies**:
- **Fair**: Split heartbeats evenly between left and right
- **Keep**: Parent keeps most heartbeats
- **Give**: Parent gives heartbeats to child

**Automatic selection**: Runtime chooses policy based on heap depth and promotion requirements.

### Hierarchical Memory Integration

**Hierarchical heaps** (HH):
- Each parallel task gets its own heap
- Heaps organized in parent-child tree
- Garbage collection can be local (per-heap)

**Spork integration**:
- Fork creates new child heap
- Join merges child heap into parent (if disentangled)
- Entanglement check prevents unsafe merging

**GC joinpoints** ([Scheduler.sml](Scheduler.sml)):
```sml
datatype gc_joinpoint =
  GCJ of {gcTaskData: gctask_data option,
          tidRight: Word64.word}
```

Tracks GC-related information for joining parallel tasks.

### Disentanglement Checking

**Disentanglement**: Two heaps are disentangled if they don't share mutable references.

**Checking**:
- Performed at join time
- If disentangled: fast join (merge heaps)
- If entangled: slow join (keep heaps separate, promote to shared heap)

**Depth limit**: Disentanglement checking disabled beyond certain depth (for performance).

## File Organization

| File | Lines | Purpose |
|------|-------|---------|
| [Scheduler.sml](Scheduler.sml) | ~1,200 | Main scheduler implementation |
| [ForkJoin.sml](ForkJoin.sml) | ~400 | ForkJoin structure and parallel primitives |
| [DequeABP.sml](DequeABP.sml) | ~140 | ABP work-stealing deque |
| [Heartbeat.sml](Heartbeat.sml) | ~60 | Heartbeat (granularity control) |
| [UnrolledLoops.sml](UnrolledLoops.sml) | ~300 | Loop unrolling implementations |
| [LoopIndex.sml](LoopIndex.sml) | ~40 | Loop index types (Word8/16/32/64/Int) |
| [Result.sml](Result.sml) | ~15 | Result type for exception handling |
| [CumulativePerProcTimer.sml](CumulativePerProcTimer.sml) | ~50 | Per-processor timing |
| [SimpleRandom.sml](SimpleRandom.sml) | ~30 | Random number generation for stealing |
| [Universal.sml](Universal.sml) | ~10 | Universal type for GC integration |

## Scheduler Structure

### Main Scheduler ([Scheduler.sml](Scheduler.sml))

**Core data structures**:

```sml
(* Joinpoint for fork-join *)
datatype 'a joinpoint =
  J of {
    leftSideThread: Thread.t,
    rightSideThread: Thread.t option ref,
    rightSideResult: 'a Result.t option ref,
    incounter: int ref,
    tidRight: Word64.word,
    spareHeartbeatsGiven: Heartbeat.token_count,
    tokenPolicy: TokenPolicy,
    gcj: gc_joinpoint option
  }

(* Token policies for promotion *)
datatype TokenPolicy =
    TokenPolicyFair   (* split evenly *)
  | TokenPolicyKeep   (* parent keeps *)
  | TokenPolicyGive   (* give to child *)
```

**Spork primitives** (from compiler):

```sml
val primSporkFair: ('a -> 'b) * (unit * 'c -> 'd) *
                   ('b -> 'e) * ('b * 'c -> 'e) *
                   (exn -> 'e) * (exn * 'c -> 'e)
                   -> 'e

val primSporkKeep: (* same signature *)
val primSporkGive: (* same signature *)
```

**Parameters**:
- `body`: Left-side task
- `spwn`: Right-side task (continuation that receives spawned data)
- `seq`: Sequential continuation (left finished before right spawned)
- `sync`: Synchronization continuation (join with right result)
- `exnseq`: Exception sequential continuation
- `exnsync`: Exception synchronization continuation

### ForkJoin Structure ([ForkJoin.sml](ForkJoin.sml))

**Core primitives**:

```sml
val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b

val parfor: int -> (int * int) -> (int -> unit) -> unit
  (* Parallel for with manual grain size *)

val parform: (int * int) -> (int -> unit) -> unit
  (* Parallel for with automatic grain size *)

val reducem: ('a * 'a -> 'a) -> 'a -> (int * int) -> (int -> 'a) -> 'a
  (* Parallel reduction with automatic grain size *)

val seqLoop: (int * int) -> (int -> unit) -> unit
  (* Sequential loop with automatic unrolling *)

val seqReduce: ('a * 'a -> 'a) -> 'a -> (int * int) -> (int -> 'a) -> 'a
  (* Sequential reduction with automatic unrolling *)

val alloc: int -> 'a array
  (* Allocate uninitialized array *)
```

**Parallel for loop** (managed):
```sml
fun parform (lo, hi) f =
  let
    fun loop (lo, hi) =
      if hi - lo <= GRAIN then
        seqLoop (lo, hi) f
      else
        let val mid = lo + (hi - lo) div 2
        in par (fn () => loop (lo, mid),
                fn () => loop (mid, hi))
           ; ()
        end
  in
    loop (lo, hi)
  end
```

**Grain size**: Automatically determined by heartbeat mechanism (not shown in code above).

### Work-Stealing Deque ([DequeABP.sml](DequeABP.sml))

**ABP deque** (Arora-Blumofe-Plaxton):

```sml
type 'a deque

val new: unit -> 'a deque
val pushBottom: 'a deque -> 'a -> unit
val popBottom: 'a deque -> 'a option
val popTop: 'a deque -> 'a option
```

**Lock-free implementation**:
- Uses atomic operations (CAS)
- Bottom (owner) uses relaxed atomic operations
- Top (thieves) use CAS for synchronization

**Circular array**:
- Fixed-size array (typically 1024 or 2048 elements)
- Wrap-around using modulo arithmetic
- Grows when full (in some implementations)

### Heartbeat Mechanism ([Heartbeat.sml](Heartbeat.sml))

**Heartbeat structure**:

```sml
type token_count = int

val getHeartbeatsRemaining: unit -> token_count
val takeHeartbeats: token_count -> unit
val giveHeartbeats: token_count -> unit
```

**Token management**:
- Each task starts with a heartbeat budget
- `takeHeartbeats n`: Decrement budget by n
- `giveHeartbeats n`: Increment budget by n (for stolen tasks)
- When budget exhausted: switch to sequential execution

**Policies**:
- **Fair**: Split heartbeats 50/50
- **Keep**: Parent keeps 90%, child gets 10%
- **Give**: Parent gives 90%, keeps 10%

### Loop Unrolling ([UnrolledLoops.sml](UnrolledLoops.sml))

**Unrolled loop implementations**:

```sml
functor UnrolledLoops (Index: sig type t ... end) =
struct
  (* 8-way unrolled loop *)
  fun seqLoopUnrolled8 (lo, hi) f =
    let
      val numBlocks = (hi - lo) div 8
      fun blockLoop i =
        if i >= numBlocks then ()
        else (f (lo + i*8);
              f (lo + i*8+1);
              ...
              f (lo + i*8+7);
              blockLoop (i+1))
      (* Handle remainder *)
      fun remainderLoop i =
        if i >= hi then ()
        else (f i; remainderLoop (i+1))
    in
      blockLoop 0;
      remainderLoop (lo + numBlocks*8)
    end

  (* Regular sequential loop *)
  fun seqLoopRegular (lo, hi) f =
    let
      fun loop i =
        if i >= hi then ()
        else (f i; loop (i+1))
    in
      loop lo
    end
end
```

**Compile-time decision**: `loop_choose` primitive chooses between unrolled and regular based on loop body size.

## Execution Flow

### Fork-Join Execution

**Example**: `par (f, g)`

1. **Fork**:
   - Create joinpoint J
   - Allocate new heap for right task
   - Push right task (g) onto deque
   - Record right task info in joinpoint

2. **Execute Left**:
   - Run f() immediately
   - Store result in joinpoint

3. **Join**:
   - Check if right task completed:
     - **Fast path**: Right already done → return results immediately
     - **Slow path**: Right not done → help execute or steal other work

4. **Synchronize**:
   - Wait for right task to complete
   - Merge or promote heaps based on disentanglement
   - Return combined result

### Work-Stealing Flow

**Worker's main loop** ([Scheduler.sml](Scheduler.sml)):

```sml
fun workerLoop () =
  case popBottom myDeque of
    SOME task =>
      (runTask task; workerLoop ())
  | NONE =>
      (* No local work, try stealing *)
      case trySteal () of
        SOME stolenTask =>
          (runTask stolenTask; workerLoop ())
      | NONE =>
          (* No work anywhere, sleep *)
          sleep ()
```

**Stealing**:
1. Choose random victim processor
2. Try `popTop` from victim's deque
3. If successful: run stolen task
4. If failed: try another victim or sleep

### Grain Size Selection

**Automatic grain size** ([ForkJoin.sml](ForkJoin.sml)):

1. **Check heartbeats**:
   - If heartbeats > 0: continue forking
   - If heartbeats = 0: switch to sequential

2. **Token policy**:
   - Runtime chooses policy based on GC state
   - Fair: Good load balance
   - Keep: Parent keeps forking ability
   - Give: Child gets forking ability

3. **Split work**:
   - Divide range in half
   - Allocate heartbeats to each half
   - Fork right, execute left

**Example**:
```sml
fun parfor grain (lo, hi) f =
  let
    fun loop (lo, hi) =
      if hi - lo <= grain then
        ForkJoin.seqLoop (lo, hi) f
      else
        let val mid = lo + (hi - lo) div 2
        in
          ForkJoin.par (
            fn () => loop (lo, mid),
            fn () => loop (mid, hi)
          )
          ; ()
        end
  in
    loop (lo, hi)
  end
```

With `parform`, grain is automatically determined by heartbeats.

## Runtime Integration

### Compiler Primitives

**Spork primitives** (handled in [closure-convert](../../mlton/closure-convert/) and [SSA](../../mlton/ssa/)):

- `spork_fair`: Fork with fair token policy
- `spork_keep`: Fork with keep policy
- `spork_give`: Fork with give policy
- `spork_forkThreadAndSetData`: Low-level thread fork
- `spork_choose`: Compile-time decision for spork vs sequential

**Loop primitives**:

- `loop_choose`: Compile-time decision for unrolled vs regular loop

### GC Integration

**Thread fork**:
```sml
val primForkThreadAndSetData:
  Thread.t * 'a -> Thread.p
```

Creates new thread with associated heap.

**Heap operations**:
```sml
structure HH = MLton.Thread.HierarchicalHeap

val splitHeap: Thread.t -> unit
val mergeHeap: Thread.t -> unit
val promoteChunks: Thread.t -> unit
```

**Disentanglement**:
```sml
structure DE = MLton.Thread.Disentanglement

val decheckMaxDepth: unit -> int option
val isDisentangled: Thread.t * Thread.t -> bool
```

### Profiling and Tracing

**GC tracing hooks**:

```sml
val traceSchedIdleEnter: gcstate -> unit
val traceSchedIdleLeave: gcstate -> unit
val traceSchedWorkEnter: gcstate -> unit
val traceSchedWorkLeave: gcstate -> unit
val traceSchedSleepEnter: gcstate -> unit
val traceSchedSleepLeave: gcstate -> unit
val traceSchedSpawn: gcstate -> unit
val traceSchedJoin: gcstate -> unit
val traceSchedJoinFast: gcstate -> unit
```

Used for performance analysis and debugging.

## Development Guide

### Understanding the Scheduler

To understand the Spork scheduler:

1. **Read ForkJoin.sml** - Start with high-level parallel primitives
2. **Study Scheduler.sml** - Understand fork-join implementation
3. **Examine DequeABP.sml** - Learn work-stealing deque
4. **Read Heartbeat.sml** - Understand granularity control
5. **Trace execution** - Follow `par` through fork → execute → join

### Modifying the Scheduler

**Adding new parallel primitives**:

1. **Add to ForkJoin.sml**: Implement in terms of existing primitives
2. **Test**: Write benchmark to verify correctness and performance
3. **Document**: Add examples and usage guide

**Changing granularity control**:

1. **Modify Heartbeat.sml**: Adjust token counts or policies
2. **Tune thresholds**: Experiment with heartbeat budgets
3. **Measure**: Profile to ensure improvement

**Optimizing work-stealing**:

1. **Tune deque size**: Adjust DequeABP capacity
2. **Change stealing strategy**: Modify victim selection
3. **Profile**: Measure steal success rate and overhead


### Example: Parallel Sum

```sml
fun sum arr =
  let
    val n = Array.length arr
    fun loop (lo, hi) =
      if hi - lo = 0 then 0
      else if hi - lo = 1 then Array.sub (arr, lo)
      else
        let
          val mid = lo + (hi - lo) div 2
          val (left, right) =
            ForkJoin.par (
              fn () => loop (lo, mid),
              fn () => loop (mid, hi)
            )
        in
          left + right
        end
  in
    loop (0, n)
  end
```

**With automatic grain size**:
```sml
fun sum arr =
  ForkJoin.reducem (op+) 0 (0, Array.length arr)
    (fn i => Array.sub (arr, i))
```

### Example: Parallel Map

```sml
fun map f arr =
  let
    val n = Array.length arr
    val result = ForkJoin.alloc n
  in
    ForkJoin.parform (0, n) (fn i =>
      Array.update (result, i, f (Array.sub (arr, i))))
    ; result
  end
```

### Example: Parallel Quicksort

```sml
fun quicksort arr =
  let
    val n = Array.length arr

    fun sort (lo, hi) =
      if hi - lo <= 1 then ()
      else
        let
          val pivot = Array.sub (arr, lo)
          val mid = partition (lo, hi, pivot)
          val _ =
            if hi - lo < THRESHOLD then
              (sort (lo, mid); sort (mid+1, hi))
            else
              ForkJoin.par (
                fn () => sort (lo, mid),
                fn () => sort (mid+1, hi)
              )
        in
          ()
        end
  in
    sort (0, n)
  end
```

## See Also

- [Runtime GC](../../../runtime/gc/) - Hierarchical heap implementation
- [Compiler SSA](../../../mlton/ssa/) - Handles spork primitives
- [Closure Convert](../../../mlton/closure-convert/) - Compiles to SSA with spork
- [CLAUDE.md](../../../CLAUDE.md) - MPL parallel programming overview

## References

- **Work-Stealing**: "The Data Locality of Work Stealing" (Acar et al.)
- **ABP Deques**: "Thread Scheduling for Multiprogrammed Multiprocessors" (Arora, Blumofe, Plaxton)
- **Fork-Join**: "Cilk: An Efficient Multithreaded Runtime System" (Blumofe et al.)
- **Hierarchical Memory**: "Hierarchical Memory Management for Parallel Programs" (Acar et al.)
- **Disentanglement**: "Provably Space-Efficient Parallel Functional Programming" (Acar et al.)
