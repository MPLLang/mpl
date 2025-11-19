# Runtime Basis Library FFI

The `runtime/basis/` directory contains C implementations of foreign function interface (FFI) bindings for the Standard ML Basis Library and MLton/MPL extensions.

**Total Files**: ~118 C source files organized by module

## Overview

These files implement the "primitive" operations that the basis library (written in SML) calls into. Each SML primitive declaration in the basis library corresponds to a C function here.

The interface between ML and C is defined in [../basis-ffi.h](../basis-ffi.h).

## Top-Level Files

### Core Utilities

- **[coerce.c](coerce.c)** / **[coerce.h](coerce.h)** - Type coercion functions
  - Conversions between ML and C types
  - Safe casts between different integer/pointer types
  - Used extensively throughout FFI layer

- **[cpointer.c](cpointer.c)** / **[cpointer.h](cpointer.h)** - C pointer operations
  - C pointer manipulation from ML
  - Null pointer checks
  - Pointer arithmetic (when necessary)

- **[Stdio.c](Stdio.c)** - Standard I/O operations
  - Basic stdio wrappers (stdin, stdout, stderr)
  - Buffering control

## Module Organization

### Integer / Arithmetic

#### [IntInf/](IntInf/)
Arbitrary-precision integer (IntInf) operations:
- Integration with GMP (GNU Multiple Precision Arithmetic Library)
- Big integer arithmetic
- Conversions to/from fixed-size integers
- String conversions

Key files:
- `IntInf-gmp.h` - GMP integration

### Floating Point

#### [Real/](Real/)
Floating-point operations:
- Special value queries (isNan, isFinite, etc.)
- Classification (signBit, class)
- Rounding modes
- Math functions
- String conversions (uses gdtoa)

Key files:
- `Real-ops.c` - Real number operations
- `Real-consts.c` - Real number constants (infinity, NaN, etc.)
- `Math-fns.c` - Mathematical functions (sin, cos, sqrt, etc.)
- `gdtoa.c` - Real to/from string conversion

### Word Operations

#### [Word/](Word/)
Word (unsigned integer) operations:
- Bitwise operations
- Shifts and rotates
- Word size conversions

Key files:
- `Word-ops.c` - Word operations

### POSIX Interface

#### [Posix/](Posix/)
POSIX API bindings organized by category:

##### [Posix/FileSys/](Posix/FileSys/)
File system operations:
- File metadata: `stat.c`, `fstat.c`, `lstat.c`
- Directory operations: `Dirstream.c`, `chdir.c`, `mkdir.c`, `getcwd.c`
- Permissions: `chmod.c`, `fchmod.c`, `chown.c`, `fchown.c`, `access.c`
- Hard/soft links: `link.c`, `symlink.c`, `readlink.c`
- File operations: `open2.c`, `open3.c`, `rename.c`, `unlink.c`, `truncate.c`, `ftruncate.c`
- Special files: `mkfifo.c`
- Path configuration: `pathconf.c`, `fpathconf.c`
- Umask: `umask.c`
- Constants: `FileSys-consts.c`

##### [Posix/IO/](Posix/IO/)
POSIX I/O operations:
- Basic I/O: `read.c`, `write.c`, `close.c`
- File descriptor operations: `dup.c`, `dup2.c`, `pipe.c`
- File positioning: `lseek.c`
- Control: `fcntl.c`, `fsync.c`
- File locking: `FLock.c`
- TTY operations: `isatty.c`, `ttyname.c`
- Constants: `IO-consts.c`

##### [Posix/ProcEnv/](Posix/ProcEnv/)
Process environment:
- User/group IDs: `getuid.c`, `geteuid.c`, `getgid.c`, `getegid.c`, `setuid.c`, `setgid.c`
- Groups: `getgroups.c`, `getlogin.c`
- Process IDs: `getpid.c`, `getppid.c`, `getpgrp.c`, `setpgid.c`, `setsid.c`
- Environment: `environ.c`, `getenv.c`, `setenv.c`
- System info: `uname.c`, `sysconf.c`
- Time: `times.c`
- TTY: `ctermid.c`, `ttyname.c`, `isatty.c`

##### [Posix/Process/](Posix/Process/)
Process control:
- Process creation: `fork.c`, `exec.c`, `exece.c`, `execp.c`
- Process termination: `exit.c`, `kill.c`
- Wait operations: `wait.c`, `waitpid.c`
- Alarms: `alarm.c`, `pause.c`, `sleep.c`, `nanosleep.c`
- Constants: `Process-consts.c`

##### [Posix/SysDB/](Posix/SysDB/)
System database access:
- User database: `Group.c`, `Passwd.c`
- Group operations: `getgrgid.c`, `getgrnam.c`
- User operations: `getpwuid.c`, `getpwnam.c`

### Network I/O

#### [Net/Socket/](Net/Socket/)
Network socket operations:
- Socket creation: `Socket.c`, `socketPair.c`
- Connection: `connect.c`, `bind.c`, `listen.c`, `accept.c`
- I/O: `send.c`, `sendTo.c`, `recv.c`, `recvFrom.c`
- Options: `GenericSock.c`, `INetSock.c`, `UnixSock.c`
- Shutdown: `shutdown.c`
- Address operations: `sockAddrToString.c`
- Network byte order: `NetHostDB.c`, `NetProtDB.c`, `NetServDB.c`
- Host/network database lookups

### System Interface

#### [System/OS/IO/](System/OS/IO/)
OS-specific I/O:
- Poll/select: `poll.c`
- Constants: `IO-consts.c`

### MLton/MPL Extensions

#### [MLton/](MLton/)
MLton/MPL-specific runtime features:

##### Top-level MLton files:
- **[bug.c](MLton/bug.c)** - Runtime error reporting
  - `MLton_bug()` - Report runtime errors and abort

##### [MLton/Itimer/](MLton/Itimer/)
Interval timers:
- `set.c`, `get.c` - setitimer/getitimer wrappers
- Used for profiling and signals

##### [MLton/Process/](MLton/Process/)
Process utilities:
- `create.c` - Process creation (fork + exec)
- `spawne.c`, `spawnp.c` - Process spawning
- `env.c` - Environment variable access
- `reap.c` - Child process reaping

##### [MLton/Rlimit/](MLton/Rlimit/)
Resource limits:
- `get.c`, `set.c` - getrlimit/setrlimit
- `Rlimit-consts.c` - Resource limit constants
- Controls stack size, heap size, open files, etc.

##### [MLton/Rusage/](MLton/Rusage/)
Resource usage statistics:
- `getrusage.c` - Get resource usage (CPU time, memory, etc.)
- Used by GC statistics and profiling

##### [MLton/Syslog/](MLton/Syslog/)
System logging:
- `Syslog.c` - syslog integration
- Logging to system log daemon

## FFI Conventions

### Naming Convention

C functions follow this pattern:
```
<Module>_<operation>
```

Examples:
- `Posix_FileSys_stat` - Posix.FileSys.stat
- `Real_Math_sin` - Real.Math.sin
- `MLton_Rusage_getrusage` - MLton.Rusage.getrusage

### Type Conversions

ML types map to C types via generated headers:
- `ml-types.h` - ML type definitions
- `c-types.h` - C type definitions

Common conversions:
- ML `int` → C `Int32` or `Int64` (depending on default-type)
- ML `word` → C `Word32` or `Word64`
- ML `real` → C `Real64` (double)
- ML `string` → C `const char*` (null-terminated)
- ML `char` → C `Char8` (uint8_t)
- ML `pointer` → C `Pointer` (void*)

### Error Handling

POSIX functions typically:
1. Call the underlying system call
2. Check for error (return value -1, NULL, etc.)
3. Set ML-accessible errno if error occurs
4. Return result or error indicator

Example pattern:
```c
Int Posix_FileSys_access(NullString8_t path, Word mode) {
  return access((const char*)path, mode);
}
```

The ML code checks return values and raises appropriate exceptions.

### Memory Management

- **Strings**: ML strings are garbage collected; C code must not free them
- **Pointers**: C pointers returned to ML are typically wrapped in ML `pointer` type
- **Arrays**: ML arrays are GC'd; C code gets temporary pointer access
- **Allocation**: C code can allocate ML objects via GC interface (see `gc/new-object.h`)

## Constants and Platform Variations

Files named `*-consts.c` define platform-specific constants:
- File access modes (O_RDONLY, O_WRONLY, etc.)
- Signal numbers
- Socket options
- Resource limits
- Error codes

These are generated or defined per-platform to handle variations across operating systems.

## Building

Basis library FFI is built as part of the runtime:

```bash
make runtime
```

Each `.c` file is compiled into multiple `.o` variants (debug, detect, trace, etc.) and linked into `libmlton*.a`.

## Usage from Basis Library

The basis library (in `basis-library/`) declares primitives using `_import`:

```sml
(* In basis-library/posix/file-sys.sml *)
val stat: string -> stat = _import "Posix_FileSys_stat" runtime private;
```

This binds the ML function `stat` to the C function `Posix_FileSys_stat` in this directory.

## Adding New FFI Functions

1. **Add C implementation**: Create `.c` file in appropriate subdirectory
2. **Declare in header**: Add to `basis-ffi.h` (if public) or local `.h` file
3. **Declare in basis**: Add `_import` declaration in basis library `.sml` file
4. **Rebuild**: `make runtime && make basis`

Example:
```c
// In runtime/basis/Posix/FileSys/newop.c
#include "platform.h"

Int Posix_FileSys_newop(NullString8_t path) {
  // Implementation
  return result;
}
```

```sml
(* In basis-library/posix/file-sys.sml *)
val newop = _import "Posix_FileSys_newop" runtime private : string -> int;
```

## Platform-Specific Code

Some FFI functions have platform-specific implementations:
- Windows vs. Unix
- Linux-specific system calls
- BSD variants

Use `#ifdef` preprocessor directives:
```c
#if defined(_WIN32)
  // Windows implementation
#else
  // Unix implementation
#endif
```

Platform detection is in [../platform.h](../platform.h).

## Testing

Basis library tests indirectly test FFI functions:
```bash
make check  # Runs regression tests
```

## Common Issues

### Missing Symbols
**Symptom**: Linker error about undefined reference

**Cause**: FFI function declared in basis but not implemented in C

**Fix**: Implement the C function or remove the `_import` declaration

### Type Mismatches
**Symptom**: Runtime crashes or wrong values

**Cause**: ML type doesn't match C type signature

**Fix**: Ensure `_import` type matches C function signature exactly

### Platform Incompatibility
**Symptom**: Build fails on certain platforms

**Cause**: Using platform-specific system calls without guards

**Fix**: Add `#ifdef` guards or provide fallback implementation

## See Also

- [../README.md](../README.md) - Runtime system overview
- [../gc/README.md](../gc/README.md) - GC interface
- [../platform/README.md](../platform/README.md) - Platform-specific code
- [../../basis-library/](../../basis-library/) - Basis library (SML side)
- [../basis-ffi.h](../basis-ffi.h) - FFI declarations
