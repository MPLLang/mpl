# Platform-Specific Runtime Code

The `runtime/platform/` directory contains platform and architecture-specific code that abstracts operating system and CPU differences.

## Overview

This layer provides:
- **OS Abstraction**: Platform-specific initialization, memory mapping, system calls
- **Architecture Definitions**: Word size, endianness, alignment, register conventions
- **Atomic Operations**: Platform-specific atomic instruction implementations
- **System Utilities**: Platform-specific helper functions

The build system automatically selects the appropriate platform files based on the target system.

## File Categories

### Operating System Support

Platform-specific initialization and system integration:

#### Unix-like Systems
- **[linux.{c,h}](linux.c)** - Linux-specific code
  - `/proc` filesystem integration
  - Linux-specific system calls
  - Memory display via `/proc/self/status`

- **[darwin.{c,h}](darwin.c)** - macOS/Darwin
  - macOS-specific initialization
  - Mach kernel integration

- **[freebsd.{c,h}](freebsd.c)** - FreeBSD
- **[netbsd.{c,h}](netbsd.c)** - NetBSD
- **[openbsd.{c,h}](openbsd.c)** - OpenBSD
- **[solaris.{c,h}](solaris.c)** - Solaris/OpenSolaris
- **[aix.{c,h}](aix.c)** - IBM AIX
- **[hpux.{c,h}](hpux.c)** - HP-UX
- **[hurd.{c,h}](hurd.c)** - GNU/Hurd

#### Other Systems
- **[cygwin.{c,h}](cygwin.c)** - Cygwin (Windows POSIX layer)
- **[mingw.{c,h}](mingw.c)** - MinGW (Windows native)
- **[windows.c](windows.c)** - Windows-specific code
- **[wasi.{c,h}](wasi.c)** - WebAssembly System Interface (WASI)

#### Unix Common Code
- **[nonwin.c](nonwin.c)** - Common code for all non-Windows platforms
  - Shared Unix utilities
  - POSIX-compatible implementations

### Architecture Support

Architecture-specific definitions (header-only):

#### x86 Family
- **[x86.h](x86.h)** - 32-bit x86 (IA-32)
  - 32-bit word size
  - Little-endian
  - 4-byte alignment

- **[amd64.h](amd64.h)** - x86-64 (AMD64, x86_64)
  - 64-bit word size
  - Little-endian
  - 8-byte alignment

#### ARM Family
- **[arm.h](arm.h)** - 32-bit ARM
  - 32-bit word size
  - Little-endian (typically)
  - 4-byte alignment

- **[arm64.h](arm64.h)** - 64-bit ARM (AArch64)
  - 64-bit word size
  - Little-endian
  - 8-byte alignment

#### PowerPC Family
- **[powerpc.h](powerpc.h)** - 32-bit PowerPC
  - 32-bit word size
  - Big-endian
  - 4-byte alignment

- **[powerpc64.h](powerpc64.h)** - 64-bit PowerPC
  - 64-bit word size
  - Big-endian (or little-endian on POWER8+)
  - 8-byte alignment

#### RISC-V
- **[riscv.h](riscv.h)** - RISC-V (32-bit and 64-bit)
  - Configurable word size
  - Little-endian
  - Natural alignment

#### MIPS
- **[mips.h](mips.h)** - MIPS (32-bit and 64-bit)
  - 32-bit or 64-bit word size
  - Big-endian or little-endian
  - Natural alignment

#### Other Architectures
- **[sparc.h](sparc.h)** - SPARC (Sun)
- **[ia64.h](ia64.h)** - Intel Itanium (IA-64)
- **[s390.h](s390.h)** - IBM System/390, z/Architecture
- **[alpha.h](alpha.h)** - DEC Alpha
- **[hppa.h](hppa.h)** - HP PA-RISC
- **[m68k.h](m68k.h)** - Motorola 68000
- **[loongarch64.h](loongarch64.h)** - LoongArch 64-bit
- **[wasm32.h](wasm32.h)** - WebAssembly 32-bit

### Atomic Operations

Platform-specific atomic instruction implementations:

- **[atomics-gcc-gte48.h](atomics-gcc-gte48.h)** - GCC >= 4.8 atomics
  - Uses GCC/Clang `__atomic_*` builtins (C11 standard)
  - Preferred for modern compilers
  - Better memory ordering control

- **[atomics-gcc-lt48.h](atomics-gcc-lt48.h)** - GCC < 4.8 atomics
  - Uses older `__sync_*` builtins
  - Fallback for older compilers
  - Less precise memory ordering

The build system selects the appropriate file based on compiler version.

### Memory Management

Memory mapping and related operations:

- **[mmap.c](mmap.c)** - Memory mapping via `mmap()`
  - Virtual memory allocation
  - File-backed mappings
  - Anonymous mappings for heap

- **[mmap-protect.c](mmap-protect.c)** - Memory protection
  - `mprotect()` wrapper
  - Read/write/execute permissions
  - Used for GC barriers (if needed)

- **[mremap.c](mremap.c)** - Memory remapping (Linux-specific)
  - `mremap()` system call
  - Efficient heap resizing on Linux

- **[use-mmap.c](use-mmap.c)** - mmap feature detection
  - Check if mmap is available and functional

### System Utilities

Platform-specific helper functions:

- **[diskBack.unix.c](diskBack.unix.c)** - Disk-backed memory (Unix)
  - Swap-backed virtual memory
  - Used for large heaps exceeding RAM

- **[displayMem.proc.c](displayMem.proc.c)** - Memory display via `/proc`
  - Linux-specific memory usage display
  - Uses `/proc/self/status`

- **[recv.nonblock.c](recv.nonblock.c)** - Non-blocking receive
  - Non-blocking socket operations
  - Used by network I/O

- **[sysconf.c](sysconf.c)** - System configuration (POSIX `sysconf`)
  - Query system limits and parameters
  - Page size, max threads, etc.

- **[sysctl.c](sysctl.c)** - System control (BSD `sysctl`)
  - Query/set kernel parameters (BSD systems)

- **[setenv.putenv.c](setenv.putenv.c)** - Environment variable setting
  - `setenv()` / `putenv()` wrappers
  - Platform compatibility

- **[setenv.h](setenv.h)** - Environment variable header

## Key Header Files in Parent Directory

These headers are in `runtime/` but define platform interfaces:

- **[platform.h](../platform.h)** - Main platform detection and configuration
  - Detects OS and architecture
  - Includes appropriate platform-specific headers
  - Defines basic types (`Int32`, `Word64`, `Pointer`, etc.)
  - Feature detection macros

- **[platform.c](../platform.c)** - Platform initialization
  - Entry point for platform-specific setup
  - Calls OS-specific initialization

## Platform Detection

The build system and `platform.h` detect the platform using preprocessor macros:

### Operating System Detection
```c
#if defined(__linux__)
  #define PLATFORM_LINUX
#elif defined(__APPLE__)
  #define PLATFORM_DARWIN
#elif defined(__FreeBSD__)
  #define PLATFORM_FREEBSD
#elif defined(_WIN32)
  #define PLATFORM_WINDOWS
// ... etc.
#endif
```

### Architecture Detection
```c
#if defined(__x86_64__) || defined(__amd64__)
  #define ARCH_AMD64
#elif defined(__i386__)
  #define ARCH_X86
#elif defined(__aarch64__)
  #define ARCH_ARM64
#elif defined(__arm__)
  #define ARCH_ARM
// ... etc.
#endif
```

### Word Size
Automatically determined from architecture:
```c
#if defined(ARCH_AMD64) || defined(ARCH_ARM64)
  #define WORD_SIZE 64
#else
  #define WORD_SIZE 32
#endif
```

## Architecture Headers

Each architecture header (e.g., `amd64.h`) defines:

1. **Word Size**: `#define WORD_SIZE 64`
2. **Pointer Size**: `#define POINTER_BITS 64`
3. **Endianness**: `#define BIG_ENDIAN` or `#define LITTLE_ENDIAN`
4. **Alignment**: Required alignment for various types
5. **Register Conventions**: Calling conventions, special registers
6. **Cache Line Size**: For performance tuning

Example from `amd64.h`:
```c
#define ARCH_AMD64
#define WORD_SIZE 64
#define POINTER_BITS 64
// Little-endian by default
```

## Atomic Operations

Atomics are critical for the concurrent GC and parallel scheduler. The platform layer provides:

### Atomic Functions (via `atomics-*.h`)

- **Compare-and-Swap (CAS)**:
  ```c
  bool __atomic_compare_exchange_n(type *ptr, type *expected, type desired, ...);
  ```

- **Fetch-and-Add**:
  ```c
  type __atomic_fetch_add(type *ptr, type val, int memorder);
  ```

- **Load/Store**:
  ```c
  type __atomic_load_n(type *ptr, int memorder);
  void __atomic_store_n(type *ptr, type val, int memorder);
  ```

- **Memory Barriers**:
  ```c
  __atomic_thread_fence(int memorder);
  ```

### Memory Ordering

The GCC/Clang builtins support C11 memory orderings:
- `__ATOMIC_RELAXED` - No ordering constraints
- `__ATOMIC_ACQUIRE` - Acquire semantics
- `__ATOMIC_RELEASE` - Release semantics
- `__ATOMIC_ACQ_REL` - Acquire-release
- `__ATOMIC_SEQ_CST` - Sequentially consistent (default)

MPL uses these for lock-free data structures in the GC (ABP deques, concurrent lists, etc.).

## Memory Mapping

The platform layer abstracts memory mapping:

### mmap() Wrapper
```c
void* GC_mmapAnon(size_t length);  // Anonymous mapping for heap
void* GC_mmapFile(const char *path, size_t length);  // File-backed mapping
int GC_munmap(void *start, size_t length);  // Unmap memory
```

### Memory Protection
```c
int GC_mprotect(void *addr, size_t len, int prot);  // Change protection
```

Protection flags:
- `PROT_READ` - Readable
- `PROT_WRITE` - Writable
- `PROT_EXEC` - Executable
- `PROT_NONE` - No access

Used for:
- Allocating heap memory
- Implementing write barriers (if needed)
- Debugging (protect free memory)

## OS-Specific Initialization

Each OS implementation (e.g., `linux.c`) provides:

1. **System Initialization**: Setup OS-specific state
2. **Memory Display**: Report memory usage
3. **Feature Detection**: Check for available system calls
4. **Compatibility Shims**: Implement missing functions

Example: Linux initialization
- Read `/proc/self/status` for memory usage
- Setup signal handlers
- Configure memory overcommit behavior

## Building

The build system automatically selects platform files:

```makefile
# In runtime/Makefile
ifeq ($(OS),Linux)
  PLATFORM_SOURCES = platform/linux.c
  ARCH_HEADER = platform/$(ARCH).h
endif
```

The appropriate `.c` and `.h` files are included based on detected platform.

## Adding New Platform Support

To add support for a new platform:

1. **Create OS-specific files**:
   - `platform/newos.c` - OS initialization and utilities
   - `platform/newos.h` - OS-specific constants and macros

2. **Create/update architecture header** (if new architecture):
   - `platform/newarch.h` - Architecture definitions

3. **Update platform.h**:
   - Add platform detection for new OS/architecture

4. **Update Makefile**:
   - Add build rules for new platform

5. **Test thoroughly**:
   - Build runtime on new platform
   - Run regression tests
   - Test parallel programs

## Testing

Platform-specific code is tested via:

```bash
make runtime  # Build for current platform
make check    # Run regression tests
```

Cross-compilation testing requires appropriate toolchain and qemu (for architecture emulation).

## Common Issues

### Atomic Operations Not Available
**Symptom**: Build fails with undefined atomic symbols

**Cause**: Compiler too old or architecture doesn't support atomics

**Fix**: Use newer compiler (GCC >= 4.8) or implement platform-specific atomics

### Memory Mapping Fails
**Symptom**: Runtime crashes with mmap errors

**Cause**: Insufficient virtual memory, wrong flags, or platform incompatibility

**Fix**: Check memory limits, verify mmap parameters, add platform-specific handling

### Wrong Endianness
**Symptom**: Data corruption, wrong values

**Cause**: Endianness not detected correctly

**Fix**: Verify architecture header has correct endian definition

### Alignment Faults
**Symptom**: SIGBUS on ARM, SPARC, etc.

**Cause**: Unaligned memory access on architectures requiring aligned access

**Fix**: Ensure proper alignment in GC allocation and data layout

## Performance Considerations

### Cache Line Size
Architecture headers define `CACHE_LINE_SIZE` (typically 64 bytes). This is used for:
- Padding to avoid false sharing in concurrent data structures
- Aligning frequently-accessed data

### Page Size
Page size is queried at runtime via `sysconf(_SC_PAGESIZE)`. Important for:
- Memory mapping alignment
- Heap block sizing
- GC chunk allocation

### Huge Pages
Some platforms support huge pages (2MB, 1GB) for better TLB performance:
- Linux: `mmap()` with `MAP_HUGETLB`
- FreeBSD: `mmap()` with `MAP_ALIGNED_SUPER`

(Not currently used by MPL runtime, but could be added for performance)

## See Also

- [../README.md](../README.md) - Runtime system overview
- [../gc/README.md](../gc/README.md) - GC implementation (uses atomics)
- [../util/README.md](../util/README.md) - Utility functions
- [../platform.h](../platform.h) - Main platform header
