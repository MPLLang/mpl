# Runtime Utilities

The `runtime/util/` directory contains low-level utility functions and macros used throughout the runtime system.

**Total Files**: 15 files (headers and small C implementations)

## Overview

These utilities provide:
- **Error Handling**: Fatal error reporting
- **Logging**: Debug and diagnostic output
- **Memory Utilities**: Alignment, pointer operations, safe allocation
- **String Conversion**: Type-to-string formatting
- **Synchronization**: Spinlocks for low-level locking
- **Endianness**: Byte order conversions
- **Debugging**: Valgrind integration

## Files

### Error Handling

#### [die.{c,h}](die.c)
Fatal error handling:

```c
void die(const char *fmt, ...);  // Print error and abort
void diee(const char *fmt, ...); // Print error with errno and abort
```

- Prints error message to stderr
- Aborts program immediately
- Used for unrecoverable runtime errors
- `diee` variant includes `strerror(errno)`

Example usage:
```c
if (result == NULL) {
  die("Memory allocation failed");
}

if (fd < 0) {
  diee("Failed to open file");  // Includes errno message
}
```

### Logging and Diagnostics

#### [log.{c,h}](log.c)
Structured logging system:

```c
void logInit(void);                              // Initialize logging
void logDeinit(void);                            // Cleanup logging
void logf(const char *fmt, ...);                 // Log formatted message
void logMsg(const char *msg);                    // Log simple message
```

Features:
- Conditional compilation: Only active if `DEBUG` or `MPL_TRACING` defined
- Timestamp prefixes
- Thread-safe output
- Configurable verbosity

Log levels (via macros):
- `LOG_INFO` - Informational messages
- `LOG_DEBUG` - Debug messages
- `LOG_ERROR` - Error messages

Used throughout GC for debugging:
```c
if (DEBUG) {
  logf("GC: Collected %zu bytes\n", bytesCollected);
}
```

### Memory Utilities

#### [align.h](align.h)
Alignment operations (header-only):

```c
// Align pointer/size up to boundary (must be power of 2)
#define alignUp(p, alignment) ...

// Align pointer/size down to boundary
#define alignDown(p, alignment) ...

// Check if pointer is aligned
#define isAligned(p, alignment) ...
```

Example:
```c
void *ptr = malloc(100);
void *aligned = (void*)alignUp((uintptr_t)ptr, 64);  // Align to 64-byte boundary
```

Used extensively in GC for:
- Aligning heap chunks to page boundaries
- Aligning objects within chunks
- Ensuring cache-line alignment for concurrent data structures

#### [pointer.h](pointer.h)
Pointer arithmetic helpers (header-only):

```c
// Add bytes to pointer
#define pointerAdd(p, bytes) ((void*)((char*)(p) + (bytes)))

// Subtract pointers (get byte distance)
#define pointerDiff(p1, p2) ((size_t)((char*)(p1) - (char*)(p2)))

// Compare pointers
#define pointerLT(p1, p2) ((char*)(p1) < (char*)(p2))
```

Avoids pointer arithmetic pitfalls and improves readability.

#### [safe.h](safe.h)
Safe wrappers for system calls (header-only):

```c
// Safe malloc (dies on failure)
#define safeMalloc(size) ...

// Safe calloc (dies on failure)
#define safeCalloc(nmemb, size) ...

// Safe realloc (dies on failure)
#define safeRealloc(ptr, size) ...

// Safe strdup (dies on failure)
#define safeStrdup(str) ...

// Safe file operations
#define safeFopen(path, mode) ...
#define safeFclose(file) ...
#define safeFread(ptr, size, nmemb, stream) ...
#define safeFwrite(ptr, size, nmemb, stream) ...
```

These wrappers:
- Check return values
- Call `die()` on failure
- Eliminate need for error checking at call sites

Example:
```c
void *mem = safeMalloc(1024);  // Guaranteed to succeed or abort
// No need to check if mem == NULL
```

#### [read_write.h](read_write.h)
Safe read/write wrappers (header-only):

```c
// Read exactly N bytes (handles EINTR, partial reads)
ssize_t safeRead(int fd, void *buf, size_t count);

// Write exactly N bytes (handles EINTR, partial writes)
ssize_t safeWrite(int fd, const void *buf, size_t count);
```

These functions:
- Retry on `EINTR` (interrupted system call)
- Loop until all bytes are read/written
- Return -1 on unrecoverable error

Used for:
- Loading heap from binary
- Checkpointing (if implemented)
- Logging to files

### Synchronization

#### [spinlock.{c,h}](spinlock.c)
Simple spinlock implementation:

```c
typedef struct {
  volatile int locked;
} spinlock_t;

void spinlock_init(spinlock_t *lock);    // Initialize spinlock
void spinlock_lock(spinlock_t *lock);    // Acquire lock (spin until available)
void spinlock_unlock(spinlock_t *lock);  // Release lock
int spinlock_trylock(spinlock_t *lock);  // Try to acquire (non-blocking)
```

Implementation:
- Uses atomic compare-and-swap
- Busy-waiting (spins in loop)
- No fairness guarantees

Used for:
- Short critical sections in GC
- Protecting concurrent data structures
- Should NOT be used for long-held locks (use pthread_mutex instead)

Example:
```c
spinlock_t lock;
spinlock_init(&lock);

spinlock_lock(&lock);
// Critical section
spinlock_unlock(&lock);
```

**Warning**: Spinlocks are dangerous in user-space (thread may be preempted while holding lock). Use sparingly and only for very short critical sections.

### Type Conversions

#### [to-string.{c,h}](to-string.c)
Formatted type conversions:

```c
// Convert integer to string (with various formats)
const char* uintToString(unsigned int n);        // Decimal
const char* uintToHexString(unsigned int n);     // Hexadecimal
const char* intToString(int n);                  // Signed decimal

// Convert size to human-readable string
const char* sizeToString(size_t bytes);          // "1.5K", "3.2M", "1.7G"

// Convert pointer to string
const char* pointerToString(const void *ptr);    // "0x7fff12345678"
```

Features:
- Uses static buffers (not thread-safe but convenient)
- Human-readable output for logging
- Multiple formats available

Used extensively in logging and statistics:
```c
logf("Allocated %s at %s\n", sizeToString(size), pointerToString(ptr));
// Output: "Allocated 1.5M at 0x7fff12345678"
```

### Endianness

#### [endian.h](endian.h)
Byte order conversion (header-only):

```c
// Swap bytes (endianness conversion)
uint16_t bswap16(uint16_t x);  // Swap 2-byte value
uint32_t bswap32(uint32_t x);  // Swap 4-byte value
uint64_t bswap64(uint64_t x);  // Swap 8-byte value

// Convert to/from big-endian
uint32_t htobe32(uint32_t x);  // Host to big-endian
uint32_t betoh32(uint32_t x);  // Big-endian to host

// Convert to/from little-endian
uint32_t htole32(uint32_t x);  // Host to little-endian
uint32_t letoh32(uint32_t x);  // Little-endian to host
```

Platform detection:
- Automatically detects host endianness
- No-op conversions on matching endian platforms
- Actual swap on mismatched platforms

Used for:
- Network byte order (always big-endian)
- Binary file format (if specified endianness)
- Cross-platform data exchange

### Debugging Tools

#### [valgrind.h](valgrind.h)
Valgrind integration (header-only):

```c
// Mark memory region as defined
VALGRIND_MAKE_MEM_DEFINED(addr, len);

// Mark memory region as undefined
VALGRIND_MAKE_MEM_UNDEFINED(addr, len);

// Mark memory region as inaccessible
VALGRIND_MAKE_MEM_NOACCESS(addr, len);

// Create memory pool
VALGRIND_CREATE_MEMPOOL(pool, rzB, is_zeroed);

// Destroy memory pool
VALGRIND_DESTROY_MEMPOOL(pool);
```

When running under Valgrind:
- Helps Valgrind understand custom allocator behavior
- Reduces false positives
- Enables precise leak detection

When NOT running under Valgrind:
- Macros expand to nothing (zero overhead)

Used in GC to:
- Mark GC-managed memory as defined/undefined
- Help Valgrind track heap allocations
- Detect use-after-free and other memory errors

Example:
```c
void *mem = GC_allocate(size);
VALGRIND_MAKE_MEM_DEFINED(mem, size);  // Tell Valgrind this memory is OK to use
```

## Usage Patterns

### Error Handling Pattern
```c
int fd = open(path, O_RDONLY);
if (fd < 0) {
  diee("Failed to open %s", path);  // Dies with errno message
}
// Continue with valid fd
```

### Logging Pattern
```c
#if DEBUG
  logf("Processing %zu objects\n", count);
#endif
```

### Alignment Pattern
```c
size_t size = ...;
size_t aligned = alignUp(size, CACHE_LINE_SIZE);  // Align to cache line
```

### Safe Allocation Pattern
```c
void *mem = safeMalloc(size);  // Dies on failure, so mem is always valid
// Use mem without checking for NULL
```

## Build Integration

Utility functions are compiled into `util.o` and linked into all runtime configurations:

```bash
make runtime  # Builds util.o along with other runtime objects
```

Headers are included via:
```c
#include "util/die.h"
#include "util/log.h"
// etc.
```

## Configuration

### Compile-Time Options

- **DEBUG** - Enable debug logging
  - Activates `logf()` and other debug output
  - Increases runtime verbosity

- **MPL_TRACING** - Enable event tracing
  - Activates tracing hooks
  - Used with external trace analysis tools

### Runtime Options

Logging can be controlled via environment variables (if implemented):
```bash
export DEBUG_RUNTIME=1  # Enable verbose logging
./program
```

## Performance Considerations

### Spinlocks
- **Use sparingly**: Spinlocks waste CPU cycles
- **Keep critical sections SHORT**: A few instructions only
- **Prefer lock-free algorithms**: Use atomics directly when possible
- **Consider pthread_mutex**: For longer critical sections

### Logging Overhead
- Logging statements compiled out if `DEBUG` not defined
- Use `if (DEBUG) { logf(...); }` pattern for zero overhead in production

### Alignment
- Proper alignment improves performance (cache efficiency)
- Misalignment causes crashes on some architectures (ARM, SPARC)
- Align frequently-accessed data to cache line boundaries (64 bytes)

## Common Issues

### Spinlock Deadlock
**Symptom**: Program hangs

**Cause**: Thread preempted while holding spinlock, or acquiring lock twice

**Fix**: Use trylock with timeout, or switch to pthread_mutex

### Alignment Faults
**Symptom**: SIGBUS crash on ARM/SPARC

**Cause**: Accessing unaligned data

**Fix**: Use `alignUp()` to ensure proper alignment

### Log Output Not Appearing
**Symptom**: No debug output despite setting DEBUG

**Cause**: DEBUG not defined during compilation

**Fix**: Rebuild with `make DEBUG=true`

## Testing

Utilities are tested indirectly through:
- Runtime system usage
- Regression tests
- Manual testing with Valgrind

Spinlocks can be stress-tested with concurrent workloads.

## See Also

- [../README.md](../README.md) - Runtime system overview
- [../gc/README.md](../gc/README.md) - GC implementation (major user of utilities)
- [../platform/README.md](../platform/README.md) - Platform-specific code
