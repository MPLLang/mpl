---
layout: page
title: Common Commands
---

# Common Commands
{: .no_toc }

## Table of Contents
{: .no_toc .text-delta }
1. Table of Contents
{:toc}

## (Re)building the compiler and run-time system

### Linux

**To build everything**, this will typically take a few minutes:
```bash
$ make clean && make
```

**To rebuild the run-time system**, assuming you previously did a full `make`
but have since modified something in `runtime/` or `runtime/gc/`:
```bash
$ make runtime
```

**To rebuild the basis library**, assuming you previously did a full `make`
but have since modified something in `basis-library/` (for example, the
scheduler):
```bash
$ make basis 
```


### Mac

{: .warning}
MPL was developed with x86 in mind and has not yet been thoroughly tested on
the new Apple {M1, M2, M3} chips. Bug reports and fixes are welcome.

{: .note}
> You will need to install GNU Make and will need to inform the Makefile
> about the location of GMP. We recommend using Homebrew:
> ```
> $ brew install make gmp
> ```
> Note that this installs GNU Make with the executable name `gmake`. All of
> the Mac-specific commands below are run through `gmake` instead of the
> system `make`.
>
> Next, we recommend editing `Makefile.config` to set `WITH_GMP_DIR` to point
> to the right place:
> ```diff
> diff --git a/Makefile.config b/Makefile.config
> index 96f13dc0e..405c6b909 100644
> --- a/Makefile.config
> +++ b/Makefile.config
> @@ -15,7 +15,7 @@ RANLIB := ranlib
>  STRIP := strip
>  
>  # Specify GMP include and library paths, if not on default search paths.
> -WITH_GMP_DIR :=
> +WITH_GMP_DIR := $(shell brew --prefix gmp)
>  ifneq ($(WITH_GMP_DIR),)
>  WITH_GMP_INC_DIR := $(WITH_GMP_DIR)/include
>  WITH_GMP_LIB_DIR := $(WITH_GMP_DIR)/lib
> ```
> If you do this, then you can exclude `WITH_GMP_DIR=...` from each of the
> commands below.

**To build everything**, this will typically take a few minutes:
```bash
$ gmake clean && gmake WITH_GMP_DIR=$(brew --prefix gmp)
```

**To rebuild the run-time system**, assuming you previously did a full `make`
but have since modified something in `runtime/` or `runtime/gc/`:
```bash
$ gmake runtime WITH_GMP_DIR=$(brew --prefix gmp)
```

**To rebuild the basis library**, assuming you previously did a full `make`
but have since modified something in `basis-library/` (for example, the
scheduler):
```bash
$ gmake basis WITH_GMP_DIR=$(brew --prefix gmp)
```

## Building and running examples

```bash
$ cd examples
$ make fib  # or something else---see the PROGRAMS list in examples/Makefile
$ bin/fib @mpl procs 4 --   # run on 4 processors
```

You can also build an example in debug mode, which makes it a bit easier to
peruse with `gdb`:

```bash
$ cd examples
$ make fib.dbg
$ gdb --args bin/fib.dbg @mpl procs 4 --
```