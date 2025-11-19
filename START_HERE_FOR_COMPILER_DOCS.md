# Resume Compiler Documentation Here

**Status**: Runtime docs complete ✅ | Compiler docs ready to start ⬜

## Quick Start (30 seconds)

1. **Read**: [MLTON_DOCUMENTATION_CHECKPOINT.md](MLTON_DOCUMENTATION_CHECKPOINT.md)
2. **Reference**: [MLTON_DOCUMENTATION_PLAN.md](MLTON_DOCUMENTATION_PLAN.md)
3. **Start**: Create `mlton/README.md` following Phase 1 priorities

## What's Done

✅ Runtime documentation complete (7 files, ~3500 lines, ~350 source files)
✅ All docs scrubbed and standalone (no CLAUDE.md references)
✅ Ready for separate docs branch

## What's Next

Create compiler documentation in this order:

### Phase 1 (Start Here - Highest Priority)
1. **mlton/README.md** - Compiler overview (~6-8 hours)
2. **mlton/ssa/README.md** - SSA IR and passes (~8-10 hours) ⭐ Most important
3. **mlton/control/README.md** - Compiler flags (~3-4 hours)
4. **mlton/atoms/README.md** - Primitives (~3-4 hours)

### Phase 2
5. mlton/front-end/README.md
6. mlton/xml/README.md
7. mlton/backend/README.md

### Phase 3
8. mlton/elaborate/README.md
9. mlton/core-ml/README.md
10. mlton/closure-convert/README.md

## Templates to Follow

Use runtime docs as templates:
- **Structure**: runtime/README.md
- **Module detail**: runtime/gc/README.md
- **Concise utility**: runtime/util/README.md

## Key Files to Study

- `mlton/main/compile.fun` - Compilation pipeline
- `mlton/ssa/*.fun` - All optimization passes
- `mlton/control/control-flags.sml` - Compiler flags
- `mlton/atoms/prim.fun` - Primitives

## Documentation Rules

✅ DO:
- Reference material (timeless)
- How-to guides
- Architecture documentation
- Examples and troubleshooting

❌ DON'T:
- Reference CLAUDE.md
- Include "WIP" or "TODO"
- Mention "current branch" or development work
- Temporary notes

## Files Summary

**Completed docs** (ready for docs branch):
- runtime/README.md
- runtime/gc/README.md
- runtime/basis/README.md
- runtime/platform/README.md
- runtime/util/README.md
- runtime/gdtoa/README.md
- DOCUMENTATION_INDEX.md

**Working files** (reference/planning):
- MLTON_DOCUMENTATION_PLAN.md (detailed strategy)
- MLTON_DOCUMENTATION_CHECKPOINT.md (full resumption guide)
- DOCUMENTATION_STATUS.md (status overview)
- This file (quick start)

**Exclude from docs**:
- CLAUDE.md (development instructions, keep in main branch)

---

**Ready to start!** Begin with mlton/README.md following the plan in MLTON_DOCUMENTATION_CHECKPOINT.md
