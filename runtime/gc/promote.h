/* Copyright (C) 2017 Adrien Guatto.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef PROMOTE_H
#define PROMOTE_H

pointer HM_Promote(GC_state s, struct HM_ChunkInfo *dst_chunk, pointer src);

#endif  /* PROMOTE_H */
