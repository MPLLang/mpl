/* Copyright (C) 2017 Ram Raghunathan
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef TLS_OBJECTS_H_
#define TLS_OBJECTS_H_

struct TLSObjects {
  struct rusage MLton_Rusage_self;
  struct rusage MLton_Rusage_children;
  struct rusage MLton_Rusage_gc;
};

#endif /* TLS_OBJECTS_H_ */
