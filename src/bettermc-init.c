#include "bettermc.h"
#include <R_ext/Rdynload.h>

void R_init_bettermc(DllInfo *info) {
  static const R_CallMethodDef callMethods[]  = {
    {"copy2shm", (DL_FUNC) &copy2shm, 4},
    {"allocate_from_shm", (DL_FUNC) &allocate_from_shm, 6},
    {"is_altrep", (DL_FUNC) &is_altrep, 1},
    {"is_allocated", (DL_FUNC) &is_allocated, 1},
    {"unlink_all_shm", (DL_FUNC) &unlink_all_shm, 2},
    {"char_map", (DL_FUNC) &char_map, 1},
    {"char_map_long", (DL_FUNC) &char_map_long, 1},
    {"set_attr", (DL_FUNC) &set_attr, 2},
    {"semaphore_open", (DL_FUNC) &semaphore_open, 4},
    {"semaphore_post", (DL_FUNC) &semaphore_post, 1},
    {"semaphore_wait", (DL_FUNC) &semaphore_wait, 1},
    {"semaphore_close", (DL_FUNC) &semaphore_close, 1},
    {"semaphore_unlink", (DL_FUNC) &semaphore_unlink, 1},
    {"semaphorev_open", (DL_FUNC) &semaphorev_open, 1},
    {"semaphorev_post", (DL_FUNC) &semaphorev_post, 2},
    {"semaphorev_wait", (DL_FUNC) &semaphorev_wait, 2},
    {"semaphorev_unlink", (DL_FUNC) &semaphorev_unlink, 1},
    {"is_uneval_promise", (DL_FUNC) &is_uneval_promise, 2},
    {NULL, NULL, 0}
  };

  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}
