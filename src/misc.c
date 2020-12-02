#include "bettermc.h"

SEXP is_uneval_promise(SEXP name, SEXP env) {
  SEXP object = Rf_findVar(installChar(STRING_ELT(name, 0)), env);

  if (TYPEOF(object) == PROMSXP) {
    return ScalarLogical(PRVALUE(object) == R_UnboundValue);
  }

  return ScalarLogical(FALSE);
}
