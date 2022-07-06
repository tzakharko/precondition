#include <Rinternals.h>
#include <assert.h>


// Check that all arguments in ... evaluate to TRUE 
//
// Performs no data type coercion and silently treats errors as FALSE
SEXP ffi_check_conditions(SEXP call, SEXP op, SEXP args, SEXP env) {
  SEXP dots = Rf_findVar(R_DotsSymbol, env);
  int error = 0;

  for(; dots != R_NilValue; dots = CDR(dots)) {
    // evaluate the condition and check if it is a vector of TRUEs
    SEXP r = PROTECT(R_tryEvalSilent(CAR(dots), R_EmptyEnv, &error));
    if(error != 0 || Rf_isObject(r) || !Rf_isLogical(r)) {
      UNPROTECT(1);
      return Rf_ScalarLogical(0);
    }
    int* rptr = LOGICAL(r);
    for(int i = 0; i < LENGTH(r); i++) {
      if(!rptr[i]) {
        UNPROTECT(1);
        return Rf_ScalarLogical(0);        
      }
    }
    UNPROTECT(1);
  }

  return Rf_ScalarLogical(1);
}

static const R_ExternalMethodDef external_ffi_funcs[] = {
  {"ffi_check_conditions",  (DL_FUNC) &ffi_check_conditions, 0},

  {NULL, NULL, 0}
};


void R_init_precondition(DllInfo *info)
{
  // register the FFI routines
  R_registerRoutines(info, NULL, NULL, NULL, external_ffi_funcs);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}
