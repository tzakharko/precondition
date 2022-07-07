#include <Rinternals.h>
#include <assert.h>


// Unwrap promises to the original expressions
static SEXP substitute_promise(SEXP maybe_promise) {
  while(TYPEOF(maybe_promise) == PROMSXP) {
    maybe_promise = R_PromiseExpr(maybe_promise);
  }

  return maybe_promise;
}


// Check that all arguments in ... evaluate to TRUE 
// 
// First argument can be a character literal in which case it is ignored
//
// Performs no data type coercion and silently treats errors as FALSE
SEXP ffi_check_conditions(SEXP call, SEXP op, SEXP args, SEXP env) {
  SEXP dots = Rf_findVar(R_DotsSymbol, env);
  int error = 0;

  for(; dots != R_NilValue; dots = CDR(dots)) {
    SEXP check = CAR(dots);

    // skip the optional assertion message
    if(IS_SCALAR(substitute_promise(check), STRSXP)) continue;
    
    // evaluate the condition and check if it is TRUE
    SEXP r = R_tryEvalSilent(check, R_EmptyEnv, &error);
    if(!(error == 0 && IS_SCALAR(r, LGLSXP) && LOGICAL(r)[0] == 1)) {
      return Rf_ScalarLogical(0);
    }
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
