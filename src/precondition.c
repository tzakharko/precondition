#include <Rinternals.h>
#include <assert.h>


static  SEXP context_frame = NULL;

SEXP ffi_get_context_frame(void) {
  return context_frame == NULL ? R_NilValue : context_frame;
}

// used to pass data to R_ExecWithCleanup
typedef struct {
  SEXP message;
  SEXP expr;
  SEXP expr_env;
  SEXP prev_context_frame;
} assertion_info_t;


SEXP diagnose_assertion_failure(assertion_info_t* assertion_info) {
  // if diagnostics has been performed, we stop
  if(Rf_findVarInFrame(context_frame, install(".diagnostics")) != R_UnboundValue) {
    return R_NilValue;
  }

  // diagnose the expression by calling the diagnosis function in the correct frame
  SEXP diagnosis_fun = PROTECT(Rf_findFun(install("diagnose_expressions"), context_frame));
  SEXP diagnosis_call = PROTECT(Rf_lang2(diagnosis_fun, assertion_info->expr));
  SEXP details = R_tryEval(diagnosis_call, assertion_info->expr_env, NULL);

  // check that debug_expressions does not fail
  if(details == NULL) error("internal error in the package precondition");

  details = PROTECT(details);

  // call R diagnose_assertion_failure()
  SEXP call = PROTECT(Rf_lang3(
    install("diagnose_assertion_failure"),
    PROTECT(Rf_lang3(install("make_default_message"), assertion_info->message, details)),
    details
  ));
  SET_TAG(CDDR(call), install(".details"));
  R_tryEvalSilent(call, context_frame, NULL);

  UNPROTECT(5);

  return R_NilValue;
}


void context_cleanup(assertion_info_t* assertion_info) {
  // restore the context frame
  context_frame = assertion_info->prev_context_frame;
}



SEXP ffi_assert_all(SEXP call, SEXP op, SEXP args, SEXP frame) {
  SEXP dots = Rf_findVarInFrame(frame, R_DotsSymbol);
  
  // push the context 
  assertion_info_t assertion_info = { R_NilValue, R_NilValue, R_NilValue, context_frame};
  context_frame = frame;
  

  for(; dots != R_NilValue; dots = CDR(dots)) {
    // inspect the next argument
    SEXP arg = CAR(dots);

    if(TYPEOF(arg) == PROMSXP) {
      assertion_info.expr = R_PromiseExpr(arg);
      assertion_info.expr_env = PRENV(arg);
    } else {
      assertion_info.expr = arg;
      assertion_info.expr_env = R_EmptyEnv;
    }

    // scalar strings are diagnostic messages
    if (IS_SCALAR(assertion_info.expr, STRSXP)) {
      assertion_info.message = assertion_info.expr;
      continue;      
    }

    // evaluate the argument
    SEXP result = R_tryEvalSilent(arg, R_EmptyEnv, NULL);

    if(result == NULL || !IS_SCALAR(result, LGLSXP) || LOGICAL(result)[0] != 1) {
      // diagnose the failure with context cleanup
      R_ExecWithCleanup(
        (SEXP (*)(void *))&diagnose_assertion_failure, 
        &assertion_info,
        (void (*)(void *))&context_cleanup,
        &assertion_info
      );

      return Rf_ScalarLogical(0);
    }
  }

  // restore the context frame
  context_frame = assertion_info.prev_context_frame;
  return Rf_ScalarLogical(1);
}


static const R_CallMethodDef call_ffi_funcs[] = {
  {"ffi_get_context_frame",  (DL_FUNC) &ffi_get_context_frame, 0},
  
  {NULL, NULL, 0}
};


static const R_ExternalMethodDef external_ffi_funcs[] = {
  {"ffi_assert_all",  (DL_FUNC) &ffi_assert_all, 0},
  
  {NULL, NULL, 0}
};


void R_init_precondition(DllInfo *info)
{
  // register the FFI routines
  R_registerRoutines(info, NULL, call_ffi_funcs, NULL, external_ffi_funcs);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}
