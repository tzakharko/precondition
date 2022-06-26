# # Panic and abort execution
# panic <- function(msg, .call = parent.frame()) {
#   # format the prefix
#   prefix <- format_error_prefix("Panic", find_call(.call))

#   # format and display the message
#   msg <- paste0(c("", prefix, format_error_bullets(msg), ""), collapse = "\n")
#   .Call(ffi_show_message, msg)

#   # print the traceback
#   if(has_rlang()) {
#     trace <- rlang::trace_back(bottom = .call)
#     trace_msg <- paste0(c("Traceback:", format(trace), ""), collapse = "\n")

#     .Call(ffi_show_message, trace_msg)
#   } else {
#     trace <- map_chr(sys.calls()[-sys.nframe()], format_call)
#     trace <- paste0(" ", seq_along(trace), ". ", trace)
#     trace_msg <- paste0(c("Traceback:", trace, ""), collapse = "\n")

#     .Call(ffi_show_message, trace_msg)
#   }

#   # in an interactive session we exit from the top level, in a script we 
#   # teardown the interpreter
#   if(interactive()) {
#     .Call(ffi_show_message, "Fatal error: panicked, aborting execution")
#     evalq(return(invisible(NULL)), .GlobalEnv)
#   } else {
#     .Call(ffi_suicide, "panicked, aborting execution")
#   }
# }