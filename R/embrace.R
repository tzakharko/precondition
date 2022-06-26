# Check if an expression is embraced usign the `{{` operator
is_embraced <- function(expr) {
  is.call(expr) &&
  identical(expr[[1]], as.name("{")) &&
  length(expr) == 2 &&
  is.call(expr[[2]]) &&
  identical(expr[[2]][[1]], as.name("{")) &&
  length(expr[[2]]) == 2 
}

  
# Remove the instances of the embrace operator from an expression, 
# cleaning it up, while reporting back any embraced subexpressions
deembrace <- function(expr, embraced_expr_callback = identity) {
  if(is_embraced(expr)) {
    expr <- expr[[2]][[2]]
    embraced_expr_callback(expr)
  } else 
  if(is.pairlist(expr) || is.call(expr)) {
    for(i in seq_len(length(expr))) {
      expr[[i]] <- deembrace(expr[[i]], embraced_expr_callback)
    }
  }

  expr
}
