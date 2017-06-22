nlapply = function(X, FUN, ...){
  answer = lapply(X = X, FUN = FUN, ...)
  names(answer) = X
  answer
  }
