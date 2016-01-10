#' Copy arguments into env and re-bind any function's lexical scope to bindTargetEnv .
#' 
#' See http://winvector.github.io/Parallel/PExample.html for example use.
#' 
#' 
#' Used to send data along with a function in situations such as parallel execution 
#' (when the global environment would not be available).  Typically called within 
#' a function that constructs the worker function to pass to the parallel processes
#' (so we have a nice lexical closure to work with).
#' 
#' @param ... variables to be bound to bindTargetEnvironment
#' @param bindTargetEnv environment to bind to
#' @param objNames additional names to lookup in parent environment and bind
#' @param names of functions to NOT rebind the lexical environments of
bindToEnv <- function(...,bindTargetEnv=parent.frame(),objNames=c(),doNotRebind=c()) {
  # get names of variables used as ... arguments in function call
  dnames <- sapply(substitute(list(...))[-1],deparse)
  # get the values
  dvalues <- list(...)
  # makes sure there are no other named assignments
  if(!is.null(names(dvalues))) {
    stop("unexpected named assignments")
  }
  # merge in any value (non "=") assignments as names
  names(dvalues) <- dnames
  # now bind the values into environment
  # and switch any functions to this environment!
  for(var in names(dvalues)) {
    val <- dvalues[[var]]
    if(is.function(val) && (!(var %in% doNotRebind))) {
      # replace function's lexical environment with our target (DANGEROUS)
      environment(val) <- bindTargetEnv
    }
    # assign object to target environment, only after any possible alteration
    assign(var,val,envir=bindTargetEnv)
  }
  # same for any names from the parent environment
  if(!is.null(objNames)) {
    for(var in objNames) {
      val <- get(var,envir=parent.frame())
      if(is.function(val) && (!(var %in% doNotRebind))) {
        # replace function's lexical environment with our target (DANGEROUS)
        environment(val) <- bindTargetEnv
      }
      # assign object to target environment, only after any possible alteration
      assign(var,val,envir=bindTargetEnv)
    }
  }
}

