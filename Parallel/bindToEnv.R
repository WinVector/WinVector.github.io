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
#' @param bindTargetEnv environment to bind to
#' @param objNames additional names to lookup in parent environment and bind
#' @param names of functions to NOT rebind the lexical environments of
bindToEnv <- function(bindTargetEnv=parent.frame(),objNames,doNotRebind=c()) {
  # Bind the values into environment
  # and switch any functions to this environment!
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

