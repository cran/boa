"boa.par" <-
function(...)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   if(nargs() == 0)  return(.boa.par)
   temp <- list(...)
   if((length(temp) == 1) && is.null(names(temp))) {
      arg <- temp[[1]]
      switch(mode(arg),
         list = temp <- arg,
         character = return(.boa.par[[arg]]),
         temp <- NULL
      )
   }
   changed <- NULL
   globals <- names(temp)
   idx <- is.element(globals, names(.boa.par))
   if(!all(idx)) {
      cat("Warning: invalid arguments\n")
      print(globals[!idx])
   }
   if(any(idx)) {
      globals <- globals[idx]
      pclass <- unlist(lapply(.boa.par[globals], "data.class"))
      idx <- unlist(lapply(temp[globals], "data.class")) == pclass
      if(!all(idx)) {
         cat("Warning: arguments must be of type\n")
         print(pclass[!idx])
      }
      if(any(idx)) {
         globals <- globals[idx]
         current <- .boa.par
         changed <- current[globals]
         current[globals] <- temp[globals]
         assign(".boa.par", current, envir = globalenv())
      }
   }
   invisible(changed)
}
