"boa.chain" <-
function(...)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   if(nargs() == 0)  return(.boa.chain)
   temp <- list(...)
   if((length(temp) == 1) && is.null(names(temp))) {
      arg <- temp[[1]]
      switch(mode(arg),
         list = temp <- arg,
         character = return(.boa.chain[[arg]]),
         temp <- NULL
      )
   }
   idx <- intersect(names(temp), names(.boa.chain))
   if(length(idx) > 0) {
      current <- .boa.chain
      current[idx] <- temp[idx]
      assign(".boa.chain", current, envir = globalenv())
   } else {
      cat("Warning: invalid arguments\n")
   }
   invisible()
}
