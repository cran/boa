"boa.load" <-
function(name, envir = globalenv())
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   loaded <- FALSE
   if(exists(name, envir = envir)) {
      obj <- get(name, envir = envir)
      if(is.character(obj$version$name) && is.numeric(obj$version$major) &&
         is.numeric(obj$version$minor)) {
         ver <- boa.version()
         loaded <- (obj$version$name == ver$name) &&
                   ((obj$version$major < ver$major) ||
                       ((obj$version$major == ver$major)
                       && (obj$version$minor >= ver$minor)))
      }
      if(loaded) {
         boa.par(obj$par)
         boa.par(dev.list = numeric(0))
         boa.chain(obj$chain)
      } else {
         cat("Warning: object is incompatible with this version of BOA\n")
      }
   } else {
      cat("Warning: object not found\n")
   }

   return(loaded)
}
