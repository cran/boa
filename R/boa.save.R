"boa.save" <-
function(name, envir = globalenv())
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   saved <- !is.element(name, c(".boa.par", ".boa.pardesc", ".boa.chain",
                                ".boa.version"))
   if(saved) {
      assign(name, list(par = boa.par(),
                        chain = boa.chain(),
                        version = boa.version()), envir = envir)
   } else {
      cat("Warning: object name is in use by the program\n")
   }

   return(saved)
}
