"boa.quit" <-
function()
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   cat("Exiting BOA session...\n")
   remove(".boa.par", envir = globalenv())
   remove(".boa.pardesc", envir = globalenv())
   remove(".boa.chain", envir = globalenv())
   remove(".boa.version", envir = globalenv())
   invisible()
}
