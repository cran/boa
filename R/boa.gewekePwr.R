"boa.gewekePwr" <-
function(link)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   spec <- NULL
   pnames <- boa.pnames(link)
   n <- nrow(link)
   nspans <- min(1 + sqrt(n) / 0.3, n - 1)
   if(n > 2) {
      for(i in pnames) {
         spec <- c(spec, spec.pgram(link[, i], spans = nspans, demean = TRUE,
                                    detrend = FALSE, plot = FALSE)$spec[1])
      }
      spec <- 10^(spec / 10)
   } else {
      spec <- rep(NA, length(pnames))
   }

   return(structure(spec, names = pnames))
}
