"boa.chain.import" <-
function(prefix, path = boa.par("path"), type = "ASCII")
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   link <- NULL
   switch(type,
      "ASCII" = link <- boa.importASCII(prefix, path),
      "BUGS"  = link <- boa.importBUGS(prefix, path),
      "S"     = if(length(prefix) && exists(prefix))
                   link <- as.matrix(get(prefix))
                else
                   cat("Warning: could not find object", prefix, "to import.\n"),
      cat("Warning: import type not supported\n")
   )

   return(is.matrix(link) && boa.chain.add(link, prefix))
}
