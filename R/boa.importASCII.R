"boa.importASCII" <-
function(prefix, path = NULL)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   link <- NULL
   filename <- paste(path, paste(prefix, boa.par("ASCIIext"), sep=""), sep = "/")
   if(file.exists(filename)) {
      data <- read.table(filename, header = TRUE)
      idx <- match("iter", names(data), nomatch = 0)
      if(idx > 0) {
         dimnames(data)[[1]] <- data[[idx]]
         data[[idx]] <- NULL
      }
      link <- as.matrix(data)
   } else {
      cat("Warning: could not find '", filename, "' to import\n", sep = "")
   }

   return(link)
}
