"boa.plot.acf" <-
function(lname, pname, annotate = boa.par("legend"))
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   drawn <- FALSE
   parm <- boa.getparms(boa.chain("work")[[lname]], pname)
   if(is.matrix(parm)) {
      drawn <- TRUE
      result <- acf(parm, plot = FALSE)
      plot(result$lag, result$acf, xlab = "Lag", ylab = "Autocorrelation",
           main = pname, ylim = c(-1, 1), type = "h")
      abline(0, 0)
      usr <- par("usr")
      if (annotate)
         legend(x = usr[2], y = 1, xjust = 1, yjust = 1,
                legend = substring(lname, first = 1, last = 16), bty = "n")
   }

   return(drawn)
}
