"boa.plot.acf" <-
function(lname, pname, legend = boa.par("legend"))
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   drawn <- FALSE
   parm <- boa.getparms(boa.chain("work")[[lname]], pname)
   if(is.matrix(parm)) {
      drawn <- TRUE
      result <- acf(parm, plot = FALSE)
      plot(result$lag, result$acf, xlab = "Lag", ylab = pname,
           ylim = c(-1, 1), type = "h")
      abline(0, 0)
      usr <- par("usr")
      if (legend) key(x = usr[2], y = 1, corner = c(1, 1),
                      text = substring(lname, first = 1, last = 16),
                      adj = 1, transparent = TRUE)
   }

   return(drawn)
}
