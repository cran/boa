"boa.plot.geweke" <-
function(lname, pname, bins = boa.par("geweke.bins"),
                            p.first = boa.par("geweke.first"),
                            p.last = boa.par("geweke.last"),
                            alpha = boa.par("alpha"))
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   drawn <- FALSE
   parm <- boa.getparms(boa.chain("work")[[lname]], pname)
   if(is.matrix(parm)) {
      riter <- range(boa.iter(parm))
      x <- unique(round(seq(riter[2], riter[1], length = bins + 1)))[-1]
      y <- NULL
      for(i in x) {
         y <- c(y, boa.geweke(boa.getiter(parm, i:riter[2]),
                              p.first, p.last)[1,"Z-Score"])
      }
      if(any(is.finite(y))) {
         drawn <- TRUE
         q.upper <- qnorm(1 - alpha / 2)
         ylim <- range(y, -q.upper, q.upper, na.rm = TRUE)
         plot(x, y, xlab = "First Iteration in Segment", ylab = "Z-Score",
              ylim = ylim)
         abline(q.upper, 0, lty = 2)
         abline(0, 0)
         abline(-q.upper, 0, lty = 2)
         usr <- par("usr")
         key(x = usr[2], y = ylim[2], corner = c(1, 1),
             text = paste(pname, ":", substring(lname, first = 1,
                         last = 16)), adj = 1, transparent = TRUE)
         title("Geweke Convergence Diagnostic")
      }
   }

   return(drawn)
}
