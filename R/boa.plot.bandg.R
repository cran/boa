"boa.plot.bandg" <-
function(bins = boa.par("gandr.bins"),
                           win = boa.par("gandr.win"))
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   drawn <- FALSE
   work <- boa.chain("work")
   work.support <- boa.chain("work.support")
   riter <- NULL
   for(i in names(work))  riter <- range(riter, boa.iter(work[[i]]))
   x <- unique(round(seq(min(riter[1] + 49, riter[2]), riter[2],
                         length = bins)))
   Rp <- NULL
   Rmax <- NULL
   for(i in x) {
      result <- boa.chain.gandr(work, work.support, 1, window = win, to = i)
      Rp <- c(Rp, result$mpsrf)
      Rmax <- c(Rmax, max(result$psrf))
   }
   idx <- is.finite(Rp)
   if(any(idx)) {
      drawn <- TRUE
      x <- x[idx]
      Rp <- spline(x, Rp[idx])
      Rmax <- spline(x, Rmax[idx])
      ylim <- range(1, Rp$y, Rmax$y)
      plot(Rmax, xlab = "Last Iteration in Segment", ylab = "Shrink Factor",
           ylim = ylim, type = "l")
      lines(Rp, lty = 2)
      abline(1, 0, lty = 3)
      usr <- par("usr")
      key(x = usr[2], y = ylim[2], corner = c(1, 1),
          text = list(c("Rp", "Rmax")), lines = list(lty = c(2, 1)),
          transparent = TRUE)
      title("Brooks & Gelman Multivariate Shrink Factors")
   }

   return(drawn)
}
