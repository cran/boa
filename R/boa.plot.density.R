"boa.plot.density" <-
function(lnames, pname, bandwidth = boa.par("bandwidth"),
                             window = boa.par("kernel"),
                             legend = boa.par("legend"))
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   work <- boa.chain("work")
   ipname <- list()
   xydensity <- list()
   xlim <- NULL
   ylim <- NULL
   key.names <- NULL
   lnames <- intersect(names(work), lnames)
   k <- 0
   for(i in lnames) {
      ipname[[i]] <- intersect(boa.pnames(work[[i]]), pname)
      for(j in ipname[[i]]) {
         k <- k + 1
         width <- bandwidth(work[[i]][, j])
         xydensity[[k]] <- density(work[[i]][, j], n = 100, width = width,
                                   window = window)
         xlim <- range(xlim, xydensity[[k]]$x)
         ylim <- range(ylim, xydensity[[k]]$y)
      }
      key.names <- c(key.names, substring(i, first = 1, last = 16))
   }
   drawn <- k > 0
   if(drawn) {
      plot(xlim, ylim, xlab = pname, ylab = "Density", xlim = xlim,
           ylim = ylim, type = "n")
      k <- 0
      for(i in lnames) {
         for(j in ipname[[i]]) {
            k <- k + 1
            lines(xydensity[[k]], lty = k)
            parm <- work[[i]][, j]
            points(parm, rep(0, length(parm)), pch = k)
         }
      }
      if(legend) key(x = xlim[2], y = ylim[2], corner = c(1, 1),
        text = list(key.names), lines = list(lty = 1:k), transparent = TRUE)
      title("Density Plot")
   }

   return(drawn)
}
