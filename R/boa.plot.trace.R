"boa.plot.trace" <-
function(lnames, pname, legend = boa.par("legend"))
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   work <- boa.chain("work")
   ipname <- list()
   ybar <- list()
   xlim <- NULL
   ylim <- NULL
   key.names <- NULL
   lnames <- intersect(names(work), lnames)
   for(i in lnames) {
      ipname[[i]] <- intersect(boa.pnames(work[[i]]), pname)
      ylim <- range(ylim, work[[i]][, ipname[[i]]])
      xlim <- range(xlim, boa.iter(work[[i]]))
      key.names <- c(key.names, substring(i, first = 1, last = 16))
   }
   drawn <- is.vector(key.names)
   if(drawn) {
      plot(xlim, ylim, xlab = "Iteration", ylab = pname,
           xlim = xlim, ylim = ylim, type = "n")
      k <- 0
      for(i in lnames) {
         for(j in ipname[[i]]) {
            k <- k + 1
            lines(boa.iter(work[[i]]), work[[i]][, j], lty = k)
         }
      }
      if(legend) key(x = xlim[2], y = ylim[2], corner = c(1, 1),
        text = list(key.names), lines = list(lty = 1:k), transparent = TRUE)
      title("Trace Plot")
   }

   return(drawn)
}
