"boa.plot" <-
function(type, dev = boa.par("dev"), mfdim = boa.par("plot.mfdim"),
                     newplot = boa.par("plot.new"),
                     onelink = boa.par("plot.onelink"))
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   drawn <- FALSE
   switch(type,
      "acf"     = { foo <- "boa.plot.acf"
                    foo.args <- expression(list(largs[[idx[i]]],
                                                pargs[[idx[i]]]))
                    onelink <- TRUE },
      "bandg"   = { foo <- "boa.plot.bandg"
                    foo.args <- list()
                    onelink <- FALSE },
      "density" = { foo <- "boa.plot.density"
                    foo.args <- expression(list(largs[[idx[i]]],
                                                pargs[[idx[i]]])) },
      "gandr"   = { foo <- "boa.plot.gandr"
                    foo.args <- expression(list(pargs[[idx[i]]]))
                    onelink <- FALSE },
      "geweke"  = { foo <- "boa.plot.geweke"
                    foo.args <- expression(list(largs[[idx[i]]],
                                                pargs[[idx[i]]]))
                    onelink <- TRUE },
      "history" = { foo <- "boa.plot.history"
                    foo.args <- expression(list(largs[[idx[i]]],
                                                pargs[[idx[i]]])) },
      "trace"   = { foo <- "boa.plot.trace"
                    foo.args <- expression(list(largs[[idx[i]]],
                                                pargs[[idx[i]]])) },
      { foo <- NULL
        cat("Warning: plot type does not exist\n") }
   )
   if(is.character(foo)) {
      work <- boa.chain("work")
      lnames <- names(work)
      largs <- pargs <- list(0)
      pidx <- NULL
      for(i in lnames) {
         pnames <- boa.pnames(work[[i]])
         for(j in pnames) {
            if(onelink) {
               n <- length(largs)
               largs[[n + 1]] <- i
               pargs[[n + 1]] <- j
               pidx <- c(pidx, paste(j, i))
            } else if(is.element(j, pidx)) {
               largs[[j]] <- c(largs[[j]], i)
            } else {
               largs[[j]] <- i
               pargs[[j]] <- j
               pidx <- c(pidx, j)
            }
         }
      }
      largs[[1]] <- pargs[[1]] <- NULL

      if(!newplot)  boa.plot.close(boa.par("dev.list"))

      idx <- order(pidx)
      n <- length(idx)
      size <- prod(mfdim)
      newdim <- mfdim
      imin <- ifelse(mfdim[1] <= mfdim[2], 1, 2)
      imax <- imin %% 2 + 1
      ratio <- mfdim[imin] / mfdim[imax]
      for(i in 1:n) {
         if((size == 1) || ((i %% size) == 1)) {
            boa.plot.open(dev)
            nleft <- n - i + 1
            while((nleft <= prod(newdim)) && (i == 1)) {
               if(newdim[1] > 1) {
                  mfdim <- c(ceiling(nleft / newdim[2]), newdim[2])
                  newdim[imax] <- newdim[imax] - 1
                  newdim[imin] <- round(ratio * newdim[imax])
               } else {
                  mfdim <- c(1, nleft)
                  newdim <- 0
               }
            }
            boa.plot.par(mfdim)
         }
         drawn <- do.call(foo, args = eval(foo.args)) || drawn
         boa.plot.title()
      }
   }

   return(drawn)
}
