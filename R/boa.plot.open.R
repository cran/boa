"boa.plot.open" <-
function(which = boa.par("dev"))
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   created <- NULL
   devices <- c("graphsheet", "motif", "openlook", "win.graph", "windows",
                "X11")
   if(is.element(which, devices) && exists(which)) {
      do.call(which, args = list())
      created <- dev.cur()
      boa.par(dev.list = intersect(c(boa.par("dev.list"), created), dev.list()))
   } else {
      cat("Warning: graphics device not supported on this platform\n")
   }

   return(created)
}
