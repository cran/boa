#-------------------------------------------------------------------------------
# File: boa.core.q
# Description: Library of core functions for the Bayesian Output Analysis
#    Program (BOA)
# Author: Brian J. Smith <brian-j-smith@uiowa.edu>
#-------------------------------------------------------------------------------


boa.chain <- function(...)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   if(nargs() == 0)  return(.boa.chain)
   temp <- list(...)
   if((length(temp) == 1) && is.null(names(temp))) {
      arg <- temp[[1]]
      switch(mode(arg),
         list = temp <- arg,
         character = return(.boa.chain[[arg]]),
         temp <- NULL
      )
   }
   idx <- intersect(names(temp), names(.boa.chain))
   if(length(idx) > 0) {
      current <- .boa.chain
      current[idx] <- temp[idx]
      assign(".boa.chain", current, envir = globalenv())
   } else {
      cat("Warning: invalid arguments\n")
   }
   invisible()
}


boa.init <- function()
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   assign(".boa.par",
          list(acf.lags     = c(1, 5, 10, 50),
               alpha        = 0.05,
               bandwidth    = function(x)  0.5 * diff(range(x)) /
                                           (log(length(x)) + 1),
               batch.size   = 50,
               dev          = options()$device,
               dev.list     = numeric(0),
               ASCIIext     = ".txt",
               gandr.bins   = 20,
               gandr.win    = 0.5,
               geweke.bins  = 10,
               geweke.first = 0.1,
               geweke.last  = 0.5,
               handw.error  = 0.1,
               kernel       = "gaussian",
               legend       = TRUE,
               path         = "",
               plot.mfdim   = c(3, 2),
               plot.new     = FALSE,
               plot.onelink = FALSE,
               quantiles    = c(0.025, 0.5, 0.975),
               randl.error  = 0.005,
               randl.delta  = 0.001,
               randl.q      = 0.025,
               title        = TRUE), envir = globalenv())
   assign(".boa.pardesc",
          structure(rbind(
             c("Analysis", "Brooks, Gelman & Rubin", "Alpha Level", "alpha",
               ""),
             c("Analysis", "Brooks, Gelman & Rubin", "Window Fraction",
               "gandr.win", ""),
             c("Analysis", "Geweke", "Window 1 Fraction", "geweke.first", ""),
             c("Analysis", "Geweke", "Window 2 Fraction", "geweke.last", ""),
             c("Analysis", "Heidelberger & Welch", "Accuracy", "handw.error",
               ""),
             c("Analysis", "Heidelberger & Welch", "Alpha Level", "alpha",
               ""),
             c("Analysis", "Raftery & Lewis", "Accuracy", "randl.error", ""),
             c("Analysis", "Raftery & Lewis", "Alpha Level", "alpha", ""),
             c("Analysis", "Raftery & Lewis", "Delta", "randl.delta", ""),
             c("Analysis", "Raftery & Lewis", "Quantile", "randl.q", ""),
             c("Analysis", "Statistics", "ACF Lags", "acf.lags", ""),
             c("Analysis", "Statistics", "Alpha Level", "alpha", ""),
             c("Analysis", "Statistics", "Batch Size", "batch.size", ""),
             c("Analysis", "Statistics", "Quantiles", "quantiles", ""),
             c("Data", "Files", "Working Directory", "path",
               "Specified directory must not end with a slash"),
             c("Data", "Files", "ASCII File Ext", "ASCIIext", ""),
             c("Plot", "Brooks & Gelman", "Number of Bins", "gandr.bins", ""),
             c("Plot", "Brooks & Gelman", "Window Fraction", "gandr.win", ""),
             c("Plot", "Density", "Bandwidth", "bandwidth",
               "This defines the standard deviation of the smoothing kernel"),
             c("Plot", "Density", "Kernel", "kernel",
               "Possible kernels are gaussian, rectangular, triangular, or cosine"),
             c("Plot", "Gelman & Rubin", "Alpha Level", "alpha", ""),
             c("Plot", "Gelman & Rubin", "Number of Bins", "gandr.bins", ""),
             c("Plot", "Gelman & Rubin", "Window Fraction", "gandr.win", ""),
             c("Plot", "Geweke", "Alpha Level", "alpha", ""),
             c("Plot", "Geweke", "Number of Bins", "geweke.bins", ""),
             c("Plot", "Geweke", "Window 1 Fraction", "geweke.first", ""),
             c("Plot", "Geweke", "Window 2 Fraction", "geweke.last", ""),
             c("Plot", "Graphics", "Legend", "legend",
               "Include plot legends (T/F)"),
             c("Plot", "Graphics", "Title", "title", "Include plot title (T/F)"),
             c("Plot", "Graphics", "Keep Previous Plots", "plot.new", ""),
             c("Plot", "Graphics", "Plot Layout", "plot.mfdim", ""),
             c("Plot", "Graphics", "Plot Chains Separately", "plot.onelink", "")),
             dimnames =
                list(NULL, c("group", "method", "desc", "par", "note"))),
          envir = globalenv())
   assign(".boa.chain",
          list(master         = list(),
               master.support = list(),
               work           = list(),
               work.support   = list(),
               work.sync      = TRUE), envir = globalenv())
   assign(".boa.version",
          list(name     = "BOA",
               major    = 1,
               minor    = 1,
               revision = 2,
               system   = version$system), envir = globalenv())
   boa.license()
   invisible()
}


boa.load <- function(name, envir = globalenv())
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   loaded <- FALSE
   if(exists(name, envir = envir)) {
      obj <- get(name, envir = envir)
      if(is.character(obj$version$name) && is.numeric(obj$version$major) &&
         is.numeric(obj$version$minor)) {
         ver <- boa.version()
         loaded <- (obj$version$name == ver$name) &&
                   ((obj$version$major < ver$major) ||
                       ((obj$version$major == ver$major)
                       && (obj$version$minor >= ver$minor)))
      }
      if(loaded) {
         boa.par(obj$par)
         boa.par(dev.list = numeric(0))
         boa.chain(obj$chain)
      } else {
         cat("Warning: object is incompatible with this version of BOA\n")
      }
   } else {
      cat("Warning: object not found\n")
   }

   return(loaded)
}


boa.par <- function(...)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   if(nargs() == 0)  return(.boa.par)
   temp <- list(...)
   if((length(temp) == 1) && is.null(names(temp))) {
      arg <- temp[[1]]
      switch(mode(arg),
         list = temp <- arg,
         character = return(.boa.par[[arg]]),
         temp <- NULL
      )
   }
   changed <- NULL
   globals <- names(temp)
   idx <- is.element(globals, names(.boa.par))
   if(!all(idx)) {
      cat("Warning: invalid arguments\n")
      print(globals[!idx])
   }
   if(any(idx)) {
      globals <- globals[idx]
      pclass <- unlist(lapply(.boa.par[globals], "data.class"))
      idx <- unlist(lapply(temp[globals], "data.class")) == pclass
      if(!all(idx)) {
         cat("Warning: arguments must be of type\n")
         print(pclass[!idx])
      }
      if(any(idx)) {
         globals <- globals[idx]
         current <- .boa.par
         changed <- current[globals]
         current[globals] <- temp[globals]
         assign(".boa.par", current, envir = globalenv())
      }
   }
   invisible(changed)
}


boa.quit <- function()
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   cat("Exiting BOA session...\n")
   remove(".boa.par", envir = globalenv())
   remove(".boa.pardesc", envir = globalenv())
   remove(".boa.chain", envir = globalenv())
   remove(".boa.version", envir = globalenv())
   invisible()
}


boa.save <- function(name, envir = globalenv())
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   saved <- !is.element(name, c(".boa.par", ".boa.pardesc", ".boa.chain",
                                ".boa.version"))
   if(saved) {
      assign(name, list(par = boa.par(),
                        chain = boa.chain(),
                        version = boa.version()), envir = envir)
   } else {
      cat("Warning: object name is in use by the program\n")
   }

   return(saved)
}