"boa.menu.setpar" <-
function(group)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   if(missing(group)) {
      value <- boa.print.par()
   } else {
      value <- boa.print.par(group)
   }
   par.names <- value[, "par"]
   par.notes <- value[, "note"]
   cat("\n")
   idx <- ""
   while((length(idx) > 0) && !is.element(idx, seq(par.names))) {
      cat("Select parameter to change or press <ENTER> to continue\n")
      idx <- scan(what = "", n = 1, strip.white = TRUE)
   }
   if(length(idx) > 0) {
      cat("\n")
      idx <- as.numeric(idx)
      if(nchar(par.notes[idx]))  cat("NOTE:", par.notes[idx], "\n")
      switch(data.class(boa.par(par.names[idx])),
         "numeric"   = { cat("Enter new numeric value\n")
                         value <- eval(parse(text = scan(what = "", n = 1,
                                  sep = "\n")))
                       },
         "character" = { cat("Enter new character string\n")
                         value <- scan(what = "", n = 1, sep = "\n")
                       },
         "logical"   = { cat("Enter new logical value\n")
                         value <- eval(parse(text = scan(what = "", n = 1)))
                       },
         "function"  = { cat("Enter new function followed by a blank line\n")
                         value <- eval(parse(text = paste(scan(what = "",
                                  sep = "\n"), collapse = "\n")))
                       },
         value <- NULL
      )
      boa.par(structure(list(value), names = par.names[idx]))
   }
   invisible()
}
