"boa.menu.import" <-
function()
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   mtitle <- "\nIMPORT DATA MENU\n----------------"
   choices <- c("Back",
                "---------------------------+",
                "BUGS Output File           |",
                "Flat ASCII File            |",
                "Data Matrix Object         |",
                "View Format Specifications |",
                "Options...                 |",
                "---------------------------+")
   idx <- 1
   while(idx > 0) {
      idx <- menu(choices, title = mtitle)
      switch(idx,
         "1" = idx <- -1,
         "2" = NULL,
         "3" = { cat("\nEnter filename prefix without the .ind or .out ",
                     "extension [Working Directory: ",
                     deparse(boa.par("path")), "]\n", sep = "")
                 value <- scan(what = "", n = 1, strip.white = TRUE)
                 if(boa.chain.import(value, type = "BUGS"))
                    cat("+++ Data successfully imported +++\n")
               },
         "4" = { cat("\nEnter filename prefix without the ",
                     boa.par("ASCIIext"), " extension [Working Directory: ",
                     deparse(boa.par("path")), "]\n", sep = "")
                 value <- scan(what = "", n = 1, strip.white = TRUE)
                 if(boa.chain.import(value, type = "ASCII"))
                    cat("+++ Data successfully imported +++\n")
               },
         "5" = { cat("\nEnter object name [none]\n")
                 value <- scan(what = "", n = 1, strip.white = TRUE)
                 if(boa.chain.import(value, type = "S"))
                    cat("+++ Object successfully imported +++\n")
               },
         "6" = { cat("\nBUGS\n",
                     "- Bayesian inference Using Gibbs Sampling output files",
                     " (*.ind and *.out)\n",
                     "- files must be located in the Working Directory (see",
                     " Options)\n",
                     "\nASCII\n",
                     "- ASCII file (*", boa.par("ASCIIext"), ") containing",
                     " the monitored parameters from one run of the\n",
                     "  sampler\n",
                     "- file must be located in the Working Directory (see",
                     " Options)\n",
                     "- parameters are stored in space, comma, or tab",
                     " delimited columns\n",
                     "- parameter names must appear in the first row\n",
                     "- iteration numbers may be specified in a column",
                     " labeled 'iter'\n",
                     "\nMatrix Object\n",
                     "- S or R numeric matrix whose columns contain the",
                     " monitored parameters from one\n",
                     "  run of the sampler\n",
                     "- iteration numbers and parameter names may be",
                     " specified in the dimnames\n", sep = "")
                 cat("\nPress <ENTER> to continue")
                 readline()
               },
         "7" = boa.menu.setpar("Data"),
         "8" = NULL
      )
   }

   return(abs(idx))
}
