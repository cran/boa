"boa.menu.par" <-
function()
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   mtitle <- "\nGLOBAL OPTIONS MENU\n==================="
   choices <- c("Back",
                "------------+",
                "Analysis... |",
                "Data...     |",
                "Plot...     |",
                "All...      |",
                "------------+")
   idx <- 1
   while(idx > 0) {
      idx <- menu(choices, title = mtitle)
      switch(idx,
         "1" = idx <- -1,
         "2" = NULL,
         "3" = boa.menu.setpar("Analysis"),
         "4" = boa.menu.setpar("Data"),
         "5" = boa.menu.setpar("Plot"),
         "6" = boa.menu.setpar(),
         "7" = NULL
      )
   }

   return(abs(idx))
}
