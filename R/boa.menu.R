"boa.menu" <-
function(recover = FALSE)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   if(!recover) {
      boa.init()
      cat("NOTE: if the menu unexpectedly terminates, type \"boa.menu(recover",
          "= TRUE)\" to\n",
          "restart and recover your work\n", sep = "")
   }
   mtitle <- "\nBOA MAIN MENU\n*************"
   choices <- c("File     >>", "Data     >>", "Analysis >>", "Plot     >>",
                "Options  >>", "Window   >>")
   idx <- 1
   while(idx != 99) {
      idx <- menu(choices, title = mtitle)
      switch(idx,
         "1" = idx <- boa.menu.file(),
         "2" = boa.menu.data(),
         "3" = boa.menu.analysis(),
         "4" = boa.menu.plot(),
         "5" = boa.menu.par(),
         "6" = boa.menu.window()
      )
   }
   boa.quit()
   invisible()
}
