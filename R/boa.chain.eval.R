"boa.chain.eval" <-
function(expr, pname)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
{
   master <- boa.chain("master")
   master.support <- boa.chain("master.support")
   for(i in names(master)) {
      pnames <- boa.pnames(master[[i]])
      if(is.element(pname, pnames)) {
         cat("Warning: found ", pname, " in ", i, "; skipping chain\n",
             sep = "")
      } else {
         pnames <- c(pnames, pname)
         master[[i]] <- cbind(master[[i]],
                              eval(expr, as.data.frame(master[[i]])))
         dimnames(master[[i]])[[2]] <- pnames
         master.support[[i]] <- cbind(master.support[[i]], c(-Inf, Inf))
         dimnames(master.support[[i]])[[2]] <- pnames
      }
   }
   if(boa.chain("work.sync")) {
      boa.chain(master = master, master.support = master.support,
                work = master, work.support = master.support)
   } else {
      boa.chain(master = master, master.support = master.support)
   }
   invisible()
}
