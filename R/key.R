"key" <-
function(x = c(par("usr")[1:2] %*% c(0.01, 0.99)),
                y = c(par("usr")[3:4] %*% c(0.01, 0.99)), ..., title = "",
                align = TRUE, background = 0, border = 0, between = 1,
                corner = c(missing(x), 1), divide = 3, transparent = FALSE,
                cex = par("cex"), cex.title = max(cex), col = par("col"),
                lty = par("lty"), lwd = par("lwd"), font = par("font"),
                pch = par("pch"), adj = 0, type = "l", size = 5, columns = 1,
                between.columns = 3, angle = 0, density = -1, plot = TRUE,
                text.width.multiplier = 1)
{
   oldpar <- par("xpd")
   on.exit(par(oldpar))
   rest <- list(...)

   colnames <- names(rest)
   for(i in seq(colnames)[colnames == "text"])
      if(!is.list(rest[[i]]))  rest[[i]] <- list(rest[[i]])
   actions <- c("lines", "points", "text")
   colnames <- actions[match(colnames, actions, nomatch = 0)]

   pxy <- par("pin") / par("fin")
   nrows <- max(sapply(unlist(rest, recursive = FALSE), length))
   ncols <- length(colnames)
   text.adj <- width <- height <- matrix(0, nrows, ncols)
   between <- rep(pxy[1] * strwidth(" ") * between, ncols)

   replen <- function(a, b, n)  rep(if(is.null(a)) b else a, n)
   for(j in 1:ncols) {
      this <- rest[[j]]
      this$size <- replen(this$size, size, nrows)
      this$type <- replen(this$type, type, nrows)
      this$col <- replen(this$col, col, nrows)
      this$lty <- replen(this$lty, lty, nrows)
      this$lwd <- replen(this$lwd, lwd, nrows)
      this$adj <- replen(this$adj, adj, nrows)
      this$font <- replen(this$font, font, nrows)
      this$pch <- replen(this$pch, pch, nrows)
      rest[[j]] <- this
      for(i in 1:nrows) {
         cxy <- pxy * c(strwidth(" "), strheight(" "))
         switch(colnames[j],
            points = { width[i, j] <- cxy[1]
                       height[i, j] <- cxy[2] },
            lines  = { width[i, j] <- cxy[1] * this$size[i]
                       height[i, j] <- cxy[2] },
            text   = { width[i, j] <- pxy[1] * text.width.multiplier *
                                      strwidth(this[[1]][i])
                       height[i, j] <- cxy[2]
                       text.adj[i, j] <- this$adj[i] }
         )
      }
      if(align)  width[, j] <- max(width[, j])
   }

   xpos <- matrix(x, nrows, ncols)
   ypos <- matrix(y - cumsum(height[, 1]), nrows, ncols)
   for(j in seq(length = ncols - 1))
      xpos[, j + 1] <- xpos[, j] + width[, j] + between[j]
   xmax <- max(xpos[, ncols] + width[, ncols])
   i <- (text.adj != 0)
   if(any(i))  xpos[i] <- xpos[i] + width[i]
   title.excess <- max(0, x + text.width.multiplier * pxy[1] *
                          strwidth(title) - xmax)
   xmax <- xmax + title.excess
   if(nchar(title) > 0)
      ypos <- ypos - pxy[2] * strheight(title)
   ymin <- min(ypos)
   if(!plot)  return(c(xmax - x, y - ymin))
   x.offset <- (x - xmax) * corner[1]
   y.offset <- (y - ymin) * (1 - corner[2])
   xpos <- xpos + x.offset + title.excess / 2
   ypos <- ypos + y.offset + height / 2

   if(!transparent)
      polygon(c(x, xmax, xmax, x) + x.offset, c(y, y, ymin, ymin) + y.offset,
              col = background, border = border)

   if(nchar(title) > 0)
      text((x + xmax) / 2 + x.offset, y + y.offset - pxy[2] *
           strheight(title) / 2, title, adj = 0.5)

   for(j in 1:ncols) {
      this <- rest[[j]]
      for(i in 1:nrows) {
         switch(colnames[j],
            points = { points(xpos[i, j], ypos[i, j],
                              col = this$col[i], font = this$font[i],
                              pch = this$pch[i]) },
            lines  = { if(this$type[i] != "p") {
                          lines(xpos[i, j] +
                                seq(0, width[i, j], length = divide),
                                rep(ypos[i, j], divide),
                                lwd = this$lwd[i], type = this$type[i],
                                lty = this$lty[i], pch = this$pch[i],
                                font = this$font[i], col = this$col[i])
                          if(this$type[i] == "b" || this$type[i] == "o")
                             points(xpos[i, j] +
                                    seq(0, width[i, j], length = divide),
                                    rep(ypos[i, j], divide),
                                    lwd = this$lwd[i], type = "p", lty = 1,
                                    pch = this$pch[i], font = this$font[i],
                                    col = this$col[i])
                       } else {
                          points(xpos[i, j] + width[i, j] / 2, ypos[i, j],
                                 lwd = this$lwd[i], type = this$type[i],
                                 lty = this$lty[i], pch = this$pch[i],
                                 font = this$font[i], col = this$col[i])
                       } },
            text   = { text(xpos[i, j], ypos[i, j], this[[1]][i],
                            adj = this$adj[i], col = this$col[i],
                            font = this$font[i]) }
         )
      }
   }
   invisible()
}
