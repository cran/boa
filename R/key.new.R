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
    legend.args <- list()

    legend.args$x <- x
    legend.args$y <- y
    legend.args$xjust <- corner[1]
    legend.args$yjust <- corner[2]
    legend.args$bty <- ifelse(border == 0, "n", "o")

    key.args <- list(...)
    key.names <- names(key.args)
    if(is.element("text", key.names)) {
       legend.args$legend <- key.args$text[[1]]
    }
    if(is.element("lines", key.names)) {
       temp <- key.args$lines
       temp.names <- names(temp)
       if(is.element("lty", temp.names)) legend.args$lty <- temp$lty
       if(is.element("lwd", temp.names)) legend.args$lwd <- temp$lwd
    }

    do.call("legend", legend.args)
}
