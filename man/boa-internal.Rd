\name{survival-internal}
\alias{boa.menu.analysis}
\alias{boa.menu.chains}
\alias{boa.menu.coda}
\alias{boa.menu.data}
\alias{boa.menu.file}
\alias{boa.menu.import}
\alias{boa.menu.par}
\alias{boa.menu.parms}
\alias{boa.menu.plot}
\alias{boa.menu.plotcoda}
\alias{boa.menu.plotdesc}
\alias{boa.menu.setpar}
\alias{boa.menu.stats}
\alias{boa.menu.window}
\alias{key}

\title{Internal BOA Functions}
\description{Internal BOA functions}

\usage{
boa.menu.analysis()
boa.menu.chains()
boa.menu.coda()
boa.menu.data()
boa.menu.file()
boa.menu.import()
boa.menu.par()
boa.menu.parms()
boa.menu.plot()
boa.menu.plotcoda()
boa.menu.plotdesc()
boa.menu.setpar(group)
boa.menu.stats()
boa.menu.window()
key(x = c(par("usr")[1:2] %*% c(0.01, 0.99)),
y = c(par("usr")[3:4] %*% c(0.01, 0.99)), ..., title = "", align = TRUE,
background = 0, border = 0, between = 1, corner = c(missing(x), 1), divide = 3,
transparent = FALSE, cex = par("cex"), cex.title = max(cex), col = par("col"),
lty = par("lty"), lwd = par("lwd"), font = par("font"), pch = par("pch"),
adj = 0, type = "l", size = 5, columns = 1, between.columns = 3, angle = 0,
density = -1, plot = TRUE, text.width.multiplier = 1)
}

\details{These are not to be called by the user}
\keyword{internal}
