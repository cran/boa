\name{boa.plot.open}
\alias{boa.plot.open}

\title{Open Graphics Windows}
\description{
  Opens a graphics window.
}
\usage{
boa.plot.open(which = boa.par("dev"))
}

\arguments{
  \item{which}{Character string giving the name of the function that creates 
  graphics windows on the current platform. For Unix systems this is either 
  "motif", "openlook", or "X11". For windows, "win.graph" should be specified.}
}

\value{
  If a graphics windows is successfully opened, the number of the current 
  graphics window is returned.
}

\author{Brian J. Smith}

\seealso{ \code{\link{boa.plot.close}} }

\keyword{utilities}
