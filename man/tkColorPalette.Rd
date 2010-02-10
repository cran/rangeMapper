
\name{tkColorPalette}
\alias{tkColorPalette}
\title{
  Choose a color palette.
}
\description{
 Interactively choose a color palette. 
}
\usage{
tkColorPalette(pal, name, palette.size = 45, envir = .GlobalEnv)
}
\arguments{
  \item{pal}{\code{pal} a named list as e.g. returned by \code{\link{brewer.pal.get}}.}
  \item{name}{A quoted \code{name} to be assigned to  \code{envir}.}
  \item{palette.size}{\code{palette.size} is the size of the tiles (\code{tkbutton}-s) in pixels.}
  \item{envir}{the \code{\link{environment}} to use.}
}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\seealso{
  \code{\link{colorRampPalette}}
  \code{\link{brewer.pal.get}}
}
\examples{
\dontrun{
## These cannot be run by examples() but should be OK when pasted
## into an interactive R session 
pals =  brewer.pal.get ()
tkColorPalette(pal = pals, name  = "pal1")
tkColorPalette(pal = pals, name  = "pal2", palette.size = 17)
pal1
pal2
}
}

\keyword{tcltk}
\keyword{color}
