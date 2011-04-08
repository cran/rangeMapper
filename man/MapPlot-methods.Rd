
\name{plot-methods}
\docType{methods}
\alias{plot,SpatialPixelsRangeMap,missing-method}


\title{Plot a SpatialPixelsRangeMap}

\description{
This is a wrapper around \code{\link{spplot}}
}

\section{Methods}{
	\describe{
	\item{\code{signature(x = "SpatialPixelsRangeMap", y = "missing")}}{

	\bold{x=SpatialPixelsRangeMap}

	\code{plot(x, colorpalette = brewer.pal.get('Spectral')[11:1], ncols = 20, scales = FALSE, style = "equal",  ...)}

	\tabular{rll}{
	\tab \code{x} \tab a SpatialPixelsRangeMap object \cr
	\tab \code{colorpalette}  \tab  A color palette. See also \code{\link{tkColorPalette}} which allows for interactive choice of color palettes \cr
	\tab \code{ncols}  \tab  Number of color classes required, default to 20; argument to be passed to \code{\link{classIntervals}}   \cr
	\tab \code{scales}  \tab  If \sQuote{FALSE}, default, axes scale are not drawn.  \cr
	\tab \code{style}  \tab  class interval style; see \code{\link{classIntervals}} for more details  \cr
	\tab \code{...}  \tab  Any argument that can be passed to \code{\link[fields]{image.plot}} and to \code{\link{spplot} }   \cr
	}		
		
} 
	}
		}


\seealso{
\code{\link{rangeMapper}} for a complete example.
}


\author{Mihai Valcu \email{valcu@orn.mpg.de}}

\keyword{methods}





	




















