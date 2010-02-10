
\name{global.bbox}
\alias{global.bbox}
\alias{global.bbox.save}
\alias{global.bbox.fetch}
\title{Global bounding box}
\description{
Computes, sets or retrives the global spatial bounding box.
}

\details{
	\link{global.bbox} computes the global bounding box of the selected files.
	\link{global.bbox.save} saves the global bounding box to the \sQuote{metadata} table toghether with the proj4 string. 
}


\usage{
	global.bbox(shpFiles, check.proj)
	global.bbox.save(shpFiles, con)
	global.bbox.fetch(con)
	}
	
\arguments{
	  \item{shpFiles}{range file location on disk as retrived by \code{\link{selectShpFiles}}}
	  \item{check.proj}{
		If \sQuote{FALSE}, the default, the proj4 string of the 1st range file will be saved to the database. 
		If \sQuote{TRUE} the proj4 string of every range will be checked and the function will stop and given an error if more than one projection is found.}
	  \item{con}{A valid sqlite connection.}
	}	
	
\seealso{
	\code{\link{rangeMapper}} for a complete example.
	\code{\link{proj4string}}
	\code{\link{bbox}} 
	}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}
\keyword{spatial}
\keyword{sqlite}
