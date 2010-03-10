
\name{rangeMapper}
\alias{rangeMapper}
\docType{package}
\title{rangeMapper, a package to manipulate species range (extent-of-occurrence) maps.}
\description{ 
	\pkg{rangeMapper} performs range maps interpolation with a chosen regular grid, then computes at each grid cell a chosen statistic and displays the resulting map. 
	}
\details{
	\pkg{rangeMapper} is a package built around  RSQLite, sp, rgdal and maptools for manipulating species range (extent-of-occurrence) maps of e.g. species breeding distribution ranges.
	\pkg{rangeMapper} comes with an user-friendly platform-independent tcltk graphical user interface.
}
\keyword{package}
\keyword{spatial}

\author{
	Mihai Valcu \email{valcu@orn.mpg.de}, \url{http://orn.mpg.de/mitarbeiter/valcu.html} \cr
	James Dale \email{J.Dale@massey.ac.nz}, \url{http://quelea.net}}


\section{Getting Started}{
	For a quick start open the graphical user interface (\bold{gui}) by typing \code{rangeMapper()}. Mouse over the buttons to see further notes (tool-tips) regarding each step. \cr
	A tutorial can be accessed from Help/\sQuote{Get started} under the \bold{gui} or by browsing the \sQuote{doc} package directory.\cr
	See also the example below in the \emph{examples} section on how to use \pkg{rangeMapper} from the command line.
	}
 
 
\section{The rangeMapper pipeline}{
	 \itemize{
		\item Initiate a new project (an empty sqlite database) using a pre-defined template. 
		\item Extract the global bounding box of all selected range maps.
		\item Generate a regular grid (the canvas) onto the previously constructed global bounding box.  
		\item Perform polygon-grid overlay of all selected range maps.
		\item Optionally import non-spatial data to be mapped at each grid cell 
		(data are saved as a 'BIO' table to database).
		\item Compute a chosen statistic (richness, average, median or any user-defined statistic) 
		at each grid cell (data are saved as a 'MAP' table to the database).
		\item  Extract, save and/or plot the MAP.
  }
}  
 

 \references{
Jetz, W; Sekercioglu, Cagan H.; Boehning-Gaese, K(2008) The Worldwide Variation in Avian Clutch Size across Species and Space PLoS Biology, 6-12\cr
Hurlbert, AH; Jetz, W (2007) Species richness, hotspots, and the scale dependence of range maps in ecology and conservation, PNAS, 13384-13389\cr
Graves, GR; Rahbek, C(2005) Source pool geometry and the assembly of continental avifaunas, PNAS 102-22, 7871-7876\cr
Orme, CDL; Davies, RG; Burgess, M; et al.(2005) Global hotspots of species richness are not congruent with endemism or threat, NATURE, 436-7053,1016-1019
}

 
\examples{

\dontrun{
## These cannot be run by examples() but should be OK when pasted
## into an interactive R session 

# INITIATE A NEW PROJECT
	setwd(tempdir())
	dbcon = dbConnect(dbDriver("SQLite"), dbname= "test.sqlite")
	db.ini(dbcon) 

# GET DATA
	# Range data
	f= system.file(package="rangeMapper", "extdata", "wrens", "vector")
	ranges = selectShpFiles(f, ogr = FALSE, polygons.only = TRUE)
	# Life-history data
	data(wrens)

# PREPARE PROJECT
	# upload bounding box to database
	global.bbox.save(ranges, dbcon) 
	# upload grid size
	gridSize.save(dbcon, gridSize = 250000)  
	#  save canvas to database
	canvas.save(dbcon) 

# PROCESS RANGES
	ranges = selectShpFiles(f, ogr = TRUE, polygons.only = TRUE)
	processRanges(ranges, dbcon, metadata = TRUE)

# SAVE BIO TABLE
	bio.save(dbcon, table_name = NULL, wrens, common_id= "species_name") 

# MAKE MAPS
	rangeMap.save(dbcon, FUN = sd, biotab = "wrens", biotrait = "body_mass",
		table.nam= "wrens_sd_body_mass")
	rangeMap.save(dbcon, biotab = "wrens" ,table.nam= "wrens_richness")

	
# FETCH MAPS
	map1 = rangeMap.fetch(dbcon, "wrens_sd_body_mass")
	map2 = rangeMap.fetch(dbcon, "wrens_richness")
	
	summary(map1)
	summary(map2)
	
# PLOT MAPS
	spplot(map1, "body_mass")
	spplot(map2, "species_richness")
}
}


\keyword{spatial}
\keyword{gui}
\keyword{tcltk}






