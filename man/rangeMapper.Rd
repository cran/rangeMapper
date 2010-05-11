
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
		\item Optionally select a subset based on an existing 'MAP', 'BIO' or 'metadata' table.) 
		\item Compute a chosen statistic (richness, average, median or any user-defined statistic). 
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

	require(rangeMapper)
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
	gridSize.save(dbcon, gridSize = 200000)  
	#  save canvas to database
	canvas.save(dbcon) 

# PROCESS RANGES
	ranges = selectShpFiles(f, ogr = TRUE, polygons.only = TRUE)
	processRanges(ranges, dbcon, metadata = TRUE)

# SAVE BIO TABLE
	bio.save(dbcon, dat = wrens, common_id = "species_name") 

# MAKE MAPS
	#species richness.
	rangeMap.save(dbcon, table.nam = "richness",  biotrait = richness~1 , show.sql = FALSE)

	# Body mass map; no subset
	# mean
	rangeMap.save(dbcon, FUN = mean, biotab = "wrens", biotrait = body_mass~1, table.nam = "mean_body_mass")
	# cv
	rangeMap.save(dbcon, FUN = function(x) sd(x)/mean(x), biotab = "wrens", biotrait = body_mass~1, table.nam = "cv_body_mass")
	
	# Clutch size map; subsseting by BIO table field
	rangeMap.save(dbcon, FUN = mean, biotab = "wrens", biotrait = clutch_size ~1, table.nam = "mean_clutch_size_bm15", 
				subset =  list(BIO_wrens = "body_mass > 15") )
	
	# Body mass map; subsseting by BIO table field AND MAP table
	rangeMap.save(dbcon, FUN = mean, biotab = "wrens", biotrait = body_mass ~1, table.nam = "mean_body_mass_subset1", 
						subset = list(BIO_wrens   = "body_mass > 10", 
						             MAP_richness =  "species_richness BETWEEN 2 AND 5") )

	# Body mass map; subsseting by a BIO table, a MAP table AND  the metadata_ranges table
	rangeMap.save(dbcon, FUN = mean, biotab = "wrens", biotrait = body_mass ~ 1, table.nam = "mean_body_mass_subset2", 
						subset = list(BIO_wrens = "body_mass > 10", 
						              MAP_richness = "species_richness BETWEEN 2 AND 5",
						              metadata_ranges = "Area > 55000000000") )
	
# FETCH MAPS
	richness          = rangeMap.fetch(dbcon, "richness")
	bodyMass          = rangeMap.fetch(dbcon, "mean_body_mass")
	bodyMassSubset1   = rangeMap.fetch(dbcon, "mean_body_mass_subset1")
	bodyMassSubset2   = rangeMap.fetch(dbcon, "mean_body_mass_subset2")
	clutchSize        = rangeMap.fetch(dbcon, "mean_clutch_size_bm15")
	
	summary(richness)
	summary(clutchSize)
	
# PLOT MAPS
	# choose a brewer pal palette
	tkColorPalette(pal = brewer.pal.get(), name  = "SpectralPalette")

	# plot species richness
	rangeMap.plot(richness, SpectralPalette, ncols = 6, style = "equal")
	
	# plot body mass maps
	rangeMap.plot(bodyMass, SpectralPalette, ncols = 20, style = "fisher")
	rangeMap.plot(bodyMassSubset1, SpectralPalette, ncols = 20, style = "fisher")
	rangeMap.plot(bodyMassSubset2, SpectralPalette, ncols = 20, style = "fisher")
	
# plot clutch size subset for body_mass
	rangeMap.plot(clutchSize, SpectralPalette, ncols = 20, style = "fisher")
		
	

}
}


\keyword{spatial}
\keyword{gui}
\keyword{tcltk}






