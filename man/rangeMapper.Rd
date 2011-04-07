
\name{rangeMapper}
\alias{rangeMapper}
\docType{package}

\title{rangeMapper: A platform for the study of macroecology of life history traits.}
\description{ 
	\pkg{rangeMapper} is a front end platform for the study of macroecology of life history traits at both inter-specific and assemblage levels. 
	}
\details{
	The package uses species range (extent-of-occurrence) vector polygons and life history traits datasets to build up maps (e.g. species richness, mean body mass, ...). \cr
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
		\item Extract/define the global bounding box of all selected range maps.
		\item Generate a regular grid (the canvas) using the previously constructed global bounding box.  
		\item Perform polygon-grid overlay of all selected range maps.
		\item Optionally compute pre-defined or user-defined range structure indexes.
		\item Optionally import non-spatial data to be mapped at each grid cell (data are saved as a 'BIO' table to database).		
		\item Optionally import georeferenced raster files.
		\item Compute a chosen statistical model at each grid cell optionally using complex subsets.
		\item  Save, plot, export the MAP-s.
  }
}  

 

\references{
Valcu, M., Dale, J. and Kempenaers, B. (in prep) rangeMapper: A platform for the study of macroecology of life history traits.
}

 
\examples{
#########################################

require(rangeMapper)

# Start or open a project
wd = tempdir()

# open the GUI, messages will be re-directed here
if(interactive() ) rangeMapper()

dbcon = rangeMap.start(file = "wrens.sqlite", 
	overwrite = TRUE, dir = tempdir() )

# To connect latter to the project created with rangeMap.start use
dbcon = rangeMap.open(path = 
	paste(tempdir(), "wrens.sqlite" , sep = .Platform$file.sep) )

# Project summary
summary(new("rangeMap", CON = dbcon) )

# Breeding range vector files location
f = system.file(package = "rangeMapper", 
	"extdata", "wrens", "vector")

# There are several alternatives to save the global bounding box, 
# a few are  exemplified below
# The default creates a global unprojected bounding box
global.bbox.save(con = dbcon)

# Creating a global bbox using the  breeding range vector files

# manually remove all data from the bbox  and proj4string table
RMQuery(dbcon, 'DELETE from bbox') 
RMQuery(dbcon, 'DELETE from proj4string')

global.bbox.save(con = dbcon, bbox = f) 

plot( global.bbox.fetch(dbcon) , axes = TRUE)
	
# Creating a global bbox using the  breeding range vector files 
# and re-projecting
# manually remove all data from the bbox table
RMQuery(dbcon, 'DELETE from bbox') 
RMQuery(dbcon, 'DELETE from proj4string')

global.bbox.save(con = dbcon,bbox = f, 
	p4s =  
	CRS(" +proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))

	
# upload grid size using the default
gridSize.save(dbcon)  

# upload grid size to 100000 sqm
RMQuery(dbcon, 'DELETE from gridSize')
gridSize.save(dbcon, gridSize = 100000) 

#  save canvas 
canvas.save(dbcon) 
summary(canvas.fetch(dbcon) )

# Process ranges & BIO tables

# first define a new range structure function

APratio = function(x) {
# returns 1 for a circle polygon
	# area
	A = sapply(slot(x, "polygons"), 
		function(x) slot(x, "area") )
	
	# perimeter
	a = lapply(slot(x, "polygons"),
		function(x) slot(x, "Polygons") )
	b = do.call(c, a)
	a1 = lapply(b, function(x) 
		Line(coordinates(x)))
	L = sapply(a1, LineLength)
	
	# A-P ratio
	2*(sqrt(pi*sum(A)))/sum(L)
	
}


# the APratio function will also be computed together with the pre-defined functions
processRanges(dir = f, con =  dbcon, metadata = TRUE, APratio)


# Manage BIO tables
csvfile = system.file(package="rangeMapper", "data", "wrens.csv")
bio.save(con = dbcon, loc = csvfile ,  ID = "sci_name")

rm.rangeMapper(con = dbcon, tablePrefix = "BIO")

data(wrens)
bio.save(con = dbcon, loc = wrens,  ID = "sci_name")

# metadata_ranges table can be merged with the wrens table
metadata2bio(dbcon)
bio.merge(dbcon, tableNam = "wrensRanges", 
		"wrens", "metadata_ranges")


# Save MAPs
rangeMap.save(dbcon) #species richness is the default

#Using sqlite aggregate functions
.sqlAggregate()
rangeMap.save(dbcon, FUN = "median" , biotab = "wrens", 
	biotrait = "body_size", tableName = "body_size")

rangeMap.save(dbcon, FUN = "median" , 
	biotab = "metadata_ranges", biotrait = "V1", tableName = "APratio")

#Using an  R function which does not require a formula argument
rangeMap.save(dbcon, FUN = mean , biotab = "wrens", 
	biotrait = "clutch_size", tableName = "clutch_size", 
		na.rm = TRUE, overwrite = TRUE)

#Using an user defined R function which has a formula argument
lmSlope = function (formula, data) {
    fm = try(lm(formula, data = data), silent = TRUE)
	if (inherits(fm, "try-error")) 
		res = NA else 
			res = coef(fm)[2]
    as.numeric(res)
}

# Map of log(body_mass) ~ clutch_size only for species richness > 10
rangeMap.save(dbcon, FUN = lmSlope, biotab = "wrens", 
	biotrait = "body_mass", 
	tableName = "slope_body_mass_clutchSize", 
	formula = log(body_mass) ~ clutch_size , overwrite = TRUE, 
	list(MAP_species_richness = "species_richness > 5")
	)

	
# rangeMap.save can be also used to  import raster maps the current project
r = system.file(package = "rangeMapper", 
	"extdata", "etopo1", "etopo1_Americas.tif")

rangeMap.save(dbcon, path = r, tableName = 'meanAltitude', 
	FUN = mean , overwrite = TRUE)

# rangeMap.save takes a subset argument on both 
# metadata_ranges, MAP and BIO tables

# clutch size of the highland species
rangeMap.save(dbcon, biotab = "wrens", biotrait = "clutch_size", 
	tableName = "highlandClutch", overwrite = TRUE, 
	subset = list(MAP_meanAltitude = "meanAltitude > 2000") )

# clutch size of restricted range lowland species 
rangeMap.save(dbcon, biotab = "wrens", biotrait = "clutch_size", 
	tableName = "endemicsLowlandClutch", overwrite = TRUE, 
		subset = list(MAP_meanAltitude = "meanAltitude < 1000",
					 metadata_ranges = "Area < 100000000000") )


# Fetch and plot Maps
	
plot(rangeMap.fetch(dbcon, 'species_richness') ,n = 10, style = "fisher")		

plot(rangeMap.fetch(dbcon, "APratio"), style = "equal" )
plot(rangeMap.fetch(dbcon, 'meanAltitude'),  style = "quantile")

clutch = rangeMap.fetch(dbcon, 
	c('clutch_size', 'slope_body_mass_clutchSize', 'body_size') )
plot(clutch, style = "fisher")

plot(rangeMap.fetch(dbcon, 'highlandClutch'), n = 4)	


image(rangeMap.fetch(dbcon, 'meanAltitude') )	
image(rangeMap.fetch(dbcon, 'endemicsLowlandClutch') , 
	add = TRUE, col = "blue")	

setwd(wd)	
	
}


\keyword{macroecology}
\keyword{spatial}
\keyword{gui}
\keyword{tcltk}






