
     \name{wrens}
     \docType{data}
     \alias{wrens}
     \title{Life history data  of the wrens family}
     \description{
       This data set gives the clutch size (mean no. of eggs) and mean body mass (grams) of 41 wren \strong{(Troglodytidae)} species.}
     \usage{data(wrens)}
     \format{A \code{data.frame} with 41 rows and 3 columns (species_name, clutch_size and body_mass). }
     \source{
	   \tabular{ll}{
   \tab Life history data compiled from various published papers.\cr
    \tab The breeding range files available in \emph{/extdata/wrens/vector/} package directory are available online at \url{http://www.natureserve.org/getData/dataSets/birdMapData/wrens.zip}. Data provided by NatureServe in collaboration with Robert Ridgely, James Zook, The Nature Conservancy - Migratory Bird Program, Conservation International - CABS, World Wildlife Fund - US, and Environment Canada - WILDSPACE.  \cr
  }
	 
	 }
     \references{
Ridgely, R. S., T. F. Allnutt, T. Brooks, D. K. McNicol, D. W. Mehlman, B. E. Young, and J. R. Zook. 2007. Digital Distribution Maps of the Birds of the Western Hemisphere, version 3.0. NatureServe, Arlington, Virginia, USA.
     }
     \keyword{datasets}
     \keyword{spatial}

	 \section{Breeding ranges post-processing}{
Each species range file was imported to a postgres database and its geometry simplyfied using \emph{\code{SELECT ST_AsText(ST_Transform(ST_SimplifyPreserveTopology(ST_GeomFromText(ST_AsText(the_geom), 4326), 0.1),3975) )  as geom, "SCI_NAME" FROM  "wrens".' range_name  'where "ORIGIN" in (1,2)}} then saved back to shp format.  The resulting \code{\link{proj4string}} spatial projection is \emph{\code{"+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"}}.
 }

 
	 



	



