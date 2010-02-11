# TODO: choose type rather than type == 5

selectShpFiles <- function(dir, ogr = TRUE, polygons.only = TRUE) {

	dir.list = list.files(dir, recursive = TRUE, full.names = TRUE, pattern = ".shp$")
	
	if(polygons.only){
		nfo = lapply(dir.list, getinfo.shape)
		is.poly =  unlist(lapply(nfo, function(x) x$type == 5))
		dir.list = dir.list[is.poly]
		}
	
	if(ogr) 
		data.frame(dsn = dirname(dir.list), layer = gsub(".shp", "", basename(dir.list)), stringsAsFactors = FALSE) else
		dir.list
}

