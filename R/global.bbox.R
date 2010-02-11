
global.bbox <- function(shpFiles, check.proj = FALSE) {

	dat = split(shpFiles, 1:length(shpFiles))
	nfo = lapply(dat, getinfo.shape)
		
	bb = do.call(rbind, lapply(nfo, function(x) c(x$minbounds[1:2], x$maxbounds[1:2]) ) )
	bb = c( xmin = min(bb[,1]), xmax = max(bb[,3]), ymin = min(bb[,2]), ymax = max(bb[,4]) )
	
	
	ogrShpFiles = data.frame(dsn = dirname(shpFiles), layer = gsub(".shp", "", basename(shpFiles)), stringsAsFactors = FALSE)
	
	if(check.proj) {
		p4s = .extract.p4s(ogrShpFiles) 
		p4s = p4s[!duplicated(p4s)]
		if(length(p4s) > 1) stop(gui.msg(paste("More than one projection found:\n", paste("  *",p4s, collapse = "\n")) ) )
		}	else 
			p4s = .extract.p4s(ogrShpFiles[1, ])
		
	attributes(bb)$p4s = p4s
	bb
}

global.bbox.save <- function(shpFiles, con) { 

	if(! .is.empty(con, "metadata") ) stop(gui.msg("Bounding box was allready saved for this project."))

		bb = global.bbox(shpFiles)
		
		metadata = c(bb, p4s= as.character(attributes(bb)$p4s), gridSize = NA)
		
		res = dbWriteTable(con, "metadata", data.frame(t(metadata)), append = TRUE, row.names = FALSE)

		if(res) gui.msg("Bounding box uploaded.")
	
}

global.bbox.fetch  <- function(con) {
	md = dbReadTable(con, "metadata")
	bb = cbind(c(md$xmin, md$xmax, md$xmax, md$xmin, md$xmin), c(md$ymin, md$ymin, md$ymax, md$ymax, md$ymin) )
	bb = SpatialPolygons(Srl = list(Polygons(list(Polygon(bb)), "bb")) )
	proj4string(bb) = md$p4s
	return(bb)
}
