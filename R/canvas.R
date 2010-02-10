
canvas.make   <- function(con) {
	
	if(is.na(.sqlQuery(con, "SELECT gridSize from metadata")$gridSize)) 
		stop(gui.msg("The grid size is missing!"))
	
		bbox     = global.bbox.fetch(con)
		cellsize = gridSize.fetch(con)
		
		return(spsample(bbox, cellsize = cellsize, type = "regular", offset = c(0.5,0.5)))
}

canvas.save   <- function(con) {

	if(!.is.empty(con, "canvas")) stop (gui.msg("Canvas was allready uploaded!") )

	cnv = canvas.make(con)
	cnv = data.frame(coordinates(cnv), id = 1:nrow(coordinates(cnv)))

	names(cnv) = c("x", "y", "id")

	res = dbWriteTable(con, "canvas", cnv, append = TRUE, row.names = FALSE) 

	if(res) gui.msg("Canvas uploaded.")


}

canvas.fetch  <- function(con) {

p4s  = .sqlQuery(con, "SELECT p4s from metadata")$p4s

cnv = dbReadTable(con, "canvas")
coordinates(cnv) = ~x+y
proj4string(cnv) = p4s
return(cnv)

}



