
gridSize.save <- function(con, gridSize) {
	

	if(!is.na(.sqlQuery(con, "SELECT gridSize from metadata")$gridSize)) 
		stop(gui.msg("The grid size was allready set!"))
	
	if(is.na(.sqlQuery(con, "SELECT xmin from metadata")$xmin)) 
		stop(gui.msg("There is no bouding box!"))
	
	.sqlQuery(con, paste("UPDATE metadata SET gridSize = ", gridSize, "where rowid = 1") )

	if(!is.na(.sqlQuery(con, "SELECT gridSize from metadata")$gridSize))
		gui.msg( paste("Grid size set to", gridSize) )
	
}

gridSize.fetch <- function(con) { 
	Q = dbSendQuery(con, "select gridSize from metadata")
	res = fetch(Q, n = -1)$gridSize
	dbClearResult(Q)
	res
}
