


.sqlQuery <- function (con, statement) {

	dat = sqliteQuickSQL(con, statement)
	dat

}

.is.empty <- function(con, dbtable) {
# returns TRUE if table is  empty FALSE otherwise
# performs a SELECT * from table limit 1;

res = .sqlQuery(con, paste("SELECT * from", dbtable, "limit 1") )
if(nrow(res) == 0) TRUE else 
	FALSE

} 

.extract.p4s <- function(ShpFiles) {
#  extract proj4 string
# EXAMPLE
#  Dir  = choose.dir(paste(system.file(package="rangeMapper"), "extdata", "wrens", "vector", sep = .Platform$file.sep))
#  ShpFiles = selectShpFiles(Dir)

fl = split(ShpFiles, ShpFiles$layer)

unlist(lapply(fl, FUN = function(x) .Call("ogrP4S", x[,1], x[,2], PACKAGE = "rgdal") ))

}









