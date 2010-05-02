


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

.sp.metadata <- function(spdf) {

	Area = sum(sapply(slot(spdf, "polygons"), function(x) slot(x, "area") ))

	metad = apply(coordinates(spdf), 2, FUN = function(x) data.frame(Median = median(x), Min = min(x), Max = max(x) ) )

	names(metad[[1]]) = paste(names(metad[[1]]), "x", sep = "_")
	names(metad[[2]]) = paste(names(metad[[2]]), "y", sep = "_")

	cbind(Area, metad[[1]], metad[[2]])
	}

.extract.indexed <-function(dbcon,table.name, name.sep = "_") {

	indx = .sqlQuery(dbcon, 
		paste("select * from sqlite_master where type = 'index' and tbl_name = '", 
				table.name, "'", sep = ""))$name
				
	.sqlQuery(dbcon, paste("PRAGMA index_info(",indx, ")" ))$name
}

.dbtable.exists <- function(con, name) {
	x = .sqlQuery(con,paste('select name from sqlite_master where type = "table" and tbl_name like', shQuote(name) ) )
	if(nrow(x)>0) TRUE else FALSE
	
	}

richness <- function(x) {
	"richness"
}










 