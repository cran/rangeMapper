
setGeneric("rangeMapFetch", function(object, ...) 					standardGeneric("rangeMapFetch") )

setMethod("rangeMapFetch",  
	signature  = "rangeMapFetch", 
		definition = function(object) {
		
    	#build tableName(s)
		mapNam = paste(object@MAP, object@tableName, sep = "")

		# map variable
		mapvar = sapply(mapNam, function(x)
					setdiff(RMQuery(object@CON, paste("pragma table_info(", x, ")"))$name, object@ID ) )		
		# sql string
		dotid = paste('x', 1:length(mapNam), sep = "")
		mapdat = paste(paste(paste(dotid,mapvar, sep = "."), object@tableName, sep = " as "), collapse = ",")
		
		sql = paste("SELECT c.x, c.y,", mapdat, 
		"from canvas as c LEFT JOIN",paste(paste(mapNam, dotid, "on c.id = ", dotid, ".id"), collapse = " LEFT JOIN "))

		
		map = RMQuery(object@CON, sql)
	
		coordinates(map) = ~ x + y

		p4s = dbReadTable(object@CON, object@PROJ4STRING)[1,1]
		
		proj4string(map) = CRS(p4s)
		
		gridded(map) = TRUE
		
		map = new("SpatialPixelsRangeMap", map, mapvar    = mapvar)
		return(map)
		}
	)	
	
# user level functions 
rangeMap.fetch <- function(dbcon, maps) { 
	
	if(missing(maps)) maps = RMQuery(dbcon, 'select name from sqlite_master where type = "table" and tbl_name like "MAP_%"')$name

	maps = gsub("MAP_", "", maps)
	
	x = new("rangeMapFetch", CON = dbcon, tableName = maps)
	rangeMapFetch(x)	

} 

