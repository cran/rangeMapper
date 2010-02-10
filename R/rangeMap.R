

rangeMap.save  <- function(dbcon, FUN = NULL , biotab, biotrait, table.nam) { 

	if( is.null(FUN) ) FUN  = length  #  "richness"
	if( !is.function(FUN) ) stop ("FUN should be a function!")
	if(!length(FUN(c(1,2))) ==1) stop(paste(fun.nam, "should return one value!"))
	
	if (!dbExistsTable(dbcon, paste("BIO", biotab, sep = "_") ) )
	 gui.msg(paste(biotab, "does not exist!") )
	
	
	if(!identical(make.db.names.default(table.nam), table.nam)) {
		table.nam = make.db.names.default(table.nam)
		warning(paste("table.nam converted to", table.nam))
		}
	
	table.nam = paste("MAP",  table.nam, sep = "_")
	
	by.id = .sqlQuery(dbcon, paste("select name from sqlite_master where type = 'index' and tbl_name = 'BIO_", biotab, "'", sep = ""))$name
	by.id = gsub(paste(biotab, "_", sep = ""), "", by.id)
	
	
	if(identical(FUN, length) ) {

	.sqlQuery(dbcon, paste("CREATE TABLE" ,table.nam, "AS SELECT id, count(id) as species_richness from ranges group by id"))
	}
		
	if(identical(FUN, mean)) {
	.sqlQuery(dbcon, 
	paste("CREATE TABLE" ,table.nam, "AS SELECT id, avg(", biotrait, ")", paste("mean", biotrait, sep = "_"), "from (", 
		paste("SELECT r.id, b.", biotrait , " from ranges r left outer join ", 
					paste("BIO", biotab, sep = "_"), " b where r.bioid = b.", by.id), 
				") group by id")
      )	
	
	}

	if(!identical(FUN, length)  & !identical(FUN, mean) ) {
	
	f = function(x) FUN( na.omit(x) )
 
	d=  .sqlQuery(dbcon,  paste("SELECT r.id, b.", biotrait , " from ranges r left outer join ", 
					paste("BIO", biotab, sep = "_"), " b where r.bioid = b.", by.id) )

	d =split(d, d$id)

	d = lapply(d, FUN = function(x) id = data.frame(id =x$id[1], stat = f(x[, biotrait]) ) )
	d= do.call("rbind", d)
	names(d) = c("id", biotrait)

	dbWriteTable(dbcon, table.nam, d, row.names = FALSE)				

	
	}


	if (dbExistsTable(dbcon, table.nam) )
		 gui.msg(paste(table.nam , "saved to database.") )
	
	
}


rangeMap.fetch <- function(dbcon, map) { 

	d = dbGetQuery(dbcon, paste("SELECT c.x, c.y, r.* from", paste("MAP", map, sep = "_") ,"r join canvas as c on c.id = r.id") )

	suppressWarnings(( coordinates(d) = ~ x + y ))
	suppressWarnings((gridded(d) = TRUE))

	d
} 


