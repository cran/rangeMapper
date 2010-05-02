

rangeMap.save  <- function(dbcon, FUN = richness, biotab = NA, biotrait, table.nam, subset = NULL, show.sql = FALSE){
	
	#  load sugests packages
	if(substitute(FUN) == "lmer") require(lme4)
	
	# check FUN
	if(!identical(FUN, richness) && is.na(biotab)) stop(gui.msg("FUN is given but biotab does not exist!"))
	if( !is.function(FUN) ) stop(gui.msg("FUN should be a function!"))
	
	# check biotrait
	if (class(biotrait) != "formula" || length(biotrait) != 3)
	stop("biotrait formula must be a two-sided formula object")
	
	# check biotab
	 if (!is.na(biotab)) {
		biotab = paste("BIO", biotab, sep = "_")
		if(!dbExistsTable(dbcon, biotab ))
			stop(gui.msg(paste(biotab, "does not exist!") ) )

		biotrait.fun = biotrait			

		biotrait = as.character(biotrait[[2L]])

		if(!biotrait%in%dbListFields(dbcon, biotab)) 
			stop(gui.msg(paste(biotrait, "does not exist in ", biotab) ) )
	 }
	 
	# check new table name 
	if(!identical(make.db.names.default(table.nam), table.nam)) {
	table.nam = make.db.names.default(table.nam)
	warning(gui.msg(paste("table.nam converted to", table.nam)))
	}
	table.nam = paste("MAP_", table.nam, sep = "")
	
	if(!is.null(subset)) {
	
	# subset sql strings
	m = subset[grep("^MAP_", names(subset))]
	b = subset[grep("^BIO_", names(subset))]
	r = subset[which(names(subset)=="metadata_ranges")]

	msql = if(length(m) > 0) paste(paste("r.id in (SELECT id FROM", names(m), "WHERE", m, ")"), collapse = " AND ") else NULL
	bsql = if(length(b) > 0) paste(paste("r.bioid in (SELECT", 
			sapply(names(b), function(x) .extract.indexed(dbcon, x)) ,
				"FROM", names(b), "WHERE", b, ")"), collapse = " AND ") else NULL
	rsql = if(length(r) > 0) paste(paste("r.bioid in (SELECT bioid FROM", names(r), "WHERE", r, ")"), collapse = " AND ") else NULL

	subset.sql = paste( c(msql, bsql, rsql), collapse = " AND ")
	
	} else 
		subset.sql  = NULL 
	
	# sql string
	sql = if(identical(FUN, richness) ) # db exec
			paste("SELECT id, count(r.id) as species_richness from ranges as r", 
				if(!is.null(subset.sql)) paste("WHERE", subset.sql), "group by r.id") else
			paste("SELECT r.id, b.* from ranges r left join ", 
				biotab, " b where r.bioid = b.", .extract.indexed(dbcon, biotab), 
				if(!is.null(subset.sql)) paste("AND", subset.sql) )
	
	sql = if(identical(FUN, mean) ) #if db exec else R exec later
			paste("SELECT id, avg(", biotrait, ")", biotrait, "from (",sql,") group by id") else sql
	
	
	if (show.sql) cat(paste("\n", strwrap(sql, width = 100)), "\n") else {		
			
	# exec sql
	if(!identical(FUN, richness)  & !identical(FUN, mean) ) {	
		
		d = .sqlQuery(dbcon,  sql)
		if(nrow(d) == 0) 
			stop(gui.msg(paste("The result is an empty MAP; nothing was saved!", sql)))	 
		
		d =split(d, d$id)
		
		d = lapply(d, FUN = function(x) id = data.frame(id =x$id[1], stat = assemblage.stat(formula = biotrait.fun, FUN = FUN, data = x) ) )
		
		d= do.call("rbind", d)
		names(d) = c("id", biotrait)
		
		dbWriteTable(dbcon, table.nam, d, row.names = FALSE)				
		} else 
			.sqlQuery(dbcon,  paste("CREATE TABLE" ,table.nam, "AS", sql) )
	
	
	# return		
	if (dbExistsTable(dbcon, table.nam) ) {
		.sqlQuery(dbcon,paste("CREATE  INDEX", paste(table.nam, "id", sep = "_") , "ON",  table.nam ,  "(id)") )
		gui.msg(paste(table.nam , "saved to database.") )
	}
		 else {
			gui.msg(paste("Error in saving",table.nam))
			gui.msg(paste("Faulty sql query:\n",strwrap(sql, width = 100)))
			
			
		}	
	}

}				


rangeMap.fetch <- function(dbcon, map) { 


	d = .sqlQuery(dbcon, paste("SELECT c.x, c.y, r.* from canvas as c LEFT JOIN", paste("MAP", map, sep = "_"),"r on c.id = r.id") )
	
	x = names(d)[!names(d)%in%c("x","y","id")]
	
	d[, x] = as.numeric(d[, x])
	
	coordinates(d) = ~ x + y
	
	p4s = .sqlQuery(dbcon,"SELECT p4s FROM metadata")$p4s
	
	proj4string(d) = CRS(p4s)
	suppressWarnings((gridded(d) = TRUE))

	d
} 


rangeMap.plot  <- function(map, colorpalette, ncols, style, scales = FALSE) {
	
	mapVar = names(map)[!names(map)=="id"]
	Int = classIntervals(map@data[, mapVar], ncols, style = style)
	
	trellis.par.set("regions", list(col= colorRampPalette(colorpalette, space = "Lab")(ncols) ) )
	
	print(spplot(map , mapVar ,scales = list(draw = scales), cuts = ncols, checkEmptyRC = FALSE, at = Int$brks))

}


