
bio.save <- function(con, table_name = NULL, dat, common_id) {

	if(!is.data.frame(dat) ) stop(gui.msg("'dat' is not a data.frame!"))

	if(is.null(table_name)) table_name = deparse(substitute(dat))
	
	tab.nam =  paste("BIO", table_name, sep = "_")
	if(dbExistsTable(con,tab.nam)) stop(gui.msg("Table allready exists!"))  
	
	if( !identical(make.db.names.default(tab.nam), tab.nam) ) {
		tab.nam = make.db.names.default(tab.nam)
		warning(paste("table.nam converted to", tab.nam))
		}
		
	nam = dat[, common_id]

	# nam should exist in ranges
	ranges.nam = .sqlQuery(con, "select distinct bioid from ranges")$bioid

	dat$has_range= is.element(nam, ranges.nam)
	
	res = dbWriteTable(con,tab.nam , dat, row.names = FALSE)

	if(res) {
	.sqlQuery(con,(paste("CREATE  INDEX", paste(table_name, common_id, sep = "_") , "ON",  tab.nam ,  "(", common_id ,")")) )
	
	gui.msg(paste("Table", table_name, "saved.") )
	}

}
























