
rm.rangeMapper <- function(con, table.nam, table.type) {
	if(missing(table.nam)) 
	table.nam = .sqlQuery(con, 
		paste("select name from sqlite_master where type = 'table' and name LIKE '",table.type,"_%'", sep = ""))$name else
	table.nam = paste(table.type, table.nam, sep = "_")
	
	if(length(table.nam) >0) {
	sql = paste("DROP TABLE if exists", table.nam)
	for (i in 1:length(sql)) .sqlQuery(con , sql[i])
	} else
	paste("The project does not have any",table.type, "tables")
}




	



