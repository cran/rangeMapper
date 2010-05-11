

db.scheleton <- function() {

	list(
		create = 
		c("CREATE TABLE metadata (xmin FLOAT,xmax FLOAT,ymin FLOAT,ymax FLOAT,p4s CHAR,gridSize FLOAT)", 
			"CREATE TABLE canvas (x FLOAT,y FLOAT,id INT)", 
			"CREATE TABLE ranges (id INT,bioid CHAR)",
			"CREATE TABLE metadata_ranges (bioid CHAR, Area FLOAT, Median_x FLOAT,Min_x FLOAT, Max_x FLOAT,Median_y FLOAT, Min_y FLOAT, Max_y FLOAT)")
		,
		index = 
		c(  "CREATE INDEX IF NOT EXISTS   id_canvas ON canvas (id)", 
			"CREATE INDEX IF NOT EXISTS   id_ranges ON ranges (id)", 
			"CREATE INDEX IF NOT EXISTS   bioid_ranges ON ranges (bioid)",
			"CREATE INDEX IF NOT EXISTS   bioid_metadata_ranges ON metadata_ranges (bioid)")
	)
}


db.ini <- function(con) {
	dropAll = .sqlQuery(con, "select 'drop table if exists ' || name from sqlite_master where type = 'table';")
	if(nrow(dropAll) == 0) dropAll = NULL else dropAll = dropAll[,1 ]
	Queries = c(dropAll, "vacuum", db.scheleton() )
	db = unlist(Queries)
	for (i in 1:length(db)) .sqlQuery(con , db[i])
}

 
































