
setGeneric("rangeMapStart", function(object, ...)  			      	standardGeneric("rangeMapStart") )

setMethod("rangeMapStart",  
		signature = "rangeMapStart", 
		definition = function(object){
			f = paste(object@dir, object@file, sep = .Platform$file.sep)
			file.exists = if(file.exists(f) )  TRUE else FALSE
			CON = dbConnect(dbDriver("SQLite"), dbname= f)
			verSql = paste("INSERT INTO version VALUES (" ,shQuote(packageDescription("rangeMapper")$Version) , ")")
			
			if(!file.exists) { 				
				Queries = object@skeleton
				db = unlist(Queries)
					for (i in 1:length(db)) 
							sqliteQuickSQL(CON , db[i])
			
			sqliteQuickSQL(CON, verSql)
			}
			
			if(object@overwrite && file.exists) {
				dropAll = sqliteQuickSQL(CON, "select 'drop table if exists ' || name from sqlite_master where type = 'table';")
				if(nrow(dropAll) == 0) dropAll = NULL else dropAll = dropAll[,1 ]
				Queries = c(dropAll, "vacuum", object@skeleton )
				db = unlist(Queries)
					for (i in 1:length(db)) sqliteQuickSQL(CON , db[i])
				
				sqliteQuickSQL(CON, verSql)
				}
		
			if(!object@overwrite && file.exists) stop(.X.Msg(paste("File", object@file, "allready exsits!")))
		
		   
			} 
	)

# user level functions 
rangeMap.start <- function(...) {

	obj = new("rangeMapStart", ... )
	
	rangeMapStart(obj)
	.X.Msg( paste("New session", Sys.time() ) , clearup = TRUE)
	
	
	.X.Msg(paste("PROJECT:", obj@file, "\nDIRECTORY:",obj@dir) )
	
	f = paste(obj@dir, obj@file, sep = .Platform$file.sep)
	
	invisible(dbConnect(dbDriver("SQLite"), dbname= f))
	
}
	

rangeMap.open <- function(path, verbose = TRUE) {
	
	.X.Msg(clearup = TRUE)
	
	dbcon = dbConnect(dbDriver("SQLite"), dbname= path)
	
	o = new("rangeMap", CON = dbcon)
	
	if(verbose) summary(o)
	
	invisible(dbcon)
}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	


