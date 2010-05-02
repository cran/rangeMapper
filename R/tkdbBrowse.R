
tkdbBrowse <- function(con, prefix = NULL, tables.name.only = FALSE, info) {
	
	require(tcltk)
	tclRequire("BWidget")
	
	
	if(!is.null(prefix) && !.dbtable.exists(con, paste(prefix, "%", sep = "") ) ) 
		stop(gui.msg(paste("The active project does not contain any", dQuote(prefix), "table" )))
	
	dbpath = dbGetInfo(con)$dbname
	
	tabs = .sqlQuery(con, "select name from sqlite_master where type = 'table';")$name
	
	fields = lapply(split(tabs, tabs), function(x) .sqlQuery(con, paste("PRAGMA table_info(", x, ");"))$name)
	
	if(!is.null(prefix)) { 
		fields = fields[grep(prefix, names(fields))]
		names(fields) = gsub(paste(prefix, "_", sep = ""), "", names(fields))
		}

	top <- tktoplevel()
	tkwm.title(top,dbGetInfo(con)$dbname)
	tkfocus(top)
	
	xScr       <- tkscrollbar(top,command=function(...)tkxview(dbTree,...),orient="horizontal")
	yScr       <- tkscrollbar(top,command=function(...)tkyview(dbTree,...))
	dbTree <- tkwidget(top,"Tree",xscrollcommand=function(...)tkset(xScr,...),
									  yscrollcommand=function(...)tkset(yScr,...),
										width=nchar(dbpath), height= length(unlist(fields )) )
	tkgrid(dbTree,yScr) 
	tkgrid(xScr)
	tkgrid.configure(yScr,stick="nsw")
	tkgrid.configure(xScr,stick="new")

	
	for(i in 1:length(fields)){
		
		tkinsert(dbTree,"end","root", names(fields)[i] ,text=names(fields)[i])
			
			if(!tables.name.only == TRUE) {
				for(j in 1:length(fields[[i]]) ) {
				tkinsert(dbTree,"end",names(fields)[i], paste(names(fields)[i], j), text= fields[[i]][j])
				}
			}
	
	}

	
	onOK = function() {
	v = tclvalue(tcl(dbTree,"selection", "get") )
	v = unlist(strsplit(v, " "))
		tabnam  = gsub("\\{", "", v[1] )
		
		fieldnam = as.numeric(gsub("\\}", "", v[2] ))
        fieldnam = fields[[tabnam]][fieldnam]
	    
		out <<- cbind(dbtable = tabnam,field = fieldnam) 
		
	   
	   tkdestroy(top)
	}
	
	
	 if(!missing(info)) 
		tkgrid(tklabel(top, text = info))

	tkgrid(tkbutton(top, text = "OK", width = 6, command = onOK ))
 

	
	tkwait.window(top)
	
	if(!exists("out")) out = NULL else
		if(tables.name.only) out = out[1] 
	
	out

	
}












