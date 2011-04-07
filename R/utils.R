

brewer.pal.get <- function(palette = NULL) {
	pal = brewer.pal.info[, ]
	pal = pal[!pal$category == "qual",]
	bp = lapply(split(pal, row.names(pal)), FUN = function(x) brewer.pal(x$maxcolors, row.names(x)))
	if(!is.null(palette) && palette%in%names(bp) ) bp = bp[palette][[1]]
	bp 		   
}


# tclttk	
tkMakeColorPalette <- function(n) {
top  =  tktoplevel()
tkwm.title(top,"Choose one or more colors")

cols = vector(mode = "list", length = 4)	  
out <<- vector(mode = "character")	  

PickColor  =  function(colCanvas) {
	  color  =  tclvalue(tcl("tk_chooseColor"))
	  if (nchar(color) > 0) { tkconfigure(colCanvas, bg = color)
	  out <<- c(out, color)
	  }
}

for(i in 1:length(cols))
 cols[[i]] = tkcanvas(top,width = 50,height = 36)

tkgrid(cols[[1]], tkbutton(top,text="Choose 1st color", height= 2, command = function() PickColor(cols[[1]]) ) )
tkgrid(cols[[2]], tkbutton(top,text="Choose 2nd color", height= 2, command = function() PickColor(cols[[2]]) ) )
tkgrid(cols[[3]], tkbutton(top,text="Choose 3rd color", height= 2, command = function() PickColor(cols[[3]]) ) )
tkgrid(cols[[4]], tkbutton(top,text="Choose 4th color", height= 2, command = function() PickColor(cols[[4]]) ) )

	onOK = function() {
		if(length(out) >= 1) {
		
		out <<- colorRampPalette(out, space = "Lab")(n)
		
		tkdestroy(top)
		}
	}

tkgrid(tkbutton(top, text = "OK", width = 6, command = onOK ),sticky = "e")
 
tkwait.window(top)

return(out)

}
	
tkColorPalette <- function(pal, name, palette.size = 45, envir = .GlobalEnv) { 

	require(tcltk); require(pixmap)

	wd = getwd()
	on.exit(setwd(wd))
	setwd(tempdir())

	top = tktoplevel()
	tkwm.title(top, "Choose a pallete")  

	frm1    = tkframe(top, relief = "ridge", borderwidth = 2)
	frm2    = tkframe(top, relief = "flat", borderwidth = 1)
	frm3    = tkframe(top, relief = "flat", borderwidth = 2)

	x = ceiling(sqrt(length(pal)))
	y = floor(sqrt(length(pal)))

	cols = rep(1:x, x)
	rows = rep(1:y, each = x)

	# Inverse palette ?
	cb = tkcheckbutton(frm2)
	cbValue = tclVar("0")
	tkconfigure(cb,variable=cbValue)
	lab = tklabel(frm2,text="Inversed palette")
	tkgrid(lab, row = 0, column = 1, sticky = "e")
	tkgrid(cb, row = 0, column = 2, sticky = "e")
	
	
	#  add name and pos  pal
	for(i in 1:length(pal) ) pal[[i]] = c(names(pal[i]), cols[i], rows[i], pal[[i]])

	lapply(	pal, function(x) {
		
		onPush = function() {
			out = x[- c(1,2,3)]
			#if inversed palette
			cbVal = as.character(tclvalue(cbValue))
			if (cbVal=="1") out = out[length(out) : 1] 
			attributes(out) = list(palette = x[1] )
			assign(name, out, envir = envir)
			tkdestroy(top)
			}  
	
			pali = x[1]
			rampi = colorRampPalette(x[- c(1,2,3)], space = "Lab")(palette.size)[palette.size:1] 
			pnmi = pixmapIndexed(rep(1:palette.size, palette.size), nrow=palette.size, col= rampi)
			write.pnm(pnmi, file = pali)
			
			img.nam = pali
			img = tkimage.create("photo", file= img.nam )
			assign(  img.nam, tkbutton(frm1, image  =  img,text= img.nam  , command  =  onPush) )
			tkgrid(get(img.nam),  column  = x[2], row = x[3],sticky = "w" )

	})

	
	# choose palette
	
		onOK = function() {
			tkdestroy(top)
			out = tkMakeColorPalette(9)
			attributes(out) = list(palette = "user_defined")
			assign(name, out, envir = envir)
			}  
	
	lab2 = tklabel(frm3,text="_______________________________")
	tkgrid(lab2, row = 0, column = 1, sticky = "w")
	tkgrid(tkbutton(frm3, text = "User defined palette", command = onOK), row = 1, column = 1, sticky = "w")
 
	
	tkpack(frm1); tkpack(frm2); tkpack(frm3)
	
	tkfocus(top)
    tkwait.window(top)
		
	
}

tkdbBrowse <- function(con, prefix = NULL, tables.name.only = FALSE, info) {
	
	require(tcltk)
	tclRequire("BWidget")
	
	
	if(!is.null(prefix) && !.dbtable.exists(con, paste(prefix, "%", sep = "") ) ) 
		stop(.X.Msg(paste("The active project does not contain any", dQuote(prefix), "table" )))
	
	dbpath = dbGetInfo(con)$dbname
	
	tabs = RMQuery(con, "select name from sqlite_master where type = 'table';")$name
	
	fields = lapply(split(tabs, tabs), function(x) RMQuery(con, paste("PRAGMA table_info(", x, ");"))$name)
	

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
	
	v = unlist(strsplit(v, "\\} \\{"))
	v[1] = gsub("\\{", "", v[1] )
	v[length(v)] = gsub("\\}", "", v[length(v)] )
	
	v = strsplit(v, " ")
	if(tables.name.only)
		v = data.frame(dbtable = unlist(v), fieldnam = NA)
	
	if(!tables.name.only) {
	v = do.call("rbind", v)
	v = data.frame(v, stringsAsFactors = FALSE)
	names(v) = c("dbtable", "fieldnam")
	for(i in 1:nrow(v) )
		v[i, "fieldnam"] = fields[v[i, "dbtable"]][[1]][as.numeric(v[i, "fieldnam"])]

	}
	

	    out <<- v 
			
	    tkdestroy(top)
	}
	
	
	 if(!missing(info)) 
		tkgrid(tklabel(top, text = info))

	tkgrid(tkbutton(top, text = "OK", width = 6, command = onOK ))
 

	
	tkwait.window(top)
	
	if(!exists("out")) out = NULL
	
	return(out)

	
}

# DB
RMQuery <- function (con, statement) {
	
	sqliteQuickSQL(con, statement)


}

.extract.indexed <-function(con,table.name) {
	# extract name of indexed colum
	indx = RMQuery(con, 
		paste("select * from sqlite_master where type = 'index' and tbl_name = '", 
				table.name, "'", sep = ""))$name
				
	RMQuery(con, paste("PRAGMA index_info(",indx, ")" ))$name
}

.dbtable.exists <- function(con, table.name) {
	# returns TRUE if the table exists on channel 
	x = RMQuery(con,paste('select name from sqlite_master where type in ("table") and tbl_name like', shQuote(table.name) ) )
	if(nrow(x)>0) TRUE else FALSE
	
	}

.dbfield.exists <-function(con, table.name, col.name) {
	# returns TRUE if the column is part of table
	stopifnot(.dbtable.exists(con, table.name))
	
	ans = length(intersect(RMQuery(con, paste("pragma table_info(", table.name, ")") )$name, col.name)) > 0
	ans
}	
	
.is.empty <- function(con, table.name) {
# returns TRUE if table is  empty FALSE otherwise
# performs a SELECT * from table limit 1;

res = RMQuery(con, paste("SELECT * from", table.name, "limit 1") )
if(nrow(res) == 0) TRUE else 
	FALSE

} 

.sqlAggregate <- function(fun){
 # list of sql aggregate functions
 # If fun is given checks for its existence else return the list of sqlite aggregate functions

funs = list(avg      = "avg", 
stdev         = "stdev",
variance      = "variance",
mode          = "mode",
median        = "median",
lower_quartile= "lower_quartile",
upper_quartile= "upper_quartile",
sum           = "total",
max           = "max",
min           = "min",
count         = "total")
 
class(funs) = "simple.list"



if(missing(fun) )
 return(funs) else if
	(fun%in%funs) return(TRUE) else
			stop(.X.Msg(sQuote(fun), "is not a known sqlite aggregate function!" ))
	}
























