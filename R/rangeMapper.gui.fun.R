
gui.make.env <- function(env = ".RangeMapper") {

assign(env , new.env(), env = .GlobalEnv)
}

gui.get.from.env <- function(x, mode = "any", envir = .RangeMapper) {

obj = try(get(x, envir = envir, mode = mode, inherits = FALSE), silent = TRUE)

if (class(obj) == "try-error") return (NULL) else return(obj)


}
 
gui.put.to.env <- function(x, value, envir = .RangeMapper) { 

obj = try(assign(x, value, envir = envir), silent = TRUE) 
if (class(obj) == "try-error") return (NULL) else return(obj)

}

gui.exists.in.env <- function(x, envir = .RangeMapper) {

obj = try(exists(x, envir = envir), silent = TRUE)
if (class(obj) == "try-error") return (FALSE) else return(obj)


}


gui.msg <- function(msg, tkMainWindow = "win", tkElement = "msg", eol = "\n", keep = FALSE, clearup = FALSE, getTime = FALSE) {

 if(getTime) msg = paste(msg, "<", Sys.time(), ">", sep = "")

 if( gui.exists.in.env(tkMainWindow) & gui.exists.in.env(tkElement) ) {

    if(clearup & gui.exists.in.env("session.msg")) {
		tkdelete(gui.get.from.env(tkElement), "0.0" , "1000.0" )
		rm("session.msg", envir = .RangeMapper)
		}
	if(!clearup) {
		if(!gui.exists.in.env("session.msg") ) gui.put.to.env("session.msg", list() ) 
		if(keep) gui.put.to.env("session.msg", c( gui.get.from.env("session.msg"), msg )  )
		
		tkdelete(gui.get.from.env(tkElement), "1.0" , "100.0" )
		
		lapply(gui.get.from.env("session.msg") , function(x) 
				  tkinsert(gui.get.from.env(tkElement), "end" , paste(x,eol) ) ) 
		if(!keep) tkinsert(gui.get.from.env(tkElement), "end" , paste(msg,eol) ) 
			
		 tkyview.moveto(gui.get.from.env(tkElement), 1)
		 tkfocus(gui.get.from.env(tkMainWindow))
		 tcl("update", "idletasks") 		 
		 }
	 
	 
	 } else {
		cat(msg, eol)
	    flush.console() 
		}
} 

gui.img <- function(gif, file=system.file("ico", paste(gif, "gif", sep = "."), package="rangeMapper")) {
tkimage.create("photo",file=file)
 }

gui.tkEntryBox <- function(txt = "enter value", default.entry = "", default.output =  "unknown", make.sql.nam = TRUE) {
	top<-tktoplevel()
	tktitle(top) <- "" 
	Name <- tclVar(default.entry)
	entry.Name <-tkentry(top, width=  "50" ,textvariable=Name)
	tkgrid(tklabel(top,text= txt))
	tkgrid(entry.Name)
	
	getNam = default.output
	
	OnOK <- function() {
		NameVal <- tclvalue(Name)
		tkdestroy(top)
		getNam <<- NameVal
	}
	OK <-tkbutton(top,text="OK", width = 6,command=OnOK)
	tkbind(entry.Name, "<Return>",OnOK)
	tkgrid(OK)
	tkfocus(top)
	
	tkwait.window(top)
	
	if(make.sql.nam) getNam = make.db.names.default(getNam) 
	
	getNam
}	

gui.tkdbBrowse.active.proj <- function() {

	dbcon = gui.get.from.env("con")
		if(is.null(dbcon)) stop(gui.msg("There is no active project!"))
	tkdbBrowse(dbcon)
	rm(out, envir = .GlobalEnv)
}

gui.help <- function(what, exec = FALSE) {

	out = switch(what,
	support.files = paste(
	system.file(package="rangeMapper", "extdata", "wrens", "vector"), "\n", 
	system.file(package="rangeMapper", "data", "wrens.csv")),
	man = system.file(package="rangeMapper", "doc", "rangeMapper.pdf"),
	citation = attributes(citation("rangeMapper"))$textVersion)

	if(exec) shell.exec(out)
	gui.msg(out)

	}


gui.dbopen <- function(new = TRUE) {

	gui.msg(clearup = TRUE)

	if(new) db = tclvalue(tkgetSaveFile(defaultextension = ".sqlite", filetypes = "{rangeMapper_project {.sqlite}}" ) ) else 
			db = tclvalue(tkgetOpenFile(defaultextension = ".sqlite", filetypes = "{rangeMapper_project {.sqlite}}" ))

	if (nchar(db)==0)  gui.msg("Nothing selected!") else {
		gui.put.to.env("con", dbConnect(dbDriver("SQLite"), dbname= db) )
				
		if(new) db.ini(gui.get.from.env("con"))
		
		gui.msg(paste("<ACTIVE PROJECT>", db), keep = TRUE, getTime = TRUE)
	}
				


	
			
}

gui.selectShpFiles <- function(ogr, polygons.only) {

	but = function() {
	top <- tktoplevel()

	tktitle(top) <- "Choose:" 

	select <- tclVar(0)

	f.but <- tkbutton(top, text = "  FILES  ", command = function() tclvalue(select) <- 1)
	d.but <- tkbutton(top, text = "DIRECTORY", command = function() tclvalue(select) <- 2)

	tkgrid(f.but, d.but, column  = 1, row= 0, sticky = "w", padx = 16)
	tkgrid(d.but, column  = 2, row= 0, sticky = "e", padx = 16)

	tkwait.variable(select)

	tkdestroy(top)
	return(as.integer(tclvalue(select)))
	}

	selectVal = but()

	if(selectVal == 2) {
		ff = selectShpFiles(tk_choose.dir(default = getwd(), caption = "Select ranges directory"), ogr = ogr, polygons.only = polygons.only)
		
		} 

	if(selectVal == 1) {


		if(ogr) { 
			ff = selectShpFiles(tk_choose.dir(default = getwd(), 
				caption = "Select the upper level directory \n and then choose several files."), ogr = ogr, polygons.only = polygons.only)
			sel = tk_select.list(ff$layer,  multiple = TRUE, title = "Select files")
			ff = ff[ff$layer%in%sel,] }
		
		 if(!ogr) {
		 	ff = selectShpFiles(tk_choose.dir(default = getwd(), 
				caption = "Select the upper level directory \n and then choose several files."), ogr = ogr, polygons.only = polygons.only)
			sel = tk_select.list(ff , multiple = TRUE, title = "Select files")
			ff = ff[ff%in%sel] }
		}	

	gui.put.to.env("ranges",ff)	

	gui.msg(paste( if(ogr)nrow(ff) else length(ff), "files selected."))
		
	
}	

gui.global.bbox.save <- function() {

	dbcon = gui.get.from.env("con")
	if(is.null(dbcon)) stop(gui.msg("There is no active project!"))
	
	gui.msg("Computing global bounding box....")
	
	ff = gui.selectShpFiles(ogr = FALSE,  polygons.only = TRUE)
	global.bbox.save(gui.get.from.env("ranges"), dbcon)
	}
	
gui.gridSize.save <- function() {

	dbcon = gui.get.from.env("con")
	if(is.null(dbcon)) 
		stop(gui.msg("There is no active project!"))
	
	if(!is.na(.sqlQuery(dbcon, "SELECT gridSize from metadata")$gridSize)) 
		stop(gui.msg("The canvas was allready constructed, the grid size cannot be changed for this project!"))

	if(is.na(.sqlQuery(dbcon, "SELECT xmin from metadata")$xmin)) 
		stop(gui.msg("There is no bouding box!"))

	
	bb  = global.bbox.fetch(dbcon)
	minSpan = min(diff(bbox(bb)[1, ]), diff(bbox(bb)[2, ]))

	WarnCellsize = minSpan/150

	top<-tktoplevel()
	Res <- tclVar(round(minSpan/50, 2) )
	entry.Res <-tkentry(top,width="20",textvariable=Res)
	tkgrid(tklabel(top,text="Enter gridSize \n(in map units):"))
	tkgrid(entry.Res)
	
	OnOK <- function() {
		val = as.numeric(tclvalue(Res))
		
		gridSize.save(dbcon , val)
		
		if(val < WarnCellsize) gui.msg("WARNING: the canvas is going to have a high resolution, the next steps can be time consumming.")
		
		tkdestroy(top)
	}

	OK.but <- tkbutton(top,text="   OK   ",command=OnOK)
	tkbind(entry.Res, "<Return>",OnOK)
	tkgrid(OK.but)
	tkfocus(top)

	tkwait.window(top)

}

gui.canvas.save <- function() {
		dbcon = gui.get.from.env("con")
		if(is.null(dbcon)) stop(gui.msg("There is no active project!"))
		
		canvas.save(dbcon)

	}	
		
gui.processRanges <- function() {
	
	dbcon = gui.get.from.env("con")
	if(is.null(dbcon)) stop(gui.msg("There is no active project!"))
	
	Files = gui.selectShpFiles(ogr = TRUE, polygons.only = TRUE)
	
	processRanges(gui.get.from.env("ranges"),dbcon)		
}

gui.bio.save <- function() {

	dbcon = gui.get.from.env("con")
		if(is.null(dbcon)) stop(gui.msg("There is no active project!"))

	f = tk_choose.files( filter = matrix(c("comma delim, sep = ';'", ".csv"), ncol = 2) )
	dat = read.csv(f, as.is = TRUE,sep = ";", strip.white = TRUE)

	tabnam = make.db.names.default(gsub(".csv", "", basename(f)))
	
    	
	common_id = tk_select.list(names(dat),  multiple = FALSE, title = "Select range ID")
	
	if(nchar(common_id) ==0) gui.msg ("Nothing selected") else 
		bio.save(dbcon, table_name = tabnam, dat = dat, common_id = common_id)
		
	}

gui.chooseFunction <- function(predifined =c("richness", "mean", "median", "sd"),functionNam = "FUN" , envir = .RangeMapper) {
	
	top  =  tktoplevel()
	tkwm.title(top, "Choose or define a function") 
	tkgrid(tklabel(top,text="Choose function"))
	FUNS  =  predifined
	comboBox  =  tkwidget(top,"ComboBox", editable=FALSE, values=FUNS)
	tkgrid(comboBox)
		onOK_1  =  function() {
			fun  =  FUNS[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1]
						
			if(! identical(fun, character(0))) {
			if(fun == "richness") 
				assign("FUN", fun , envir = envir) else
					assign("FUN", eval(parse(text= fun), envir = NULL), envir = envir)
			assign("FUN.def", fun,envir = envir)
			tkdestroy(top)
			}
			
	}
	OK1  =  tkbutton(top, text = "OK", width = 6, command = onOK_1 )
	tkgrid(OK1)

	tkgrid(tklabel(top,text="Define function"))
	txt  =  tktext(top, height=10)
	tkinsert(txt, "0.0", "function(x) {\n\n}")

	tkgrid(txt)

		onOK_2  =  function() {
			fun = tclvalue(tkget(txt,"0.0","end"))
			
			fun = try(eval(parse(text=fun), envir = NULL), silent=TRUE)
			
			if(!class(fun)[1] ==  "try-error") {
			assign("FUN", fun, envir = envir)
			assign("FUN.def", paste(strwrap(paste(deparse(fun), collapse = ""), width = 0.9 * getOption("width")/2)[1], "..."),envir = envir)
			tkdestroy(top)
			}

	}

	OK1  =  tkbutton(top, text = "OK", width = 6, command = onOK_2 )
	tkgrid(OK1)


	
	tkfocus(top)
	tkwait.window(top)
	
	
	if( gui.exists.in.env("FUN") ) gui.msg( paste("<ACTIVE FUNCTION>", gui.get.from.env("FUN.def") ), keep = TRUE )


}

gui.tkColorPalette <- function() {

	gui.msg("Loading color palettes...", keep = FALSE)

	tkColorPalette(pal = brewer.pal.get(), name = "palette" , palette.size = 45, envir = .RangeMapper)

	if( gui.exists.in.env("palette") ) gui.msg( paste("<ACTIVE PALETTE>", attributes(gui.get.from.env("palette")) ), keep = TRUE )
}

gui.rangeMap.save <- function() {
	
	dbcon = gui.get.from.env("con")
		if(is.null(dbcon)) stop(gui.msg("There is no active project!"))
		
	FUN = gui.get.from.env("FUN")
		if(is.null(FUN)) stop(gui.msg("First choose a function!"))
	
	FUN.def = gui.get.from.env("FUN.def")

	if(FUN.def == "richness") FUN = NULL	
		
	
	tab = if(FUN.def=="richness") "richness" else tkdbBrowse(dbcon, prefix = "BIO", tables.name.only = FALSE)
	
	table.nam =  gui.tkEntryBox(txt = "enter table name\n(-MAP_- prefix will be appended to it).", default.entry =  paste(tab, collapse = "_") )
	
		
	rangeMap.save(dbcon, FUN = FUN, table.nam = table.nam ,  biotab   = tab[1], biotrait = if(FUN.def=="richness") "richness" else tab[2])

	
}

gui.rangeMap.fetch <- function() {
	
	dbcon = gui.get.from.env("con")
	if(is.null(dbcon)) stop(gui.msg("There is no active project!"))

	colorPallette = gui.get.from.env("palette")
	if(is.null(colorPallette)) stop(gui.msg("There is no active palette!"))
	
	require(lattice)
	
	map = tkdbBrowse(dbcon, prefix = "MAP", tables.name.only = TRUE)

	rangeMap = rangeMap.fetch(dbcon, map)

	Ramp = colorRampPalette(colorPallette, space = "Lab")

	trellis.par.set( "regions", list(col= Ramp(100) )  )

	suppressWarnings(print(spplot(rangeMap , names(rangeMap)[!names(rangeMap)=="id"] ,  scales = list(draw = FALSE), cuts = 20)))

}






















