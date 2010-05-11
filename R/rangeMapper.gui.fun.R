
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

exists(x, envir = envir)

}

gui.yn <- function(text = "Choose!", yn = c("YES", "NO"), title) {

	if(missing(title)) title = paste("rangeMapper", packageDescription("rangeMapper")$Version) 

	font = "helvetica 12";  relief="flat"; borderwidth= 1

	top <- tktoplevel()
	but.width = ifelse(max(nchar(yn)) < 6, 6, max(nchar(yn)))
	tkwm.minsize(top, 200, 50)
	tktitle(top) <- title
	
	bar1  <- tkframe(top, relief=relief, borderwidth=borderwidth)
	tkgrid(tklabel(bar1,text=text),  sticky = "w", column  = 1, row= 0)
	tkpack(bar1, fill="both", expand = 1)
	
	bar2  <- tkframe(top, relief=relief, borderwidth=borderwidth)
	
	select <- tclVar(0)
	f.but <- tkbutton(bar2, text = yn[1], width = but.width, command = function() tclvalue(select) <- 1)
	d.but <- tkbutton(bar2, text = yn[2], width = but.width, command = function() tclvalue(select) <- 0)
	
	tkgrid(f.but, d.but, column  = 1, row= 1, sticky = "w", padx = 2)
	tkgrid(d.but, column  = 2, row= 1, sticky = "w", padx = 2)
	tkpack(bar2, fill="both", expand = 1)
	
	
	tkfocus(top)
	tkwait.variable(select)
	tkdestroy(top)
	return(as.integer(tclvalue(select)))
}

gui.msg <- function(msg, tkMainWindow = "win", tkElement = "msg", eol = "\n", keep = FALSE, clearup = FALSE, getTime = FALSE) {

 if(getTime) msg = paste( "<", Sys.time(), ">\n", msg, sep = "")

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

gui.tkEntryBox <- function(txt = "enter value", default.entry = "", default.output =  "", make.sql.nam = TRUE) {
	
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

gui.tkComboEntryBox <- function(title = "", fixedTxt = c("a", "b") ,fixedTxtLab = "choose one", freeTxt = 1,freeTxtLab = "enter one", name, envir = .RangeMapper) {
	
	tclRequire("BWidget")
	
	W = max(c(nchar(fixedTxtLab), nchar(freeTxtLab)))
	
	top  =  tktoplevel()
	tkwm.title(top, title) 
	
	Frame1   = tkframe(top)
	Frame2   = tkframe(top)
		
	labfixedText = tklabel(Frame1,text = fixedTxtLab, width = W)
	
	fixedText  =  tkwidget(Frame1, type = "ComboBox", editable=FALSE, values = fixedTxt)
	
	labfreeText = tklabel(Frame2,text = freeTxtLab, width = W)
	freeText  =  tktext(Frame2)
	freeTxtVal = tclVar(freeTxt)
	freeText = tkentry(Frame2 ,textvariable = freeTxtVal)

		onOK  =  function() {
			fixedTextVal  =  fixedTxt[as.numeric(tclvalue(tcl(fixedText,"getvalue")))+1]
			freeTextVal   =  tclvalue(tcl(freeText,"get"))
						
			gui.put.to.env(name, c(fixedTextVal, freeTextVal), envir = envir)
			tkdestroy(top)
		}

	OK  =  tkbutton(Frame2, text = "OK", width = 6, command = onOK)

	tkpack(Frame1, fill = "both")
		tkgrid(labfixedText, sticky ="w", column= 0, row = 0)
		tkgrid(fixedText,   sticky ="e" , column= 1, row = 0)

	tkpack(Frame2)
		tkgrid(labfreeText, sticky ="w", column= 0, row = 0)
		tkgrid(freeText,     sticky="e", column= 1, row = 0)
		tkgrid(OK,         sticky="e"  , column= 0, row = 1)
	
	tkfocus(top)
	tkwait.window(top)	
}

gui.tkdbBrowse.active.proj <- function() {

	dbcon = gui.get.from.env("con")
		if(is.null(dbcon)) stop(gui.msg("There is no active project!"))
	
	tkdbBrowse(dbcon)
		if(exists("out")) rm(out, envir = .GlobalEnv)

}

gui.show.metadata <- function() {
	dbcon = gui.get.from.env("con")
		if(is.null(dbcon)) stop(gui.msg("There is no active project!"))
	
	metadata = .sqlQuery(dbcon, "select * from metadata")
	metadata  = paste(paste(names(metadata), metadata[1,], sep = "\t"), collapse = "\n")

	gui.msg(metadata)

}

gui.help <- function(what) {

	wrens.shp = 	system.file(package="rangeMapper", "extdata", "wrens", "vector")
	wrens.csv = 	system.file(package="rangeMapper", "data", "wrens.csv")
	

	out = switch(what, 
			support.files= paste("'Wrens' breeding ranges and life hystory data:\n", wrens.shp, "\n", wrens.csv), 
			man          = vignette("rangeMapper"),
			citation     = attributes(citation("rangeMapper"))$textVersion
			)
		
	if(what == support.files) setwd(wrens.shp)
	
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

gui.close <- function(quitR = FALSE) {

 if(quitR) q(save = "no") else {

   if(!is.null(gui.get.from.env ("con"))) {
	  try(sqliteCloseConnection(gui.get.from.env ("con")), silent = TRUE)
	  rm("con", envir = .RangeMapper)
   }
   
   tkdestroy(gui.get.from.env ("win"))
   
    rm(list = ls( envir = .RangeMapper), envir = .RangeMapper, inherits = TRUE)
	
 }

}

gui.selectShpFiles <- function(ogr, polygons.only) {

	selectVal = gui.yn(yn = c("FILES", "DIRECTORY")) 
	
	if(selectVal == 0) { # directory
		ff = selectShpFiles(tk_choose.dir(default = getwd(), caption = "Select ranges directory"), ogr = ogr, polygons.only = polygons.only)
		} 

	if(selectVal == 1) { # files
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

	WarnCellsize = minSpan/200

	top<-tktoplevel()
	Res <- tclVar(round(minSpan/100, 2) )
	entry.Res <-tkentry(top,width="20",textvariable=Res)
	tkgrid(tklabel(top,text="Enter gridSize \n(in map units):"))
	tkgrid(entry.Res)
	
	OnOK <- function() {
		val = as.numeric(tclvalue(Res))
		
		gridSize.save(dbcon , val)
		
		if(val < WarnCellsize) gui.msg("WARNING: The canvas is going to have a high resolution, processing all ranges will be potentially time consumming.")
		
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
	
	# save to 'metadata_ranges' ?
	selectVal = gui.yn(text = "Save range centroid and range extent?") 

	if(selectVal == 1) {
	processRanges(gui.get.from.env("ranges"),dbcon, metadata = TRUE)	
		}	

	
	if(selectVal == 0) {
	processRanges(gui.get.from.env("ranges"),dbcon, metadata = FALSE)	
		} 

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

gui.chooseVariable <- function() {

	con = gui.get.from.env("con")
	if(is.null(con)) stop(gui.msg("There is no active project!"))

	if(gui.exists.in.env("VAR")) rm(list = "VAR", envir = .RangeMapper)
	
	VAR = tkdbBrowse(con, prefix = "BIO", tables.name.only = FALSE, info = "Choose a variable to map.")	
		
	gui.put.to.env("VAR", VAR)
	
	gui.msg( paste("<ACTIVE VARIABLE>", gui.get.from.env("VAR") ), keep = TRUE )
	
}

gui.chooseFunction <- function(predefined = c("richness", "mean", "median", "sd", "lmer") , functionNam = "FUN") {
	
	if(gui.exists.in.env("FUN")) rm("FUN", envir = .RangeMapper)
	
	VAR = gui.get.from.env("VAR")
	if(is.null(VAR) || is.na(VAR[1]) ){
		gui.put.to.env("VAR", c(NA, "richness") )
		predefined = "richness"
		}
		

	top  =  tktoplevel()
	tkwm.title(top, "Choose or define a function") 
	
	Frame1   = tkframe(top, relief= "ridge", borderwidth= 1)
	Frame2   = tkframe(top, relief= "ridge", borderwidth= 1)
		
	lab1 = tklabel(Frame1,text="CHOOSE FUNCTION")
	
	# predefined functions
	FUNS  =  predefined
	comboFUN  =  tkwidget(Frame1,"ComboBox", editable=FALSE, values=FUNS)
	
	# custom function
	lab3 = tklabel(Frame2,text="DEFINE FUNCTION")
	txtFUN  =  tktext(Frame2, height=5)
	tkinsert(txtFUN, "0.0", "function(x) {\n\n}")

	# formula
	depvar = paste(gui.get.from.env("VAR")[2], "~")
	depvarLab = tklabel(Frame1, text = depvar)
	Formula =  tclVar("1")
	pred  = tkentry(Frame1, width=  "50" , textvariable= Formula)

		onOK  =  function() {
			# pre-defined function
			fun  =  FUNS[as.numeric(tclvalue(tcl(comboFUN,"getvalue")))+1]
			
			if(identical(fun, character(0))) { # custom function
				fun = tclvalue(tkget(txtFUN,"0.0","end"))
				}
			
			#formula
			formVal = formula( paste(depvar, tclvalue(Formula) ) )	
				
			# check if valid
			valid.fun = !class(try(eval(parse(text=fun), envir = NULL), silent=TRUE)) ==  "try-error"
			valid.formula = class(formVal) == "formula"
			
			if(valid.fun  && valid.formula) { # assign
				gui.put.to.env("FUN", eval(parse(text= fun), envir = NULL) )
				gui.put.to.env("FUN.def", fun)
				gui.put.to.env("FUN.formula", formVal)
				
				tkdestroy(top)
			}
		}

	# pack all
	tkbind(pred, onOK)
	
	OK  =  tkbutton(Frame2, text = "OK", width = 6, command = onOK)

	tkpack(Frame1, fill="both", expand = 1)
		tkgrid(lab1    , sticky="w", columnspan= 1, column= 0, row = 0)
		tkgrid(comboFUN, sticky="w", columnspan= 1, column= 1, row = 1)
		tkgrid(depvarLab,sticky="w", columnspan= 1, column= 2, row = 1)
		tkgrid(pred    , sticky="w", columnspan= 1, column= 3, row = 1)
	
	tkpack(Frame2)
		tkgrid(lab3,columnspan= 1, sticky="w", column= 0, row = 0)
		tkgrid(txtFUN ,columnspan= 1, sticky="w", column= 0, row = 1)
		tkgrid(OK ,columnspan= 1, sticky="w", column= 0, row = 2)
		

	if( gui.exists.in.env("FUN") ) gui.msg( paste("<ACTIVE FUNCTION>", gui.get.from.env("FUN.def") ), keep = TRUE )

	tkfocus(top)
	# tkwait.window(top)

}

gui.chooseSubset <- function() {

	dbcon = gui.get.from.env("con")
		if(is.null(dbcon)) stop(gui.msg("There is no active project!"))

	if(gui.exists.in.env("SUBSET")) rm("SUBSET", envir = .RangeMapper)
		
	tabs = .sqlQuery(dbcon, "select name from sqlite_master where type = 'table' and (name like 'BIO_%' or name like 'MAP_%' or name = 'metadata_ranges')")$name
	tabs = lapply(split(tabs, tabs), function(x) .sqlQuery(dbcon, paste("PRAGMA table_info(", x, ");"))$name)
	
	SQL  = lapply(tabs , function(x) paste(x, collapse  = "\n") )
	
	# gui
	top = tktoplevel()
	tktitle(top) = "Choose subsets!" 
	
	queryTxt = rep("", length(tabs))
	text = lapply(tabs, function(x) NULL)
	textField = lapply(tabs, function(x) NULL)
		
	for(i in 1:length(tabs) ) {
		tkgrid(tklabel(top, text = names(tabs[i])), sticky="w", column  = 0, row = i)
		
		text[[i]] <- tclVar(queryTxt[i])
		textField[[i]] <- tkentry(top,width = "75", textvariable=text[[i]])
		
		tk2tip(textField[[i]], SQL[[i]])
		tkgrid(textField[[i]], sticky="w", column  = 1, row = i)
		}
	
	OnOK <- function() {
		Out <<- text
		tkdestroy(top)
	}
	
	OK  = tkbutton(top, text="OK", width = 6, command=OnOK)
	tkbind(textField[[i]], "<Return>",OnOK)
	tkgrid(OK, sticky="e", column  = 1)
	
	tkfocus(top)
	tkwait.window(top)
	
	Res = 	lapply(Out, function(x) tclvalue(x))
	Res = Res[unlist(lapply(Res, nchar))>0]
	

	gui.put.to.env("SUBSET", Res)
		
	
	if( gui.exists.in.env("SUBSET") ) gui.msg( paste("<ACTIVE SUBSET>", gui.get.from.env("SUBSET") ), keep = FALSE )

	
	
	}

gui.tkColorPalette <- function() {

	gui.msg("Loading color palettes...", keep = FALSE)

	pal =  c(brewer.pal.get(), list(heatcol = heat.colors (9),terraincol = terrain.colors(9), topocol = topo.colors(9) ))
 	
	tkColorPalette(pal = pal, name = "palette" , palette.size = 43, envir = .RangeMapper)

	if( gui.exists.in.env("palette") ) gui.msg( paste("<ACTIVE PALETTE>", attributes(gui.get.from.env("palette")) ), keep = TRUE )
}

gui.rangeMap.save <- function() {
	
	dbcon = gui.get.from.env("con")
		if(is.null(dbcon)) stop(gui.msg("There is no active project!"))
		
	FUN = gui.get.from.env("FUN")
		if(is.null(FUN)) stop(gui.msg("First choose a function!"))
		
	VAR = gui.get.from.env("VAR")
	
	FUN.def = gui.get.from.env("FUN.def")
	FUN.formula = gui.get.from.env("FUN.formula")
	
	subsetSQL = gui.get.from.env("SUBSET")
	
	suggested.tab.nam = paste( c(VAR[1], FUN.def), collapse  = "_") 
	table.nam =  gui.tkEntryBox(txt = "enter table name\n(-MAP_- prefix will be appended to it).", default.entry =  suggested.tab.nam )
		
	gui.msg( paste("Please wait! Computing", table.nam , ". This will take some time for computing intensive functions and/or large grids.") )
	
	t1 = Sys.time()
	
	rangeMap.save(dbcon, FUN = FUN, table.nam = table.nam ,  biotab   = VAR[1], biotrait = FUN.formula, subset = subsetSQL)

	if(.dbtable.exists(dbcon, "table.nam"))
			gui.msg( paste(table.nam , "saved to the active project! Ellapsed time:", round(difftime(Sys.time(), t1, 2)), "mins" ) )
	
	
	
	}

gui.rangeMap.plot <- function() {
	
	dbcon = gui.get.from.env("con")
	if(is.null(dbcon)) stop(gui.msg("There is no active project!"))

	colorpalette = gui.get.from.env("palette")
	if(is.null(colorpalette)) stop(gui.msg("There is no active palette!"))
	
	map = tkdbBrowse(dbcon, prefix = "MAP", tables.name.only = TRUE)

	rangeMap = rangeMap.fetch(dbcon, map)
	
	gui.put.to.env("rangeMap", rangeMap)
	
	# ncols and style
	classType = c("sd", "equal", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks")
	gui.tkComboEntryBox(title = "plot MAP", fixedTxt = classType, fixedTxtLab = "Class interval type",  freeTxt = 20, freeTxtLab = "Number of classes", name = "CLASSINT")
	
	classint = gui.get.from.env("CLASSINT")	
	
	rangeMap.plot(rangeMap, colorpalette, ncols = as.numeric(classint[2]) , style = classint[1]) 
	
}

gui.rangeMap.rm <- function(table.type) {
	dbcon = gui.get.from.env("con")
	if(is.null(dbcon)) stop(gui.msg("There is no active project!"))

	table.nam = tkdbBrowse(dbcon, prefix = table.type, tables.name.only = TRUE)
	if(exists("out")) rm(out, envir = .GlobalEnv)

	if(!is.null(table.nam) && is.na(table.nam) ) {
	rm.rangeMapper(dbcon, table.type = table.type)
	gui.msg(paste("All", table.type, "tables deleted!"), keep = FALSE)
	}	
	
	if(!is.null(table.nam) && !is.na(table.nam)) {
	rm.rangeMapper(dbcon, table.nam =table.nam, table.type = table.type)
	gui.msg(paste(table.nam, "delleted!"), keep = FALSE)
	}	

}

































