
#UTILS
.X.Msg <- function(.X.Msg=Sys.time(), tkMainWindow = "win", tkElement = ".X.Msg", eol = "\n", keep = TRUE, clearup = FALSE, getTime = FALSE, envir = ".RangeMapper") {

   .X.Msg	= paste(.X.Msg, collapse = eol)
   .X.Msg	= paste("\xBB\xBB", .X.Msg, collapse = " ")
   
   
  if(getTime) .X.Msg = paste( "<", Sys.time(), ">\n", .X.Msg, sep = "")
  
  if(exists(envir)) env = eval(parse(text = envir)) else env = NULL
  
  # if  tcltk envir is set
  if(is.environment(env) && exists(tkMainWindow, envir = env) && exists(tkElement, envir = env) ) {

	  .X.MsgWindow = get(tkElement, envir = env)
	  mainWindow = get(tkMainWindow, envir = env)
	  
	# prepare  message container 
	 if(! exists("session.X.Msg", envir = env) ) assign("session.X.Msg", list(), envir = env)
	 
	# clearup any existing messages from env	
		if(clearup) {
			tkdelete(.X.MsgWindow, "0.0" , "1000.0" )
			assign("session.X.Msg", list(), envir = env)
			}
	# if .X.Msg is to be kept then append it to session.X.Msg 
		if(keep) assign("session.X.Msg", c( get("session.X.Msg", envir = env), .X.Msg ) , envir = env )
			
	#  print to GUI element
		tkdelete(.X.MsgWindow, "1.0" , "100.0" ) 
		.X.MsgList = get("session.X.Msg", envir = env)
		
		lapply(.X.MsgList , function(x) tkinsert(.X.MsgWindow, "end" , paste(x,eol) ) )
		
		if(!keep) tkinsert(.X.MsgWindow, "end" , paste(.X.Msg,eol) ) 
		
		tkyview.moveto(get(tkElement, envir = env), 1)
		tkfocus(get(tkMainWindow, envir = env ))
		
		tcl("update", "idletasks") 		 

}	else   cat(.X.Msg, eol)
	
    invisible(flush.console() )
	
} 

.X.make.env <- function(env = ".RangeMapper") {

assign(env , new.env(), env = .GlobalEnv)
}

.X.get <- function(x, mode = "any", envir = .RangeMapper) {

obj = try(get(x, envir = envir, mode = mode, inherits = FALSE), silent = TRUE)

if (class(obj) == "try-error") return (NULL) else return(obj)


}

.X.put <- function(x, value, envir = .RangeMapper) { 

obj = try(assign(x, value, envir = envir), silent = TRUE) 
if (class(obj) == "try-error") return (NULL) else return(obj)

}

.X.exists <- function(x, envir = .RangeMapper) {

exists(x, envir = envir)

}

.X.yn <- function(text = "Choose!", yn = c("YES", "NO"), title) {

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

.X.img <- function(gif, file=system.file("ico", paste(gif, "gif", sep = "."), package="rangeMapper")) {
tkimage.create("photo",file=file)
 }

.X.tkEntryBox <- function(txt = "enter value", default.entry = "", default.output =  "", make.sql.nam = TRUE) {
	
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

.X.tkComboEntryBox <- function(title = "", fixedTxt = c("a", "b") ,fixedTxtLab = "choose one", freeTxt = 1,freeTxtLab = "enter one", name, envir = .RangeMapper) {
	
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
						
			.X.put(name, c(fixedTextVal, freeTextVal), envir = envir)
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

.X.tkdbBrowse.active.proj <- function() {

	dbcon = .X.get("con")
		if(is.null(dbcon)) stop(.X.Msg("There is no active project!"))
	
	tkdbBrowse(dbcon)
		if(exists("out")) rm(out, envir = .GlobalEnv)

}

# MAIN GUI ELEMENTS 
.X.dbopen <- function(new = TRUE) {

	.X.Msg("Starting new session....", clearup = TRUE)
	.X.Msg("", getTime = TRUE, keep = TRUE)
	
	if(new) {
		path = tclvalue(tkgetSaveFile(defaultextension = ".sqlite", filetypes = "{rangeMapper_project {.sqlite}}" ) )
		 if(nchar(path)==0)  stop(.X.Msg("Nothing selected!"))
		dbcon = rangeMap.start(file = basename(path), overwrite = TRUE, dir = dirname(path) )
		
		}
	
	if(!new) {
		path = tclvalue(tkgetOpenFile(defaultextension = ".sqlite", filetypes = "{rangeMapper_project {.sqlite}}" ) )
		if(nchar(path)==0)  stop(.X.Msg("Nothing selected!"))
		dbcon = rangeMap.open(path, verbose = TRUE)
		}
		
		.X.put("con", dbcon)
		.X.put("path", path)
		
		print(summary(new("rangeMap", CON = .X.get("con"))))

				
	}
	
.X.close <- function(quitR = FALSE) {

 if(quitR) q(save = "no") else {

   if(!is.null( .X.get("con"))) {
	  try(sqliteCloseConnection(.X.get("con")), silent = TRUE)
	  rm("con", envir = .RangeMapper)
   }
   
   tkdestroy(.X.get ("win"))
   rm(list = ls( envir = .RangeMapper), envir = .RangeMapper, inherits = TRUE)
   rm(.RangeMapper, envir = .GlobalEnv)
   
	
 }

}

.X.show.metadata <- function() {
	dbcon = .X.get("con")
	if(is.null(dbcon)) stop(.X.Msg("There is no active project!"))
	
	print(summary(new("rangeMap", CON = .X.get("con"))))

}

.X.help <- function(what) {

	wrens.shp = 	system.file(package="rangeMapper", "extdata", "wrens", "vector")
	wrens.csv = 	system.file(package="rangeMapper", "data", "wrens.csv")
	

	out = switch(what, 
			support.files = paste("'Wrens' breeding ranges and life hystory data:\n", wrens.shp, "\n", wrens.csv), 
			man           = print(vignette("rangeMapper")),
			citation      = citation("rangeMapper")$textVersion
			)
		
	if(what == "support.files") setwd(wrens.shp)
	
	.X.Msg(out)
	}
	
.X.global.bbox.save <- function() {

	dbcon = .X.get("con")
		if(is.null(dbcon)) stop(.X.Msg("There is no active project!"))

	Dir = tk_choose.dir(default = getwd(), caption = "Select ranges directory")
	
	global.bbox.save(con = dbcon , bbox = Dir)
	
}
	
.X.gridSize.save <- function() {

	dbcon = .X.get("con")
	if(is.null(dbcon)) 
		stop(.X.Msg("There is no active project!"))
	
	
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
		
		gridSize.save(dbcon ,gridSize = val)
		
		if(val < WarnCellsize) .X.Msg("WARNING: The canvas is going to have a high resolution, processing all ranges will be potentially time consumming.")
		
		tkdestroy(top)
	}

	OK.but <- tkbutton(top,text="   OK   ",command=OnOK)
	tkbind(entry.Res, "<Return>",OnOK)
	tkgrid(OK.but)
	tkfocus(top)

	tkwait.window(top)

}
 
.X.canvas.save <- function() {
		dbcon = .X.get("con")
		if(is.null(dbcon)) stop(.X.Msg("There is no active project!"))
		
		canvas.save(dbcon)

	}	
		
.X.processRanges <- function() {
	
	dbcon = .X.get("con")
	if(is.null(dbcon)) stop(.X.Msg("There is no active project!"))

	selectVal = .X.yn(text = "Save range centroid and range extent?") 

	if(selectVal == 1) {
		processRanges(con = dbcon,dir = tk_choose.dir(default = getwd(), caption = "Select ranges directory"), metadata = TRUE)	
		}	
	
	if(selectVal == 0) {
		processRanges(con = dbcon,dir = tk_choose.dir(default = getwd(), caption = "Select ranges directory"), metadata = TRUE)		} 

}

.X.bio.save <- function() {

	dbcon = .X.get("con")
		if(is.null(dbcon)) stop(.X.Msg("There is no active project!"))

	f = tk_choose.files( filters = matrix(c("comma delim, sep = ';'", ".csv"), ncol = 2) )
	
	dat = read.csv(f, as.is = TRUE,sep = ";", strip.white = TRUE)
	
	tabnam = make.db.names.default(gsub(".csv", "", basename(f)))
	
    common_id = tk_select.list(names(dat),  multiple = FALSE, title = "Select range ID")
	
	if(nchar(common_id) ==0) .X.Msg ("Nothing selected") else 
		bio.save(con = dbcon, loc =  dat,  ID = common_id, tableName = tabnam)

}

.X.chooseVariable <- function() {

	con = .X.get("con")
	if(is.null(con)) stop(.X.Msg("There is no active project!"))

	if(.X.exists("VAR")) rm(list = "VAR", envir = .RangeMapper)
	
	VAR = tkdbBrowse(con, prefix = "BIO", tables.name.only = FALSE, info = "Choose a variable to map.")	
		
	.X.put("VAR", VAR[1, ])
	
	.X.Msg = .X.get("VAR")
	.X.Msg( paste("<ACTIVE VARIABLE>", paste(.X.Msg[2], "in table ", .X.Msg[1])), keep = TRUE )
	
}

.X.chooseFunction <- function() {
	
	# all sqlite aggreegate functions + functions in assemblageStat.R without (default) extra arguments
	predefined =  as.character(.sqlAggregate())
	
	if(.X.exists("FUN")) rm("FUN", envir = .RangeMapper)
	
	top  =  tktoplevel()
	tkwm.title(top, "Choose or define a function") 
	
	Frame1   = tkframe(top, relief= "ridge", borderwidth= 1)
	Frame2   = tkframe(top, relief= "ridge", borderwidth= 1)
		
	lab1 = tklabel(Frame1,text = "CHOOSE FUNCTION")
	
	# predefined functions
	FUNS  =  predefined
	comboFUN  =  tkwidget(Frame1,"ComboBox", editable = TRUE, values=FUNS)
	
	# custom function
	lab3 = tklabel(Frame2,text = "DEFINE FUNCTION")
	txtFUN  =  tktext(Frame2, height=5)
	tkinsert(txtFUN, "0.0", "function(x) {\n\n}")

	# formula
	depvar = paste(.X.get("VAR")[2], "~")
	depvarLab = tklabel(Frame1, text = depvar)
	Formula =  tclVar("")
	pred  = tkentry(Frame1, width = "50" , textvariable= Formula)

		onOK  =  function() {
			#formula
			formVal = try(formula( paste(depvar, tclvalue(Formula) ) )	, silent = TRUE)
				if(nchar(tclvalue(Formula) )> 0 && inherits(formVal, "try-error" )) 
					stop(tkmessageBox(message = "invalid R formula!", icon = "error", type = "ok") )
					
			# pre-defined function
			fun  =  FUNS[as.numeric(tclvalue(tcl(comboFUN,"getvalue")))+1]
			
			# parse if R function OR keep as string if sql function
			if(! identical(fun, character(0))) {
				if(! fun%in%.sqlAggregate() )
					fun = eval(parse(text= fun), envir = NULL)
			}
			
			# custom function if no pre-defined
			if(identical(fun, character(0))) { 
				fun = tclvalue(tkget(txtFUN,"0.0","end"))
				fun = try(eval(parse(text= fun), envir = NULL), silent = TRUE)
				
				if(inherits(fun, "try-error" )) 
					stop(tkmessageBox(message = "invalid R function!", icon = "error", type = "ok") )
			
			}
			
			# save output	
			.X.put("FUN", fun)
			.X.put("FUN.formula", formVal)

			funStr = .X.get("FUN")
			funStr = gsub(" ", "", paste(deparse(funStr), collapse = "" ) )
			
			
			.X.Msg( paste("<ACTIVE FUNCTION>", funStr ), keep = TRUE )
			
			tkdestroy(top)
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
		


	tkfocus(top)
	tkwait.window(top)

}

.X.chooseSubset <- function() {

	dbcon = .X.get("con")
		if(is.null(dbcon)) stop(.X.Msg("There is no active project!"))

	if(.X.exists("SUBSET")) rm("SUBSET", envir = .RangeMapper)
		
	tabs = RMQuery(dbcon, "select name from sqlite_master where type = 'table' and (name like 'BIO_%' or name like 'MAP_%' or name = 'metadata_ranges')")$name
	tabs = lapply(split(tabs, tabs), function(x) RMQuery(dbcon, paste("PRAGMA table_info(", x, ");"))$name)
	
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
	

	.X.put("SUBSET", Res)
		
	
	if( .X.exists("SUBSET") ) .X.Msg( paste("<ACTIVE SUBSET>", .X.get("SUBSET") ), keep = FALSE )

	
	
	}

.X.tkColorPalette <- function() {

	.X.Msg("Loading color palettes...", keep = FALSE)

	pal =  c(brewer.pal.get(), list(heatcol = heat.colors (9),terraincol = terrain.colors(9), topocol = topo.colors(9) ))
 	
	tkColorPalette(pal = pal, name = "palette" , palette.size = 43, envir = .RangeMapper)

	if( .X.exists("palette") ) .X.Msg( paste("<ACTIVE PALETTE>", attributes(.X.get("palette")) ), keep = TRUE )
}

.X.rangeMap.save <- function() {
	t1 = Sys.time()
	dbcon       =.X.get("con")
	FUN         =.X.get("FUN")
	VAR         =.X.get("VAR")
	FUN.formula =.X.get("FUN.formula")
	subsetSQL   =.X.get("SUBSET")
	
	if(is.null(subsetSQL)) subsetSQL = list()
	if(is.null(dbcon)) stop(.X.Msg("There is no active project!"))

	if(is.null(FUN)) {
		.X.Msg("Since no function was chosen SPECIES RICHNESS will be computed by default!")
		tableName = "species_richness"
		rangeMap.save(dbcon, tableName = tableName)
		} 
	
	
	if(is.character(FUN)) {
		suggested.tab.nam = paste( c(VAR[1], VAR[2], FUN), collapse  = "_")
		tableName =  .X.tkEntryBox(txt = "enter table name\n(-MAP_- prefix will be appended to it).", default.entry =  suggested.tab.nam, default.output = "." )
		if(tableName!="_")
		rangeMap.save(CON = dbcon, FUN = FUN, biotab =  VAR[1][,1], biotrait =  VAR[2][,1], tableName = tableName, subset = subsetSQL)
		}
	

	if(is.function(FUN)) {
		suggested.tab.nam = paste( c(VAR[1], VAR[2], "aggregate_R_function"), collapse  = "_")
		tableName =  .X.tkEntryBox(txt = "enter table name\n(-MAP_- prefix will be appended to it).", default.entry =  suggested.tab.nam )
		
		if(tableName!="_") {
			if(is.null(FUN.formula) )
			rangeMap.save(CON = dbcon, FUN = FUN, biotab =  VAR[1][,1], biotrait =  VAR[2][,1], tableName = tableName, subset = subsetSQL) else
				rangeMap.save(CON = dbcon, FUN = FUN, biotab =  VAR[1][,1], biotrait =  VAR[2][,1], formula = FUN.formula, tableName = tableName, subset = subsetSQL)
		}
		
		
		}
		

	}

.X.rangeMap.plot <- function() {
	
	dbcon = .X.get("con")
	if(is.null(dbcon)) stop(.X.Msg("There is no active project!"))

	colorpalette = .X.get("palette")
	if(is.null(colorpalette)) stop(.X.Msg("There is no active palette!"))
	
	map = tkdbBrowse(dbcon, prefix = "MAP", tables.name.only = TRUE)

	rangeMap = rangeMap.fetch(dbcon, map$dbtable )
	
	# ncols and style
	classType = c("sd", "equal", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks")
	.X.tkComboEntryBox(title = "plot MAP", fixedTxt = classType,  fixedTxtLab = "Class interval type",  freeTxt = 20, freeTxtLab = "Number of classes", name = "CLASSINT")
	
	classint = .X.get("CLASSINT")	
	
	plot(rangeMap, colorpalette = colorpalette, ncols = as.numeric(classint[2]) , style = classint[1]) 

}

.X.rangeMap.rm <- function(table.type) {
	dbcon = .X.get("con")
	if(is.null(dbcon)) stop(.X.Msg("There is no active project!"))

	nam = as.character(tkdbBrowse(dbcon, prefix = table.type, tables.name.only = TRUE)$dbtable)
	if(exists("out")) rm(out, envir = .GlobalEnv)

	table.nam = paste(table.type, nam, sep = "_")
	
	if(all(is.na(nam)) && .X.yn("Really delete all ?") )
				rm.rangeMapper(dbcon, tablePrefix = table.type)

	if(! all(is.na(nam)) ) 
		rm.rangeMapper(dbcon, tableName = table.nam, tablePrefix = table.type)
	

}

.X.mapImport <- function() {

	dbcon = .X.get("con")
	if(is.null(dbcon)) stop(.X.Msg("There is no active project!"))


	rangeMap.save(dbcon , path = tk_choose.files(caption = "Select file to import", multi = FALSE))
	
	
	


}

.X.metadata2bio <-function() {
	dbcon = .X.get("con")
		if(is.null(dbcon)) stop(.X.Msg("There is no active project!"))
	
	metadata2bio (dbcon)

}

.X.rangeMap.export <-function() {

	dbcon = .X.get("con")
		if(is.null(dbcon)) stop(.X.Msg("There is no active project!"))
		
	path = tkchooseDirectory()

	rangeMap.export(con = dbcon, tclvalue(path))



}

.X.bio.merge <- function() {
	dbcon = .X.get("con")
	if(is.null(dbcon)) stop(.X.Msg("There is no active project!"))
	tableName = .X.tkEntryBox("Name of the merged BIO table")
	bio.merge(con = dbcon, tableName = tableName)
	
	
}

















