
brewer.pal.get <- function(palette = NULL) {
	pal = brewer.pal.info[, ]
	pal = pal[!pal$category == "qual",]
	bp = lapply(split(pal, row.names(pal)), FUN = function(x) brewer.pal(x$maxcolors, row.names(x)))
	if(!is.null(palette) && palette%in%names(bp) ) bp = bp[palette][[1]]
	bp 		   
	}

tkColorPalette <- function(pal , name, palette.size = 45, envir = .GlobalEnv) { 

	require(tcltk)

	wd = getwd()
	on.exit(setwd(wd))
	setwd(tempdir())


	top = tktoplevel()
	tkwm.title(top, "Choose a pallete")  

	frm1    = tkframe(top, relief = "ridge", borderwidth = 2)
	frm2    = tkframe(top, relief = "ridge", borderwidth = 1)

	x = ceiling(sqrt(length(pal)))
	y = floor(sqrt(length(pal)))

	cols = rep(1:x, x)
	rows = rep(1:y, each = x)

	# Inverse palette ?
	cb = tkcheckbutton(frm2)
	cbValue = tclVar("0")
	tkconfigure(cb,variable=cbValue)
	lab = tklabel(frm2,text="Inversed palette")
	tkgrid(lab, row = 0, column = 1)
	tkgrid(cb, row = 0, column = 2)
	
	

	#  add name and pos  pal
	for(i in 1:length(pal) ) pal[[i]] = c(names(pal[i]), cols[i], rows[i], pal[[i]])
	
	
	
	lapply(	pal, function(x) {
		
		onOK = function() {
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
			assign(  img.nam, tkbutton(frm1, image  =  img,text= img.nam  , command  =  onOK) )
			tkgrid(get(img.nam),  column  = x[2], row = x[3],sticky = "w" )

	})



	
	# pack
	tkpack(frm1)
	tkpack(frm2)
    tkwait.window(top)
		
	
}

















