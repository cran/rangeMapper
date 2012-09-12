
x.tooltips <- function(tip) {

switch(tip,
# bar 1
Create      = "START PROJECT\n A new sqlite database will be created.\n Type ?rangeMap.start for more info.",
Open        = "OPEN PROJECT\n Connect to an existing sqlite database",
Bbox        = "COMPUTE CANVAS EXTENT\n Compute the global bounding box of the project.\n Type ?global.bbox for more info.",
gridSize    = "INPUT GRID SIZE\n This is the cell size (i.e. the distance between two neighboring points) in map units. \n Type ?gridSize.save for more info.",
canvasUpload= "COMPUTE CANVAS\n Compute canvas using the bounding box and the grid size.\n Type ?canvas.save for more info.",
Ranges      = "PROCESS RANGES\n Perform polygons intersection with the canvas.\n Type ?processRanges for more info.",
Bio         = "IMPORT 'BIO' data\n Import the table (.csv format, ';' separated) containing the variables to be mapped.\n You will need to choose the column corresponding to the names of the range files.\n Type ?bio.save for more info.",   
# bar 2
Var         = "CHOOSE A VARIABLE to be mapped from an existing BIO table.\n 
				Skip this step if species richness is the variable of interest and proceed to 'Save map'.", 
Fun         = "CHOOSE OR DEFINE THE FUNCTION to be applied at each pixel.", 
Subset      = "CHOOSE ONE OR MORE SUBSETS using existing BIO, MAP or metadata_ranges table", 
saveMap     = "SAVE A MAP of a chosen variable.\n Type ?rangeMap.save for more info.", 
palette    = "CHOOSE A COLOR PALETTE.\n Type ?tkColorPalette for more info.", 
Map         = "DISPLAY A MAP on the default device.\n Type ?spplot for more info.\n Type ?classInt::classIntervals for more info on class intervals construction", 
# bar 3
OffGui      = "CLOSE RANGEMAPPER GUI! \n To re-open rangeMapper type 'rangeMapper()' at the R prompter",
OffAll      = "Quit rangeMapper AND R!")

}

rangeMapper <- function() {

   tclRequire("BWidget"); tclRequire("Tktable")
   
   font = "helvetica 10"; fg = "#08306B" ; bg = "#F7FBFF"; relief = "flat"; borderwidth = 0

   if( !is.null ( x.get("win")) ) stop (tkmessageBox(message = "rangeMapper is allready open!", icon = "error", type = "ok") )

      if(!exists(".RangeMapper")) x.make.env()

         x.put("win", tktoplevel() )

         win = x.get("win")

         # window manager
		if(.Platform$OS.type == "windows") 
		 tkwm.iconbitmap(win, system.file("ico", "favicon.ico", package="rangeMapper"))
         tkwm.title(win,paste("rangeMapper", packageDescription("rangeMapper")$Version))
         tkwm.resizable(win, 0, 0)
         tcl("wm", "protocol", win, "WM_DELETE_WINDOW", quote(x.Msg("Please close the window using the lower bar button!")))


         top  <- function() {
            x.put("topMenu",tkmenu(win) )
            topMenu  <- x.get("topMenu")
            tkconfigure(win, menu=topMenu)
            
			HelpMenu <- tkmenu(topMenu, tearoff=FALSE)
            ProjectMenu <- tkmenu(topMenu, tearoff=FALSE)
            MapMenu <- tkmenu(topMenu, tearoff=FALSE)
            metadata_ranges2bioMenu <- tkmenu(topMenu, tearoff=FALSE)

			tkadd(ProjectMenu,"command",label="Browse the active project", command = function() x.tkdbBrowse.active.proj() )
			tkadd(ProjectMenu,"command",label="Remove MAP tables", command = function() x.rangeMap.rm("MAP") )
			tkadd(ProjectMenu,"command",label="Remove BIO tables", command = function() x.rangeMap.rm("BIO") )
          	tkadd(ProjectMenu,"command",label="show project's metadata", command = function() x.show.metadata() )
  
			tkadd(MapMenu,"command",label="import external MAP", command = function() x.mapImport()  )
			tkadd(MapMenu,"command",label="export MAPs to geotiff", command = function() x.rangeMap.export()  )
			
			tkadd(metadata_ranges2bioMenu,"command", label= "convert metadata_ranges to BIO table", command = function() x.metadata2bio()  )
			tkadd(metadata_ranges2bioMenu,"command", label= "merge all BIO tables", command = function() x.bio.merge()  )
			
			tkadd(HelpMenu,"command",label= "Get started",command = function() x.help("man") )
			tkadd(HelpMenu,"command",label= "Example files",command = function() x.help("support.files") )
			tkadd(HelpMenu,"command",label= "About",command = function() x.help("citation") )

			tkadd(topMenu, "cascade", label= "Project",menu=ProjectMenu)
			tkadd(topMenu, "cascade", label= "Maps",menu=MapMenu)
			tkadd(topMenu, "cascade", label= "Tables", menu = metadata_ranges2bioMenu)
			tkadd(topMenu, "cascade", label= "Help",menu=HelpMenu)

         }

         bar1 <- function() {
            bar1    <- tkframe(win, relief=relief, borderwidth=borderwidth, background=bg)


            # ARROWS
            arrow1    =  tklabel(bar1, image=x.img("arrow"),foreground = fg, background = bg )
            arrow2    =  tklabel(bar1, image=x.img("arrow"),foreground = fg, background = bg )
            arrow3    =  tklabel(bar1, image=x.img("arrow"),foreground = fg, background = bg )
            # BUTTONS
			Create    =   tkbutton(bar1,image    = x.img("new") ,command           = function() x.dbopen(new= TRUE)  )
			Open      =   tkbutton(bar1,image    = x.img("open")   ,command        = function() x.dbopen(new= FALSE)  )
			Bbox      =   tkbutton(bar1,image    = x.img("bbox") , command         = function() x.global.bbox.save() )
			gridSize  =   tkbutton(bar1,image    = x.img("resolution") , command   = function() x.gridSize.save() )
			canvasUpload  =   tkbutton(bar1,image= x.img("uploadCanvas") , command = function() x.canvas.save() )
            Ranges     =   tkbutton(bar1,image= x.img("intersectRange") , command  = function() x.processRanges() )
			Bio        =   tkbutton(bar1,image= x.img("uploadBio") ,command        = function() x.bio.save()  )
			
			
            # TIPS
            tk2tip(Create, x.tooltips("Create") )
            tk2tip(Open, x.tooltips("Open") )
            tk2tip(Bbox, x.tooltips("Bbox"))
            tk2tip(gridSize, x.tooltips("gridSize") )
            tk2tip(canvasUpload, x.tooltips("canvasUpload") )
            tk2tip(Ranges, x.tooltips("Ranges"))
            tk2tip(Bio, x.tooltips("Bio"))


            # PLACE ON  GRID
            # LABELS
            tkgrid(tklabel(bar1,text = "Initiate project",font = font, foreground = fg, background = bg),sticky="ns",columnspan = 2, column  = 1, row = 0)
            tkgrid(tklabel(bar1,text = "Prepare canvas",  font = font, foreground = fg, background = bg),sticky="ns",columnspan = 3, column  = 4, row = 0)
            tkgrid(tklabel(bar1,text = "Upload ranges",   font = font, foreground = fg, background = bg),sticky="ns",columnspan = 1, column  = 8, row = 0)
            tkgrid(tklabel(bar1,text = "Upload Bio table",font = font, foreground = fg, background = bg),sticky="ns",columnspan = 1, column  = 10, row = 0)
			
			tkgrid(tklabel(bar1, image=x.img("wren"), background= bg), sticky="ns",columnspan = 1, column  = 11, row = 0)
            
			# BUTTONS
            tkgrid(Create,     column  = 1, row = 1, sticky= "e")
            tkgrid(Open,       column  = 2, row = 1, sticky= "w")
            tkgrid(arrow1,     column  = 3, row = 1, sticky= "n")
			tkgrid(Bbox,       column  = 4, row = 1, sticky= "w")
			tkgrid(gridSize,   column  = 5, row = 1, sticky= "w")
            tkgrid(canvasUpload, column= 6, row = 1, sticky= "w")
            tkgrid(arrow2,     column  = 7, row = 1, sticky= "n")
            tkgrid(Ranges,     column  = 8, row = 1, sticky= "ns")
            tkgrid(arrow3,     column  = 9, row = 1, sticky= "n")
            tkgrid(Bio,        column  =10, row = 1, sticky= "ns")

            tkpack(bar1, fill="both", expand = 1)
         }

         hline <- function() {
            Hline  <- tkframe(win, relief=relief, borderwidth=borderwidth, background=bg)

            tkgrid(tklabel(Hline, image=x.img("hline"), background= bg), columnspan =1, rowspan = 1, sticky= "nsew")
            tkpack(Hline, fill="both", expand = 1)
         }

         bar2 <- function() {
            bar2    <-tkframe(win, relief=relief, borderwidth=borderwidth, background=bg)

            # ARROWS
            arrow1  <- tklabel(bar2, image   = x.img("arrow"),foreground       = fg, background  = bg  )
            arrow2  <- tklabel(bar2, image   = x.img("arrow"),foreground       = fg, background  = bg  )
            arrow3  <- tklabel(bar2, image   = x.img("arrow"),foreground       = fg, background  = bg  )
            arrow4  <- tklabel(bar2, image   = x.img("arrow"),foreground       = fg, background  = bg  )
            arrow5  <- tklabel(bar2, image   = x.img("arrow"),foreground       = fg, background  = bg  )
           # BUTTONS
            Var       <-  tkbutton(bar2, image = x.img("variable")      ,command= function() x.chooseVariable()   )
            Fun       <-  tkbutton(bar2, image = x.img("function")      ,command= function() x.chooseFunction()  )
            Subset    <-  tkbutton(bar2, image = x.img("subsetMap")     ,command= function() x.chooseSubset()  )
            saveMap   <-  tkbutton(bar2, image = x.img("saveMap")       ,command= function() x.rangeMap.save() )
			palette  <-  tkbutton(bar2, image = x.img("colorPalette")  ,command= function() x.tkColorPalette() )
            Map       <-  tkbutton(bar2, image = x.img("plotMap")       ,command= function() x.rangeMap.plot() )
			
            # TIPS
            tk2tip(Var, x.tooltips("Var"))
            tk2tip(Fun, x.tooltips("Fun"))
            tk2tip(Subset, x.tooltips("Subset"))
            tk2tip(saveMap, x.tooltips("saveMap"))
			tk2tip(palette, x.tooltips("palette"))
            tk2tip(Map, x.tooltips("Map"))

			# LABELS
			tkgrid(tklabel(bar2,text    = "Choose variable",font   = font, foreground= fg, background= bg),sticky="w",columnspan= 2, column= 1, row = 0)
			tkgrid(tklabel(bar2,text    = "Choose function",font   = font, foreground= fg, background= bg),sticky="w",columnspan= 2, column= 4, row = 0)
			tkgrid(tklabel(bar2,text    = "Choose subset",  font   = font, foreground= fg, background= bg),sticky="w",columnspan= 2, column= 7, row = 0)
			tkgrid(tklabel(bar2,text    = "Save map",       font   = font, foreground= fg, background= bg),sticky="w",columnspan= 2, column= 10, row = 0)
			tkgrid(tklabel(bar2,text    = "Choose Palette", font   = font, foreground= fg, background= bg),sticky="w",columnspan= 2, column= 14, row = 0)
			tkgrid(tklabel(bar2,text    = "Display map",    font   = font, foreground= fg, background= bg),sticky="w",columnspan= 2, column= 17, row= 0)
			# BUTTONS
			tkgrid(Var, column       = 1, row  = 1,  sticky   = "e")
			tkgrid(arrow1, column    = 3, row  = 1,  sticky   = "ne")    
			tkgrid(Fun, column       = 4, row  = 1,  sticky   = "e")
			tkgrid(arrow2, column    = 6, row  = 1,  sticky   = "ne")
			tkgrid(Subset, column    = 7, row  = 1,  sticky   = "e")
			tkgrid(arrow3, column    = 9, row  = 1,  sticky   = "ne")
			tkgrid(saveMap, column  = 10, row  = 1,  sticky   = "e")
			tkgrid(arrow4, column    = 12, row  = 1,  sticky   = "ne")			
			tkgrid(palette, column  = 14, row  = 1,  sticky   = "e")
			tkgrid(arrow5, column    = 16, row   = 1, sticky   = "ne")
			tkgrid(Map,    column    = 17,row   = 1,  sticky   = "e")


            tkpack(bar2, fill="both", expand = 1)
         }

         Info <- function() {
               x.put("x.MsgFrame", tkframe(win, relief=relief, borderwidth=borderwidth, background=bg) )
               x.MsgFrame = x.get("x.MsgFrame")

               x.put("scr",tkscrollbar(x.MsgFrame, repeatinterval = 10, command=function(...)tkyview(x.MsgFrame,...)) )
               scr = x.get("scr")

               x.put("x.Msg", tktext(x.MsgFrame,bg=bg, fg = fg,font= font, borderwidth=borderwidth,yscrollcommand=function(...)tkset(scr,...)))
               x.Msg = x.get("x.Msg")

               tkgrid(x.Msg,scr,column = 0, row = 0, sticky="ns")

               tkpack(x.MsgFrame, fill="both", expand = 1)
            }
		 
         bar3 <- function() {
            bar3   <- tkframe(win, relief=relief, borderwidth=borderwidth, background=bg)

            OffGui    <-  tkbutton(bar3, image= x.img("switchOffBlue") ,command = function() x.close())
			OffAll    <-  tkbutton(bar3, image= x.img("switchOffRed") ,command = function() x.close(quitR = TRUE) )

		   tk2tip(OffGui, "CLOSE RANGEMAPPER GUI! \n To re-open rangeMapper type 'rangeMapper()' at the R prompter")
		   tk2tip(OffAll, "Quit rangeMapper AND R!")

		   tkgrid(OffGui,column  = 0, row= 0,   sticky= "w")
		   tkgrid(OffAll,column  = 1, row= 0,   sticky= "w")

           tkpack(bar3 , fill="both", expand = 1)
            }

            top()
            bar1()
            hline()
            bar2()
			hline()           
			Info()
			hline()
            bar3()

            tkfocus(win)


         }

	
