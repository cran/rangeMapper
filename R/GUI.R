
.X.tooltips <- function(tip) {

switch(tip,
# bar 1
Create      = "START PROJECT\n A new sqlite database will be created.\n Type ?db.ini for more info.",
Open        = "OPEN PROJECT\n Connect to an existing sqlite database",
Bbox        = "COMPUTE CANVAS EXTENT\n Compute the global bounding box of the project.\n Type ?global.bbox for more info.",
gridSize    = "INPUT GRID SIZE\n This is the cell size (i.e. the distance between two neighboring points) in map units. \n Type ?gridSize for more info.",
canvasUpload= "COMPUTE CANVAS\n Compute canvas using the bounding box and the grid size.\n Type ?canvas for more info.",
Ranges      = "PROCESS RANGES\n Perform polygons intersection with the canvas.\n Type ?processRanges for more info.",
Bio         = "IMPORT 'BIO' data\n Import the table (.csv format, ';' separated) containing the variables to be mapped.\n You will need to choose the column corresponding to the names of the range files.\n Type ?bio.save for more info.",   
# bar 2
Var         = "CHOOSE A VARIABLE to be mapped from an existing BIO table.\n 
				Skip this step if species richness is the variable of interest and proceed to 'Save map'.", 
Fun         = "CHOOSE OR DEFINE THE FUNCTION to be applied at each pixel.", 
Subset      = "CHOOSE ONE OR MORE SUBSETS using existing BIO, MAP or metadata_ranges table", 
saveMap     = "SAVE A MAP of a chosen variable.\n Type ?rangeMap.save for more info.", 
palette    = "CHOOSE A COLOR PALETTE.\n Type ?tkColorPalette for more info.", 
Map         = "DISPLAY A MAP on the default device.\n Type ?rangeMap.plot and ?spplot for more info.\n Type ?classInt::classIntervals for more info on class intervals construction", 
# bar 3
OffGui      = "CLOSE RANGEMAPPER GUI! \n To re-open rangeMapper type 'rangeMapper()' at the R prompter",
OffAll      = "Quit rangeMapper AND R!")

}

rangeMapper <- function() {

   tclRequire("BWidget"); tclRequire("Tktable")
   
   font = "helvetica 10"; fg = "#08306B" ; bg = "#F7FBFF"; relief = "flat"; borderwidth = 0

   if( !is.null ( .X.get("win")) ) stop (tkmessageBox(message = "rangeMapper is allready open!", icon = "error", type = "ok") )

      if(!exists(".RangeMapper")) .X.make.env()

         .X.put("win", tktoplevel() )

         win = .X.get("win")

         # window manager
		if(.Platform$OS.type == "windows") 
		 tkwm.iconbitmap(win, system.file("ico", "favicon.ico", package="rangeMapper"))
         tkwm.title(win,paste("rangeMapper", packageDescription("rangeMapper")$Version))
         tkwm.resizable(win, 0, 0)
         tcl("wm", "protocol", win, "WM_DELETE_WINDOW", quote(.X.Msg("Please close the window using the lower bar button!")))


         top  <- function() {
            .X.put("topMenu",tkmenu(win) )
            topMenu  <- .X.get("topMenu")
            tkconfigure(win, menu=topMenu)
            
			HelpMenu <- tkmenu(topMenu, tearoff=FALSE)
            ProjectMenu <- tkmenu(topMenu, tearoff=FALSE)
            MapMenu <- tkmenu(topMenu, tearoff=FALSE)
            metadata_ranges2bioMenu <- tkmenu(topMenu, tearoff=FALSE)

			tkadd(ProjectMenu,"command",label="Browse the active project", command = function() .X.tkdbBrowse.active.proj() )
			tkadd(ProjectMenu,"command",label="Remove MAP tables", command = function() .X.rangeMap.rm("MAP") )
			tkadd(ProjectMenu,"command",label="Remove BIO tables", command = function() .X.rangeMap.rm("BIO") )
          	tkadd(ProjectMenu,"command",label="show project's metadata", command = function() .X.show.metadata() )
  
			tkadd(MapMenu,"command",label="import external MAP", command = function() .X.mapImport()  )
			tkadd(MapMenu,"command",label="export MAPs to geotiff", command = function() .X.rangeMap.export()  )
			
			tkadd(metadata_ranges2bioMenu,"command", label= "convert metadata_ranges to BIO table", command = function() .X.metadata2bio()  )
			tkadd(metadata_ranges2bioMenu,"command", label= "merge all BIO tables", command = function() .X.bio.merge()  )
			
			tkadd(HelpMenu,"command",label= "Get started",command = function() .X.help("man") )
			tkadd(HelpMenu,"command",label= "Example files",command = function() .X.help("support.files") )
			tkadd(HelpMenu,"command",label= "About",command = function() .X.help("citation") )

			tkadd(topMenu, "cascade", label= "Project",menu=ProjectMenu)
			tkadd(topMenu, "cascade", label= "Maps",menu=MapMenu)
			tkadd(topMenu, "cascade", label= "Tables", menu = metadata_ranges2bioMenu)
			tkadd(topMenu, "cascade", label= "Help",menu=HelpMenu)

         }

         bar1 <- function() {
            bar1    <- tkframe(win, relief=relief, borderwidth=borderwidth, background=bg)


            # ARROWS
            arrow1    =  tklabel(bar1, image=.X.img("arrow"),foreground = fg, background = bg )
            arrow2    =  tklabel(bar1, image=.X.img("arrow"),foreground = fg, background = bg )
            arrow3    =  tklabel(bar1, image=.X.img("arrow"),foreground = fg, background = bg )
            # BUTTONS
			Create    =   tkbutton(bar1,image    = .X.img("new") ,command           = function() .X.dbopen(new= TRUE)  )
			Open      =   tkbutton(bar1,image    = .X.img("open")   ,command        = function() .X.dbopen(new= FALSE)  )
			Bbox      =   tkbutton(bar1,image    = .X.img("bbox") , command         = function() .X.global.bbox.save() )
			gridSize  =   tkbutton(bar1,image    = .X.img("resolution") , command   = function() .X.gridSize.save() )
			canvasUpload  =   tkbutton(bar1,image= .X.img("uploadCanvas") , command = function() .X.canvas.save() )
            Ranges     =   tkbutton(bar1,image= .X.img("intersectRange") , command  = function() .X.processRanges() )
			Bio        =   tkbutton(bar1,image= .X.img("uploadBio") ,command        = function() .X.bio.save()  )
			
			
            # TIPS
            tk2tip(Create, .X.tooltips("Create") )
            tk2tip(Open, .X.tooltips("Open") )
            tk2tip(Bbox, .X.tooltips("Bbox"))
            tk2tip(gridSize, .X.tooltips("gridSize") )
            tk2tip(canvasUpload, .X.tooltips("canvasUpload") )
            tk2tip(Ranges, .X.tooltips("Ranges"))
            tk2tip(Bio, .X.tooltips("Bio"))


            # PLACE ON  GRID
            # LABELS
            tkgrid(tklabel(bar1,text = "Initiate project",font = font, foreground = fg, background = bg),sticky="ns",columnspan = 2, column  = 1, row = 0)
            tkgrid(tklabel(bar1,text = "Prepare canvas",  font = font, foreground = fg, background = bg),sticky="ns",columnspan = 3, column  = 4, row = 0)
            tkgrid(tklabel(bar1,text = "Upload ranges",   font = font, foreground = fg, background = bg),sticky="ns",columnspan = 1, column  = 8, row = 0)
            tkgrid(tklabel(bar1,text = "Upload Bio table",font = font, foreground = fg, background = bg),sticky="ns",columnspan = 1, column  = 10, row = 0)
			
			tkgrid(tklabel(bar1, image=.X.img("wren"), background= bg), sticky="ns",columnspan = 1, column  = 11, row = 0)
            
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

            tkgrid(tklabel(Hline, image=.X.img("hline"), background= bg), columnspan =1, rowspan = 1, sticky= "nsew")
            tkpack(Hline, fill="both", expand = 1)
         }

         bar2 <- function() {
            bar2    <-tkframe(win, relief=relief, borderwidth=borderwidth, background=bg)

            # ARROWS
            arrow1  <- tklabel(bar2, image   = .X.img("arrow"),foreground       = fg, background  = bg  )
            arrow2  <- tklabel(bar2, image   = .X.img("arrow"),foreground       = fg, background  = bg  )
            arrow3  <- tklabel(bar2, image   = .X.img("arrow"),foreground       = fg, background  = bg  )
            arrow4  <- tklabel(bar2, image   = .X.img("arrow"),foreground       = fg, background  = bg  )
            arrow5  <- tklabel(bar2, image   = .X.img("arrow"),foreground       = fg, background  = bg  )
           # BUTTONS
            Var       <-  tkbutton(bar2, image = .X.img("variable")      ,command= function() .X.chooseVariable()   )
            Fun       <-  tkbutton(bar2, image = .X.img("function")      ,command= function() .X.chooseFunction()  )
            Subset    <-  tkbutton(bar2, image = .X.img("subsetMap")     ,command= function() .X.chooseSubset()  )
            saveMap   <-  tkbutton(bar2, image = .X.img("saveMap")       ,command= function() .X.rangeMap.save() )
			palette  <-  tkbutton(bar2, image = .X.img("colorPalette")  ,command= function() .X.tkColorPalette() )
            Map       <-  tkbutton(bar2, image = .X.img("plotMap")       ,command= function() .X.rangeMap.plot() )
			
            # TIPS
            tk2tip(Var, .X.tooltips("Var"))
            tk2tip(Fun, .X.tooltips("Fun"))
            tk2tip(Subset, .X.tooltips("Subset"))
            tk2tip(saveMap, .X.tooltips("saveMap"))
			tk2tip(palette, .X.tooltips("palette"))
            tk2tip(Map, .X.tooltips("Map"))

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
               .X.put(".X.MsgFrame", tkframe(win, relief=relief, borderwidth=borderwidth, background=bg) )
               .X.MsgFrame = .X.get(".X.MsgFrame")

               .X.put("scr",tkscrollbar(.X.MsgFrame, repeatinterval = 10, command=function(...)tkyview(.X.MsgFrame,...)) )
               scr = .X.get("scr")

               .X.put(".X.Msg", tktext(.X.MsgFrame,bg=bg, fg = fg,font= font, borderwidth=borderwidth,yscrollcommand=function(...)tkset(scr,...)))
               .X.Msg = .X.get(".X.Msg")

               tkgrid(.X.Msg,scr,column = 0, row = 0, sticky="ns")

               tkpack(.X.MsgFrame, fill="both", expand = 1)
            }
		 
         bar3 <- function() {
            bar3   <- tkframe(win, relief=relief, borderwidth=borderwidth, background=bg)

            OffGui    <-  tkbutton(bar3, image= .X.img("switchOffBlue") ,command = function() .X.close())
			OffAll    <-  tkbutton(bar3, image= .X.img("switchOffRed") ,command = function() .X.close(quitR = TRUE) )

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

	
