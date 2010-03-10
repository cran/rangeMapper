

rangeMapper <- function() {

   require(tcltk); require(tcltk2); tclRequire("BWidget"); require(RColorBrewer); require(pixmap)

   font = "helvetica 10"; fg = "#08306B" ; bg = "#F7FBFF"; relief="flat"; borderwidth= 0

   if( !is.null (gui.get.from.env ("win")) ) stop (tkmessageBox(message = "rangeMapper is allready open!", icon = "error", type = "ok") )

      if(!exists(".RangeMapper")) gui.make.env()

         gui.put.to.env("win", tktoplevel() )

         win = gui.get.from.env("win")

         # window manager
         tkwm.title(win,paste("rangeMapper", packageDescription("rangeMapper")$Version))
         tkwm.resizable(win, 0, 0)
         tcl("wm", "protocol", win, "WM_DELETE_WINDOW", quote(gui.msg("Please close the window using the lower bar button!")))


         top  <- function() {
            gui.put.to.env("topMenu",tkmenu(win) )
            topMenu  <- gui.get.from.env("topMenu")
            tkconfigure(win, menu=topMenu)
            FileMenu <- tkmenu(topMenu, tearoff=FALSE)
            ToolsMenu <- tkmenu(topMenu, tearoff=FALSE)

            tkadd(FileMenu,"command",label="Get started",command=function() gui.help(what = "man", exec = TRUE) )
            tkadd(FileMenu,"command",label="Example files",command=function() gui.help(what = "support.files") )
            tkadd(FileMenu,"command",label="About",command=function() gui.help(what = "citation") )

            tkadd(ToolsMenu,"command",label="Browse the active project",command=function() gui.tkdbBrowse.active.proj() )

            tkadd(topMenu, "cascade", label="Help",menu=FileMenu)
            tkadd(topMenu, "cascade", label="Tools",menu=ToolsMenu)

         }

         logo <- function() {
            Logo  <- tkframe(win, relief=relief, borderwidth=borderwidth, background=bg)

            tkgrid(tklabel(Logo, image=gui.img("logo"), background= bg), columnspan =1, rowspan = 1, sticky= "e")
            tkpack(Logo, fill = "both", expand = 1)
         }

         bar1 <- function() {
            bar1    <- tkframe(win, relief=relief, borderwidth=borderwidth, background=bg)


            # ARROWS
            arrow1    =  tklabel(bar1, image=gui.img("arrow"),foreground = fg, background = bg )
            arrow2    =  tklabel(bar1, image=gui.img("arrow"),foreground = fg, background = bg )
            arrow3    =  tklabel(bar1, image=gui.img("arrow"),foreground = fg, background = bg )
            # BUTTONS
            Create    =   tkbutton(bar1,image    = gui.img("new") ,command          = function() gui.dbopen(new= TRUE)  )
            Open      =   tkbutton(bar1,image    = gui.img("open")   ,command       = function() gui.dbopen(new= FALSE)  )
            Bbox      =   tkbutton(bar1,image    = gui.img("bbox") , command        = function() gui.global.bbox.save() )
            gridSize  =   tkbutton(bar1,image    = gui.img("resolution") , command  = function() gui.gridSize.save() )
            canvasUpload  =   tkbutton(bar1,image= gui.img("uploadCanvas") , command= function() gui.canvas.save() )
            Ranges     =   tkbutton(bar1,image= gui.img("intersectRange") , command  = function() gui.processRanges() )
            Bio        =   tkbutton(bar1,image= gui.img("uploadBio") ,command  = function() gui.bio.save()  )


            # TIPS
            tk2tip(Create, "START PROJECT \nA new sqlite database will be created.\nType ?db.ini for more info.")
            tk2tip(Open, "OPEN PROJECT\nConnect to an existing sqlite database")
            tk2tip(Bbox, "COMPUTE CANVAS EXTENT\nCompute the global bounding box of the project.\nType ?global.bbox for more info.")
            tk2tip(gridSize, "INPUT GRID SIZE\nThis is the cell size (i.e. the distance btween two neigbouring points) in map units. \n Type ?gridSize for more info.")
            tk2tip(canvasUpload, "COMPUTE CANVAS\nCompute canvas using the bounding box and the grid size.\nType ?canvas for more info.")
            tk2tip(Ranges, "PROCESS RANGES\nPerform polygons intersection with the canvas. \nType ?processRanges for more info.")
            tk2tip(Bio, "IMPORT 'BIO' data\nImport the table (.csv format, ';' separated) containing the variables to be mapped.\n You will need to choose the column coresponding to the names of the range files.\nType ?bio.save for more info.")


            # PLACE ON  GRID
            # LABELS
            tkgrid(tklabel(bar1,text = "Initiate project",font = font, foreground = fg, background = bg),sticky="ns",columnspan = 2, column  = 1, row = 0)
            tkgrid(tklabel(bar1,text = "Prepare canvas",  font = font, foreground = fg, background = bg),sticky="ns",columnspan = 3, column  = 4, row = 0)
            tkgrid(tklabel(bar1,text = "Upload ranges",   font = font, foreground = fg, background = bg),sticky="ns",columnspan = 1, column  = 8, row = 0)
            tkgrid(tklabel(bar1,text = "Upload Bio table",font = font, foreground = fg, background = bg),sticky="ns",columnspan = 1, column  = 10, row = 0)

            # BUTTONS
            tkgrid(Create,     column  = 1, row = 1, sticky= "e")
            tkgrid(Open,       column  = 2, row = 1, sticky= "w")
            tkgrid(arrow1,     column  = 3, row = 1, sticky= "n")
            tkgrid(Bbox,     column    = 4, row = 1, sticky= "w")
            tkgrid(gridSize, column    = 5, row = 1, sticky= "w")
            tkgrid(canvasUpload, column= 6, row = 1, sticky= "w")
            tkgrid(arrow2,     column  = 7, row = 1, sticky= "n")
            tkgrid(Ranges,     column  = 8, row = 1, sticky= "ns")
            tkgrid(arrow3,     column  = 9, row = 1, sticky= "n")
            tkgrid(Bio,        column  =10, row = 1, sticky= "ns")

            tkpack(bar1, fill="both", expand = 1)
         }
         hline <- function() {
            Hline  <- tkframe(win, relief=relief, borderwidth=borderwidth, background=bg)

            tkgrid(tklabel(Hline, image=gui.img("hline"), background= bg), columnspan =1, rowspan = 1, sticky= "nsew")
            tkpack(Hline, fill="both", expand = 1)
         }

         bar2 <- function() {
            bar2    <-tkframe(win, relief=relief, borderwidth=borderwidth, background=bg)

            # ARROWS
            arrow1  <- tklabel(bar2, image   =gui.img("arrow"),foreground       = fg, background  = bg  )
            arrow2  <- tklabel(bar2, image   =gui.img("arrow"),foreground       = fg, background  = bg  )
            arrow3  <- tklabel(bar2, image   =gui.img("arrow"),foreground       = fg, background  = bg  )
            dummy1  <- tklabel(bar2, image   =gui.img("dummy"),foreground       = fg, background  = bg  )
            dummy2  <- tklabel(bar2, image   =gui.img("dummy"),foreground       = fg, background  = bg  )

            # BUTTONS
            Fun       <-  tkbutton(bar2, image= gui.img("function")      ,command= function() gui.chooseFunction()  )
            Make      <-  tkbutton(bar2, image= gui.img("makeMap")       ,command= function() gui.rangeMap.save()  )
            Pallette  <-  tkbutton(bar2, image= gui.img("colorPalette")  ,command= function() gui.tkColorPalette() )
            Map       <-  tkbutton(bar2, image= gui.img("plotMap")       ,command= function() gui.rangeMap.fetch() )

            # TIPS
            tk2tip(Fun, "Choose or define the function to be applied at each pixel.")
            tk2tip(Make, "Make a map of a chosen variable.\nType ?rangeMap for more info.")
            tk2tip(Pallette, "Choose a color pallette.\nType ?tkColorPalette for more info.")
            tk2tip(Map, "Plot map on the default device.\nType ?rangeMap.fetch and ?spplot for more info.")


            # PLACE ON GRID

            # LABELS
            tkgrid(tklabel(bar2,text         = "Choose Function",    font       = font, foreground= fg, background= bg),sticky="w",columnspan= 2, column= 1, row = 0)
            tkgrid(tklabel(bar2,text         = "Make map",    font              = font, foreground= fg, background= bg),sticky="w",columnspan= 2, column= 4, row = 0)
            tkgrid(tklabel(bar2,text         = "Choose Palette", font           = font, foreground= fg, background= bg),sticky="w",columnspan= 2, column= 7, row = 0)
            tkgrid(tklabel(bar2,text         = "Plot map",    font              = font, foreground= fg, background= bg),sticky="w",columnspan= 2, column= 10, row= 0)

            # BUTTONS
            tkgrid(Fun, column               = 1, row  = 1, sticky       = "e")
            tkgrid(arrow1, column            = 3, row  = 1, sticky       = "n")
            tkgrid(Make, column              = 4, row  = 1, sticky       = "e")
            tkgrid(arrow2, column            = 6, row  = 1, sticky       = "n")
            tkgrid(Pallette, column          = 7, row  = 1, sticky       = "e")
            tkgrid(arrow3, column            = 9,row   = 1, sticky       = "n")
            tkgrid(Map,    column            = 10,row   = 1, sticky       = "e")



            tkpack(bar2, fill="both", expand = 1)
         }

         bar3 <- function() {
            bar3   <- tkframe(win, relief=relief, borderwidth=borderwidth, background=bg)

            OffGui    <-  tkbutton(bar3, image= gui.img("switchOffBlue") ,
            command = function () {
               rm("win", envir = .RangeMapper)

               if( gui.exists.in.env("con")){
                  try(sqliteCloseConnection(gui.get.from.env ("con")), silent = TRUE)
                  rm("con", envir = .RangeMapper)
               }
               if( gui.exists.in.env("session.msg")) rm("session.msg", envir = .RangeMapper)
                  tkdestroy(win)
               })


               OffAll    <-  tkbutton(bar3, image= gui.img("switchOffRed") ,command = function() q(save = "no") )

               tk2tip(OffGui, "CLOSE RANGEMAPPER GUI! \n To re-open rangeMapper type 'rangeMapper()' at the R prompter")
               tk2tip(OffAll, "Quit rangeMapper AND R!")


               tkgrid(OffGui,column  = 0, row= 0,   sticky= "w")
               tkgrid(OffAll,column  = 1, row= 0,   sticky= "w")


               tkpack(bar3 , fill="both", expand = 1)
            }

            Info <- function() {
               gui.put.to.env("msgFrame", tkframe(win, relief=relief, borderwidth=borderwidth, background=bg) )
               msgFrame = gui.get.from.env("msgFrame")

               gui.put.to.env("scr",tkscrollbar(msgFrame, repeatinterval = 1, command=function(...)tkyview(msgFrame,...)) )
               scr = gui.get.from.env("scr")

               gui.put.to.env("msg", tktext(msgFrame,bg=bg, fg = fg,font= font,yscrollcommand=function(...)tkset(scr,...)))
               msg = gui.get.from.env("msg")

               tkgrid(msg,scr,column = 1, row = 1, sticky="ns")

               tkpack(msgFrame, fill="both", expand = 1)
            }

            top()
            logo()
            bar1()
            hline()
            bar2()
            Info()
            bar3()

            tkfocus(win)


         }

	
	
	
	
	
	



