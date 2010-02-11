
processRanges <- function(Files, con) {


	cnv = canvas.fetch(con)
	 if(nrow(cnv) == 0) stop(gui.msg("The canvas is empty!"))



	 # TODO: check on update
	Startprocess = Sys.time()
	
	for( i in 1:length(Files$layer) ) {
			
	r = try(readOGR(Files$dsn[i], Files$layer[i], verbose = FALSE), silent = TRUE)
	
	if(!class(r) == "try-error") {
			 
			 
				overlayRes = overlay(r, cnv) 
				overlayRes = which(!is.na(overlayRes[, 1]))
				
				if(length(overlayRes) >0) { # do grid interpolation
					sp = cnv[overlayRes, ]
					
					o = data.frame(id = sp$id, bioid = rep(Files$layer[i], nrow(sp@coords)) ) 
					
					} else { # the polygon is smaller than a grid cell: snap to the nearest point
						ctr = apply(coordinates(r),2, mean)
						nn = spDistsN1(cnv, ctr)
						sp = cnv[which(nn == min(nn) ), ]
						o = data.frame(id = sp$id, bioid = rep(Files$layer[i], nrow(sp@coords)) )
					} 
				names(o) = c("id", "bioid")

			# save  to db
			dbWriteTable(con, "ranges", o, append = TRUE, row.names = FALSE) 
			
			# progress bar	
			gui.msg( paste("Processsing ranges, please wait!...", 
					   paste("Range:", Files$layer[i]),	
						 paste(round(i/length(Files$layer)*100,2), "% done"), 
						   paste("Elapsed time:",round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), sep = "\n") )
			
			} else gui.msg(r)
	}		
	
	# last msg
	gui.msg(paste(nrow(Files), "ranges updated to database; Elapsed time:", 
							round(difftime(Sys.time(), Startprocess, units = "mins"),1), "mins"), keep = TRUE )
	

	
}

 
 


























