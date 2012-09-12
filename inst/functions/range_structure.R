
# Area perimeter ratio
APratio = function(x) {
# returns 1 for a circle polygon
	# area
	A = sapply(slot(x, "polygons"), 
		function(x) slot(x, "area") )
	
	# perimeter
	a = lapply(slot(x, "polygons"),
		function(x) slot(x, "Polygons") )
	b = do.call(c, a)
	a1 = lapply(b, function(x) 
		Line(coordinates(x)))
	L = sapply(a1, LineLength)
	
	# A-P ratio
	2*(sqrt(pi*sum(A)))/sum(L)
	
}