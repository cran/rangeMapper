
.First.lib     <- function(lib, pkg) {
	require(utils)
	v = packageDescription("rangeMapper")$Version
	sep = paste("\n",paste(rep("-", 58),collapse = ""),"\n", collapse = "")
	cat(paste(sep, "This is rangeMapper", v,"\nType", sQuote("rangeMapper()"), "to start the graphical user interface.", sep))
	}
















