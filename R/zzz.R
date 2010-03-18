
.First.lib     <- function(lib, pkg) {
	require(utils)
	v = packageDescription("rangeMapper")$Version
	sep = paste("\n",paste(rep("-", 61),collapse = ""),"\n", collapse = "")
	cat(paste(sep, "This is rangeMapper", v,
				   "\n   Type", sQuote("rangeMapper()"), "to start the graphical user interface.",
				   "\n   Type", sQuote("?rangeMapper"), "to access the help files.",
				   
			 sep))
	
	
	
	}
















