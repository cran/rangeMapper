
.onLoad <- function(lib, pkg) {
	require(methods)
	require(utils)
	sep = paste("\n",paste(rep("-", 61),collapse = ""),"\n", collapse = "")
	cat(paste(sep, "This is", pkg, packageDescription("rangeMapper")$Version,
				   "\n   Type", sQuote("rangeMapper()"), "to start the graphical user interface.",
				   "\n   Type", sQuote("?rangeMapper"), "to access the help files.",
				   
			 sep, fill=TRUE))
	
	
	
	}