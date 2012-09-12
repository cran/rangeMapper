
.onLoad <- function(lib, pkg) {
	wd = 73
	markline = paste("\n",paste(rep("-", wd),collapse = ""),"\n", collapse = "")
	
	 dsc <- packageDescription(pkg)
	
	chunk1 = paste("This is", pkg, dsc$Version,
	   "\n   Type", sQuote("rangeMapper()"), "to start the graphical user interface.",
	   "\n   Type", sQuote("?rangeMapper"), "to access the help files.")
	
	Citation = 	paste(
				strwrap("Valcu, M., et al (2012). rangeMapper: a platform for the study of macroecology of life-history traits. Global Ecology and Biogeography 21, 945-951.", wd), collapse = "\n")
	
	chunk2 = paste("The latest version of", dQuote("AppendixS2_S5.R"), "accompanying\n", dQuote(Citation), "can be found at\n", strsplit(dsc$URL, ",")[[1]][[2]] )

		
	packageStartupMessage(markline,chunk1 ,markline,  chunk2, markline)
	
}