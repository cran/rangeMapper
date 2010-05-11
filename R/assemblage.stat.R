
assemblage.stat <- function(formula, FUN, data) {
		
	if (class(formula) != "formula" || length(formula) != 3)
	stop("formula must be a two-sided formula object")
		
	res = NA
	
	# y~ 1 
	if(length(formula[[3L]]) == 1 && as.character(formula[[3L]]) ==1) 
		res = FUN(na.omit(data[, as.character(formula[[2L]] )]))
	
	# lmer mean
	if(substitute(FUN) == "lmer")  {
		require(lme4)

		fm = try(lmer(formula, data = data), silent = TRUE)
		
		if(class(fm) == "try-error") {
			warning("lmer failed, lm will be used instead!")
			fm = try(lm(eval(parse(text = paste(formula[[2L]], "~1") ) ) , data = data), silent = TRUE)
				if(!class(fm) == "try-error") 
				res = as.numeric(coef(fm)[1])
			} else
		
		res = as.numeric(fixef(fm)[1] )
		}
	
	res

}


