
# OLS coefficients
lmSlope <- function (formula, data,...) {
	x = try(as.numeric(coef(lm(formula, data,...))[2]), silent = TRUE)
	if(inherits(x, "try-error")) x = NA
	return(x)
}	

# Mixed effect model coefficients
lmer_slope <- function (formula, data) {
	if (!require(lme4)) 
		stop("Package lme4 is not available")
	fm = try(lmer(formula, data = data), silent = TRUE)
	if (inherits(fm, "try-error")) 
		res = NA
			else res = fixef(fm)[2]
	as.numeric(res)
	}

lmer_mean <- function(formula, data, lm.on.fail = FALSE, family = binomial) {

	if(!require(lme4)) stop ("Package lme4 is not available")

	fm = try(lmer(formula, data = data, family = family), silent = TRUE)
	
	if( inherits(fm, "try-error") ) res = NA else res = fixef(fm)[1]
	
	if( inherits(fm, "try-error") & lm.on.fail) {
		fm = glm(eval(parse(text = paste(formula[[2L]], "~1") ) ) , data = data, family = family)
		res = coef(fm)[1]
		} 
	binomial()$linkinv (as.numeric(res))
}	

		
# PGLM coefficients
phyloEst <- function(formula, data, ID, vcv) {
	require(CAIC)

	data = data[data[, ID]%in% row.names(vcv), ]

	vcv = vcv[row.names(vcv)%in%data[, ID], colnames(vcv)%in%data[, ID]] 

	data = data[order(data[, ID]), ]
	rownames(data) = data[, ID]

	fm = pglmEstLambda(formula = formula, phylomat = vcv, data = data)

	as.numeric(coef(fm))
}

phyloMean <- function(...) {
	res = try(phyloEst(...), silent = TRUE)
	if (inherits(res, "try-error") ) NA else res[1]
	}

phyloSlope <- function(...) {
	res = try(phyloEst(...), silent = TRUE)
	if (inherits(res, "try-error") ) NA else res[2]
	}

	
# Robust reggresion coefficients
rlm_slope <- function (formula, data,...) {
	require(MASS)
	x = try(as.numeric(rlm(formula, data,...)$coefficients[2]), silent = TRUE)
	if(inherits(x, "try-error")) x = NA
	return(x)
}

rlm_pval <- function (formula, data,...) {
	require(MASS)
	x = try(as.numeric(summary(rlm(formula, data,...))$coefficients[2, 3]), silent = TRUE)
	if(inherits(x, "try-error")) x = NA
	return(	pt( abs(x) , lower.tail = FALSE   , df =Inf)*2 )
	
}


	
	

	
	

































