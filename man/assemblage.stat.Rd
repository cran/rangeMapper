\name{assemblage.stat}
\alias{assemblage.stat}
\title{The statistic to be applied at each grid cell}
\description{
A collection of wrappers around existing functions.
}

\arguments{
	  \item{formula}{A two-sided formula, see \code{\link{rangeMap.save}} for details}
	  \item{FUN}{The model used to compute the statistics of interest. See details}
	  \item{data}{A \code{\link{data.frame}} containing the dependent variable and posibly other variables used in the modelling process, see \code{\link{rangeMap.save}} for details}
	}

\usage{
assemblage.stat(formula, FUN, data)
}


\details{The \code{assemblage.stat} function is called by the \code{\link{rangeMap.save}} and \code{FUN} is applied at each pixel.
\code{FUN} can be:

	 \itemize{
		\item any function which can be applied on a numeric vector in which case the formula is of form y ~ 1. 
		\item \code{lmer} in (package \code{lme4}) model of form y ~ 1 + (1|random_factor1) + (1|random_factor2) + ...
	}		


}


\seealso{
	\code{\link{rangeMapper}} for a complete example.
	\code{\link{rangeMap.save}}.
	}

\author{Mihai Valcu \email{valcu@orn.mpg.de}}


 






















