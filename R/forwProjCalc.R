#' @title Perform forward projection
#' 
#' @description Forward projection of last observations
#' 
#' @param cases case counts (e.g. incidence) as stored in the 
#' casesWidePred slot of an incAnalysis object (period: rows; age: columns)
#' @param n Row index of cases to fill prediction years with
#' @param method which method to use, parameters used for DescTools::BinomCI,
#' defaults to "jeffreys": "wald", "wilson", "agresti-coull", "jeffreys", 
#' "modified wilson", "modified jeffreys", "clopper-pearson", "arcsine", 
#' "logit", "witting" or "pratt"
#' @param text Freely usable text variable
#' @param constRates calculate constant rates, use last observation of cases 
#' otherwise
#' @param population population counts in wide format (rows: years, cols: age),
#' same structure as for cases
#' @param roundFun function to apply to predictet cases (e.g. identity,
#' floor, ceiling)
#'
#' @export 
forwProj <- function(cases, n, method, text, constRates, population, roundFun=identity) {
    if (!constRates) {
	# forward projection of last observations
	val <- cases[(n),]
	for (i in (n+1):length(cases[,1])) {
	    cases[i,] <- val
	} 
    } else {
	# forward projection of constant rates
	val <- cases[(n),]/population[(n),]
	for (i in (n+1):length(cases[,1])) {
	    cases[i,] <- sapply(val*population[i,], roundFun)
	} 
    }


    return(list(cases=cases, type="FWDPROJ", method=method, 
		text=paste(method, "|CR=",
			   ifelse(constRates, "T", "F"),",", 
			   as.character(substitute(roundFun)), ";", text, sep="")))
}

#' @title Run forward projection
#' 
#' @description Applies the forward projection of last observations
#' on the incAnalysis object
#'
#' @param obj incClass instance
#' @param method which method to use, parameters used for DescTools::BinomCI,
#' defaults to "jeffreys": "wald", "wilson", "agresti-coull", "jeffreys", 
#' "modified wilson", "modified jeffreys", "clopper-pearson", "arcsine", 
#' "logit", "witting" or "pratt"
#' @param text Freely usable additional textfield
#' @param constRates if true, calculate constant rates, project last observations
#' forward, otherwise
#' @param roundFun if constRates=TRUE, how to process predicted counts. Defaults 
#' to identity, can be e.g. floor or ceiling.
#' 
#' @export
#' @return Updated incAnalysis object
runFwProj <- function(obj, method="jeffreys", text="", constRates=T, roundFun=identity) {
    res <- forwProj(obj@casesWidePred, obj@nFit, method=method, text=text, 
		    constRates=constRates, population=obj@populationWide)

    obj@results[[length(obj@results)+1]] <- res

    return (obj)
}
