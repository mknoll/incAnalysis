#' @title Perform forward projection
#' 
#' @description Forward projection of last observations
#' 
#' @param cases case counts (e.g. incidence) as stored in the 
#' casesWidePred slot of an incAnalysis object (period: rows; age: columns)
#' @param n Row index of cases to fill prediction years with
#' @param ci Confidence Interva, can be W for Wilson or PC for Pearson Clopper
#' @param text Freely usable text variable
#'
#' @export 
forwProj <- function(cases, n, ci, text) {
    val <- cases[(n),]
    for (i in (n+1):length(cases[,1])) {
	cases[i,] <- val
    }

    return(list(cases=cases, type="FWDPROJ", ci=ci, text=paste(ci, "|", text, sep="")))
}

#' @title Run forward projection
#' 
#' @description Applies the forward projection of last observations
#' on the incAnalysis object
#' @param ci Confidence Interval. Can be "W" for Wilson or 
#'	PC for Pearson-Clopper.
#' @param text Freely usable additional textfield
#' 
#' @export
#' @return Updated incAnalysis object
runFwProj <- function(obj, ci="W", text="") {
    res <- forwProj(obj@casesWidePred, obj@nFit, ci=ci, text=text)

    obj@results[[length(obj@results)+1]] <- res

    return (obj)
}
