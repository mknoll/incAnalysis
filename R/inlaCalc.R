#' @title INLA based GLM fitting
#' 
#' @description Fits GLMs using the INLA framework
#'
#' @param data data in long format as stored in the dataLongPred 
#' slot of an incAnalysis object
#' @param frm Model formula.
#' @param control.family inla parameter 
#' @param control.predictor inla parameter
#' @param control.compute inla parameter
#' @param text Free text variable
#' @import INLA
#'
#' @export
#'
#' @return list with results
inlaCalc <- function(data,
		     frm=as.formula(Y~offset(log(N))), 
		     family="poisson", 
		     control.family=list(link="log"),
		     control.predictor=list(link=1, compute=T),
		     control.compute=list(dic=T, cpo=T, waic=T),
		     text="") {
    
    res <- inla(frm, 
		family=family, 
		data=data,
		control.family=control.family,
		control.predictor=control.predictor,
		control.compute=control.compute)

    return(list(result=res, type="GLM", text=text))
}

#' @title INLA based GLM calculation
#' 
#' @description Wrapper for inlaCalc
#'
#' @export
#' @return list with results
glmCalc <- function(...) { 
    inlaCalc(...)
}


#' @title Run INLA analysis
#'
#' @description Add INLA based model analysis to an incAnalysi sobject
#'
#' @param data data in long format as stored in the dataLongPred 
#' slot of an incAnalysis object
#' @param frm Model formula.
#' @param control.family inla parameter 
#' @param control.predictor inla parameter
#' @param control.compute inla parameter
#' @param text Free text variable
#'
#' @export
#' @return Updated incAnalysis object
runInla <- function(obj,
		     frm=as.formula(Y~offset(log(N))), 
		     family="poisson", 
		     control.family=list(link="log"),
		     control.predictor=list(link=1, compute=T),
		     control.compute=list(dic=T, cpo=T, waic=T),
		     text="") {
    res <- inlaCalc(obj@dataLongPred, 
		    frm,
		    family,
		    control.family, 
		    control.predictor,
		    control.compute,
		    text=text)
    obj@results[[length(obj@results)+1]] <- res

    return (obj)
}

#' @title Run GLM analysis 
#'
#' @description Wrapper for runINLA
#'
#' @export
#' @return Updates incAnalysis object
runGLM <- function(...) {
    runInla(...)
}
