#' @title Run GLM analysis on incClass instance.
#'
#' @description Wrapper for runINLA() or runMlGLM()
#' 
#' @param method inla if INLA should be used, any other value 
#' for the classical approach
#' @param ... additional parameters
#'
#' @export
#' @return Updated incAnalysis object
runGLM <- function(method="inla", ...) {
    if (method=="inla") {
	runInla(class="GLM_INLA", ...)
    } else {
	runMlGLM(...)
    }
}


#' @title INLA based GLM calculation
#' 
#' @description Wrapper for inlaCalc() or mlCalc(),
#'
#' @param method inla for INLA or any other for classical approaches
#' @param ... additional parameters
#' 
#' @export
#' @return list with results
glmCalc <- function(method="inla", ...) { 
    if (method == "inla") {
	inlaCalc(class="GLM_INLA", ...)
    } else {
	mlCalc(...)
    }
}


#' @title ML based GLM fitting
#' 
#' @description Fits GLMs using an ML approach
#'
#' @param data data in long format as stored in the dataLongPred 
#' slot of an incAnalysis object
#' @param frm Model formula.
#' @param family Distribution 
#' @param text Free text variable
#' @param type type parameter of the predict function
#'
#' @import MASS
#' @import stats
#'
#' @export
#'
#' @return list with results
mlCalc <- function(data,
		     frm=as.formula(Y~offset(log(N))), 
		     family="poisson", 
		     text="",
		     type="link") {
 
    if (family == "poisson") {
	fit <- glm(frm, data=data, family=family)
    } else if (family == "nbinomial" || family == "nb") {
	fit <- glm.nb(frm, data=data)
    }

    res <- data.frame(PRD=predict(fit, newdata=data, type=type))
    res$SE <- predict(fit, newdata=data, se.fit=T,type=type)$se.fit

    return(list(result=res, type="GLM_ML", text=text, fit=fit))
}


#' @title Run ML GLM analysis
#'
#' @description Add ML GLM based model analysis to an incAnalysis object
#'
#' @param obj incClass object
#' @param frm Model formula.
#' @param family distibution
#' @param text Free text variable
#' @param type type used for the predict function, e.g. link
#'
#' @export
#' @return Updated incAnalysis object
runMlGLM <- function(obj,
		     frm=as.formula(Y~offset(log(N))), 
		     family="poisson", 
		     text="",
		     type="link") {
    res <- mlCalc(obj@dataLongPred, 
		    frm,
		    family,
		    text=text,
		    type=type)
    obj@results[[length(obj@results)+1]] <- res

    return (obj)
}

