#' @title GAM based predictions
#' 
#' @description Use GAMs (mgcv) package for predictions.
#' 
#' @param data data used for model fitting, in o form as stored in 
#' the dataLongPred slot of an incAnalysis object
#' @param frm Formula used for mgcv::gam
#' @param family distribution
#' @param type type parameter used for the predict function
#' @param text free-text variable
#' 
#' @import mgcv
#' @export
gamMlCalc <- function(data, 
		    frm=as.formula(Y~offset(log(N))),
		    text="",
		    family="poisson",
		    type="link"
		    ) {
    fit <- gam(frm, data=data, family=family)

    res <- data.frame(PRD=predict(fit, newdata=data, type=type))
    res$SE <- predict(fit, newdata=data, se.fit=T,type=type)$se.fit

    return(list(result=res, type="GAM_ML", text=text, fit=fit))
}

#' @title Run GAM analysis 
#'
#' @description Models data using GAMs. 
#' 
#' @param obj incClass object
#' @param frm model formula
#' @param family exponential family to use (e.g. poisson)
#' @param text freely usable text variable
#' 
#' @export
#' @return Update incAnalysis object.
runMlGAM <- function(obj,
		   frm=as.formula(Y~offset(log(N))),
		   family="poisson",
		   text="") {
    res <- gamMlCalc(obj@dataLongPred,
		   frm, 
		   family,
		   text=text)

    obj@results[[length(obj@results)+1]] <- res
    return(obj)
}


#' @title Run GAM analysis 
#'
#' @description Run GAM analysis on incClass instance
#' 
#' @param method Can be inla for INLA fitted models,
#' any other value for classical approaches
#' @param ... additional parameters 
#'
#' @description Models data using GAMs. 
#' @export 
runGAM <- function(method="inla", ...) {
    if (method=="inla") {
	runInla(class="GAM_INLA", ...)
    } else {
	runMlGAM(...)
    }
}


#' @title Run GAM analysis 
#' 
#' @description Models data using GAMs. 
#'
#' @param ... additional parameters
#'
#' @export 
gamCalc <- function(...) {
    if (method=="inla") {
	inlaCalc(class="GAM_INLA", ...)
    } else {
	gamMlCalc(...)
    }
}
