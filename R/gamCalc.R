#' @title GAM: classical approach for projections.
#' 
#' @description Use mgcv::gam() models for projections.
#' 
#' @param data data used for model fitting, in the form as stored in 
#' the dataLongPred slot of an incClass object
#' @param frm formula used for mgcv::gam()
#' @param family distribution, family parameter
#' @param type type parameter used in the predict function
#' @param text free-text variable
#' 
#' @import mgcv
#' @import stats
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

#' @title Run ML GAM analysis 
#'
#' @description Models data using GAMs, ML approach. 
#' 
#' @param obj incClass object
#' @param frm model formula
#' @param family distribution 
#' @param text freely usable text variable
#' 
#' @export
#' @return Updated incClass object.
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
#' @description Run GAM analysis on incClass instance,
#' wrapper for runInla() or runMlGAM()
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


#' @title Perform GAM analysis
#' 
#' @description Models data using GAMs,
#' independent of an incClass instancd
#'
#' @param method use INLA when set to "inla", classical
#' approach otherwise
#' @param ... additional parameters
#'
#' @export 
gamCalc <- function(method="inla", ...) {
    if (method=="inla") {
	inlaCalc(class="GAM_INLA", ...)
    } else {
	gamMlCalc(...)
    }
}
