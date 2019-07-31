#' @title INLA based GLM fitting
#' 
#' @description Fits GLMs using the INLA framework
#'
#' @param data data in long format as stored in the dataLongPred 
#' slot of an incAnalysis object
#' @param frm Model formula.
#' @param family distribution
#' @param control.family inla parameter 
#' @param control.predictor inla parameter
#' @param control.compute inla parameter
#' @param text Free text variable
#' @param class specify the type of caller, e.g. GLM für runGLM,
#' GAM for runGAM()
#' @param ... additional parameters
#'
#' @import INLA
#' @export
#'
#' @return list with results
inlaCalc <- function(data,
		     frm=as.formula(Y~offset(log(N))), 
		     family="poisson", 
		     control.family=list(link="log"),
		     control.predictor=list(link=1, compute=T),
		     control.compute=list(dic=T, cpo=T, waic=T),
		     text="",
		     class="INLA") {
    res <- inla(frm, 
		family=family, 
		data=data,
		control.family=control.family,
		control.predictor=control.predictor,
		control.compute=control.compute)

    return(list(result=res, type=class, text=text))
}

#' @title Run INLA analysis
#'
#' @description Add INLA based model analysis to an incAnalysis object 
#'
#' @param obj incClass instance
#' @param frm Model formula.
#' @param family Distribution
#' @param control.family inla parameter 
#' @param control.predictor inla parameter
#' @param control.compute inla parameter
#' @param text Free text variable
#' @param class Additional specification of the model class,
#' is set to GLM when runINLA is called from from runGLM and 
#' GAM is called from runGAM. Defaults to INLA.
#' @param ... additional parameters
#'
#' @export
#' @return Updated incAnalysis object
runInla <- function(obj,
		     frm=as.formula(Y~offset(log(N))), 
		     family="poisson", 
		     control.family=list(link="log"),
		     control.predictor=list(link=1, compute=T),
		     control.compute=list(dic=T, cpo=T, waic=T),
		     text="",
		     class="INLA") {
    res <- inlaCalc(obj@dataLongPred, 
		    frm,
		    family,
		    control.family, 
		    control.predictor,
		    control.compute,
		    text=text,
		    class=class)
    obj@results[[length(obj@results)+1]] <- res

    return (obj)
}
