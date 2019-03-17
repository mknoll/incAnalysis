#' @title GAM based predictions
#' 
#' @description Use GAMs (mgcv) package for predictions.
#' 
#' @param data data used for model fitting, in o form as stored in 
#' the dataLongPred slot of an incAnalysis object
#' @param frm Formula used for mgcv::gam
#' @param family distribution
#' @param type type parameter used for the predict function
#' 
#' @import mgcv
#' @export
gamCalc <- function(data, 
		    frm=as.formula(Y~offset(log(N))),
		    text="",
		    family="poisson",
		    type="link"
		    ) {
    fit <- gam(frm, data=data, family=family)

    res <- data.frame(PRD=predict(fit, newdata=data, type=type))
    res$SE <- predict(fit, newdata=data, se.fit=T,type=type)$se.fit

    return(list(result=res, type="GAM", text=text, fit=fit))
}

#' @title Run GAM analysis 
#'
#' @description Models data using GAMs. 
#' 
#' @param frm Model formula. Population (offset) N, observed data Y,
#' age AGE and period (year) PERIOD
#' @param family exponential family to use (e.g. poisson)
#' @param text freely usable text variable
#' 
#' @export
#' @return Update incAnalysis object.
runGAM <- function(obj,
		   frm=as.formula(Y~offset(log(N))),
		   family="poisson",
		   text="") {
    res <- gamCalc(obj@dataLongPred,
		   frm, 
		   family,
		   text=text)

    obj@results[[length(obj@results)+1]] <- res
    return(obj)
}
