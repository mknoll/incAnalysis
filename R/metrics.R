#' @title Calculates metrics for incClass instances.
#' 
#' @description Calculates and aggregates bias, coverage
#' and precision. 
#' Negative bias hints towards overestimation of the 
#' projection approach (nPred > nObs).
#' 
#' @param obj incClass instance
#' 
#' @import methods
#' @export
#'
#' @return data.frame containing metrics
metrics <- function(obj) {
    if (is.null(obj) || !.hasSlot(obj, "evalLong")) {
	warning("No data available!")
	return(NULL)
    }

    res <- list()
    for (dat in obj@evalLong) {
	if (obj@predLast == 1) {
	    dat <- dat[which(dat$PERIOD == obj@pred),]
	} 
	for (period in levels(factor(dat$PERIOD))) {
	    data <- dat[which(dat$PERIOD == period),]
	    ##TODO: NA treatment?
	    w <- which(!(is.na(data$Y) | is.na(data$PRED)))
	    ## pred and observed = 0
	    nZero <- length(which(data$Y == 0 & data$PRED == 0))
	    bias <- (data$Y[w]-data$PRED[w])/data$Y[w]
	    if (length(which(is.infinite(bias))) > 0) {
		bias <- mean(c(bias[which(!is.infinite(bias))], rep(0, nZero)), na.rm=T) ### TODO
	    } else {
		bias <- mean(c(bias, rep(0, nZero)), na.rm=T) ### TODO
	    }
	    res[[length(res)+1]] <- data.frame(SD=mean(data$SD[w], na.rm=T),
					       CVG=1-sum(data$OUTSIDE[w], na.rm=T)/length(data[w,1]),
					       BIAS=bias,
					       N_NA_PRED=length(which(is.na(data$PRED))),
					       N_NA_Y=length(which(is.na(data$Y))),
					       TYPE=data$TYPE[1],
					       TEXT=data$TEXT[1],
					       PERIOD=period)
	}
    }

    return(data.frame(do.call(rbind, res)))
}
