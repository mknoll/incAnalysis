#' @title Create PIT histograms
#' 
#' @description Creates PIT histograms for INLA 
#' fitted models
#'
#' @param obj incAnalysis object
#' @param dir output directory for plots
#' @param fnPre Filename prefix
#' 
#' @export
pitHist <- function(obj, dir=NULL, fnPre="") {
    ## entity filename
    dir <- ifelse(is.null(dir), tempdir(), dir)
    print(dir)
    eFn <- gsub('[^a-zA-Z]', '', obj@entity)
    eFn <- paste(eFn, "_", obj@nPred,"yr", sep="")

    fn <-paste(dir, "/",  eFn, "_", fnPre, ".png", sep="")
    nH <- ceiling(sqrt(length(obj@results)))
    if (nH == 0) { warning("Nothing to do!"); return() }
    png(fn, res=250, width=3000, height=3000)
    par(mfrow=c(nH, nH))
    for (res in obj@results) {

	val <- NULL
	eTx <- gsub('[^a-zA-Z]', '', res$text)

	if (res$type %in% c("INLA", "GLM", "GAM")) {
	    val <- res$result$cpo$pit
	    ## FIXME
	} else if (res$type == "BAPC") {
	    val <- res$bapc@inlares[[1]]$cpo$pit
	} else {
	    warning(paste("No PIT Histograms for ", res$type))
	}
	mn <-paste(res$type, eTx, obj@entity, collapse=" ")
	if (!is.null(val)) {
	    hist(val, main=mn)
	}
    }
    dev.off()
    
    #FIXME!
    while(!.Devices[[1]][1] == "null device") {
	dev.off()
    }
}


