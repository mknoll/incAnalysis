#' @title Evaluate model projections.
#' 
#' @description Extracts and calculates precision data 
#' and evaluates bias and coverage (within / outside credibility 
#' or confidence bands). Two-sided 95% bands/intervals are used. 
#' 
#' @param obj incAnalysis object
#' @param dir directory to create plots in
#' @param fnPre prefix for plots
#' 
#' @import DescTools
#' @import grDevices
#' @import graphics
#' @export 
#' 
#' @return updated incAnalysis object 
evaluate <- function(obj, dir=NULL, fnPre="") {
    if (length(obj@results) == 0) {
	warning("No results available (INLA might have crashed). Exiting.")
	return()
    }

    evalLong <- list()
    evalWide <- list()
    
    if (obj@predLast == 1 && !is.null(dir)) {
	eFn <- gsub('[^a-zA-Z]', '', obj@entity)
	eFn <- paste(eFn, obj@nPred, sep="_")
	nH <- ceiling(sqrt(length(obj@results)))
	if (nH == 0) { warning("Nothing to do!"); return() }

	png(paste(dir, "/EVAL_",fnPre, "_", eFn, ".png", sep=""), res=250, width=3000, height=3000)
	par(mfrow=c(nH, nH))
    }

    for (res in obj@results) {
	print("###########")
	print(res$type)

	### Plot newly predicted years
	### FIXME: einheitliche bennennung!
	if (res$type == "GLM_INLA" || res$type == "INLA" || res$type == "GAM_INLA") {
	    #### INLA
	    data <- obj@dataLong
	    data$PRED <- res$result$summary.fitted.values[,"mean"]

	    data$Q025 <- res$result$summary.fitted.values[,"0.025quant"]
	    data$Q975 <- res$result$summary.fitted.values[,"0.975quant"]
	    data$SD <- res$result$summary.fitted.values[,"sd"]

	    #### 
	    data$LOW <- data$Q025
	    data$HIGH <- data$Q975
	} else if (res$type == "BAPC") {
	    data <- obj@dataLong

	    prd <- list()
	    for (i in 1:length(res$ageProj)) {
		prd[[length(prd)+1]] <- data.frame(res$ageProj[[i]], AGE=names(res$ageProj)[i], 
						   PERIOD=rownames(res$ageProj[[i]]),
						   Q025=res$ageProj[[i]][,'0.025Q'],
						   Q975=res$ageProj[[i]][,'0.975Q'],
						   SD=res$ageProj[[i]][,'sd'])
	    }
	    prd <- do.call(rbind, prd)
	    id <- paste(data$AGE, data$PERIOD)
	    prd <- prd[match(id, paste(prd$AGE, prd$PERIOD)),]
	    data$PRED <- prd[,"mean"]
	    data$Q025 <- prd[,"Q025"]
	    data$Q975 <- prd[,"Q975"]
	    data$SD <- prd[,"SD"]
	    
	    #### 
	    data$LOW <- data$Q025
	    data$HIGH <- data$Q975
	} else if (res$type == "FWDPROJ") {
	    prj <- list()
	    for (i in 1:length(res$cases[1,])) {
		prj[[length(prj)+1]] <- data.frame(PERIOD=rownames(res$cases),
						   AGE=colnames(res$cases)[i],
						   PRED=res$cases[,i],
						   Y=obj@casesWide[,i])
	    }
	    data <- do.call(rbind, prj)

	    pop <- obj@populationLong
	    data$N <- pop$N[match(paste(data$PERIOD, data$AGE), paste(pop$YR, pop$AGE))]

	    tmpPRED <- data$PRED
	    tmpPRED[which(is.na(tmpPRED))] <- 0
	    tmpN <- data$N
	    tmpN[which(is.na(tmpN))] <- 0

	    ##############################
	    cnf <- BinomCI(tmpPRED, tmpN, method=res$method)
	    data$LOW <- cnf[,2]*data$N
	    data$HIGH <- cnf[,3]*data$N
	    # TODO: SD
	    p <- tmpPRED/tmpN
	    ### calculate standard deviation
	    data$SD <- sqrt(tmpPRED*p*(1-p))
	} else if (res$type == "GAM_ML") {
	    data <- obj@dataLong
	    ## family of gam
	    fam <- res$fit$family
	    link <- fam[[2]]
	    
	    pr <- res$result$PRD
	    se <- res$result$SE

	    #### TODO: check! / 95% CI
	    lwr <- pr-1.96*se
	    upr <- pr+1.96*se

	    data$PRED <- transf(link, pr)
	    data$Q025 <- transf(link, lwr)
	    data$Q975 <- transf(link, upr)
	    data$SD <- transf(link, se)
	    
	    #### 
	    data$LOW <- data$Q025
	    data$HIGH <- data$Q975
	} else if (res$type == "GLM_ML") {
	    data <- obj@dataLong
	    ## family of glm
	    fam <- res$fit$family
	    link <- fam[[2]]
	    
	    pr <- res$result$PRD
	    se <- res$result$SE

	    #### TODO: check! / 95% CI
	    lwr <- pr-1.96*se
	    upr <- pr+1.96*se

	    data$PRED <- transf(link, pr)
	    data$Q025 <- transf(link, lwr)
	    data$Q975 <- transf(link, upr)
	    data$SD <- transf(link, se)
	    
	    #### 
	    data$LOW <- data$Q025
	    data$HIGH <- data$Q975
	} else {
	    stop("Unknown type!")
	}

	data$DIFF <- data$PRED-data$Y
	### COVERAGE TODO
	o1 <- ifelse(data$Y < data$LOW , 1, 0)
	o2 <- ifelse(data$Y > data$HIGH , 1, 0)
	data$OUTSIDE <- o1+o2
	### BIAS
	data$MEAN <- data$PRED

	### TODO: check for age ranges
	data$AGE <- as.numeric(as.character(data$AGE))
	data$TYPE <- res$type
	data$TEXT <- res$text
	datPred <- data[which(data$PERIOD %in% obj@pred),]

	## long format data
	evalLong[[length(evalLong)+1]] <- data

	## wide format 
	m <- obj@casesWide
	for (i in 1:length(m[1,])) {
	    sub <- data[which(data$AGE == colnames(m)[i]),]
	    m[,i] <- sub$PRED[match(rownames(m), sub$PERIOD)]
	}
	evalWide[[length(evalWide)+1]] <- m


	### aggreagted for last year pred
	if (obj@predLast == 1) {
	    w <- which(data$PERIOD == obj@pred)
	    
	    rn <- data$AGE[w]
	    at <- seq(1, length(rn), by=5)
	    print(rn)
	    
	    matplot(data[w,c("LOW", "PRED", "HIGH", "Y")], type="b",
			main=paste(res$entity, res$type, res$text),
			col=c("black", "tomato", "darkgray", "royalblue"),
			ylab="#Cases", xlab="AgeGrp", lwd=c(1,2,1,2),
			lty=c(2,1,2,1), pch=19,
			xaxt='n')
	    axis(1, at=at, rn[at])
	    yMax <- max(data[w,c("HIGH", "Y")], na.rm=T)
	    legend(1, yMax, c("lwr", "pred", "up", "y"), fill=c("black",
								"tomato",
								"darkgray",
								"royalblue"))

	}
    }
    obj@evalLong <- evalLong
    obj@evalWide <- evalWide
    
    if (obj@predLast == 1 && !is.null(dir)) {
	dev.off()
    }

    return(obj)
}


transf <- function(link, x) {
    switch(link, logit=exp(x)/(1+exp(x)), log=exp(x),
	   loglog=exp(-exp(-x)), cloglog=1-exp(-exp(x)), identity=x,
	   mlogit=exp(x)/(1+sum(exp(x))) ) 
}
