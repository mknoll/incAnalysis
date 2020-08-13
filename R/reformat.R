
#'
#' @description Assures that case and population matrices match,
#' creates long format data from supplied wide format data,
#' sets values to predict to NA. Needed for the various data 
#' representations in the incAnalysis objects. Only data within 
#' the prediction and fitting periods are retained.
#'
#' @param cases data.frame with increasingly ordered years per 
#' row and age per column
#' @param population data.frame for population data, same structure as 
#' cases
#' @param pred number of years to predict
#' @param fit number of years to fit models with
#'
#' @export 
#' @return list with adjusted long and wide format data
reformat <- function(cases, population, pred=2, fit=15) {
    ## same yearly structure? (rows)
    population <- population[which(rownames(population) %in% rownames(cases)),]
    cases <- cases[which(rownames(cases) %in% rownames(population)),]
    cases <- cases[match(rownames(population), rownames(cases)),]
    if (!all(rownames(population) == rownames(cases))) {
	stop("Non-matching years (rownames)")
    }
    ## same age structure? (cols)
    if (length(population[1,]) != length(cases[1,])) {
	stop("Non-matching ages (colnames)")
    }

    # convert to long format
    df <- list()
    for (i in 1:length(cases[1,])) {
	df[[length(df)+1]] <- data.frame(AGE=colnames(cases)[i],
					 PERIOD=rownames(cases),
					 N=population[,i],
					 Y=cases[,i])
    }
    df <- do.call(rbind, df)
    df$AGE <- as.numeric(as.character(df$AGE))
    df$PERIOD <- as.numeric(as.character(df$PERIOD))

    ## Data of interest
    from <- length(cases[,1])-(fit+pred)+1
    to <- length(cases[,1])

    ##Years to retain
    yrs <- rownames(cases)[from:to]
    df <- df[which(df$PERIOD %in% yrs),]
    cases <- cases[which(rownames(cases) %in% yrs),]
    population <- population[which(rownames(population) %in% yrs),]
    df$SEQNO <- 1:length(df[,1])

    ## complete dataset
    dfCompl <- df

    ## set values which will be predicted to NA
    prdYr <- rownames(cases)[(length(cases[,1])-pred+1):length(cases[,1])]
    df$Y[which(df$PERIOD %in% prdYr)] <- NA
    dfPred <- df
    casesPred <- cases
    casesPred[which(rownames(casesPred) %in% prdYr),] <- NA
    populationPred <- population
    populationPred[which(rownames(populationPred) %in% prdYr),] <- NA


    return(list(FULL_LONG=dfCompl, 
		PRED_LONG=dfPred, 

		FULL_CASES_WIDE=cases, 
		PRED_CASES_WIDE=casesPred,

		FULL_POPULATION_WIDE=population,
		PRED_POPULATION_WIDE=populationPred,

		PRED=pred, FIT=fit))
}
