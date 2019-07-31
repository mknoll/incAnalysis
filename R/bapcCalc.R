#' @title Use BAPC as projection model
#' 
#' @description Use the BAPC model published by Riebler et al. (2017)
#' 
#' @param gf grid factor: age groups are m times wider than the period interval,
#' typically 5 for yearly observation data and 18 age groups
#' @param cases data.frame with incidence counts: ages in columns, increasing from left 
#' to right, ordered years in rows, last row: most recent observation. 
#' @param population Population data: same format as cases.
#' @param predict list of prediction parameters for BAPC
#' @param text Free text 
#' 
#' @import BAPC
#' @export 
bapcCalc <- function(population, cases, gf, 
		     predict=list(npredict=10, retro=T),
		     text="") {
    apc <- APCList(cases, population, gf)

    res <- BAPC(apc, predict)
    res <- qapc(res, percentiles=c(0.025, 0.5, 0.975))

    counts <- agespec.proj(res)

    return(list(apcList=apc, bapc=res, ageProj=counts, type="BAPC", text=text))
}

#' @title Run BAPC analysis
#' 
#' @description Perform BAPC projection on inClass object 
#' 
#' @param obj incClass object
#' @param text free text variable 
#'
#' @import BAPC
#' @export
runBAPC <- function(obj, text="") {
    res <- bapcCalc(obj@populationWide,
		    obj@casesWidePred,
		    obj@gridFactor,
		    predict=list(npredict=obj@nPred, retro=T),
		    text=text) ##TODO: check if yrs/ cat

    obj@results[[length(obj@results)+1]] <- res

    return (obj)
}
