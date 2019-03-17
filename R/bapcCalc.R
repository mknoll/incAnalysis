#' @title BAPC calc
#' @param gf grid factor: age groups are m times wider than the period interval
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
