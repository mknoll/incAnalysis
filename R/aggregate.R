#' @title Aggregate data (age groups)
#' 
#' @description Aggregates yearly to 5yr age data (sum)
#' 
#' @param population population matrix, with consecutive 
#' single years as columns
#' @param inc incidence matrix, single years in columns
#' 
#' @export
#' 
#' @return list with adjusted population and incidence data
agg <- function(population, inc) {
    ### aggregate 
    sq <- seq(1, length(population[1,]), 5)
    p2 <- list()
    inc2 <- list()
    nm <- c()
    for (i in sq) {
	tryCatch({
	    tmp <- apply(population[,c(i:(i+4))], 1, sum)
	    p2[[length(p2) +1]] <- tmp
	    nm <- c(nm, colnames(population)[i])
	}, error=function(e) {})
    }
    p2 <- data.frame(do.call(cbind, p2), check.names=F)
    colnames(p2) <- nm
    inc2 <- list()
    for (j in 1:length(inc)) {
	i2 <- list()
	for (i in sq) {
	    tryCatch({
		tmp <- apply(inc[[j]]$data[,c(i:(i+4))], 1, function(x) sum(x, na.rm=T))
		i2[[length(i2) +1]] <- tmp
	    }, error=function(e) {})
	}
	df <- data.frame(do.call(cbind, i2))
	colnames(df) <- nm
	inc2[[length(inc2)+1]] <- list(data=df, ent=inc[[j]]$ent)
    }
    names(inc2) <- names(inc)
    return(list(population=p2 ,inc=inc2))
}
