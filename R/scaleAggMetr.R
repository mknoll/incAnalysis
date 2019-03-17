#' @title Scale and aggregate metrics for score calculation
#'
#' @description Performs multiple adjustments of coverage, bias 
#' and precision values to obtain a score value.
#' 
#' @param bias Bias variable
#' @param sd Precision variable (standard variation)
#' @param cvg Coverage
#' @param wBIAS weighting factor for bias 
#' @param wSD weighting factor for standard deviation
#' @param wCVG weighting factor for coverage
#' @param plot plot data
#'
#' @export 
#' @return list with score values and matrices
scaleAggMetr <- function(bias, sd, cvg, wBIAS=1, wSD=1, wCVG=1, plot=T) {
    if (plot) {
	par(mfrow=c(3,3))
    }

    ### Minimize
    ### BIAS
    bak <- bias
    bias <- log(abs(bias))
    w <- which(is.infinite(bias)) 
    if (plot) {
	hist(bias, main="BIAS")
    }
    #bias <- abs(bias)
    if (length(w) > 0) {
	bias <- (bias-mean(bias[-w], na.rm=T))/sd(bias[-w], na.rm=T) ###TODO: na.rm
    } else {
	bias <- (bias-mean(bias[], na.rm=T))/sd(bias[], na.rm=T) ###TODO: na.rm
    }
    mn <- min(bias[which(!is.infinite((bias)))], na.rm=T)
    bias <- bias+mn*-1
    bias <- bias^8
    if (length(w) > 0) { #TODO: add for sd , cvg
	bias <- (bias-mean(bias[-w], na.rm=T))/sd(bias[-w], na.rm=T) ###TODO: na.rm
    } else {
	bias <- (bias-mean(bias[], na.rm=T))/sd(bias[], na.rm=T) ###TODO: na.rm
    }
    mn <- min(bias[which(!is.infinite((bias)))], na.rm=T)
    bias <- bias+mn*-1
    bias[w] <- 0
    bias <- bias*wBIAS
    if (plot) {
	hist(bias)
	#plot(bias~bak, ylab="Scaled bias", xlab="Original bias", pch=10, cex=0.7, xlim=c(-1,1))
	plot(bias~bak, ylab="Scaled bias", xlab="Original bias", pch=10, cex=0.7)
    }

    ### COVERAGE
    bak <- cvg
    cvg <- 1-cvg
    w <- which(log(cvg) == 0) ###Coverage = 1
    cvg <- log(cvg/(1-cvg))
    w2 <- which(is.infinite(cvg))
    if (length(w2) > 0) {
	w2 <- w2[which(!w2 %in% w)] ### Coverage = 0
    }
    wAll <- c(w, w2)
    cvg <- (cvg-mean(cvg[-wAll]))/sd(cvg[-wAll])
    if (plot) {
	hist(cvg, main="CVG")
    }
    mn <- min(cvg[-wAll])
    cvg <- cvg+mn*-1
    if (length(w2) > 0) {
	cvg[w2] <- 0  
    }
    cvg[w] <- max(cvg[which(!is.infinite(cvg))])
    cvg <- cvg*wCVG
    if (plot) {
	hist(cvg)
	plot(cvg~bak, ylab="Scaled 1-CVG", xlab="Original CVG", pch=19, cex=0.7)
    }

    #### SD
    bak <- sd
    w <- which(sd == 0) ### FIXME!
    ## TODO: chekc for inf
    sd <- log(sd)
    if (length(w) > 0) {

	sd <- (sd - mean(sd[-w], na.rm=T))/sd(sd[-w], na.rm=T)
    } else {
	sd <- (sd - mean(sd[], na.rm=T))/sd(sd[], na.rm=T)
    }
    if (plot) {
	hist(sd,main="SD")
    }
    mn <- min(sd[which(!is.infinite(sd))], na.rm=T)
    sd <- sd+mn*-1
    sd[w] <- 0
    sd <- sd*wSD
    if (plot) {
	hist(sd)
	plot(sd~log(bak), ylab="Scaled SD", xlab="Original log(SD)", pch=19, cex=0.7)

	par(mfrow=c(1,1))

    }

    return(list(VAL=bias+cvg+sd, MAT=data.frame(BIAS=bias, CVG=cvg, SD=sd)))
}
