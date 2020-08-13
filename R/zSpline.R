#' @title Create zSpline 
#'
#' @description Allows to incorporate splines into INLA 
#' based analyses
#' 
#' @param val Vector or values to fit a spline to 
#' @param a Starting value, might be NULL (lowest value)
#' @param b Ending value, might be NULL (highest value)
#' @param nKnots number of knots 
#'
#' @import splines
#'
#' @export 
createZSpline <- function(val, a=NULL, b=NULL, nKnots=NULL) {
    nKnots <- ifelse(is.null(nKnots), 15, nKnots)
    if (is.null(a) || is.null(b)) {
	a <- range(val)[1]
	b <- range(val)[2]
    }

    # Obtain the spline component of the Z matrix:
    numIntKnots <- nKnots 
    intKnots <- quantile(unique(val),
			 seq(0,1,length=(numIntKnots+2))[-c(1,(numIntKnots+2))])
    Omega <- formOmega(a,b,intKnots)
    eigOmega <- eigen(Omega)
    indsZ <- 1:(numIntKnots+2)
    UZ <- eigOmega$vectors[,indsZ]
    LZ <- t(t(UZ)/sqrt(eigOmega$values[indsZ]))     
    ## degree 3: cubic splines
    B <- bs(val,knots=intKnots,degree=3,Boundary.knots=c(a,b),intercept=TRUE)
    ZSpline <- B%*%LZ   

    return(ZSpline)
}
    

## Code taken from 
## http://faculty.washington.edu/jonno/book/nonparametric2.R 
formOmega <- function(a,b,intKnots) {
    allKnots <- c(rep(a,4),intKnots,rep(b,4)) 
    K <- length(intKnots) ; L <- 3*(K+8)
    xtilde <- (rep(allKnots,each=3)[-c(1,(L-1),L)]+ 
	       rep(allKnots,each=3)[-c(1,2,L)])/2
    wts <- rep(diff(allKnots),each=3)*rep(c(1,4,1)/6,K+7)
    Bdd <- spline.des(allKnots,xtilde,derivs=rep(2,length(xtilde)),
		      outer.ok=TRUE)$design  
    Omega     <- t(Bdd*wts)%*%Bdd     
    return(Omega)
}
