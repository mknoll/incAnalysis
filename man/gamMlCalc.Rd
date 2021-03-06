% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gamCalc.R
\name{gamMlCalc}
\alias{gamMlCalc}
\title{GAM: classical approach for projections.}
\usage{
gamMlCalc(data, frm = as.formula(Y ~ offset(log(N))), text = "",
  family = "poisson", type = "link")
}
\arguments{
\item{data}{data used for model fitting, in the form as stored in 
the dataLongPred slot of an incClass object}

\item{frm}{formula used for mgcv::gam()}

\item{text}{free-text variable}

\item{family}{distribution, family parameter}

\item{type}{type parameter used in the predict function}
}
\description{
Use mgcv::gam() models for projections.
}
