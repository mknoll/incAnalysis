% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inlaCalc.R
\name{runInla}
\alias{runInla}
\title{Run INLA analysis}
\usage{
runInla(obj, frm = as.formula(Y ~ offset(log(N))), family = "poisson",
  control.family = list(link = "log"), control.predictor = list(link =
  1, compute = T), control.compute = list(dic = T, cpo = T, waic = T),
  text = "", class = "INLA")
}
\arguments{
\item{obj}{incClass instance}

\item{frm}{Model formula.}

\item{family}{Distribution}

\item{control.family}{inla parameter}

\item{control.predictor}{inla parameter}

\item{control.compute}{inla parameter}

\item{text}{Free text variable}

\item{class}{Additional specification of the model class,
is set to GLM when runINLA is called from from runGLM and 
GAM is called from runGAM. Defaults to INLA.}

\item{...}{additional parameters}
}
\value{
Updated incClass instance
}
\description{
Add INLA based model analysis to an incClass object
}
