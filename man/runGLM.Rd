% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glmCalc.R
\name{runGLM}
\alias{runGLM}
\title{Run GLM analysis on incClass instance.}
\usage{
runGLM(method = "inla", ...)
}
\arguments{
\item{method}{inla if INLA should be used, any other value 
for the classical approach}

\item{...}{additional parameters}
}
\value{
Updated incAnalysis object
}
\description{
Wrapper for runINLA() or runMlGLM()
}
