% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glmCalc.R
\name{runMlGLM}
\alias{runMlGLM}
\title{Run ML GLM analysis}
\usage{
runMlGLM(obj, frm = as.formula(Y ~ offset(log(N))), family = "poisson",
  text = "", type = "link")
}
\arguments{
\item{obj}{incClass object}

\item{frm}{Model formula.}

\item{family}{distibution}

\item{text}{Free text variable}

\item{type}{type used for the predict function, e.g. link}
}
\value{
Updated incAnalysis object
}
\description{
Add ML GLM based model analysis to an incAnalysis object
}
