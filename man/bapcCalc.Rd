% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bapcCalc.R
\name{bapcCalc}
\alias{bapcCalc}
\title{Use BAPC as projection model}
\usage{
bapcCalc(population, cases, gf, predict = list(npredict = 10, retro = T),
  text = "")
}
\arguments{
\item{population}{Population data: same format as cases.}

\item{cases}{data.frame with incidence counts: ages in columns, increasing from left 
to right, ordered years in rows, last row: most recent observation.}

\item{gf}{grid factor: age groups are m times wider than the period interval,
typically 5 for yearly observation data and 18 age groups}

\item{predict}{list of prediction parameters for BAPC}

\item{text}{Free text}
}
\description{
Use the BAPC model published by Riebler et al. (2017)
}
