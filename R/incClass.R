# S4 Helper classes to allow NULL values
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("dfOrNULL", c("data.frame", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))

#' A S4 class representing an analysis instance 
#'
#' @name incClass-class
#' @exportClass incClass
incClass <- setClass("incClass",
		     slots=c(dateCreated="POSIXct",
			     entity="characterOrNULL",
			     population="dfOrNULL",
			     cases="dfOrNULL",
			     nPred="numericOrNULL",
			     predLast="numericOrNULL",
			     nFit="numericOrNULL",
			     pred="characterOrNULL",
			     dataLong="dfOrNULL",
			     dataLongPred="dfOrNULL",
			     casesWide="dfOrNULL",
			     casesWidePred="dfOrNULL",
			     populationWide="dfOrNULL",
			     populationLong="dfOrNULL",
			     results="listOrNULL",
			     performance="listOrNULL",
			     gridFactor="numericOrNULL",
			     evalLong="listOrNULL",
			     evalWide="listOrNULL"
			     ))

#' @title Constructor incClass
#'
#' @description Constructor for new analysis object
#'
#' @param .Object class instance
#' @param dateCreated Date of creation
#' @param population data.frame containing count data with years in
#' rows (first row: earliest year) and numeric ages in cols (increasing from 
#' left to right)
#' @param cases data.frame containing count data, same design as the
#' population data
#' @param nPred Prediction period. Calculated backwards from the earliest valid
#' (in population and incidence available) year.
#' @param nFit Model fitting period. Counted backwards from the earliest 
#' year from which the prediction period starts. For forward projection of 
#' last observation, the year previous to the start of the prediction
#' interval is selected. 
#' @param predLast Return only last observation year predictions and metrics 
#' when using the metrics() function.
#' @param gridFactor Ratio of age-width and yearly structure. If data is given 
#' in yearly fashion and 1-year age groups, the grid factor is 1. For yearly data 
#' and 5-year age groups, the grid factor is 5. 
#' @param entity Free text variable.
#' @param ... additional parameters
setMethod("initialize", "incClass",
	  function(.Object,
		   dateCreated=POSIXct,
		   population=data.frame,
		   cases=data.frame,
		   nPred=numeric,
		   nFit=numeric,
		   predLast=numeric,
		   gridFactor=numeric,
		   entity=character,
		   ...) {
	      .Object@dateCreated=Sys.time()
	      .Object@population=population
	      .Object@cases=cases
	      .Object@nPred=nPred
	      .Object@nFit=nFit
	      .Object@predLast=predLast
	      .Object@gridFactor=gridFactor
	      .Object@entity=entity

	      ## reformat data
	      tmp <- reformat(.Object@cases, 
			      .Object@population,
			      .Object@nPred,
			      .Object@nFit)
	      .Object@dataLong=tmp$FULL_LONG
	      .Object@dataLongPred=tmp$PRED_LONG

	      to <- length(cases[,1])
	      if (predLast != 1) {
		  .Object@pred=rownames(cases)[(to-nPred+1):to]
	      } else {
		  # predict only most current date
		  .Object@pred=rownames(cases)[to]
	      }

	      .Object@casesWide=tmp$FULL_CASES_WIDE #TODO
	      .Object@casesWidePred=tmp$PRED_CASES_WIDE
	      .Object@populationWide=tmp$FULL_POPULATION_WIDE

	      ## Population long
	      coll <- list()
	      for (i in 1:length(population[1,])) {
		  coll[[length(coll)+1]] <- data.frame(N=population[,i], 
						       YR=rownames(population), 
						       AGE=colnames(population)[i])
	      }
	      .Object@populationLong <- do.call(rbind, coll)

	      .Object
	  })

