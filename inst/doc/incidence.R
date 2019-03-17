## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=F-------------------------------------------------------------
#  install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

## ---- eval=F-------------------------------------------------------------
#  install.packages("BAPC", repos="http://R-Forge.R-project.org")

## ----eval=F--------------------------------------------------------------
#  if (!any(grepl("devtools", installed.packages()))) {
#      install.packages("devtools")
#  }
#  devtools::install_github("mknoll/incAnalysis")

## ------------------------------------------------------------------------
dataset <- "SEER9" #NORDCAN, Saarland
gender <- "MALE" #FEMALE
entity <- "Glioblastoma" 
ageLow <- 20 # exclude data for ages below 20

## ------------------------------------------------------------------------
#filename 
fn <- paste("population_", dataset, "_", gender, ".csv", sep="")
population <- read.csv(system.file("extdata", fn, package="incAnalysis"), check.names=F, row.names=1)

#multiply by factor 1000 to get actual counts (only SEER9 data!)
if (dataset == "SEER9") {
    population <- population*1000
}

#visualize population as heatmap
require(pheatmap)
pheatmap(population, cluster_rows=F, cluster_cols=F)

# age/ age categories
rownames(population)

#adjust population matrix rownames
if (dataset == "Saarland") {
    rownames(population) <- do.call(rbind, strsplit(trimws(rownames(population)), " "))[,1]
    rownames(population)[1] <- 0
    population <- data.frame(t(population), check.names=F)
} else if (dataset == "NORDCAN") {
    rownames(population) <- do.call(rbind, strsplit(rownames(population), "-"))[,1]
    population <- data.frame(t(population), check.names=F)
}

population[1:4,1:10]

## ------------------------------------------------------------------------
#filename 
fn <- paste(dataset, "_", entity ,"_", gender, ".csv", sep="")
incidence <- data.matrix(read.csv(system.file("extdata", fn, package="incAnalysis"), check.names=F, row.names=1))

## additionally included entities
fl <- list.files(system.file("extdata", "/", package="incAnalysis"))
fl[which(grepl("SEER9", fl))]

## ---- eval=F-------------------------------------------------------------
#  dataset <- "NORDCAN"
#  load(system.file("extdata", paste(dataset, "_incidence_", gender,sep=""), package="incAnalysis")) #coll
#  names(coll) <-  unlist(lapply(coll, function(x) x$meta$Name))
#  
#  #Entities
#  names(coll)
#  
#  incidence <- data.matrix(t(coll[which(names(coll) == "Brain, central nervous system")][[1]]$data))
#  colnames(incidence) <- colnames(population)

## ---- eval=F-------------------------------------------------------------
#  dataset <- "Saarland"
#  load(system.file("extdata", paste(dataset, "_incidence_", gender,sep=""), package="incAnalysis")) #all
#  nm <- lapply(all, function(x) x$map$Tumor)
#  names(all) <- unlist(nm)
#  
#  #Entities
#  names(all)
#  
#  incidence <- data.matrix(t(all[which(names(all) == "191 Gehirn")][[1]]$data[,]))
#  
#  ## differentially groups in population and incidence -> remove 85+ (incidence) and 85/90 (population)
#  population <- population[,1:17]
#  incidence <- incidence[,1:17]
#  colnames(incidence) <- colnames(population)

## ------------------------------------------------------------------------
# NA correspond to 0 values
incidence[which(is.na(incidence))] <- 0
incidence <- data.frame(incidence, check.names=F)

incidence[1:4,1:10]

#visualize incidence as heatmap
pheatmap(incidence, cluster_rows=F, cluster_cols=F)

## ------------------------------------------------------------------------
# get ages from colnames 
ages <- as.numeric(gsub("+", "", as.character(colnames(incidence))))
w <- which(ages >= ageLow)

incidence <- incidence[,w]
population <- population[,w]

## ------------------------------------------------------------------------
require(incAnalysis)
obj <- new("incClass", 
	   population=population,
	   cases=incidence,
	   nPred=10, #prediction period: 10 years
	   nFit=5, #fitting period
	   predLast=0, #only retrieve data for last prediction year
	   gridFactor=1, #width of age and year bins. 1 for SEER data, 5 for NORDCAN and Saarland
	   entity=entity)

## ------------------------------------------------------------------------
obj <- runFwProj(obj, ci="W") #Wilson, ci can be PC for Pearson Clopper

## ------------------------------------------------------------------------
# linear model with age and period effects
frm <- as.formula(Y~offset(log(N))+ PERIOD + AGE)
# use neg-binomial distribution
obj <- runGLM(obj, frm=frm, family="nbinomial", text="arbitrary model ID 1")

## ------------------------------------------------------------------------
### RW2
frm <- as.formula(Y~offset(log(N))+PERIOD+f(inla.group(AGE), model="rw2"))
obj <- runGLM(obj, frm=frm, family="nbinomial", text="rw2")

### Cubic spline
ZSplineAge <- createZSpline(obj@dataLong$AGE, nKnots=10)
frm <- as.formula(Y~offset(log(N))+PERIOD+AGE+f(SEQNO,model="z",Z=ZSplineAge))
obj <- runGLM(obj, frm=frm, family="poisson", text="splineAge")

## ------------------------------------------------------------------------
# multivariate smooths (tensor spline)
frm <- as.formula(Y~offset(log(N))+ te(PERIOD, AGE))
# use Poisson distribution
obj <- runGAM(obj, frm=frm, family="poisson", text="arbitrary model ID 2")

## ------------------------------------------------------------------------
obj <- runBAPC(obj)

## ---- eval=F-------------------------------------------------------------
#  pitHist(obj)

## ------------------------------------------------------------------------
obj <- evaluate(obj)

## ------------------------------------------------------------------------
df <- metrics(obj)
df

## ------------------------------------------------------------------------
require(ggplot2)
df$COL <- ifelse(as.numeric(as.character(df$PERIOD)) <= 2004, 0, 19)

# Coverage  
ggplot(df, aes(x=PERIOD, y=CVG, group=paste(TYPE, TEXT), color=paste(TYPE, TEXT))) + geom_line() + geom_point(shape=df$COL) + labs(color="Type", y="Coverage") + ggpubr::rotate_x_text()

# Precision
ggplot(df, aes(x=PERIOD, y=log(SD), group=paste(TYPE, TEXT), color=paste(TYPE, TEXT))) + geom_line() + geom_point(shape=df$COL) + labs(color="Type", y="Coverage") + ggpubr::rotate_x_text()

# Bias
#negative values -> Overestimation
ggplot(df, aes(x=PERIOD, y=BIAS, group=paste(TYPE, TEXT), color=paste(TYPE, TEXT))) + geom_line() + geom_point(shape=df$COL) + labs(color="Type", y="Coverage") + ggpubr::rotate_x_text()

## ------------------------------------------------------------------------
#calcuate score 
scr <- scaleAggMetr(bias=df$BIAS, sd=df$SD, cvg=df$CVG, plot=F)

df$SCORE <- scr$VAL
#Plot score
ggplot(df, aes(x=PERIOD, y=SCORE, group=paste(TYPE, TEXT), color=paste(TYPE, TEXT))) + geom_line() + geom_point(shape=df$COL) + labs(color="Type", y="Score") + ggpubr::rotate_x_text()

#HEatmap of adjusted metrics
df$ADJ_CVG <- scr$MAT[,"CVG"]
df$ADJ_SD <- scr$MAT[,"SD"]
df$ADJ_BIAS <- scr$MAT[,"BIAS"]

sub <- t(df[,which(grepl("ADJ_", colnames(df)))])
colnames(sub) <- paste("RN", 1:length(sub[1,]))
rownames(df) <- colnames(sub)
pheatmap(sub, annotation_col=df[,c("TYPE", "TEXT", "PERIOD", "SCORE", "SD", "CVG", "BIAS")], show_colnames=F)

