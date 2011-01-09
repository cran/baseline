### $Id: optimWizard.R 176 2011-01-09 13:52:22Z bhm $

optimWizard <- function(X, y, postproc, predictionTest, cvsegments){
## Organize optimization through GUI

if(missing(X))
	stop('No data specified')
if(missing(y))
	stop('No response specified')
if(missing(predictionTest)){
	rr <- FALSE
	predictionTest <- NULL}
else
	rr <- TRUE
if(missing(postproc)){
	pp <- FALSE
	postproc <- NULL}
else
	pp <- TRUE
if(missing(cvsegments)){
	cc <- FALSE
	cvsegments <- NULL}
else
	cc <- TRUE
bltest <- NULL

require(gWidgets)

# Set up main window with internal container
win <- gwindow("Optimisation wizard", width=450, height=300)
main <- ggroup(horizontal=FALSE)
add(win,main)

# Initialize parameters and paramter lists
nAlgs <- 0
used <- numeric(0)
genChoosers <- rGroups <- groups <- removes <- param <- parameterGroup <- parameterList <- method <- list()
param$irls$pars				<- c("lambda1", "lambda2", "wi", "maxit")
param$modpolyfit$pars		<- c("degree", "tol", "rep")
param$als$pars				<- c("lambda", "p")
param$rollingBall$pars		<- c("wm", "ws")
param$medianWindow$pars		<- c("hwm", "hws")
param$fillPeaks$pars		<- c("lambda", "hwi", "it", "int")
param$peakDetection$pars	<- c("left.right", "lwin.rwin")
param$rfbaseline$pars		<- c("NoXP", "b")

# Functions for parsing beween method numbers and mehod names
methodParse <- function(m){
	if(m==1) method <- "als"
	if(m==2) method <- "fillPeaks"
	if(m==3) method <- "irls"
	if(m==4) method <- "medianWindow"
	if(m==5) method <- "modpolyfit"
	if(m==6) method <- "peakDetection"
	if(m==7) method <- "rfbaseline"
	if(m==8) method <- "rollingBall"
	method
}
parseMethod <- function(m){
	if(m==1) method <- "Asymmetric Least Squares"
	if(m==2) method <- "Fill peaks"
	if(m==3) method <- "Iterative restricted least squares"
	if(m==4) method <- "Local medians"
	if(m==5) method <- "Iterative polynomial fitting"
	if(m==6) method <- "Peak detection"
	if(m==7) method <- "Robust baseline estimation"
	if(m==8) method <- "Rolling ball"
	method
}
algorithmParse <- function(m){
	if(m=="als") 			method <- baselineAlgorithms$als
	if(m=="fillPeaks") 		method <- baselineAlgorithms$fillPeaks
	if(m=="irls") 			method <- baselineAlgorithms$irls
	if(m=="medianWindow") 	method <- baselineAlgorithms$medianWindow
	if(m=="modpolyfit") 	method <- baselineAlgorithms$modpolyfit
	if(m=="peakDetection") 	method <- baselineAlgorithms$peakDetection
	if(m=="rfbaseline") 	method <- baselineAlgorithms$rfbaseline
	if(m=="rollingBall") 	method <- baselineAlgorithms$rollingBall
	method
}

# Function for adding a baseline correction method, including sub-functions and variables
addAlg <- function(nm){
	# Initialize parameters and groups
	nAlgs <<- nAlgs + 1
	used[nAlgs] <<- 1
	groups[[nAlgs]] <<- ggroup(horizontal=FALSE)
	rGroups[[nAlgs]] <<- ggroup(horizontal=TRUE)
	name <- methodParse(nm)
	nameLong <- parseMethod(nm)
	method[[nAlgs]] <<- name

	# Add some space and the name of the baseline correction algorithm
	addSpace(groups[[nAlgs]],20)
	add(groups[[nAlgs]],glabel(nameLong))
	addSpace(groups[[nAlgs]],10)

	# Function for setting up parameter array in GUI together with buttons and functions
	addParameterGroup <- function(nameStr, lineNo){
		genChoosers[[nAlgs]][[lineNo]] <<- gdroplist(c("-> Generate", "Linear","Exponential"), selected=1)
		tag(genChoosers[[nAlgs]][[lineNo]],"no") <<- lineNo
		tag(genChoosers[[nAlgs]][[lineNo]],"name") <<- nameStr
		parameterList[[nAlgs]][[lineNo]]  <<- c(gedit(text = "", width=1,coerce.with=as.numeric),gedit(width=5,coerce.with=as.numeric),gedit(width=10,coerce.with=as.numeric),gedit(width=15),genChoosers[[nAlgs]][[lineNo]])
		# Generate sequence based on 'From', 'To', 'Steps' and choice from droplist
		addhandlerchanged(parameterList[[nAlgs]][[lineNo]][[5]], handler = function(h,...){
			if(svalue(genChoosers[[nAlgs]][[lineNo]],index=TRUE)>1){
				type <- svalue(h$obj,index=TRUE)-1
				svalue(genChoosers[[nAlgs]][[lineNo]],index=TRUE) <<- 1
				linNo <- tag(h$obj)$no
				if(is.finite(svalue(parameterList[[nAlgs]][[linNo]][[1]])) && is.finite(svalue(parameterList[[nAlgs]][[linNo]][[2]])) && is.finite(svalue(parameterList[[nAlgs]][[linNo]][[3]]))){
					if(type==1){
						linSeq <- seq(svalue(parameterList[[nAlgs]][[linNo]][[1]]),svalue(parameterList[[nAlgs]][[linNo]][[2]]),length.out=svalue(parameterList[[nAlgs]][[linNo]][[3]]))
						linOut <- linSeq[1]
						if(length(linSeq)>1)
							for(i in 2:length(linSeq))
								linOut <- paste(linOut, linSeq[i], sep=", ")
						svalue(parameterList[[nAlgs]][[linNo]][[4]]) <- linOut
					}
					if(type==2){
						linSeq <- exp(seq(log(svalue(parameterList[[nAlgs]][[linNo]][[1]])),log(svalue(parameterList[[nAlgs]][[linNo]][[2]])),length.out=svalue(parameterList[[nAlgs]][[linNo]][[3]])))
						linOut <- linSeq[1]
						if(length(linSeq)>1)
							for(i in 2:length(linSeq))
								linOut <- paste(linOut, linSeq[i], sep=", ")
						svalue(parameterList[[nAlgs]][[linNo]][[4]]) <- linOut
					}
				} else {
					gmessage("Missing value(s) in 'From', 'To' or 'Steps'", title="Sequence", icon = "warning")
				}
			}
		})
		# Set up visual optimization array for parameters
		parameterGroup[[nAlgs]][lineNo+1,1] <<- glabel(text=nameStr)
		size(parameterList[[nAlgs]][[lineNo]][[1]]) <<- c(60,28)
		size(parameterList[[nAlgs]][[lineNo]][[2]]) <<- c(60,28)
		size(parameterList[[nAlgs]][[lineNo]][[3]]) <<- c(20,28)
		size(parameterList[[nAlgs]][[lineNo]][[4]]) <<- c(220,28)
		size(parameterList[[nAlgs]][[lineNo]][[5]]) <<- c(120,28)
		parameterGroup[[nAlgs]][lineNo+1,2] <<- parameterList[[nAlgs]][[lineNo]][[1]]
		parameterGroup[[nAlgs]][lineNo+1,3] <<- parameterList[[nAlgs]][[lineNo]][[2]]
		parameterGroup[[nAlgs]][lineNo+1,4] <<- parameterList[[nAlgs]][[lineNo]][[3]]
		parameterGroup[[nAlgs]][lineNo+1,5] <<- parameterList[[nAlgs]][[lineNo]][[4]]
		parameterGroup[[nAlgs]][lineNo+1,6] <<- parameterList[[nAlgs]][[lineNo]][[5]]
	}

	# Set up visual optimization array for parameters
	parameterList[[nAlgs]] <<- genChoosers[[nAlgs]] <<- list()
	parameterGroup[[nAlgs]] <<- glayout(homogeneous = FALSE, spacing = 5, container=groups[[nAlgs]])
	parameterGroup[[nAlgs]][1,1] <<- glabel("")
	parameterGroup[[nAlgs]][1,2] <<- glabel("From:")
	parameterGroup[[nAlgs]][1,3] <<- glabel("To:")
	parameterGroup[[nAlgs]][1,4] <<- glabel("Steps:")
	parameterGroup[[nAlgs]][1,5] <<- glabel("Sequence:")
	nameStrs <- param[method[[nAlgs]]][[1]]$pars
	lns <- length(nameStrs)
	for(i in 1:lns)
		addParameterGroup(nameStrs[i],i)
	parameterGroup[[nAlgs]][lns+2,2] <- gbutton("Collect", handler = function(h,...){
		if(exists(".baseline.current")){
			if(.baseline.current$method == name){
				for(i in 1:lns){
					svalue(parameterList[[nAlgs]][[i]][[1]]) <- .baseline.current$parValues[i]
				}
			} else {
				gmessage(paste("'.baseline.current$method' is not equal to '", name, "'", sep=""), title="Sequence", icon = "warning")
			}
		} else {
			gmessage("'.baseline.current' not found", title="Sequence", icon = "warning")
		}
	})
	parameterGroup[[nAlgs]][lns+2,3] <- gbutton("Collect", handler = function(h,...){
		if(.baseline.current$method == name){
			for(i in 1:lns){
				svalue(parameterList[[nAlgs]][[i]][[2]]) <- .baseline.current$parValues[i]
			}
		} else {
			gmessage(paste("'.baseline.current$method' is not equal to '", name, "'", sep=""), title="Sequence", icon = "warning")
		}
	})
	parameterGroup[[nAlgs]][lns+2,4] <- glabel("<- from current algorithm in baselineGUI")

	# Button for removal of method and adding the method to the main window's notebook
	removes[[nAlgs]] <<- gbutton("Remove baseline correction method")
	tag(removes[[nAlgs]],"nAlg") <<- nAlgs
	addhandlerclicked(removes[[nAlgs]], handler = function(h,...){
		nAlg <- tag(h$obj)$nAlg
		dispose(nb)
		used[nAlg] <<- 0
	})
	add(rGroups[[nAlgs]],removes[[nAlgs]],expand=FALSE)
	addSpace(groups[[nAlgs]],20)
	add(groups[[nAlgs]],rGroups[[nAlgs]],expand=FALSE)
	add(nb,groups[[nAlgs]],label=name)
	visible(parameterGroup[[nAlgs]]) <- TRUE
}

# Droplists for adding a baseline correction method and choosing post processing
methodChooser <- gdroplist(c("-> Choose additional algorithm","Asymmetric Least Squares (als)", "Fill peaks (fillPeaks)", "Iterative restricted least squares (irls)",
	"Local medians (medianWindow)", "Iterative polynomial fitting (modpolyfit)", "Peak Detection (peakDetection)", "Robust baseline estimation (rfbaseline)", "Rolling ball (rollingBall)"),
	selected=1, handler = function(h,...){if(svalue(methodChooser,index=TRUE)>1) addAlg(svalue(methodChooser,index=TRUE)-1); svalue(methodChooser,index=TRUE)<-1})
postChooser <- gdroplist(c("None","Norm (L2)", "Mean", "Median",
	"Sum", "Sum of squares", "L1 postproc", "Maximum"),	selected=1)
regChosen <- FALSE
regOutGroup <- segChooser <- segNumber <- regParam <- numeric(0)
regFrom <- regTo <- regSteps <- lambdaSequence <- numeric(0)
# Analysis droplist
regChooser <- gdroplist(c("-> Choose an analysis","PLSR / RMSEP","Ridge Regression / RMSEP"),	selected=1, handler = function(h,...){
	if(regChosen == TRUE){
		delete(regFrame, regOutGroup) # Remove old analysis if chosen
	}
	if(svalue(regChooser,index=TRUE)>1){ # Analysis chosen
		regOutGroup <<- ggroup(horizontal=TRUE)
		regIntGroup1 <- ggroup(horizontal=FALSE)
		regIntGroup2 <- ggroup(horizontal=FALSE)
		regIntGroup3 <- ggroup(horizontal=FALSE)
		regIntGroup4 <- ggroup(horizontal=TRUE)
		regIntGroup5 <- ggroup(horizontal=FALSE)
		regIntGroupA <- ggroup(horizontal=TRUE)
		regIntGroupB <- ggroup(horizontal=FALSE)
		if(svalue(regChooser,index=TRUE)==2){ # Display extra parameters for PLSR
			if(cc)
				segChooser <<- gdroplist(c("random", "consecutive", "interleaved","custom"), selected=4, handler = function(h,...){
					if(svalue(segChooser, index=TRUE)==4){
						svalue(segNumber) <- length(cvsegments)
						enabled(segNumber) <- FALSE
					} else
						enabled(segNumber) <- TRUE
				})
			else
				segChooser <<- gdroplist(c("random", "consecutive", "interleaved"), selected=1)
			segNumber <<- gedit("10")
			regParam <<- gedit()
			add(regIntGroup1, glabel("CV segment type"), expand=FALSE)
			add(regIntGroup1, segChooser, expand=FALSE)
			if(cc){
				# add(regIntGroup1, glabel(length(cvsegments)), expand=FALSE)
				svalue(segNumber) <- length(cvsegments)
				enabled(segNumber) <- FALSE
			}
			add(regIntGroup1, glabel("Number of segments"), expand=FALSE)
			add(regIntGroup1, segNumber, expand=FALSE)
			add(regIntGroup2, glabel("Number of components"), expand=FALSE)
			add(regIntGroup2, regParam, expand=FALSE)
			add(regOutGroup, regIntGroup1, expand=FALSE)
			add(regOutGroup, regIntGroup2, expand=FALSE)
		}
		if(svalue(regChooser,index=TRUE)==3){ # Display extra parameters for Ridge regression
			regFrom <<- gedit(coerce.with=as.numeric)
			size(regFrom) <<- c(60,24)
			regTo <<- gedit(coerce.with=as.numeric)
			size(regTo) <<- c(60,24)
			regSteps <<- gedit(coerce.with=as.numeric)
			size(regSteps) <<- c(60,24)
			lambdaSequence <<- gdroplist(c("-> Generate", "Linear","Exponential"))
			# Generate sequence based on 'From', 'To', 'Steps' and choice from droplist
			addhandlerchanged(lambdaSequence, handler = function(h,...){
				if(svalue(lambdaSequence,index=TRUE)>1){
					type <- svalue(lambdaSequence,index=TRUE)-1
					svalue(lambdaSequence,index=TRUE) <<- 1
					if(is.finite(svalue(regFrom)) && is.finite(svalue(regTo)) && is.finite(svalue(regSteps))){
						if(type==1){
							linSeq <- seq(svalue(regFrom),svalue(regTo),length.out=svalue(regSteps))
							linOut <- linSeq[1]
							if(length(linSeq)>1)
								for(i in 2:length(linSeq))
									linOut <- paste(linOut, linSeq[i], sep=", ")
							svalue(regParam) <- linOut
						}
						if(type==2){
							linSeq <- exp(seq(log(svalue(regFrom)),log(svalue(regTo)),length.out=svalue(regSteps)))
							linOut <- linSeq[1]
							if(length(linSeq)>1)
								for(i in 2:length(linSeq))
									linOut <- paste(linOut, linSeq[i], sep=", ")
							svalue(regParam) <- linOut
						}
					} else {
						gmessage("Missing value(s) in 'From', 'To' or 'Steps'", title="Sequence", icon = "warning")
					}
				}
			})
			paramLabel <- glabel("Ridge parameter")
			regParam <<- gedit()
			add(regIntGroup1, glabel("From"), expand=FALSE)
			add(regIntGroup1, regFrom, expand=FALSE)
			add(regIntGroup2, glabel("To"), expand=FALSE)
			add(regIntGroup2, regTo, expand=FALSE)
			add(regIntGroup3, glabel("Steps"), expand=FALSE)
			add(regIntGroup3, regSteps, expand=FALSE)
			add(regIntGroup4, lambdaSequence, expand=FALSE)
			add(regIntGroup5, glabel("Lambda sequence"), expand=FALSE)
			add(regIntGroup5, regParam, expand=FALSE)
			add(regIntGroupA, regIntGroup1, expand=FALSE)
			add(regIntGroupA, regIntGroup2, expand=FALSE)
			add(regIntGroupA, regIntGroup3, expand=FALSE)
			add(regIntGroupA, regIntGroup4, expand=FALSE)
			add(regIntGroupB, regIntGroupA, expand=FALSE)
			add(regIntGroupB, regIntGroup5, expand=FALSE)
			add(regOutGroup, regIntGroupB, expand=FALSE)
		}
		add(regFrame,regOutGroup,expand=FALSE)
		regChosen <<- TRUE
	}
})

# Notebook containing settings and baseline correction methods, group initialization
nb <- gnotebook()
sGroup <- ggroup(horizontal=TRUE)
sGroup1 <- ggroup(horizontal=FALSE)
# size(sGroup1) <- c(400,300)
sGroup2 <- gframe("Optimisation", horizontal=FALSE)
# size(sGroup2) <- c(170,300)
methFrame <- gframe('Baseline correction', horizontal=FALSE)
methGroup <- ggroup(horizontal=TRUE)
normFrame <- gframe('Post processing', horizontal=FALSE)
normGroup <- ggroup(horizontal=TRUE)
regFrame <- gframe('Analysis and quality measure', horizontal=FALSE)
regGroup <- ggroup(horizontal=TRUE)
verbCheck <- gcheckbox("Verbose", checked=TRUE)
# Verification of optimization parameters
verifyButton <- gbutton("Verify setup", handler = function(h,...){
	enabled(saveButton) <- FALSE
	enabled(startButton) <- FALSE
	# Check basic settings for optimisation
	options(warn=-1)
	if(svalue(regChooser, index=TRUE)>1)
		rp <- as.numeric(strsplit(svalue(regParam), c(","))[[1]])
	else
		rp <- NA
	options(warn=0)
	if(sum(used)==0){
		gmessage("No baseline correction algorithm chosen", title="Not ready", icon = "warning")
	} else if(svalue(regChooser, index=TRUE)==1){
		gmessage("No analysis chosen", title="Not ready", icon = "warning")
	} else if(!rr && nchar(svalue(regParam))==0){
		gmessage("Regression parameter not specified", title="Not ready", icon = "warning")
	} else if(!rr && (!is.finite(sum(rp)))){
		gmessage("Regression parameter incorrectly specified", title="Not ready", icon = "warning")
	} else {
		u <- 0
		faulty <- ""
		for(i in 1:nAlgs){
			if(used[i] == 1){
				for(j in 1:length(parameterList[[i]])){
					m <- as.numeric(strsplit(svalue(parameterList[[i]][[j]][[4]]), c(","))[[1]])
					if((length(m)==0 || sum(is.na(m))>0) && u==0){ # Where did error occur?
						u <- u+1
						faulty <- paste((param[method[[i]]][[1]]$pars[j]), "of baseline correction algorithm", method[[i]])
					}
				}
			}
		}
		if(u==0){
			# Collect data for analysis
			if(!rr){
				if(svalue(regChooser, index=TRUE) == 2){
					require(pls)
					if(cc)
						predictionTest <<- new("PLSRTest", ncomp = as.numeric(svalue(regParam)), cvsegments = cvsegments)
					else
						predictionTest <<- new("PLSRTest", ncomp = as.numeric(svalue(regParam)), cvsegments = cvsegments(dim(X)[1], as.numeric(svalue(segNumber)), type=svalue(segChooser)))
				} else if(svalue(regChooser, index=TRUE) == 3){
					predictionTest <<- new("ridgeRegressionTest", lambda = as.numeric(strsplit(svalue(regParam), c(","))[[1]]))
				}
			}
			bltest <<- list()
			q <- 0
			for(i in 1:nAlgs){
				if(used[i] == 1){
					q <- q+1
					params <- list()
					parNames <- character(length(parameterList[[i]]))
					for(j in 1:length(parameterList[[i]])){
						params[[j]] <- as.numeric(strsplit(svalue(parameterList[[i]][[j]][[4]]), c(","))[[1]])
						parNames[j] <- param[method[[i]]][[1]]$pars[j]
					}
					names(params) <- parNames
					bltest[[q]] <<- new("baselineAlgTest", algorithm = algorithmParse(method[[i]]),
							param = params)
				}
			}
			# Choice of normalisation
			if(!pp){
				if(svalue(postChooser, index=TRUE)==1)
					postproc <<- NULL
				else {
					if(svalue(postChooser, index=TRUE)==2) # Norm (L2)
						postproc <<- function(X){ for(i in 1:dim(X)[1]){ X[i,] <- X[i,]/sqrt(X[i,]%*%X[i,])};X}
					if(svalue(postChooser, index=TRUE)==3) # Mean
						postproc <<- function(X){ for(i in 1:dim(X)[1]){ X[i,] <- X[i,]/mean(X[i,])};X}
					if(svalue(postChooser, index=TRUE)==4) # Median
						postproc <<- function(X){ for(i in 1:dim(X)[1]){ X[i,] <- X[i,]/median(X[i,])};X}
					if(svalue(postChooser, index=TRUE)==5) # Sum
						postproc <<- function(X){ for(i in 1:dim(X)[1]){ X[i,] <- X[i,]/sum(X[i,])};X}
					if(svalue(postChooser, index=TRUE)==6) # Sum of squares
						postproc <<- function(X){ for(i in 1:dim(X)[1]){ X[i,] <- X[i,]/(X[i,]%*%X[i,])};X}
					if(svalue(postChooser, index=TRUE)==7) # L1 postproc
						postproc <<- function(X){ for(i in 1:dim(X)[1]){ X[i,] <- X[i,]/sum(abs(X[i,]))};X}
					if(svalue(postChooser, index=TRUE)==8) # Maximum
						postproc <<- function(X){ for(i in 1:dim(X)[1]){ X[i,] <- X[i,]/max(X[i,])};X}
				}
			}
			enabled(saveButton) <- TRUE
			enabled(startButton) <- TRUE
		} else
			gmessage(paste("Check parameter",faulty), title="Not ready", icon = "warning")
	}
})
saveButton <- gbutton("Save setup", handler = function(h,...){
    assign("baseline.bltests", bltest, .GlobalEnv)
    assign("baseline.predictionTest", predictionTest, .GlobalEnv)
    assign("baseline.postproc", postproc, .GlobalEnv)
	cat(paste("# To run optimization later:\noptimRes <- doOptim(baseline.bltests, X, y, baseline.predictionTest,\n        postproc = baseline.postproc, verbose =", svalue(verbCheck), ", cleanTmp = TRUE)\n"))
})
startButton <- gbutton("START", handler = function(h,...){
	# Run optimisation
    assign("optimRes", doOptim(bltest, X, y, predictionTest,
                               postproc = postproc, verbose = svalue(verbCheck),
                               cleanTmp = TRUE),
           .GlobalEnv)
})
enabled(saveButton) <- FALSE
enabled(startButton) <- FALSE

# Settings for correction, normalization and analysis
addSpace(sGroup1, 10, horizontal=FALSE)
add(methFrame, methodChooser, expand=FALSE)
add(methGroup, methFrame, expand=FALSE)
add(sGroup1, methGroup, expand=FALSE)
addSpace(sGroup1, 15, horizontal=FALSE)
if(!pp)
	add(normFrame, postChooser, expand=FALSE)
else
	add(normFrame, glabel("User specified"), expand=FALSE)
add(normGroup, normFrame, expand=FALSE)
add(sGroup1, normGroup, expand=FALSE)
addSpace(sGroup1, 15, horizontal=FALSE)
if(!rr)
	add(regFrame, regChooser, expand=FALSE)
else
	add(normFrame, glabel("User specified"), expand=FALSE)
add(regGroup, regFrame, expand=FALSE)
add(sGroup1, regGroup, expand=FALSE)

# Settings for optimization
addSpace(sGroup2, 10, horizontal=FALSE)
add(sGroup2, verbCheck, expand=FALSE)
addSpace(sGroup2, 30, horizontal=FALSE)
add(sGroup2, verifyButton, expand=FALSE)
addSpace(sGroup2, 30, horizontal=FALSE)
add(sGroup2, saveButton, expand=FALSE)
addSpace(sGroup2, 10, horizontal=FALSE)
add(sGroup2, startButton, expand=FALSE)

add(sGroup, sGroup1)
addSpace(sGroup, 30, horizontal=TRUE)
add(sGroup, sGroup2)
add(nb, sGroup, label="Settings")
add(main,nb,expand=TRUE)
# add(main,gstatusbar("Tester statusbar"))
}
