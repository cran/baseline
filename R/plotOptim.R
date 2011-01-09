### $Id: plotOptim.R 182 2011-01-09 21:05:18Z kristl $

plotOptim <- function(results){
## Plot optimisation through GUI

	require(gWidgets)
	require(lattice)

	# Set up main window with internal container
	win <- gwindow("Optimisation results", width=550)
	main <- ggroup(horizontal=FALSE)
	add(win,main)

	# Initialize parameters and paramter lists
	nAlgs <- 0
	progress <- toPlot <- groupPlots <- parameterList <- parameterPlots <- rGroups <- groups <- labels <- plotOne <- plotTwo <- plotFlip <- param <- list()
	oneDimNames <- character(length(results$results))
	param$irls$pars				<- c("lambda1", "lambda2", "wi", "maxit")
	param$modpolyfit$pars		<- c("degree", "tol", "rep")
	param$als$pars				<- c("lambda", "p")
	param$rollingBall$pars		<- c("wm", "ws")
	param$medianWindow$pars		<- c("hwm", "hws")
	param$fillPeaks$pars		<- c("lambda", "hwi", "it", "int")
	param$peakDetection$pars	<- c("left-right", "lwin-rwin")
	param$rfbaseline$pars		<- c("NoXP", "b")

	# Functions for parsing beween method numbers and mehod names
	methodParse2 <- function(m){
		if(m=="baseline.als") 			method <- 1
		if(m=="baseline.fillPeaks") 	method <- 2
		if(m=="baseline.irls") 			method <- 3
		if(m=="baseline.medianWindow") 	method <- 4
		if(m=="baseline.modpolyfit") 	method <- 5
		if(m=="baseline.peakDetection") method <- 6
		if(m=="baseline.rfbaseline") 	method <- 7
		if(m=="baseline.rollingBall") 	method <- 8
		method
	}
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

	# Function for adding a baseline correction method, including sub-functions and variables
	addAlg <- function(nm, result){
		delete(groups[[i]], progress[[i]])
		# Initialize parameters and groups
		nAlgs <<- nAlgs + 1
		name <- methodParse(nm)
		nameLong <- parseMethod(nm)
		method <- name

		# Add some space and the name of the baseline correction algorithm
		addSpace(groups[[nAlgs]],10, horizontal=FALSE)
		add(groups[[nAlgs]],glabel(nameLong))

		# GUI for plotting
		groupPlots[[nAlgs]] <<- ggroup(horizontal=FALSE)

		# Function for setting up parameter plotting array in GUI
		addparameterPlots <- function(nameStr, lineNo){
			if(lineNo>1){ # Baseline parameter
				if(is.null(result@param[[nameStr]])){ # Used default value during optimization
					parameterList[[nAlgs]][[lineNo]]  <<- c(gradio(c('Overall min.','Min.','Avg.','All','Chosen'),horizontal=TRUE), gcheckbox('default', checked=TRUE))
					enabled(parameterList[[nAlgs]][[lineNo]][[1]]) <- FALSE
					tag(parameterList[[nAlgs]][[lineNo]][[1]], "default") <- TRUE
				} else {
					if(max(nchar(as.character(result@param[[nameStr]],scientific=TRUE)))>8){ # More than 8 digits => use scientific format
						parameterList[[nAlgs]][[lineNo]]  <<- c(gradio(c('Overall min.','Min.','Avg.','All','Chosen'),horizontal=TRUE), gcheckboxgroup(format(result@param[[nameStr]], scientific=TRUE, digits=3), horizontal=TRUE))
					} else {
						parameterList[[nAlgs]][[lineNo]]  <<- c(gradio(c('Overall min.','Min.','Avg.','All','Chosen'),horizontal=TRUE), gcheckboxgroup(format(result@param[[nameStr]], scientific=FALSE), horizontal=TRUE))
					}
				}
			} else { # Regression parameter
				if(is.null(result@param[[1]])){
					parameterList[[nAlgs]][[lineNo]]  <<- c(gradio(c('Overall min.','Min.','Avg.','All','Chosen'),horizontal=TRUE), gcheckbox('default', checked=TRUE))
					enabled(parameterList[[nAlgs]][[lineNo]][[1]]) <- FALSE
					tag(parameterList[[nAlgs]][[lineNo]][[1]], "default") <- TRUE
				} else {
					if(max(nchar(as.character(result@param[[1]])))){
						parameterList[[nAlgs]][[lineNo]]  <<- c(gradio(c('Overall min.','Min.','Avg.','All','Chosen'),horizontal=TRUE, selected=4), gcheckboxgroup(format(result@param[[1]], scientific=TRUE, digits=3), horizontal=TRUE))
					} else {
						parameterList[[nAlgs]][[lineNo]]  <<- c(gradio(c('Overall min.','Min.','Avg.','All','Chosen'),horizontal=TRUE, selected=4), gcheckboxgroup(format(result@param[[1]], scientific=FALSE), horizontal=TRUE))
					}
				}
			}

			# Check which buttons should be available for current choices
			tag(parameterList[[nAlgs]][[lineNo]][[1]], "no") <- lineNo
			tag(parameterList[[nAlgs]][[lineNo]][[1]], "alg") <- nAlgs
			tag(parameterList[[nAlgs]][[lineNo]][[1]], "name") <- nameStr
			addhandlerchanged(parameterList[[nAlgs]][[lineNo]][[1]], handler = function(h,...){
				linNo <- tag(h$obj)$no
				nAlg <- tag(h$obj)$alg
				if(svalue(h$obj,index=TRUE) == 1){
					svalue(parameterList[[nAlg]][[linNo]][[2]],index=TRUE) <- NULL}
				else {
					svalue(parameterList[[nAlg]][[linNo]][[2]],index=TRUE) <- 1:length(result@param[[nameStr]])}
				if(svalue(h$obj,index=TRUE) == 1 || svalue(h$obj,index=TRUE) == 4){
					enabled(parameterList[[nAlg]][[linNo]][[2]]) <- FALSE}
				else{
					enabled(parameterList[[nAlg]][[linNo]][[2]]) <- TRUE}
				checkPlot(nAlg)
			})
			tag(parameterList[[nAlgs]][[lineNo]][[2]], "no") <- lineNo
			tag(parameterList[[nAlgs]][[lineNo]][[2]], "alg") <- nAlgs
			tag(parameterList[[nAlgs]][[lineNo]][[2]], "name") <- nameStr
			if(lineNo!=1)
				enabled(parameterList[[nAlgs]][[lineNo]][[2]]) <- FALSE
			if(lineNo==1)
				svalue(parameterList[[nAlgs]][[lineNo]][[2]],index=TRUE) <- 1:length(result@param[[nameStr]])
			addhandlerchanged(parameterList[[nAlgs]][[lineNo]][[2]], handler = function(h,...){
				linNo <- tag(h$obj)$no
				nAlg <- tag(h$obj)$alg
				checkPlot(nAlg)
			})

			# Set up visual optimization array for parameters
			parameterPlots[[nAlgs]][lineNo+1,1] <<- glabel(text=paste(nameStr,":",sep=""))
			parameterPlots[[nAlgs]][lineNo+1,2] <<- parameterList[[nAlgs]][[lineNo]][[1]]
			parameterPlots[[nAlgs]][lineNo+1,3] <<- parameterList[[nAlgs]][[lineNo]][[2]]
		}
		# Set up visual optimization array for parameters
		parameterList[[nAlgs]] <<- list()
		parameterPlots[[nAlgs]] <<- glayout(homogeneous = FALSE, spacing = 5, container=groupPlots[[nAlgs]])
		parameterPlots[[nAlgs]][1,1] <<- glabel("")
		parameterPlots[[nAlgs]][1,2] <<- glabel("Which:")
		parameterPlots[[nAlgs]][1,3] <<- glabel("")
		nameStrs <- c(names(result@param[1]), param[method][[1]]$pars) ##### ##### #### ####### ##### ####
		lns <- length(nameStrs)
		for(i in 1:lns)
			addparameterPlots(nameStrs[i],i)

		addSpace(groups[[nAlgs]], 10, horizontal=FALSE)
		add(groups[[nAlgs]], groupPlots[[nAlgs]], expand=FALSE)

		# Plot buttons
		plotOne[[nAlgs]] <<- gbutton("Curve plot")
		plotTwo[[nAlgs]] <<- gbutton("Level plot")
		tag(plotOne[[nAlgs]], "alg") <- nAlgs
		tag(plotTwo[[nAlgs]], "alg") <- nAlgs

		# Plot curves
		addhandlerchanged(plotOne[[nAlgs]], handler = function(h,...){
			nAlg <- tag(h$obj)$alg
			if(is.vector(toPlot[[nAlg]])){ # Single curve
				plot(names(toPlot[[nAlg]]),toPlot[[nAlg]], type='l', ylab=results$results[[nAlg]]@qualMeasName, xlab=oneDimNames[nAlg])
			} else { # Multiple curves
				if(svalue(plotFlip[[nAlg]])==FALSE){
					plot(rownames(toPlot[[nAlg]]),toPlot[[nAlg]][,1], type='l', xlab=names(dimnames(toPlot[[nAlg]]))[1], ylim=c(min(toPlot[[nAlg]]),max(toPlot[[nAlg]])), ylab=results$results[[nAlg]]@qualMeasName, axes=FALSE)
					axis(1, at=rownames(toPlot[[nAlg]]))
					axis(2)
					box()
					for(i in 2:dim(toPlot[[nAlg]])[2]){
						lines(rownames(toPlot[[nAlg]]),toPlot[[nAlg]][,i], col=i)
					}
					legend(x="topright", legend=colnames(toPlot[[nAlg]]), col=1:dim(toPlot[[nAlg]])[2], lty=1, title=names(dimnames(toPlot[[nAlg]]))[2])
				} else { # Transpose matrix before plotting
					plot(colnames(toPlot[[nAlg]]),toPlot[[nAlg]][1,], type='l', xlab=names(dimnames(toPlot[[nAlg]]))[2], ylim=c(min(toPlot[[nAlg]]),max(toPlot[[nAlg]])), ylab=results$results[[nAlg]]@qualMeasName, axes=FALSE)
					axis(1, at=colnames(toPlot[[nAlg]]))
					axis(2)
					box()
					for(i in 2:dim(toPlot[[nAlg]])[1]){
						lines(colnames(toPlot[[nAlg]]),toPlot[[nAlg]][i,], col=i)
					}
					legend(x="topright", legend=rownames(toPlot[[nAlg]]), col=1:dim(toPlot[[nAlg]])[1], lty=1, title=names(dimnames(toPlot[[nAlg]]))[1])
				}
			}
		})

		# Level plot
		addhandlerchanged(plotTwo[[nAlgs]], handler = function(h,...){
			nAlg <- tag(h$obj)$alg
			Gray <- function(n){gray(seq(0,1, length.out=n))}
			if(svalue(plotFlip[[nAlg]])==FALSE){
				plot(levelplot(toPlot[[nAlg]], xlab=names(dimnames(toPlot[[nAlg]]))[1], ylab=names(dimnames(toPlot[[nAlg]]))[2], col.regions = Gray))
			} else { # Transpose matrix before plotting
				plot(levelplot(t(toPlot[[nAlg]]), xlab=names(dimnames(toPlot[[nAlg]]))[2], ylab=names(dimnames(toPlot[[nAlg]]))[1], col.regions = Gray))
			}
		})
		plotFlip[[nAlgs]] <<- gcheckbox("flip")
		enabled(plotTwo[[nAlgs]]) <<- FALSE
		enabled(plotFlip[[nAlgs]]) <<- FALSE
		rGroups[[nAlgs]] <<- ggroup(horizontal=TRUE)
		add(rGroups[[nAlgs]], plotOne[[nAlgs]],expand=FALSE)
		add(rGroups[[nAlgs]], plotTwo[[nAlgs]],expand=FALSE)
		add(rGroups[[nAlgs]], plotFlip[[nAlgs]],expand=FALSE)
		addSpace(groups[[nAlgs]], 10, horizontal=FALSE)
		add(groups[[nAlgs]], rGroups[[nAlgs]],expand=FALSE)

		# add(nb,groups[[nAlgs]],label=name)
		visible(parameterPlots[[nAlgs]]) <<- TRUE
		checkPlot(nAlgs)
	}

	# Check what can be plotted
	checkPlot <- function(nAlg){
		m <- list(results$results[[nAlg]])
		nam <- ""
		mins <- avg <- character(0)
		j <- 2
		k <- 0
		l <- 0
		# Prepare call to function 'qualMeas'
		for(i in 1:length(parameterList[[nAlg]])){
			def <- tag(parameterList[[nAlg]][[i]][[1]])$default
			if(is.null(def)){
				if(svalue(parameterList[[nAlg]][[i]][[1]])=="Overall min."){
					m[[j]] <- "overall"
					nam[j] <- tag(parameterList[[nAlg]][[i]][[1]])$name
					j <- j+1
				} else
				if(svalue(parameterList[[nAlg]][[i]][[1]])=="All"){
					m[[j]] <- "all"
					nam[j] <- tag(parameterList[[nAlg]][[i]][[1]])$name
					j <- j+1
				} else
				if(svalue(parameterList[[nAlg]][[i]][[1]])=="Chosen"){
					m[[j]] <- svalue(parameterList[[nAlg]][[i]][[2]],index=TRUE)
					nam[j] <- tag(parameterList[[nAlg]][[i]][[1]])$name
					j <- j+1
				} else
				if(svalue(parameterList[[nAlg]][[i]][[1]])=="Min."){
					k <- k+1
					mins[k] <- tag(parameterList[[nAlg]][[i]][[1]])$name
					m[[j]] <- svalue(parameterList[[nAlg]][[i]][[2]],index=TRUE)
					nam[j] <- tag(parameterList[[nAlg]][[i]][[1]])$name
					j <- j+1
				}
				if(svalue(parameterList[[nAlg]][[i]][[1]])=="Avg."){
					l <- l+1
					avg[l] <- tag(parameterList[[nAlg]][[i]][[1]])$name
					m[[j]] <- svalue(parameterList[[nAlg]][[i]][[2]],index=TRUE)
					nam[j] <- tag(parameterList[[nAlg]][[i]][[1]])$name
					j <- j+1
				}
			}
		}            # End for loop
		if(k>0){
			m[[j]] <- mins
			nam[j] <- "MIN"
			j <- j+1
		}
		if(l>0){
			m[[j]] <- avg
			nam[j] <- "AVG"
		}

		names(m) <- nam
		toPlot[[nAlg]] <<- drop(do.call(qualMeas,m)) # What will be plotted?
		if(length(dim(toPlot[[nAlg]]))>2){ # Too many dimensions to plot
			enabled(plotOne[[nAlg]]) <- FALSE
			enabled(plotTwo[[nAlg]]) <- FALSE
			enabled(plotFlip[[nAlg]]) <- FALSE
		} else if((length(dim(toPlot[[nAlg]]))==2) && (prod(dim(toPlot[[nAlg]]))>0)){ # Two dimensions to plot
			enabled(plotOne[[nAlg]]) <- TRUE
			enabled(plotTwo[[nAlg]]) <- TRUE
			enabled(plotFlip[[nAlg]]) <- TRUE
		} else if(is.null(dim(toPlot[[nAlg]])) && (length(toPlot[[nAlg]])>1)){ # One dimension to plot
			enabled(plotOne[[nAlg]]) <- TRUE
			enabled(plotTwo[[nAlg]]) <- FALSE
			enabled(plotFlip[[nAlg]]) <- FALSE
			for(i in 1:length(parameterList[[nAlg]])){
				if((svalue(parameterList[[nAlg]][[i]][[1]])=="Chosen" || svalue(parameterList[[nAlg]][[i]][[1]])=="Avg." || svalue(parameterList[[nAlg]][[i]][[1]])=="All" || svalue(parameterList[[nAlg]][[i]][[1]])=="Avg.") && length(svalue(parameterList[[nAlg]][[i]][[2]]))>1){
					oneDimNames[nAlg] <<- tag(parameterList[[nAlg]][[i]][[1]])$name
				}
			}
		} else { # No dimensions to plot
			enabled(plotOne[[nAlg]]) <- FALSE
			enabled(plotTwo[[nAlg]]) <- FALSE
			enabled(plotFlip[[nAlg]]) <- FALSE
		}
	}

	# ############# #
	# Main notebook #
	# ############# #
	nb <- gnotebook()
	add(main,nb,expand=TRUE)

	# Prepare notebook for algorithms
	for(i in 1:length(results$results)){
		groups[[i]] <- ggroup(horizontal=FALSE)
		name <- methodParse(methodParse2(results$baselineTests[[i]]@algorithm@funcName))
		add(nb,groups[[i]],label=name)
		progress[[i]] <- glabel('Setting up GUI...')
		add(groups[[i]], progress[[i]], expand=FALSE)
	}
	# ######### #
	# Comparing #
	# ######### #
	algNames <- c("-> Choose first result set")
	for(i in 1:length(results$results)){ # Prepare choices for result set chooser
		name <- methodParse(methodParse2(results$baselineTests[[i]]@algorithm@funcName))
		algNames[i+1] <- name
	}

	# Label, button, radio buttons and groups
	compareLabel <- glabel('Compare algorithms')
	curveButton <- gbutton(paste('Plot',results$results[[1]]@qualMeasName,'against',names(results$results[[1]]@param)[1]), handler = function(h,...){
			ns <- list()
			if(svalue(minBest,index=TRUE)==3){
				nams <- character(length(results$results))
				for(i in 1:length(results$results)){
					nam <- c("", names(results$results[[1]]@param)[1], "AVG")
					m <- list(results$results[[i]])
					minOver <- svalue(minWhich,index=TRUE)
					m[[2]] <- minOver
					m[[3]] <- names(results$results[[1]]@param)[1]
					names(m) <- nam
					ns.tmp <- do.call(qualMeas,m)
					the.min <- which(ns.tmp==min(ns.tmp), arr.ind = TRUE)
					the.dim <- dimnames(ns.tmp)
					
					nam <- c("")
					m <- list(results$results[[i]])
					nam <- append(nam, names(results$results[[1]]@param)[1])
					m[[2]]   <- "all"
					for(j in 2:length(results$results[[i]]@param)){
						nam <- append(nam, names(results$results[[i]]@param)[j])
						m[[j+1]] <- the.min[j]
					}
					names(m) <- nam
					ns[[i]] <- drop(do.call(qualMeas,m))
					nams[i] <- methodParse(methodParse2(results$baselineTests[[i]]@algorithm@funcName))
				}
			} else {
				nams <- character(length(results$results))
				nam <- c("", names(results$results[[1]]@param)[1], "DEFAULT")
				for(i in 1:length(results$results)){
					m <- list(results$results[[i]])
					m[[2]] <- "all"
					if(svalue(minBest)=="Overall min.")
						m[[3]] <- "overall.min"
					else
						m[[3]] <- "cond.min"
					names(m) <- nam
					ns[[i]] <- drop(do.call(qualMeas,m))
					nams[i] <- methodParse(methodParse2(results$baselineTests[[i]]@algorithm@funcName))
				}
			}
			plot(names(ns[[1]]),ns[[1]], type='l', xlab=nam[2], ylab=results$results[[1]]@qualMeasName)
			legend(x="topright", legend=nams, col=1:length(ns), lty=1)
			if(length(ns)>1){
				for(i in 2:length(ns)){
					lines(names(ns[[i]]),ns[[i]], col=i)
				}
			}
		})
#	minBest <- gradio(c("overall.min","cond.min"), horizontal=TRUE)
	result <- results$results[[1]]
	method <- methodParse(methodParse2(results$baselineTests[[1]]@algorithm@funcName))
	nameStrs <- c(names(result@param[1]), param[method][[1]]$pars)
	if(max(nchar(as.character(result@param[[nameStrs[1]]],scientific=TRUE)))>8){ # More than 8 digits => use scientific format
		minBest  <- gradio(c("Overall min.","Min.", "Min. avg."))
		minWhich <- gcheckboxgroup(format(result@param[[nameStrs[1]]], checked=!logical(length(result@param[[nameStrs[1]]])), scientific=TRUE, digits=3), horizontal=TRUE)
		enabled(minWhich) <- FALSE
	} else {
		minBest  <- gradio(c("Overall min.","Min.", "Min. avg."))
		minWhich <- gcheckboxgroup(result@param[[nameStrs[1]]], checked=logical(length(result@param[[nameStrs[1]]])), horizontal=TRUE)
		enabled(minWhich) <- FALSE}
	addhandlerchanged(minBest, handler = function(h,...){
		if(svalue(minBest,index=TRUE)==3){
			enabled(minWhich) <- TRUE
			svalue(minWhich, index=TRUE) <- 1:length(result@param[[nameStrs[1]]])
		} else {
			enabled(minWhich) <- FALSE
			svalue(minWhich, index=TRUE) <- integer()}
	})
	addhandlerchanged(minWhich, handler = function(h,...){
		if(length(svalue(minWhich, index=TRUE)) == 0 && svalue(minBest,index=TRUE)==3){
			enabled(curveButton) <- FALSE
		} else {
			enabled(curveButton) <- TRUE}
	})
	
	sGroup <- ggroup(horizontal=FALSE)
	mGroup <- ggroup(horizontal=TRUE)
	
	# Combine elements
	addSpace(sGroup,10,horizontal=FALSE)
	add(sGroup,compareLabel,expand=FALSE)
	addSpace(sGroup,10,horizontal=FALSE)
	add(mGroup,minBest,expand=FALSE)
	add(mGroup,minWhich,expand=FALSE)
	addSpace(mGroup,10,horizontal=TRUE)
	add(mGroup,curveButton,expand=FALSE)
	add(sGroup,mGroup,expand=FALSE)
	add(nb, sGroup, label="Compare")

	# Build GUI for algorithms
	for(i in 1:length(results$results)){
		nm <- methodParse2(results$baselineTests[[i]]@algorithm@funcName)
		addAlg(nm, results$results[[i]]);
	}
}
