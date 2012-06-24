### $Id: baselineGUI.R 192 2012-06-19 08:36:53Z kristl $

## Baseline parameters, expandable list
baselineAlgorithmsGUI <- list()
baselineAlgorithmsGUI$irls					<- as.data.frame(matrix(c(0,10,0.1,5, 5,15,0.1,8, 0,0.5,0.01,0.05, 50,200,25,100), 4,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$irls)		<- list(par=c("lambda1", "lambda2", "wi", "maxit"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$irls$current		 	<- c(5,8,0.05,100)
baselineAlgorithmsGUI$irls$name				<- c("Primary smoothing", "Main smoothing", "Weighting", "Maximum iterations")
baselineAlgorithmsGUI$modpolyfit			<- as.data.frame(matrix(c(0,10,1,4, 0,15,1,4, 25,200,25,100), 3,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$modpolyfit)	<- list(par=c("degree", "tol", "rep"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$modpolyfit$current	<- c(4,4,100)
baselineAlgorithmsGUI$modpolyfit$name		<- c("Polynomial degree", "Update tolerance 10^-", "Max #iterations")
baselineAlgorithmsGUI$als					<- as.data.frame(matrix(c(0,15,1,6, 0,0.5,0.001,0.05), 2,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$als)			<- list(par=c("lambda", "p"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$als$current			<- c(6,0.05)
baselineAlgorithmsGUI$als$name				<- c("Smoothing parameter", "Residual weighting")
baselineAlgorithmsGUI$rollingBall 			<- as.data.frame(matrix(c(0,500,10,300, 0,500,10,200), 2,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$rollingBall)	<- list(par=c("wm", "ws"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$rollingBall$current	<- c(300,200)
baselineAlgorithmsGUI$rollingBall$name		<- c("Min/max window width", "Smoothing window width")
baselineAlgorithmsGUI$medianWindow			<- as.data.frame(matrix(c(0,500,10,300, 0,500,10,200), 2,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$medianWindow)<- list(par=c("hwm", "hws"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$medianWindow$current	<- c(300,200)
baselineAlgorithmsGUI$medianWindow$name		<- c("Median window half width", "Smoothing window half width")
baselineAlgorithmsGUI$fillPeaks		 		<- as.data.frame(matrix(c(0,15,1,6, 10,500,10,50, 1,100,1,10, 100,5000,100,2000), 4,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$fillPeaks)	<- list(par=c("lambda", "hwi", "it", "int"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$fillPeaks$current		<- c(6,50,10,2000)
baselineAlgorithmsGUI$fillPeaks$name		<- c("Primary smoothing", "Half width of local windows", "Maximum number of iterations", "Number of buckets")
baselineAlgorithmsGUI$peakDetection		 	<- as.data.frame(matrix(c(50,500,50,300, 10,200,10,50), 2,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$peakDetection)<- list(par=c("left.right", "lwin.rwin"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$peakDetection$current <- c(300,50)
baselineAlgorithmsGUI$peakDetection$name	<- c("Peak window width", "Smoothing window width")
baselineAlgorithmsGUI$rfbaseline		 	<- as.data.frame(matrix(c(100,5000,10,1000, 1,5,0.5,3.5), 2,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$rfbaseline)	<- list(par=c("NoXP", "b"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$rfbaseline$current	<- c(1000,3.5)
baselineAlgorithmsGUI$rfbaseline$name		<- c("Number of regression points", "Relative weighting")

baselineGUI <- function(spectra, method='irls', labels, rev.x=FALSE){
  require(gWidgets)
  if(exists("baselineAlgorithmsGUI",envir=.GlobalEnv)){
    bAGUI <- get("baselineAlgorithmsGUI",envir=.GlobalEnv)
  } else {
    bAGUI <- baselineAlgorithmsGUI
  }
  
  
  ##
  ## Ititialise variables that are shared between the elements of the GUI:
  ##
  
  
  ## Spectrum parameters
  Y <- spectra; specNo <- 1
  n <- length(Y[1,]); n1 <- length(Y[1,])
  x <- 1:n1
  if(missing(labels)) # X-axis labels
    labels <- 1:n
  
  ## Plotting parameters
  xz <- 1; yz <- 1; xc <- 0; yc <-0
  setZoom <- function(){
    xz <<- 1; yz <<- 1; xc <<- 0; yc <<-0
  }
  gridOn <- FALSE           # Is the grid on?
  visibleZoom <- FALSE      # Has the zoom tools been activated?
  visibleCustomize <- FALSE # Has the parameter customization been activated?
  visibleExport <- FALSE	  # Has the export of all spectra been started?
  
  ##
  ## Define functions that are used by the GUI elements
  ##
  
  
  ## --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
  ## Baseline computation
  baseline.compute <- function(){
    clearPlot()
    ## Compute baseline based on current method and settings
    spec <- list()
    command <- "spec <- baseline(Y[specNo,,drop=FALSE]"
    for(i in 1:dim(bAGUI[[method]])[1]){
      command <- paste(command, ", ", rownames(bAGUI[[method]])[i], "=", bAGUI[[method]]$current[i], sep="")
    }
    command <- paste(command, ", method='", method, "')", sep="")
    eval(parse(text=command))
    ## Kludge to aviod warnings from R CMD check:
    assign("baseline.result", spec, .GlobalEnv)
    .baseline.current <<- list(method=method, parNames=rownames(bAGUI[[method]]), parValues=bAGUI[[method]]$current)
    updatePlot()
  }                                   # end of baseline.compute
  
  ## --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
  ## Clear plot
  clearPlot <- function(){
    par(new = TRUE, mfrow = c(1,1))
    plot(0, 0, xlim=c(-1,1), ylim=c(-1,1), xlab="", ylab="", main="",
         axes=FALSE, col='white')
    par(new=FALSE)
    C <- as.list(par("usr")); names(C) <- c("xmin", "xmax", "ymin", "ymax")
    ##print(unix.time(
    rect(C$xmin, C$ymin, C$xmax, C$ymax, angle = 0, density = 20,
         col = 'white', lwd = 2)
    ##))
    text(0, 0, labels = "Calculating baseline...", col = 'blue', cex = 2)
  }
  
  ## --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
  ## Update plot
  updatePlot <- function() {
    ## FIXME: get is a kludge to avoid warnings from R CMD check:
    plot(get("baseline.result", .GlobalEnv), grid = gridOn, labels = labels, rev.x = rev.x,
         zoom = list(xz = xz, yz = yz, xc = xc, yc = yc))
  }                                   # end of updatePlot
  
  ## --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
  ## Zoom control window
  zoomControl <- function(){
    ## Initialize zoom sliders
    visibleZoom <<- TRUE
    zoomX <- gslider(from=1,to=100,by=.1, value=1, handler = function(h,...){ xz <<- svalue(zoomX); updatePlot()})
    zoomY <- gslider(from=1,to=100,by=.5, value=1, handler = function(h,...){ yz <<- svalue(zoomY); updatePlot()})
    centerX <- gslider(from=-100,to=100,by=.1, value=0, handler = function(h,...){ xc <<- svalue(centerX); updatePlot()})
    centerY <- gslider(from=-100,to=100,by=.1, value=0, handler = function(h,...){ yc <<- svalue(centerY); updatePlot()})
    resetZoom <- gbutton(text = "Reset zoom and center", handler = function(h,...){ svalue(zoomX)<-1; svalue(zoomY)<-1; svalue(centerX)<-0; svalue(centerY)<-0; updatePlot()})
    gridCheck <- gcheckbox('Grid', handler = function(h,...){ gridOn <<- svalue(gridCheck); updatePlot()})
    
    ## Make zoom window
    zoomWindow <- gwindow("Plot properties", width=300)
    superGroup <- ggroup(horizontal=FALSE,container=zoomWindow)
    
    ## Add zoom sliders
    #	add(superGroup,glabel("X"),expand=TRUE)
    subgroupXz <- gframe("X zoom",horizontal=FALSE)
    add(subgroupXz,zoomX,expand=TRUE)
    subgroupXc <- gframe("X center",horizontal=FALSE)
    add(subgroupXc,centerX,expand=TRUE)
    add(superGroup,subgroupXz,expand=TRUE)
    add(superGroup,subgroupXc,expand=TRUE)
    addSpace(superGroup,20,horizontal=FALSE)
    #	add(superGroup,glabel("Y"),expand=TRUE)
    subgroupYz <- gframe("Y zoom",horizontal=FALSE)
    add(subgroupYz,zoomY,expand=TRUE)
    subgroupYc <- gframe("Y center",horizontal=FALSE)
    add(subgroupYc,centerY,expand=TRUE)
    add(superGroup,subgroupYz,expand=TRUE)
    add(superGroup,subgroupYc,expand=TRUE)
    subgroup3 <- ggroup(horizontal=TRUE,expand=TRUE)
    add(subgroup3,resetZoom,expand=TRUE)
    add(subgroup3,gridCheck,expand=FALSE)
    add(superGroup,subgroup3,expand=TRUE)
    addhandlerdestroy(zoomWindow, handler=function(h,...){visibleZoom <<- FALSE})
  }                                   # end of zoomControl
  
  ## --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
  ## Apply to all spectra
  exportControl <- function(){
    visibleExport <<- TRUE
    spec <- list()
    command <- "spec <- baseline(Y"
    for(i in 1:dim(bAGUI[[method]])[1]){
      command <- paste(command, ", ", rownames(bAGUI[[method]])[i], "=", bAGUI[[method]]$current[i], sep="")
    }
    command <- paste(command, ", method='", method, "')", sep="")
    eval(parse(text=command))
    
    exportName <- gedit(text="corrected.spectra", width=20)
    doExport   <- gbutton(text = "Apply and export", handler = function(h,...){the.name <- svalue(exportName); cat("\nCorrecting ..."); the.export <- spec; assign(the.name, the.export,envir = .GlobalEnv);dispose(exportWindow);cat("\nSaved as: ",the.name, sep="")})
    exportWindow <- gwindow("Apply correction to all spectra", width=300)
    superGroup   <- ggroup(horizontal=FALSE,container=exportWindow)
    subgroup     <- gframe("Object name",horizontal=FALSE)
    add(subgroup,exportName,expand=TRUE)
    add(subgroup,doExport,expand=TRUE)
    add(superGroup,subgroup,expand=TRUE)
    addhandlerdestroy(exportWindow, handler=function(h,...){visibleExport <<- FALSE})
  }
  
  ## --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
  ## Slider configuration window
  parameterControl <- function(){
    ## Initialize spectrum type chooser and parameter fields
    visibleCustomize <<- TRUE
    
    saveParameters <- function(){
      theSet <- bAGUI[method][[1]]
      for(i in 1:length(parameterList)){
        for(j in 1:4){
          theSet[i,j] <- svalue(parameterList[[i]][[j]])
        }
      }
      bAGUI[method][[1]][,1:3] <<- theSet[,1:3]
      bAGUI[method][[1]]$current <<- theSet[,4]
      delete(outerParam,remParam); createMethodSliders(); dispose(parameterWindow)# ; baseline.compute()
    }                               # end of saveParameters
    
    addParameterGroup <- function(nameStr, lineNo){
      parameterList[[lineNo]]  <<- c(gedit(text = "", width=1,coerce.with=as.numeric),gedit(width=5,coerce.with=as.numeric),gedit(width=10,coerce.with=as.numeric),gedit(width=15,coerce.with=as.numeric))
      parameterGroup[lineNo+1,1] <<- glabel(text=nameStr)
      size(parameterList[[lineNo]][[1]]) <- c(60,20)
      size(parameterList[[lineNo]][[2]]) <- c(60,20)
      size(parameterList[[lineNo]][[3]]) <- c(60,20)
      size(parameterList[[lineNo]][[4]]) <- c(60,20)
      parameterGroup[lineNo+1,2] <<- parameterList[[lineNo]][[1]]
      parameterGroup[lineNo+1,3] <<- parameterList[[lineNo]][[2]]
      parameterGroup[lineNo+1,4] <<- parameterList[[lineNo]][[3]]
      parameterGroup[lineNo+1,5] <<- parameterList[[lineNo]][[4]]
    }                               # end of addParameterGroup
    
    setParameters <- function(){
      theSet <- bAGUI[method][[1]]
      for(i in 1:length(parameterList)){
        for(j in 1:4){
          svalue(parameterList[[i]][[j]]) <<- theSet[i,j]
        }
      }
    }
    
    saveButton <- gbutton(text = "Apply parameters", handler = function(h,...) saveParameters())
    
    ## Make parameter window
    parameterWindow <- gwindow("Slider configuration", width=300)
    superGroup <- ggroup(horizontal=FALSE,container=parameterWindow)
    #        choiceGroup <- gframe("Type of spectra", container=superGroup, horizontal=FALSE)
    #        add(choiceGroup,typeChooser,expand=FALSE)
    addSpace(superGroup,10,horizontal=FALSE)
    
    ## Add edit fields
    parameterList <- list()
    parameterGroup <- glayout(homogeneous = FALSE, spacing = 5, container=superGroup)
    parameterGroup[1,1] <- glabel("")
    parameterGroup[1,2] <- glabel("From:")
    parameterGroup[1,3] <- glabel("To:")
    parameterGroup[1,4] <- glabel("Spacing:")
    parameterGroup[1,5] <- glabel("Start:")
    nameStrs <- rownames(bAGUI[method][[1]])
    for(i in 1:length(nameStrs))
      addParameterGroup(nameStrs[i],i)
    setParameters()
    visible(parameterGroup) <- TRUE
    addSpring(superGroup)
    
    ## Add buttons
    subgroupButtons <- ggroup(horizontal=TRUE)
    add(subgroupButtons,saveButton,expand=FALSE)
    add(superGroup,subgroupButtons,expand=FALSE)
    addhandlerdestroy(parameterWindow, handler=function(h,...){visibleCustomize <<- FALSE})
  }                                   # end of parameterControl
  
  ## --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
  ## Set up sliders according to chosen baseline correction method
  sVals <- list()
  sliders <- resets <- tmps <- list()
  iI <- 0
  
  createMethodSliders <- function(){
    remParam <<- ggroup(horizontal=FALSE, container=outerParam)
    
    #############
    ## General ##
    #############
    
    ## Collect values for sliders
    sVals <<- bAGUI[[method]]
    sliders <<- resets <<- tmps <<- list()
    iI <<- dim(sVals)[1]
    for(i in 1:iI){
      sliders[[i]] <<- gslider(from=sVals[i,1], to=sVals[i,2], by=sVals[i,3], value=bAGUI[[method]]$current[i],
                              handler = function(h,...){ for(a in 1:iI){bAGUI[[method]]$current[a] <<- svalue(sliders[[a]]); .baseline.current$parValues[a] <<- svalue(sliders[[a]])}})
      resets[[i]]  <<- gbutton(text = "Reset", handler = function(h,...){for(a in 1:iI) svalue(sliders[[a]])<<-sVals[a,4]})
      tmps[[i]]    <<- gframe(paste(sVals[i,6], " (", rownames(sVals)[i], ")", sep=""), container=remParam, horizontal=TRUE)
      add(tmps[[i]], sliders[[i]], expand=TRUE); add(tmps[[i]], resets[[i]], expand=FALSE)
    }
  }                                   # end of createMethodSliders
  
  
  
  ##
  ## Create main GUI window
  ##
  
  
  ## Initialize spectrum chooser slider and method chooser
  if(dim(Y)[1] > 1)
    spectrumNo <- gslider(from=1, to=dim(Y)[1], by=1, value=1, handler = function(h,...) specNo<<-svalue(spectrumNo))
  if(exists("baselineAlgorithms",envir=.GlobalEnv)){
    bA <- get("baselineAlgorithms",envir=.GlobalEnv)
  } else {
    bA <- baselineAlgorithms
  }
  tmp <- sort(names(bAGUI))
  names <- character(length(tmp))
  for(i in 1:length(tmp)){ # Let bAGUI control, and bA have descriptions -------------
    names[i] <- paste("'", ifelse(is.null(bA[[tmp[i]]]@description),"",bA[[tmp[i]]]@description), " (", tmp[i], ")'", sep="")
  }
  methodChooser <- gdroplist(names,
                             selected=which(names(bA)==method), handler = function(h,...){method <<- names(bA)[svalue(methodChooser,index=TRUE)]; delete(outerParam,remParam); createMethodSliders(); setZoom(); baseline.compute()})
  
  ## Initialize window and main containers
  window <- gwindow("Baseline correction", width=300)
  superGroup2 <- ggroup(horizontal=FALSE,container=window)
  if(dim(Y)[1] > 1){
    tmp <- gframe("Spectrum number", container=superGroup2, horizontal=TRUE)
    add(tmp,spectrumNo, expand=TRUE)
  }
  add(superGroup2,methodChooser, expand=FALSE)
  addSpring(superGroup2)
  Settings <- ggroup(container=superGroup2, horizontal=FALSE)
  outerParam <- ggroup(horizontal=FALSE, container=Settings)
  remParam <- ggroup()
  
  createMethodSliders()               # Add algorithm slides
  
  ## Add buttons
  addSpring(superGroup2)
  buttonGroup <- ggroup(horizontal=TRUE, container=superGroup2)
  plotButton <- gbutton(text = "Update plot", handler=function(h,...) baseline.compute())
  add(buttonGroup,plotButton,expand=TRUE)
  newZoom <- gbutton(text = "Zoom", handler = function(h,...){ if(visibleZoom==FALSE) zoomControl() })
  add(buttonGroup,newZoom,expand=FALSE)
  newParameter <- gbutton(text = "Customize", handler = function(h,...){ if(visibleCustomize==FALSE) parameterControl()})
  add(buttonGroup,newParameter,expand=FALSE)
  newExport <- gbutton(text = "Apply to all", handler = function(h,...){ if(visibleExport==FALSE) exportControl()})
  add(buttonGroup,newExport,expand=FALSE)
  
  ## Display initial plot
  plot(0,0, xlim=c(-1,1), ylim=c(-1,1), xlab="", ylab="", main="", axes=FALSE, col='white')
  baseline.compute()
}                                       # end of baselineGUI
