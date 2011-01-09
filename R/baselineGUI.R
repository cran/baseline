### $Id: baselineGUI.R 170 2011-01-03 20:38:25Z bhm $
baselineGUI <- function(spectra, method='irls', labels, rev.x=FALSE){
    require(gWidgets)


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

    ## Baseline parameters - spectypes is numeric (1-4) in this order "Custom", "MALDI-TOF", "NMR", "Raman"
    param <- list(irls=0,modpolyfit=0,als=0,rollingBall=0,medianWindow=0,fillPeaks=0,rfbaseline=0)
    param$irls <- list(custom=0,MALDI=0,NMR=0,Raman=0,current=0)
    param$irls$pars		<- c("lambda1", "lambda2", "wi", "maxit")
    param$irls$custom		<- matrix(c(0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0), 4,4, byrow=TRUE)
    param$irls$MALDI 		<- matrix(c(0,10,0.1,5, 5,15,0.1,8, 0,0.5,0.01,0.05, 50,200,25,100), 4,4, byrow=TRUE)
    param$irls$NMR	 	<- matrix(c(0,10,0.1,5, 5,15,0.1,8, 0,0.5,0.01,0.05, 50,200,25,100), 4,4, byrow=TRUE)
    param$irls$Raman 		<- matrix(c(0,10,0.1,5, 5,15,0.1,8, 0,0.5,0.01,0.05, 50,200,25,100), 4,4, byrow=TRUE)
    param$irls$current		<- list(specType=2, slide=c(5,8,0.05,100))
    param$modpolyfit <- list(custom=0,MALDI=0,NMR=0,Raman=0,current=0)
    param$modpolyfit$pars	<- c("degree", "tol", "rep")
    param$modpolyfit$custom 	<- matrix(c(0,0,0,0, 0,0,0,0, 0,0,0,0), 3,4, byrow=TRUE)
    param$modpolyfit$MALDI 	<- matrix(c(0,10,1,4, 0,15,1,4, 25,200,25,100), 3,4, byrow=TRUE)
    param$modpolyfit$NMR	<- matrix(c(0,10,1,4, 0,15,1,4, 25,200,25,100), 3,4, byrow=TRUE)
    param$modpolyfit$Raman	<- matrix(c(0,10,1,4, 0,15,1,4, 25,200,25,100), 3,4, byrow=TRUE)
    param$modpolyfit$current	<- list(specType=2, slide=c(4,4,100))
    param$als <- list(custom=0,MALDI=0,NMR=0,Raman=0,current=0)
    param$als$pars		<- c("lambda", "p")
    param$als$custom 		<- matrix(c(0,0,0,0, 0,0,0,0), 2,4, byrow=TRUE)
    param$als$MALDI 		<- matrix(c(0,15,1,6, 0,0.5,0.001,0.05), 2,4, byrow=TRUE)
    param$als$NMR 		<- matrix(c(0,15,1,6, 0,0.5,0.001,0.05), 2,4, byrow=TRUE)
    param$als$Raman 		<- matrix(c(0,15,1,6, 0,0.5,0.001,0.05), 2,4, byrow=TRUE)
    param$als$current		<- list(specType=2, slide=c(6,0.05))
    param$rollingBall <- list(custom=0,MALDI=0,NMR=0,Raman=0,current=0)
    param$rollingBall$pars	<- c("wm", "ws")
    param$rollingBall$custom 	<- matrix(c(0,0,0,0, 0,0,0,0), 2,4, byrow=TRUE)
    param$rollingBall$MALDI 	<- matrix(c(0,500,10,300, 0,500,10,200), 2,4, byrow=TRUE)
    param$rollingBall$NMR 	<- matrix(c(0,500,10,300, 0,500,10,200), 2,4, byrow=TRUE)
    param$rollingBall$Raman 	<- matrix(c(0,500,10,300, 0,500,10,200), 2,4, byrow=TRUE)
    param$rollingBall$current	<- list(specType=2, slide=c(300,200))
    param$medianWindow <- list(custom=0,MALDI=0,NMR=0,Raman=0,current=0)
    param$medianWindow$pars	<- c("hwm", "hws")
    param$medianWindow$custom	<- matrix(c(0,0,0,0, 0,0,0,0), 2,4, byrow=TRUE)
    param$medianWindow$MALDI 	<- matrix(c(0,500,10,300, 0,500,10,200), 2,4, byrow=TRUE)
    param$medianWindow$NMR	<- matrix(c(0,500,10,300, 0,500,10,200), 2,4, byrow=TRUE)
    param$medianWindow$Raman 	<- matrix(c(0,500,10,300, 0,500,10,200), 2,4, byrow=TRUE)
    param$medianWindow$current	<- list(specType=2, slide=c(300,200))
    param$fillPeaks <- list(custom=0,MALDI=0,NMR=0,Raman=0,current=0)
    param$fillPeaks$pars		<- c("lambda", "hwi", "it", "int")
    param$fillPeaks$custom	<- matrix(c(0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0), 4,4, byrow=TRUE)
    param$fillPeaks$MALDI 	<- matrix(c(0,15,1,6, 10,500,10,50, 1,100,1,10, 100,5000,100,2000), 4,4, byrow=TRUE)
    param$fillPeaks$NMR	 	<- matrix(c(0,15,1,6, 10,500,10,50, 1,100,1,10, 100,5000,100,2000), 4,4, byrow=TRUE)
    param$fillPeaks$Raman 	<- matrix(c(0,15,1,6, 10,500,10,50, 1,100,1,10, 100,5000,100,2000), 4,4, byrow=TRUE)
    param$fillPeaks$current	<- list(specType=2, slide=c(6,50,10,2000))
    param$peakDetection <- list(custom=0,MALDI=0,NMR=0,Raman=0,current=0)
    param$peakDetection$pars	<- c("left-right", "lwin-rwin")
    param$peakDetection$custom 	<- matrix(c(0,0,0,0, 0,0,0,0), 2,4, byrow=TRUE)
    param$peakDetection$MALDI 	<- matrix(c(50,500,50,300, 10,200,10,50), 2,4, byrow=TRUE)
    param$peakDetection$NMR 	<- matrix(c(50,500,50,300, 10,200,10,50), 2,4, byrow=TRUE)
    param$peakDetection$Raman 	<- matrix(c(50,500,50,300, 10,200,10,50), 2,4, byrow=TRUE)
    param$peakDetection$current	<- list(specType=2, slide=c(300,50))
    param$rfbaseline <- list(custom=0,MALDI=0,NMR=0,Raman=0,current=0)
    param$rfbaseline$pars	<- c("NoXP", "b")
    param$rfbaseline$custom 	<- matrix(c(0,0,0,0, 0,0,0,0), 2,4, byrow=TRUE)
    param$rfbaseline$MALDI 	<- matrix(c(100,5000,10,1000, 1,5,0.5,3.5), 2,4, byrow=TRUE)
    param$rfbaseline$NMR 	<- matrix(c(100,5000,10,1000, 1,5,0.5,3.5), 2,4, byrow=TRUE)
    param$rfbaseline$Raman 	<- matrix(c(100,5000,10,1000, 1,5,0.5,3.5), 2,4, byrow=TRUE)
    param$rfbaseline$current	<- list(specType=2, slide=c(1000,3.5))


    ##
    ## Define functions that are used by the GUI elements
    ##


    ## --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
    ## Baseline computation
    baseline.compute <- function(){
        clearPlot()
	## Compute baseline based on current method and settings
        blocal <- function(method){
            switch(method,
                   irls = baseline(Y[specNo,,drop=FALSE], lambda1=param$irls$current$slide[1], lambda2=param$irls$current$slide[2], wi=param$irls$current$slide[3], maxit=param$irls$current$slide[4], method='irls'),
                   modpolyfit = baseline(Y[specNo,,drop=FALSE], degree=param$modpolyfit$current$slide[1], tol=10^(-param$modpolyfit$current$slide[2]), rep=param$modpolyfit$current$slide[3], method='modpolyfit'),
                   als = baseline(Y[specNo,,drop=FALSE], lambda=param$als$current$slide[1], p=param$als$current$slide[2], method='als'),
                   rollingBall = baseline(Y[specNo,,drop=FALSE], wm=param$rollingBall$current$slide[1], ws=param$rollingBall$current$slide[2], method='rollingBall'),
                   medianWindow = baseline(Y[specNo,,drop=FALSE], hwm=param$medianWindow$current$slide[1], hws=param$medianWindow$current$slide[2], method='medianWindow'),
                   fillPeaks = baseline(Y[specNo,,drop=FALSE], lambda=param$fillPeaks$current$slide[1], hwi=param$fillPeaks$current$slide[2], it=param$fillPeaks$current$slide[3], int=param$fillPeaks$current$slide[4], method='fillPeaks'),
                   rfbaseline = baseline(Y[specNo,,drop=FALSE], span=NULL, NoXP=param$rfbaseline$current$slide[1], b=param$rfbaseline$current$slide[2], method='rfbaseline'),
                   peakDetection = baseline(Y[specNo,,drop=FALSE], left=param$peakDetection$current$slide[1], right=param$peakDetection$current$slide[1], lwin=param$peakDetection$current$slide[2], rwin=param$peakDetection$current$slide[2], method='peakDetection')
                   )
        }                            # end of blocal
        ## Kludge to aviod warnings from R CMD check:
        assign("baseline.result", blocal(method), .GlobalEnv)
        .baseline.current <<- list(method=method, parNames=param[[method]]$pars, parValues=param[[method]]$current$slide)
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
		clocal <- function(method){
            switch(method,
                   irls = baseline(Y, lambda1=param$irls$current$slide[1], lambda2=param$irls$current$slide[2], wi=param$irls$current$slide[3], maxit=param$irls$current$slide[4], method='irls'),
                   modpolyfit = baseline(Y, degree=param$modpolyfit$current$slide[1], tol=10^(-param$modpolyfit$current$slide[2]), rep=param$modpolyfit$current$slide[3], method='modpolyfit'),
                   als = baseline(Y, lambda=param$als$current$slide[1], p=param$als$current$slide[2], method='als'),
                   rollingBall = baseline(Y, wm=param$rollingBall$current$slide[1], ws=param$rollingBall$current$slide[2], method='rollingBall'),
                   medianWindow = baseline(Y, hwm=param$medianWindow$current$slide[1], hws=param$medianWindow$current$slide[2], method='medianWindow'),
                   fillPeaks = baseline(Y, lambda=param$fillPeaks$current$slide[1], hwi=param$fillPeaks$current$slide[2], it=param$fillPeaks$current$slide[3], int=param$fillPeaks$current$slide[4], method='fillPeaks'),
                   rfbaseline = baseline(Y, span=NULL, NoXP=param$rfbaseline$current$slide[1], b=param$rfbaseline$current$slide[2], method='rfbaseline'),
                   peakDetection = baseline(Y, left=param$peakDetection$current$slide[1], right=param$peakDetection$current$slide[1], lwin=param$peakDetection$current$slide[2], rwin=param$peakDetection$current$slide[2], method='peakDetection')
                   )
        }                            # end of clocal

        exportName <- gedit(text="corrected.spectra", width=20)
		doExport   <- gbutton(text = "Apply and export", handler = function(h,...){the.name <- svalue(exportName); cat("\nCorrecting ..."); the.export <- clocal(method); assign(the.name, the.export,envir = .GlobalEnv);dispose(exportWindow);cat("\nSaved as: ",the.name, sep="")})
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

        typeChooser <- gdroplist(c("Custom", "MALDI-TOF", "NMR", "Raman"),
                                 selected=param[method][[1]]$current$specType, handler = function(h,...){param[method][[1]]$current$specType <<- svalue(typeChooser,index=TRUE);setParameters()})

        saveParameters <- function(){
            theSet <- param[method][[1]][specTypeParse(param[method][[1]]$current$specType)][[1]]
            for(i in 1:length(parameterList)){
                for(j in 1:4){
                    theSet[i,j] <- svalue(parameterList[[i]][[j]])
                }
            }
            param[method][[1]]["custom"][[1]] <<- theSet
            param[method][[1]]$current$specType <<- 1
            param[method][[1]]$current$slide <<- theSet[,4]
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
            theSet <- param[method][[1]][specTypeParse(param[method][[1]]$current$specType)][[1]]
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
        choiceGroup <- gframe("Type of spectra", container=superGroup, horizontal=FALSE)
        add(choiceGroup,typeChooser,expand=FALSE)
        addSpace(superGroup,10,horizontal=FALSE)

        ## Add edit fields
        parameterList <- list()
        parameterGroup <- glayout(homogeneous = FALSE, spacing = 5, container=superGroup)
        parameterGroup[1,1] <- glabel("")
        parameterGroup[1,2] <- glabel("From:")
        parameterGroup[1,3] <- glabel("To:")
        parameterGroup[1,4] <- glabel("Spacing:")
        parameterGroup[1,5] <- glabel("Start:")
        nameStrs <- param[method][[1]]$pars
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
        if(m=="als") 			method <- 1
        if(m=="fillPeaks") 		method <- 2
        if(m=="irls") 			method <- 3
        if(m=="medianWindow") 	method <- 4
        if(m=="modpolyfit") 	method <- 5
        if(m=="peakDetection") 	method <- 6
        if(m=="rfbaseline") 	method <- 7
        if(m=="rollingBall") 	method <- 8
        method
    }
    specTypeParse <- function(m){
        if(m==1) method <- "custom"
        if(m==2) method <- "MALDI"
        if(m==3) method <- "NMR"
        if(m==4) method <- "Raman"
        method
    }

    ## --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
    ## Set up sliders according to chosen baseline correction method
    createMethodSliders <- function(){
        remParam <<- ggroup(horizontal=FALSE, container=outerParam)

	##########
	## IRLS ##
	##########
        if(method=="irls"){
            ## Collect values for sliders
            sVals <- param$irls[param$irls$current$specType][[1]]

            ## Initialize parameter sliders and reset buttons
            slider1 <- gslider(from=sVals[1,1],to=sVals[1,2],by=sVals[1,3], value=param$irls$current$slide[1], handler = function(h,...){ param$irls$current$slide[1] <<- svalue(slider1); .baseline.current$parValues[1] <<- svalue(slider1)})
            slider2 <- gslider(from=sVals[2,1],to=sVals[2,2],by=sVals[2,3], value=param$irls$current$slide[2], handler = function(h,...){ param$irls$current$slide[2] <<- svalue(slider2); .baseline.current$parValues[2] <<- svalue(slider2)})
            slider3 <- gslider(from=sVals[3,1],to=sVals[3,2],by=sVals[3,3], value=param$irls$current$slide[3], handler = function(h,...){ param$irls$current$slide[3] <<- svalue(slider3); .baseline.current$parValues[3] <<- svalue(slider3)})
            slider4 <- gslider(from=sVals[4,1],to=sVals[4,2],by=sVals[4,3], value=param$irls$current$slide[4], handler = function(h,...){ param$irls$current$slide[4] <<- svalue(slider4); .baseline.current$parValues[4] <<- svalue(slider4)})
            reset1 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider1)<<-sVals[1,4]})
            reset2 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider2)<<-sVals[2,4]})
            reset3 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider3)<<-sVals[3,4]})
            reset4 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider4)<<-sVals[4,4]})

            ## Add parameter sliders
            tmp <- gframe("Primary smoothing (lambda1)", container=remParam, horizontal=TRUE)
            add(tmp,slider1, expand=TRUE); add(tmp,reset1, expand=FALSE)
            tmp <- gframe("Main smoothing (lambda2)", container=remParam, horizontal=TRUE)
            add(tmp,slider2, expand=TRUE); add(tmp,reset2, expand=FALSE)
            tmp <- gframe("Weighting (wi)", container=remParam, horizontal=TRUE)
            add(tmp,slider3, expand=TRUE); add(tmp,reset3, expand=FALSE)
            tmp <- gframe("Maximum iterations (maxit)", container=remParam, horizontal=TRUE)
            add(tmp,slider4, expand=TRUE); add(tmp,reset4, expand=FALSE)
        }

	################
	## modpolyfit ##
	################
        if(method=="modpolyfit"){
            ## Collect values for sliders
            sVals <- param$modpolyfit[param$modpolyfit$current$specType][[1]]

            ## Initialize parameter sliders and reset buttons
            slider1 <- gslider(from=sVals[1,1],to=sVals[1,2],by=sVals[1,3], value=param$modpolyfit$current$slide[1], handler = function(h,...){ param$modpolyfit$current$slide[1] <<- svalue(slider1); .baseline.current$parValues[1] <<- svalue(slider1)})
            slider2 <- gslider(from=sVals[2,1],to=sVals[2,2],by=sVals[2,3], value=param$modpolyfit$current$slide[2], handler = function(h,...){ param$modpolyfit$current$slide[2] <<- svalue(slider2); .baseline.current$parValues[2] <<- svalue(slider2)})
            slider3 <- gslider(from=sVals[3,1],to=sVals[3,2],by=sVals[3,3], value=param$modpolyfit$current$slide[3], handler = function(h,...){ param$modpolyfit$current$slide[3] <<- svalue(slider3); .baseline.current$parValues[3] <<- svalue(slider3)})
            reset1 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider1)<<-sVals[1,4]})
            reset2 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider2)<<-sVals[2,4]})
            reset3 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider3)<<-sVals[3,4]})

            ## Add parameter sliders
            tmp <- gframe("Polynomial degree (degree)", container=remParam, horizontal=TRUE)
            add(tmp,slider1, expand=TRUE); add(tmp,reset1, expand=FALSE)
            tmp <- gframe("Update tolerance (10^(-tol))", container=remParam, horizontal=TRUE)
            add(tmp,slider2, expand=TRUE); add(tmp,reset2, expand=FALSE)
            tmp <- gframe("Max #iteraions (rep)", container=remParam, horizontal=TRUE)
            add(tmp,slider3, expand=TRUE); add(tmp,reset3, expand=FALSE)
        }

	#########
	## ALS ##
	#########
        if(method=="als"){
            ## Collect values for sliders
            sVals <- param$als[param$als$current$specType][[1]]

            ## Initialize parameter sliders and reset buttons
            slider1 <- gslider(from=sVals[1,1],to=sVals[1,2],by=sVals[1,3], value=param$als$current$slide[1], handler = function(h,...){ param$als$current$slide[1] <<- svalue(slider1); .baseline.current$parValues[1] <<- svalue(slider1)})
            slider2 <- gslider(from=sVals[2,1],to=sVals[2,2],by=sVals[2,3], value=param$als$current$slide[2], handler = function(h,...){ param$als$current$slide[2] <<- svalue(slider2); .baseline.current$parValues[2] <<- svalue(slider2)})
            reset1 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider1)<<-sVals[1,4]})
            reset2 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider2)<<-sVals[2,4]})

            ## Add parameter sliders
            tmp <- gframe("Smoothing parameter (lambda)", container=remParam, horizontal=TRUE)
            add(tmp,slider1, expand=TRUE); add(tmp,reset1, expand=FALSE)
            tmp <- gframe("Residual weighting (p)", container=remParam, horizontal=TRUE)
            add(tmp,slider2, expand=TRUE); add(tmp,reset2, expand=FALSE)
        }

	#################
	## rollingBall ##
	#################
        if(method=="rollingBall"){
            ## Collect values for sliders
            sVals <- param$rollingBall[param$rollingBall$current$specType][[1]]

            ## Initialize parameter sliders and reset buttons
            slider1 <- gslider(from=sVals[1,1],to=sVals[1,2],by=sVals[1,3], value=param$rollingBall$current$slide[1], handler = function(h,...){ param$rollingBall$current$slide[1] <<- svalue(slider1); .baseline.current$parValues[1] <<- svalue(slider1)})
            slider2 <- gslider(from=sVals[2,1],to=sVals[2,2],by=sVals[2,3], value=param$rollingBall$current$slide[2], handler = function(h,...){ param$rollingBall$current$slide[2] <<- svalue(slider2); .baseline.current$parValues[1] <<- svalue(slider2)})
            reset1 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider1)<<-sVals[1,4]})
            reset2 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider2)<<-sVals[2,4]})

            ## Add parameter sliders
            tmp <- gframe("Min/max window width (wm)", container=remParam, horizontal=TRUE)
            add(tmp,slider1, expand=TRUE); add(tmp,reset1, expand=FALSE)
            tmp <- gframe("Smoothing window width (ws)", container=remParam, horizontal=TRUE)
            add(tmp,slider2, expand=TRUE); add(tmp,reset2, expand=FALSE)
        }

	##################
	## medianWindow ##
	##################
        if(method=="medianWindow"){
            ## Collect values for sliders
            sVals <- param$medianWindow[param$medianWindow$current$specType][[1]]

            ## Initialize parameter sliders and reset buttons
            slider1 <- gslider(from=sVals[1,1],to=sVals[1,2],by=sVals[1,3], value=param$medianWindow$current$slide[1], handler = function(h,...){ param$medianWindow$current$slide[1] <<- svalue(slider1); .baseline.current$parValues[1] <<- svalue(slider1)})
            slider2 <- gslider(from=sVals[2,1],to=sVals[2,2],by=sVals[2,3], value=param$medianWindow$current$slide[2], handler = function(h,...){ param$medianWindow$current$slide[2] <<- svalue(slider2); .baseline.current$parValues[2] <<- svalue(slider2)})
            reset1 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider1)<<-sVals[1,4]})
            reset2 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider2)<<-sVals[2,4]})

            ## Add parameter sliders
            tmp <- gframe("Median window half width (hwm)", container=remParam, horizontal=TRUE)
            add(tmp,slider1, expand=TRUE); add(tmp,reset1, expand=FALSE)
            tmp <- gframe("Smoothing window half width (hws)", container=remParam, horizontal=TRUE)
            add(tmp,slider2, expand=TRUE); add(tmp,reset2, expand=FALSE)
        }

	##############
	## fillPeaks ##
	##############
        if(method=="fillPeaks"){
            ## Collect values for sliders
            sVals <- param$fillPeaks[param$fillPeaks$current$specType][[1]]

            ## Initialize parameter sliders and reset buttons
            slider1 <- gslider(from=sVals[1,1],to=sVals[1,2],by=sVals[1,3], value=param$fillPeaks$current$slide[1], handler = function(h,...){ param$fillPeaks$current$slide[1] <<- svalue(slider1); .baseline.current$parValues[1] <<- svalue(slider1)})
            slider2 <- gslider(from=sVals[2,1],to=sVals[2,2],by=sVals[2,3], value=param$fillPeaks$current$slide[2], handler = function(h,...){ param$fillPeaks$current$slide[2] <<- svalue(slider2); .baseline.current$parValues[2] <<- svalue(slider2)})
            slider3 <- gslider(from=sVals[3,1],to=sVals[3,2],by=sVals[3,3], value=param$fillPeaks$current$slide[3], handler = function(h,...){ param$fillPeaks$current$slide[3] <<- svalue(slider3); .baseline.current$parValues[3] <<- svalue(slider3)})
            slider4 <- gslider(from=sVals[4,1],to=sVals[4,2],by=sVals[4,3], value=param$fillPeaks$current$slide[4], handler = function(h,...){ param$fillPeaks$current$slide[4] <<- svalue(slider4); .baseline.current$parValues[4] <<- svalue(slider4)})
            reset1 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider1)<<-sVals[1,4]})
            reset2 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider2)<<-sVals[2,4]})
            reset3 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider3)<<-sVals[3,4]})
            reset4 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider4)<<-sVals[4,4]})

            ## Add parameter sliders
            tmp <- gframe("Primary smoothing (lambda)", container=remParam, horizontal=TRUE)
            add(tmp,slider1, expand=TRUE); add(tmp,reset1, expand=FALSE)
            tmp <- gframe("Half width of local windows (hwi)", container=remParam, horizontal=TRUE)
            add(tmp,slider2, expand=TRUE); add(tmp,reset2, expand=FALSE)
            tmp <- gframe("Maximum number of iterations (it)", container=remParam, horizontal=TRUE)
            add(tmp,slider3, expand=TRUE); add(tmp,reset3, expand=FALSE)
            tmp <- gframe("Number of buckets (int)", container=remParam, horizontal=TRUE)
            add(tmp,slider4, expand=TRUE); add(tmp,reset4, expand=FALSE)
        }

	################
	## rfbaseline ##
	################
        if(method=="rfbaseline"){
            ## Collect values for sliders
            sVals <- param$rfbaseline[param$rfbaseline$current$specType][[1]]

            ## Initialize parameter sliders and reset buttons
            slider1 <- gslider(from=sVals[1,1],to=sVals[1,2],by=sVals[1,3], value=param$rfbaseline$current$slide[1], handler = function(h,...){ param$rfbaseline$current$slide[1] <<- svalue(slider1); .baseline.current$parValues[1] <<- svalue(slider1)})
            slider2 <- gslider(from=sVals[2,1],to=sVals[2,2],by=sVals[2,3], value=param$rfbaseline$current$slide[2], handler = function(h,...){ param$rfbaseline$current$slide[2] <<- svalue(slider2); .baseline.current$parValues[2] <<- svalue(slider2)})
            reset1 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider1)<<-sVals[1,4]})
            reset2 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider2)<<-sVals[2,4]})

            ## Add parameter sliders
            tmp <- gframe("Number of regression points (NoXP)", container=remParam, horizontal=TRUE)
            add(tmp,slider1, expand=TRUE); add(tmp,reset1, expand=FALSE)
            tmp <- gframe("Relative weighting (b)", container=remParam, horizontal=TRUE)
            add(tmp,slider2, expand=TRUE); add(tmp,reset2, expand=FALSE)
        }

	###################
	## peakDetection ##
	###################
        if(method=="peakDetection"){
            ## Collect values for sliders
            sVals <- param$peakDetection[param$peakDetection$current$specType][[1]]

            ## Initialize parameter sliders and reset buttons
            slider1 <- gslider(from=sVals[1,1],to=sVals[1,2],by=sVals[1,3], value=param$peakDetection$current$slide[1], handler = function(h,...){ param$peakDetection$current$slide[1] <<- svalue(slider1); .baseline.current$parValues[1] <<- svalue(slider1)})
            slider2 <- gslider(from=sVals[2,1],to=sVals[2,2],by=sVals[2,3], value=param$peakDetection$current$slide[2], handler = function(h,...){ param$peakDetection$current$slide[2] <<- svalue(slider2); .baseline.current$parValues[2] <<- svalue(slider2)})
            reset1 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider1)<<-sVals[1,4]})
            reset2 <- gbutton(text = "Reset", handler = function(h,...){svalue(slider2)<<-sVals[2,4]})

            ## Add parameter sliders
            tmp <- gframe("Peak window width (left-right)", container=remParam, horizontal=TRUE)
            add(tmp,slider1, expand=TRUE); add(tmp,reset1, expand=FALSE)
            tmp <- gframe("Smoothing window width (lwin-rwin)", container=remParam, horizontal=TRUE)
            add(tmp,slider2, expand=TRUE); add(tmp,reset2, expand=FALSE)
        }
    }                                   # end of createMethodSliders


    ##
    ## Create main GUI window
    ##


    ## Initialize spectrum chooser slider and method chooser
    if(dim(Y)[1] > 1)
        spectrumNo <- gslider(from=1, to=dim(Y)[1], by=1, value=1, handler = function(h,...) specNo<<-svalue(spectrumNo))
    methodChooser <- gdroplist(c("Asymmetric Least Squares (als)", "Fill peaks (fillPeaks)", "Iterative restricted least squares (irls)",
                                 "Local medians (medianWindow)", "Iterative polynomial fitting (modpolyfit)", "Peak Detection (peakDetection)", "Robust baseline estimation (rfbaseline)", "Rolling ball (rollingBall)"),
                               selected=parseMethod(method), handler = function(h,...){method <<- methodParse(svalue(methodChooser,index=TRUE)); delete(outerParam,remParam); createMethodSliders(); setZoom(); baseline.compute()})

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
