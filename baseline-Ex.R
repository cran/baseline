pkgname <- "baseline"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('baseline')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("PLSRTest-class")
### * PLSRTest-class

flush(stderr()); flush(stdout())

### Name: PLSRTest-class
### Title: Class "PLSRTest"
### Aliases: PLSRTest-class
### Keywords: classes

### ** Examples

showClass("PLSRTest")



cleanEx()
nameEx("baseline-class")
### * baseline-class

flush(stderr()); flush(stdout())

### Name: baseline-class
### Title: Class "baseline"
### Aliases: baseline-class getBaseline,baseline-method
###   getCall,baseline-method getCorrected,baseline-method
###   getSpectra,baseline-method
### Keywords: classes

### ** Examples

showClass("baseline")



cleanEx()
nameEx("baseline-package")
### * baseline-package

flush(stderr()); flush(stdout())

### Name: baseline-package
### Title: Baseline correction
### Aliases: baseline-package
### Keywords: baseline spectra

### ** Examples

data(milk)
bc.irls <- baseline(milk$spectra[1,, drop=FALSE])
plot(bc.irls)



cleanEx()
nameEx("baseline")
### * baseline

flush(stderr()); flush(stdout())

### Name: baseline
### Title: Baseline correction
### Aliases: baseline
### Keywords: baseline spectra

### ** Examples

data(milk)
bc.irls <- baseline(milk$spectra[1,, drop=FALSE])
plot(bc.irls)



cleanEx()
nameEx("baseline.als")
### * baseline.als

flush(stderr()); flush(stdout())

### Name: baseline.als
### Title: Asymmetric Least Squares
### Aliases: baseline.als
### Keywords: baseline spectra

### ** Examples

data(milk)
bc.als <- baseline(milk$spectra[1,, drop=FALSE], lambda=10, method='als')
plot(bc.als)



cleanEx()
nameEx("baseline.fillPeaks")
### * baseline.fillPeaks

flush(stderr()); flush(stdout())

### Name: baseline.fillPeaks
### Title: Fill peaks
### Aliases: baseline.fillPeaks
### Keywords: baseline spectra

### ** Examples

data(milk)
bc.fillPeaks <- baseline(milk$spectra[1,, drop=FALSE], lambda=6,
	hwi=50, it=10, int=2000, method='fillPeaks')
plot(bc.fillPeaks)



cleanEx()
nameEx("baseline.irls")
### * baseline.irls

flush(stderr()); flush(stdout())

### Name: baseline.irls
### Title: Iterative Restricted Least Squares
### Aliases: baseline.irls
### Keywords: baseline spectra

### ** Examples

data(milk)
bc.irls <- baseline(milk$spectra[1,, drop=FALSE], method='irls')
plot(bc.irls)



cleanEx()
nameEx("baseline.lowpass")
### * baseline.lowpass

flush(stderr()); flush(stdout())

### Name: baseline.lowpass
### Title: Low-pass FFT filter
### Aliases: baseline.lowpass
### Keywords: baseline spectra

### ** Examples

data(milk)
bc.lowpass <- baseline(milk$spectra[1,, drop=FALSE], method='lowpass')
plot(bc.lowpass)



cleanEx()
nameEx("baseline.medianWindow")
### * baseline.medianWindow

flush(stderr()); flush(stdout())

### Name: baseline.medianWindow
### Title: Median window
### Aliases: baseline.medianWindow
### Keywords: baseline spectra

### ** Examples

data(milk)
bc.medianWindow <- baseline(milk$spectra[1,, drop=FALSE], hwm=300,
	method='medianWindow')
plot(bc.medianWindow)



cleanEx()
nameEx("baseline.modpolyfit")
### * baseline.modpolyfit

flush(stderr()); flush(stdout())

### Name: baseline.modpolyfit
### Title: Modified polynomial fitting
### Aliases: baseline.modpolyfit
### Keywords: baseline spectra

### ** Examples

data(milk)
bc.modpolyfit <- baseline(milk$spectra[1,, drop=FALSE], method='modpolyfit', deg=6)
plot(bc.modpolyfit)



cleanEx()
nameEx("baseline.peakDetection")
### * baseline.peakDetection

flush(stderr()); flush(stdout())

### Name: baseline.peakDetection
### Title: Simultaneous Peak Detection and Baseline Correction
### Aliases: baseline.peakDetection
### Keywords: baseline spectra

### ** Examples

data(milk)
bc.peakDetection <- baseline(milk$spectra[1,, drop=FALSE], method='peakDetection',
	left=300, right=300, lwin=50, rwin=50)
plot(bc.peakDetection)



cleanEx()
nameEx("baseline.rfbaseline")
### * baseline.rfbaseline

flush(stderr()); flush(stdout())

### Name: baseline.rfbaseline
### Title: Robust Baseline Estimation
### Aliases: baseline.rfbaseline
### Keywords: baseline spectra

### ** Examples

data(milk)
bc.rbe <- baseline(milk$spectra[1,, drop=FALSE], method='rfbaseline',
  span=NULL, NoXP=1000)
plot(bc.rbe)



cleanEx()
nameEx("baseline.rollingBall")
### * baseline.rollingBall

flush(stderr()); flush(stdout())

### Name: baseline.rollingBall
### Title: Rolling ball
### Aliases: baseline.rollingBall
### Keywords: baseline spectra

### ** Examples

data(milk)
bc.rollingBall <- baseline(milk$spectra[1,, drop=FALSE], wm=200, ws=200,
	method='rollingBall')
plot(bc.rollingBall)



cleanEx()
nameEx("baselineAlg-class")
### * baselineAlg-class

flush(stderr()); flush(stdout())

### Name: baselineAlg-class
### Title: Class "baselineAlg"
### Aliases: baselineAlg-class
### Keywords: classes

### ** Examples

showClass("baselineAlg")



cleanEx()
nameEx("baselineAlgResult-class")
### * baselineAlgResult-class

flush(stderr()); flush(stdout())

### Name: baselineAlgResult-class
### Title: Class "baselineAlgResult"
### Aliases: baselineAlgResult-class
### Keywords: classes

### ** Examples

showClass("baselineAlgResult")



cleanEx()
nameEx("baselineAlgTest-class")
### * baselineAlgTest-class

flush(stderr()); flush(stdout())

### Name: baselineAlgTest-class
### Title: Class "baselineAlgTest"
### Aliases: baselineAlgTest-class
### Keywords: classes

### ** Examples

showClass("baselineAlgTest")



cleanEx()
nameEx("baselineAlgorithms")
### * baselineAlgorithms

flush(stderr()); flush(stdout())

### Name: baselineAlgorithms
### Title: List of available baseline algorithms
### Aliases: baselineAlgorithms
### Keywords: baseline

### ** Examples

## Get a list of all algorithms:
names(baselineAlgorithms)
## Show the descriptions
sapply(baselineAlgorithms, description)
## Add new algorithm
baseline.my.alg <- function(spectra, kappa=1, gamma=1){
   baseline  <- spectra-kappa+gamma
   corrected <- spectra-baseline
   list(baseline=baseline,corrected=corrected)
}

baselineAlgorithms$my.alg = new("baselineAlg",
     name = "my.alg",
     description = "A new baseline correction algorithm",
     funcName = "baseline.my.alg",
     param = data.frame(
        name = c("kappa","gamma"), # maxit
        integer = c(FALSE, FALSE),
        min = c(0, 0),
        incl.min = c(TRUE, TRUE),
        default = c(1, 1),
        max = c(Inf, 1),
        incl.max = c(FALSE, TRUE)
    ))



cleanEx()
nameEx("baselineAlgorithmsGUI")
### * baselineAlgorithmsGUI

flush(stderr()); flush(stdout())

### Name: baselineAlgorithmsGUI
### Title: List of available baseline algorithms for GUI function
### Aliases: baselineAlgorithmsGUI
### Keywords: baseline

### ** Examples

## Get a list of all algorithms:
names(baselineAlgorithmsGUI)
## Add new algorithm:
baselineAlgorithmsGUI$my.alg <- as.data.frame(matrix(c(0,20,1,1, 0,20,1,1), 2,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$my.alg) <- list(par=c("kappa", "gamma"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$my.alg$current <- c(1,1)
baselineAlgorithmsGUI$my.alg$name <- c("Subtractive constand", "Additive constant")



cleanEx()
nameEx("baselineGUI")
### * baselineGUI

flush(stderr()); flush(stdout())

### Name: baselineGUI
### Title: Interactive plotting tool
### Aliases: baselineGUI
### Keywords: baseline spectra

### ** Examples

data(milk)
## Not run: baselineGUI(milk$spectra)



cleanEx()
nameEx("custom.baseline")
### * custom.baseline

flush(stderr()); flush(stdout())

### Name: custom.baseline
### Title: Customized baseline correction
### Aliases: custom.baseline
### Keywords: baseline spectra

### ** Examples

data(milk)
spectrum1  <- milk$spectra[1,1:10000,drop=FALSE]
ordinary   <- baseline(spectrum1, method="als", lambda=6, p=0.01)
customized <- custom.baseline(spectrum1, 2900, c(1,20), trans.win=100, just.plot=FALSE, method="als", lambda=6, p=0.01)
plot(1:10000,spectrum1, type='l')
lines(1:10000,getBaseline(ordinary), lty=2, col=2, lwd=2)
lines(1:10000,customized$baseline, lty=3, col=3, lwd=2)




cleanEx()
nameEx("getBaseline")
### * getBaseline

flush(stderr()); flush(stdout())

### Name: getBaseline
### Title: Functions to extract the components of a "baseline" object
### Aliases: getBaseline getSpectra getCorrected getCall
### Keywords: spectra baseline

### ** Examples

data(milk)
bl <- baseline(milk$spectra[1:2,])
getBaseline(bl)
getSpectra(bl)
getCorrected(bl)
getCall(bl)



cleanEx()
nameEx("milk")
### * milk

flush(stderr()); flush(stdout())

### Name: milk
### Title: MALDI-TOF mass spectra
### Aliases: milk
### Keywords: datasets

### ** Examples

data(milk)
plot(milk$spectra[1,], type = "l")



cleanEx()
nameEx("optimWizard")
### * optimWizard

flush(stderr()); flush(stdout())

### Name: optimWizard
### Title: Visual tool for setting up optimization
### Aliases: optimWizard
### Keywords: baseline spectra

### ** Examples

## Not run: 
##D data(milk)
##D X <- milk$spectra[,-1]
##D y <- milk$spectra[,1]
##D optimWizard(X,y)
## End(Not run)



cleanEx()
nameEx("plotBaseline")
### * plotBaseline

flush(stderr()); flush(stdout())

### Name: plotBaseline
### Title: Plot method for "baseline" objects
### Aliases: plotBaseline plot,baseline-method
### Keywords: baseline plot

### ** Examples

data(milk)
bl <- baseline(milk$spectra[1,, drop=FALSE])
plot(bl)
## Not run: plot(bl, zoom = TRUE)



cleanEx()
nameEx("predictionResult-class")
### * predictionResult-class

flush(stderr()); flush(stdout())

### Name: predictionResult-class
### Title: Class "predictionResult"
### Aliases: predictionResult-class
### Keywords: classes

### ** Examples

showClass("predictionResult")



cleanEx()
nameEx("ridgeRegressionTest-class")
### * ridgeRegressionTest-class

flush(stderr()); flush(stdout())

### Name: ridgeRegressionTest-class
### Title: Class "ridgeRegressionTest"
### Aliases: ridgeRegressionTest-class
### Keywords: classes

### ** Examples

showClass("ridgeRegressionTest")



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
