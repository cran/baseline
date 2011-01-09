## $Id: baseline.R 170 2011-01-03 20:38:25Z bhm $
### Main baseline correction function, and definition of class baseline.

###
### Baseline class
###
setClass("baseline",
         representation(baseline = "matrix", corrected = "matrix",
                        spectra = "matrix", call = "language")
         )

###
### Top level baseline correction function
###

baseline <- function (spectra, method = "irls", ...) {
    ## Get baseline algorithm function name:
    method <- match.arg(method, names(baselineAlgorithms))
    baseFunc <- funcName(baselineAlgorithms[[method]])

    ## Run baseline algorithm:
    res <- do.call(baseFunc, list(spectra, ...))

    ## Build and return the object:
    new("baseline",
        baseline = res$baseline,
        corrected = res$corrected,
        spectra = spectra,
        call = match.call()
        )
}


###
### Extraction methods
###
setGeneric("getSpectra", function(object) standardGeneric("getSpectra"))
setMethod("getSpectra", "baseline", function(object) object@spectra)
setGeneric("getCorrected", function(object) standardGeneric("getCorrected"))
setMethod("getCorrected", "baseline", function(object) object@corrected)
setGeneric("getBaseline", function(object) standardGeneric("getBaseline"))
setMethod("getBaseline", "baseline", function(object) object@baseline)
setGeneric("getCall", function(object) standardGeneric("getCall"))
setMethod("getCall", "baseline", function(object) object@call)
