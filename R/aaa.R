### $Id: aaa.R 151 2011-01-02 15:26:27Z bhm $

###
### Start-up setup
###
.onLoad <- function(...){
    ## To avoid warnings in R CMD check:
    if(!exists(".baseline.current"))
        assign(".baseline.current", list(), .GlobalEnv)
}
