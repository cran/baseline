baseline.fillPeaks <- function(spectra, lambda, hwi, it, int){
## Iterative baseline correction algorithm based on mean suppression
## By Kristian Hovde Liland
## $Id: baseline.fillPeaks.R 170 2011-01-03 20:38:25Z bhm $

# INPUT:
# spectra - rows of spectra
# lambda  - 2nd derivative constraint of primary smoothing
# hwi     - half width of local window
# it      - number of iterations in suppression loop
# int     - number of intervals to make of data
#
# OUTPUT:
# baseline  - proposed baseline
# corrected - baseline corrected spectra

  np <- dim(spectra)
  if (missing(int)) int <- np[1]-1
  baseline  <- matrix(0,np[1],np[2])

  # Sparse empty matrix (m x m)
  speye <- as.matrix.csr(0,np[2],np[2])

  # Diagonal sparse matrix (m x m)
  diag(speye) <- 1
  D <- diff(speye,differences=2)
  ww <- rep.int(1, np[2])
  W <- as.matrix.csr(0,np[2],np[2])
  diag(W) <- ww

  # Primary smoothing
  Yorig <- spectra
  U <- chol(W+10^lambda*t(D)%*%D)
  spectra <- t(backsolve(U, t(spectra)))

  # Exponential decrease in interval width
  if(it != 1){
      d1 <- log10(hwi)
      d2 <- 0
      w <- ceiling((10)^c(d1+(0:(it-2))*(d2-d1)/(floor(it)-1), d2))
  } else{
      w <- hwi
  }

  # Computes center points
  minip <- numeric(int)
  lims <- seq(from = 1, to = np[2], length = int + 1)
  lefts <- ceiling(lims[-(int+1)])
  rights <- floor(lims[-1])
  minip <- round((lefts + rights)/2)

  # Iterate through spectra
  for(s in 1:np[1]){
      # Computes center values
      xx    <- numeric(int)
      for (i in 1:int) xx[i] <- min(spectra[s,lefts[i]:rights[i]])

      # Iterate suppression
      for(k in 1:it){
          # Current window width
          w0 <- w[k]

          # Point-wise iteration to the right
          for(i in 2:(int-1)){
              # Interval cut-off close to edges
              if(i <= w0){
                  v <- i-1
              } else if((int-i) < w0) {
                  v <- int-i
              } else {
                  v <- w0
              }

              # Baseline suppression
              a <- mean(xx[(i-v):(i+v)])
              xx[i] <- min(a,xx[i])
          }

          # Point-wise iteration to the left
          for(i in 2:(int-1)){
              j <- int-i+1
              # Interval cut-off close to edges
              if(j <= w0){
                  v <- j-1
              } else if((int-j) < w0) {
                  v <- int-j
              } else {
                  v <- w0
              }

              # Baseline suppression
              a <- mean(xx[(j-v):(j+v)])
              xx[j] <- min(a,xx[j])
          }
      }

      # Prepare minimum vector
      minip[1] <- 1
      minip[int] <- np[2]

      # Interpolation between minimum points in buckets
      xxx <- approx(minip, xx, 1:np[2])$y

      baseline[s,] <- xxx
  }
  retur <- list(baseline = baseline, corrected = Yorig - baseline)
}
