%%% $Id: baseline.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baseline}
\alias{baseline}
\title{ Baseline correction }
\description{
  Common framework for baseline correction
}
\usage{
baseline(spectra, method = "irls", ...)
}
\arguments{
  \item{spectra}{ Matrix with spectra in rows }
  \item{method}{ Baseline correction method }
  \item{\dots}{ Additional parameters, sent to the method }
}
\details{
  Estimates baselines for the \code{spectra}, using the algorithm named
  in \code{method}.
}
\value{
  An object of class \code{\linkS4class{baseline}}.
}

\seealso{The functions implementing the baseline algorithms:
  \code{\link{baseline.als}}, \code{\link{baseline.fillPeaks}},
  \code{\link{baseline.irls}}, \code{\link{baseline.lowpass}},
  \code{\link{baseline.medianWindow}},
  \code{\link{baseline.modpolyfit}},
  \code{\link{baseline.peakDetection}},
  \code{\link{baseline.rfbaseline}}, \code{\link{baseline.rollingBall}}
}
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\examples{
data(milk)
bc.irls <- baseline(milk$spectra[1,, drop=FALSE])
\dontrun{
	plot(bc.irls)
}
}

\keyword{ baseline }
\keyword{ spectra }
