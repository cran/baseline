%%% $Id: baselineGUI.Rd 180 2011-01-09 14:44:33Z bhm $
\name{baselineGUI}
\alias{baselineGUI}

\title{ Interactive plotting tool }
\description{
  An interactive plotting tool for dynamic visualization of baselines and their effect using gWidgets with GTK
}
\usage{
baselineGUI(spectra, method='irls', labels, rev.x = FALSE)
}
\arguments{
  \item{spectra}{ Matrix with spectra in rows }
  \item{method}{ Baseline correction method (optional)}
  \item{labels}{ Labels for X-axis (optional)}
  \item{rev.x}{ Reverse X-axis (optional, default=FALSE)}
}
\details{
  Creates and updates a list containing current baseline and spectrum (baseline.result)
}
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\examples{
data(milk)
\dontrun{baselineGUI(milk$spectra)}
}
\keyword{ baseline }
\keyword{ spectra }
