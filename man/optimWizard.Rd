%%% $Id: optimWizard.Rd 169 2011-01-03 20:24:13Z bhm $
\name{optimWizard}
\alias{optimWizard}

\title{ Visual tool for setting up optimization }
\description{
  Set up optimization through a graphical user interface. Optionally collecting values directly from 'baselineGUI'
}
\usage{
optimWizard(X, y, postproc, predictionTest, cvsegments)
}
\arguments{
  \item{X}{ Matrix with spectra in rows }
  \item{y}{ Response vector or matrix in analysis }
  \item{postproc}{ Custum function for post processing of spectra (optional) }
  \item{predictionTest}{ Custom prediction object (optional) }
  \item{cvsegments}{ Cross-validation segments (optional)}
}
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\examples{
\dontrun{
X <- spectra[1:100,-1]
y <- spectra[1:100,1]
optimWizard(X,y)
}
}
\keyword{ baseline }
\keyword{ spectra }