%%% $Id: baseline-package.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baseline-package}
\alias{baseline-package}
\docType{package}
\title{
Baseline correction
}
\description{
A common framework with implementations of several baseline correction methods
}
\details{
\tabular{ll}{
Package: \tab baseline\cr
Type: \tab Package\cr
Version: \tab 1.2-0\cr
Date: \tab 2015-03-11\cr
License: \tab GPL-2\cr
}
Use function baseline for baseline correction. This function takes matrices of
spectra, a method name and parameters needed for the specific method. See
helpfiles for details.
}
\author{
Kristian Hovde Liland and Bjørn-Helge Mevik

Maintainer: Kristian Hovde Liland <kristian.liland@nmbu.no>
}
\references{
Andreas F. Ruckstuhl, Matthew P. Jacobson, Robert W. Field,
  James A. Dodd:
  Baseline subtraction using robust local regression estimation;
CHAD A. LIEBER and ANITA MAHADEVAN-JANSEN:
  Automated Method for Subtraction of Fluorescence from
  Biological Raman Spectra;
Mark S. Friedrichs:
  A model-free algorithm for the removal of baseline artifacts;
AHMET K. ATAKAN, W. E. BLASS, and D. E. JENNINGS:
  Elimination of Baseline Variations from a Recorded Spectrum
  by Ultra-low Frequency Filtering;
M.A. Kneen, H.J. Annegarn:
  Algorithm for fitting XRF, SEM and PIXE X-ray spectra backgrounds;
K.H. Liland, B.-H. Mevik, E.-O. Rukke, T.
  Almøy, M. Skaugen and T. Isaksson (2009)
  Quantitative whole spectrum analysis with MALDI-TOF MS, Part I:
  Measurement optimisation.
  \emph{Chemometrics and Intelligent Laboratory Systems}, \bold{96}(2),
  210--218.
  }
\keyword{ baseline }
\keyword{ spectra }
\examples{
data(milk)
bc.irls <- baseline(milk$spectra[1,, drop=FALSE])
\dontrun{
	plot(bc.irls)
}
}
