\name{baselineAlgorithms}
\alias{baselineAlgorithms}
\docType{data}
\title{
  List of available baseline algorithms
}
\description{
  A list with descriptions of all baseline algorithms available through
  the optimisaiont framework and graphical user interface.  The elements
  of the list are \code{\linkS4class{baselineAlg}} objects.  The list is
  used by the code to extract names and information about the baseline
  algorithms.
}
\details{
  The list is not meant for usage by end-users.

  The names of the list must match the \code{name} slot of the
  elements.
}
\examples{
## Get a list of all algorithms:
names(baselineAlgorithms)
## Show the descriptions
sapply(baselineAlgorithms, description)
}
\keyword{baseline}
