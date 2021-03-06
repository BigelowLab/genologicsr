% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Project.R
\name{ProjectRefClass_get_artifacts}
\alias{ProjectRefClass_get_artifacts}
\title{Retrieve the artifacts associated with a project}
\arguments{
\item{what}{request either 'submitted' (default) or 'all'  sample artifacts}

\item{SS}{optional list of Samples in the Project.  If NULL then these
are first retrieved which can be slow.}
}
\value{
a list of list of zero or more SampleRefClass or NULL
}
\description{
This method may take a while depending upon the number of samples in the 
system. The 'all' option is available but may be ill advised as it can be
very very slow.
}
\seealso{
Other Project: \code{\link{ProjectRefClass_get_containers}},
  \code{\link{ProjectRefClass_get_researcher}},
  \code{\link{ProjectRefClass_get_samples}},
  \code{\link{ProjectRefNode_DELETE}},
  \code{\link{ProjectRefNode_POST}}
}
