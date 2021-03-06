% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Lims.R
\name{LimsRefClass_POST}
\alias{LimsRefClass_POST}
\title{POST a resource}
\arguments{
\item{x}{XML::xmlNode to or NodeRefClass POST}

\item{uri}{character if NULL taken from \code{x}}

\item{asNode}{logical, if TRUE return a class that inherits NodeRefClass.
This should be FALSE for batch processing.}

\item{...}{further arguments for httr::POST}
}
\value{
XML::xmlNode or NodeRefClass
}
\description{
POST a resource
}
\seealso{
Other Lims: \code{\link{LimsRefClass-class}},
  \code{\link{LimsRefClass_ATTACH}},
  \code{\link{LimsRefClass_BROWSE}},
  \code{\link{LimsRefClass_DELETE}},
  \code{\link{LimsRefClass_PUSH}},
  \code{\link{LimsRefClass_PUT}},
  \code{\link{LimsRefClass_check}},
  \code{\link{LimsRefClass_get_artifactgroups}},
  \code{\link{LimsRefClass_get_byLimsid}},
  \code{\link{LimsRefClass_uri}},
  \code{\link{LimsRefClass_userpwd}},
  \code{\link{LimsRefClass_validate_session}},
  \code{\link{get_NSMAP}}
}
