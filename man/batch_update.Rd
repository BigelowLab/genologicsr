% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Lims.R
\name{batch_update}
\alias{batch_update}
\title{Update one or more XML::xmlNodes using batch resources}
\usage{
batch_update(x, lims, asNode = TRUE, rel = c("artifacts", "containers",
  "samples")[1])
}
\arguments{
\item{x}{a list of one or more XML::xmlNode objects}

\item{lims}{LimsRefClass object}

\item{asNode}{logical, if TRUE return a named list of NodeRefClass objects}

\item{rel}{the relative namespace into the "batch/retrieve"}
}
\value{
list of XML::xmlNode or NodeRefClass
}
\description{
Return order is *not* enforced to be the same as the input order
}
