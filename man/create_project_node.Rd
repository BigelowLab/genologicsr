% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Project.R
\name{create_project_node}
\alias{create_project_node}
\title{Create a project XML::xmlNode suitable for POSTing}
\usage{
create_project_node(name = NULL, open_date = format(as.POSIXct(Sys.time()),
  format = "\%Y-\%m-\%d"), close_date = NULL, invoice_date = NULL,
  researcher = NULL)
}
\arguments{
\item{name}{character project name (required)}

\item{open_date}{character, open date or NULL}

\item{close_date}{character, the close date or NULL}

\item{invoice_date}{character, invoice date or NULL}

\item{researcher}{character URI (required)}
}
\value{
XML::xmlNode
}
\description{
Create a project XML::xmlNode suitable for POSTing
}
