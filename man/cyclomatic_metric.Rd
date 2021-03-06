% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{cyclomatic_metric}
\alias{cyclomatic_metric}
\title{Cyclomatic metric of McCabe}
\usage{
cyclomatic_metric(file_path, signavio = FALSE,
  path_log_already_created = FALSE, generate_new_path_log = FALSE,
  time_to_generate_path_log = 1500)
}
\arguments{
\item{file_path}{document object created using the create_internal_document function}

\item{signavio}{boolean which indicates whether the file stems from signavio}

\item{path_log_already_created}{boolean which indicates whether the path log has already been created before or not. When you are not sure, it is best to use the standard which is false}

\item{generate_new_path_log}{used when it is not possible to save the path log such as with the Rapid miner or in unit tests and examples}

\item{time_to_generate_path_log}{time which is the maximum time to generate a new path log in seconds. The standard setting is 1500 seconds.}
}
\value{
an integer indicating the coefficient of network connectivity
}
\description{
Cyclomatic metric takes into account the behavioral complexity of a process model. It is calculated by taking the number of activities minus
the number of events, gateways and connector activities plus the number of strongly connected components.
The number of strongly connected components is calculated by taking the number of exclusive gateways at depth level zero, when the depth is calculated only including exclusive gateways
}
\examples{
\dontshow{file_path <- system.file("extdata", "doc.txt", package="understandBPMN")}
cyclomatic_metric(file_path, generate_new_path_log = TRUE)
}
