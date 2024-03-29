% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_dcmt.R
\docType{data}
\name{wf_pars_lookup}
\alias{wf_pars_lookup}
\title{Lookup table listing windfarm and turbine parameters. Used to generate UI
input widgets dynamically.}
\format{
A dataframe with the following columns:
\describe{
\item{par_name}{used in coding}
\item{par_label}{used for display parameter name in UI}
\item{par_id}{used for inputIDs in e.g. buttons, plots, etc}
\item{par_dist}{probability distribution assumed for the parameter}
\item{dflt_mean}{default for the mean}
\item{dflt_sd}{default for the sd}
}
}
\usage{
wf_pars_lookup
}
\description{
Lookup table listing windfarm and turbine parameters. Used to generate UI
input widgets dynamically.
}
\keyword{datasets}
