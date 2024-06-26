% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_helpers.R
\name{label2id}
\alias{label2id}
\title{Convert variable name to id, suitable for use in coding}
\usage{
label2id(label)
}
\arguments{
\item{label}{a character string, the name/label of the variable}
}
\value{
character string, the variable's id, to use in coding
}
\description{
Converts variable labels (e.g. species or windfarm names) to an id formatted
so it can be used in coding (i.e. replace white spaces and dashes with
underscores). The function's main intention is to standardize label-to-id
conversion
}
