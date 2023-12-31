% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocess_functions.R
\name{prepare_inputs}
\alias{prepare_inputs}
\title{Prepare Input Data for Cerberus}
\usage{
prepare_inputs(time_series_data, config = cerberus_config(), min_max_df = NULL)
}
\arguments{
\item{time_series_data}{timeseries dataframe}

\item{config}{cerberus configuration list}

\item{min_max_df}{}

\item{max_range}{dataframe of min and max by feature}
}
\value{
list of prepared data for Cerberus
}
\description{
This function prepares a timeseries dataframe into the array structures
required for cerberus
}
