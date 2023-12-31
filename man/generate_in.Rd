% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postprocess_functions.R
\name{generate_in}
\alias{generate_in}
\title{Create Cerberus Inputs for Generative Predictions}
\usage{
generate_in(responses, timestamp, resampled_time_series, features, config)
}
\arguments{
\item{responses}{generated Cerberus responses}

\item{timestamp}{POSIXct timestamp from which to generate predictions}

\item{resampled_time_series}{prepared Ceberus resampled timeseries data list}

\item{features}{character array. Timeseries features used in Cerberus}

\item{config}{Cerberus configuration list.}
}
\value{
generated Cerberus heads
}
\description{
This function creates the inputs required for predictive generation by a
trained Cerberus model
}
