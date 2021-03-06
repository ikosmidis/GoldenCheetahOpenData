% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_methods.R
\name{print.gcod_db}
\alias{print.gcod_db}
\alias{print.gcod_remote_db}
\alias{print.gcod_local_db}
\title{Print methods for objects of class \code{gcod_db}, as produced by \code{\link[=get_athlete_ids]{get_athlete_ids()}}.}
\usage{
\method{print}{gcod_db}(x, txtplot = FALSE, ...)

\method{print}{gcod_remote_db}(x, txtplot = FALSE, ...)

\method{print}{gcod_local_db}(x, txtplot = FALSE, ...)
}
\arguments{
\item{x}{an object of class \code{gcod_db}}

\item{txtplot}{logical indicating whether or not text barplots
should be printed for the remote and local perspectives showing
the percentage of athlete ID records modified per year
quarter. Default is \code{FALSE}.}

\item{...}{currently not used.}
}
\description{
Print methods for objects of class \code{gcod_db}, as produced by \code{\link[=get_athlete_ids]{get_athlete_ids()}}.
}
\seealso{
\code{\link[=get_athlete_ids]{get_athlete_ids()}} \code{\link[=download_workouts]{download_workouts()}} \code{\link[=extract_workouts]{extract_workouts()}}
}
