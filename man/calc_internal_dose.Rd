% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_internal_dose.R
\name{calc_internal_dose}
\alias{calc_internal_dose}
\title{Calculate internal chemical dose}
\usage{
calc_internal_dose(C_ext, IR, time = 1, BW = 1, scaling = 1)
}
\arguments{
\item{C_ext}{ambient chemical concentration in \eqn{\frac{mg}{m^3}}}

\item{IR}{inhalation rate in \eqn{\frac{m^3}{day}}}

\item{time}{total time in \eqn{days}}

\item{BW}{body weight in \eqn{kg}}

\item{scaling}{scaling factor encompassing any required unit adjustments}
}
\value{
list of matrices containing internal chemical doses in
\eqn{\frac{mg}{kg}}
}
\description{
Estimate the internal dose from inhalation of a chemical given inhalation
rate, time, and body weight
}
\details{
Input \code{C_ext} must be a matrix or list of matrices. Input \code{IR} must be an
atomic vector or list of atomic vectors. The \code{time}, \code{BW} and \code{scaling}
arguments are scalars.

The internal dose is calculated as:
\deqn{D_{int} = \frac{C_{ext} \times IR \times time}{BW} \times scaling}
}
\examples{
# Single population
C_ext <- matrix(1:15, ncol = 3)
IR <- 1:5
calc_internal_dose(C_ext, IR)

# Multiple populations
C_ext <- list(
  "a" = matrix(1:15 / 10, ncol = 3),
  "b" = matrix(1:8, ncol = 2)
)
IR <- list(1:5, 1:4 / 2)
calc_internal_dose(C_ext, IR)

}
