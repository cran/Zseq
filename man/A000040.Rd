% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Prime.R
\name{Prime}
\alias{Prime}
\alias{A000040}
\title{Prime numbers}
\usage{
Prime(n, gmp = TRUE)
}
\arguments{
\item{n}{the number of first \code{n} entries from the sequence.}

\item{gmp}{a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.}
}
\value{
a vector of length \code{n} containing first entries from the sequence.
}
\description{
Under OEIS \href{https://oeis.org/A000040}{A000040}, a \emph{Prime} number is a natural number with no positive divisors other than 1 and itself.
First 6 prime numbers are 2, 3, 5, 7, 11, 13.
}
\examples{
## generate first 30 Regular numbers
print(Prime(30))

}
