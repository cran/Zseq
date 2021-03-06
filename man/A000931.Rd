% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Padovan.R
\name{Padovan}
\alias{Padovan}
\alias{A000931}
\title{Padovan numbers}
\usage{
Padovan(n, gmp = TRUE)
}
\arguments{
\item{n}{the number of first \code{n} entries from the sequence.}

\item{gmp}{a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.}
}
\value{
a vector of length \code{n} containing first entries from the sequence.
}
\description{
Under OEIS \href{https://oeis.org/A000931}{A000931}, the \emph{n}th \emph{Padovan} number is given as
\deqn{F_n = F_{n-2} + F_{n-3}}
where the first 6 entries are 1, 0, 0, 1, 0, 1.
}
\examples{
## generate first 30 Padovan numbers
print(Padovan(30))

}
