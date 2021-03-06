% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Motzkin.R
\name{Motzkin}
\alias{Motzkin}
\alias{A001006}
\title{Motzkin numbers}
\usage{
Motzkin(n, gmp = TRUE)
}
\arguments{
\item{n}{the number of first \code{n} entries from the sequence.}

\item{gmp}{a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.}
}
\value{
a vector of length \code{n} containing first entries from the sequence.
}
\description{
Under OEIS \href{https://oeis.org/A001006}{A001006}, a \emph{Motzkin} number for a given \emph{n} is the number
of ways for drawing non-intersecting chords among \emph{n} points on a circle,
where the first 7 entries are 1, 1, 2, 4, 9, 21, 51.
}
\examples{
## generate first 30 Motzkin numbers
print(Motzkin(30))

}
