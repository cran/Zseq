% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Factorial.Alternating.R
\name{Factorial.Alternating}
\alias{Factorial.Alternating}
\alias{A005165}
\title{Alternating Factorial numbers}
\usage{
Factorial.Alternating(n, gmp = TRUE)
}
\arguments{
\item{n}{the number of first \code{n} entries from the sequence.}

\item{gmp}{a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.}
}
\value{
a vector of length \code{n} containing first entries from the sequence.
}
\description{
Under OEIS \href{https://oeis.org/A005165}{A005165}, an \emph{Alternating Factorial} is the absolute value of the alternating sum of the
first \code{n} factorials of positive integers. First 6 such numbers are  	0, 1, 1, 5, 19, 101.
}
\examples{
## generate first 5 Alternating Factorial numbers
print(Factorial.Alternating(5))

}
\seealso{
\code{\link{Factorial}}
}
