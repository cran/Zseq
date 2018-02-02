#' Number of steps for Juggler sequence
#'
#' Under OEIS \href{https://oeis.org/A007320}{A007320}, a \emph{Number of steps for Juggler sequence} literally counts the number of steps
#' required for a sequence that starts from \code{n}. First 6 terms are  	0, 1, 6, 2, 5, 2 that \code{n} starting from 0 is conventional choice.
#' Note that when it counts \emph{number of steps}, not the length of the sequence including the last 1.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 10 numbers of steps for Juggler sequences
#' print(Juggler.Nsteps(10))
#'
#' @seealso \code{\link{Juggler}}
#' @rdname  A007320
#' @aliases  A007320
#' @export
Juggler.Nsteps <- function(n, gmp=TRUE){
  ## Preprocessing for 'start'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  for (i in 2:n){
    output[i] = length(Juggler(i))-1
  }

  ## Rmpfr
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}
