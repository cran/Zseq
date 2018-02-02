#' Factorial numbers
#'
#' Under OEIS \href{https://oeis.org/A000142}{A000142}, a \emph{Factorial} is the product of all positive integers smaller than or equal to the number.
#' First 6 such numbers are   	1, 1, 2, 6, 24, 120
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 10 Factorials
#' print(Factorial(10))
#'
#' @rdname A000142
#' @aliases A000142
#' @export
Factorial <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  output[1] = 1
  for (i in 2:n){
    output[i] = gmp::factorialZ(as.bigz(i-1))
  }

  ## gmp
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}
