#' Triangular numbers
#'
#' Under OEIS \href{https://oeis.org/A000217}{A000217}, a \emph{Triangular} number counts objects arranged in an equilateral triangle.
#' First 6 Triangular numbers are 0, 1, 3, 6, 10, 15.
#'
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Triangular numbers
#' print(Triangular(20))
#'
#' @rdname A000217
#' @aliases A000217
#' @export
Triangular <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  for (i in 1:n){
    output[i] = gmp::chooseZ(as.bigz(i), 2)
  }


  ## Rmpfr
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}
