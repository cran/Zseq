#' Square numbers
#'
#' Under OEIS \href{https://oeis.org/A000290}{A000290}, a \emph{Square} number is
#' \deqn{A_n = n^2}
#' for \eqn{n\ge 0}. First 6 Square numbers are  	0, 1, 4, 9, 16, 25.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Square numbers
#' print(Square(20))
#'
#' @rdname A000290
#' @aliases A000290
#' @export
Square <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz((0:(n-1))^2)

  ## Rmpfr
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}
