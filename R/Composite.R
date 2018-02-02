#' Composite numbers
#'
#' Under OEIS \href{https://oeis.org/A005100}{A002808}, a \emph{composite} number is
#' a positive integer that can be represented as multiplication of two smaller positive integers.
#' The first 6 composite numbers are 4, 6, 8, 9, 10, 12.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Composite numbers
#' print(Composite(30))
#'
#' @rdname A002808
#' @aliases A002808
#' @export
Composite <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(4)
  if (n>1){
    iter = 1
    tgt  = as.bigz(4)
    while (iter<n){
      tgt = tgt + 1
      if (!gmp_isprime(tgt)){
        output = append(output, tgt)
        iter = iter + 1
      }
    }
  }

  ## Rmpfr
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}
