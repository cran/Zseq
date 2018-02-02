#' Abundant numbers
#'
#' Under OEIS \href{https://oeis.org/A005101}{A005101}, an \emph{abundant} number is
#' a number whose proper divisors sum up to the extent greater than the number itself. First
#' 6 abundant numbers are 12, 18, 20, 24, 30, 36.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Abundant numbers and print it
#' print(Abundant(30))
#'
#' @seealso \code{\link{Deficient}}, \code{\link{Perfect}}
#' @rdname A005101
#' @aliases A005101
#' @export
Abundant <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = gmp::as.bigz(numeric(n))
  iter   = 0
  tgt    = as.bigz(10)
  while (iter<n){
    tgt = tgt + 1 # update the tgt number
    if (sum(gmp_divisors_proper(tgt)) > tgt){
      iter = iter + 1
      output[iter] = tgt
    }
  }

  ## Return GMP
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}


