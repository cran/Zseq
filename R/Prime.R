#' Prime numbers
#'
#' Under OEIS \href{https://oeis.org/A000040}{A000040}, a \emph{Prime} number is a natural number with no positive divisors other than 1 and itself.
#' First 6 prime numbers are 2, 3, 5, 7, 11, 13.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Regular numbers
#' print(Prime(30))
#'
#' @rdname A000040
#' @aliases A000040
#' @export
Prime <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  output[1] = as.bigz(2)
  if (n>1){
    tgt = as.bigz(3)
    iter = 1
    while (iter<n){
      if (gmp_isprime(tgt)){
        iter = iter+1
        output[iter] = tgt
      }
      tgt = tgt + 2
    }
  }
  ## gmp
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}
