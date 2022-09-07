#' Deficient numbers
#'
#' Under OEIS \href{https://oeis.org/A005100}{A005100}, a \emph{deficient} number is
#' a number whose proper divisors sum up to the extent smaller than the number itself. First
#' 6 deficient numbers are 	1, 2, 3, 4, 5, 7
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' \donttest{
#' ## generate first 30 Deficient numbers
#' print(Deficient(30))
#' }
#'
#' @seealso \code{\link{Abundant}}, \code{\link{Perfect}}
#' @rdname A005100
#' @aliases A005100
#' @export
Deficient <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  output[1] = as.bigz(1)
  iter   = 0
  tgt    = 1
  while (iter<(n-1)){
    tgt = tgt + 1 # update the tgt number
    if  (sum(gmp_divisors_proper(tgt)) < tgt){
      iter = iter + 1
      output[iter+1] = tgt
    }
  }

  ## Rmpfr
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}

