#' Odious numbers
#'
#' Under OEIS \href{https://oeis.org/A000069}{A000069}, an \emph{Odious} number has an odd number of 1's in its binary expansion.
#' First 6 Odious numbers are   	1, 2, 4, 7, 8, 11.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Odious numbers
#' print(Odious(20))
#'
#' @seealso \code{\link{Evil}}
#' @rdname A000069
#' @aliases A000069
#' @export
Odious <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  output[1] = 1
  if (n>1){
    tgt  = as.bigz(1)
    iter = 1
    while (iter<n){
      tgt = tgt + 1
      if (!is.Evil(tgt)){
        iter = iter+1
        output[iter] = tgt
      }
    }
  }
  ## Rmpfr
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}
