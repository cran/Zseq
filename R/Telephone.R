#' Telephone numbers
#'
#' Under OEIS \href{https://oeis.org/A000085}{A000085}, a \emph{Telephone} number - also known as \emph{Involution} number - is counting the number
#' of connection patterns in a telephone system with \code{n} subscribers, or in a more mathematical term, the number of self-inverse permutations on
#' \code{n} letters. First 6 Telephone numbers are  	1, 1, 2, 4, 10, 26,
#'
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Regular numbers
#' print(Telephone(20))
#'
#' @rdname A000085
#' @aliases A000085 Involution
#' @export
Telephone <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  output[1] = as.bigz(1)
  if (n>=2){
    output[2] = as.bigz(1)
  }
  if (n>2){
    for (i in 3:n){
      output[i] = output[i-1] + (i-2)*as.bigz(output[i-2]) # here the multiplier (i-2) is not identical to OEIS : just for starting index issue.
    }
  }

  ## Rmpfr
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}
