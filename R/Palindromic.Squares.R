#' Palindromic squares
#'
#' Under OEIS \href{https://oeis.org/A002779}{A002779}, a \emph{Palindromic square} is a number that is
#' both Palindromic and Square. First 6 such numbers are 0, 1, 4, 9, 121, 484. It uses only the base 10 decimals.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 10 palindromic squares
#' print(Palindromic.Squares(10))
#'
#' @rdname A002779
#' @aliases A002779
#' @export
Palindromic.Squares <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Base
  base = as.integer(10)


  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  output[1] = 0
  if (n>1){
    tgt = as.bigz(0)
    iter= 1
    while (iter < n){
      tgt  = tgt + 1
      tgt2 = (tgt^2)
      if (is.Palindromic(tgt2, base)){
        iter = iter + 1
        output[iter] = tgt2
      }
    }
  }

  ## Rmpfr
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}


#' @keywords internal
#' @noRd
is.Square <- function(n){
  tgt = sqrt(as.integer(n))
  if (abs(tgt-round(tgt))>sqrt(.Machine$double.eps)){
    return(FALSE)
  } else {
    return(TRUE)
  }
}
