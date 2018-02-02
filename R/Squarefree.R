#' Squarefree numbers
#'
#' Under OEIS \href{https://oeis.org/A005117}{A005117}, a \emph{Squarefree} number is
#' a number that are not divisible by a square of a smaller integer greater than 1. First 6 Squarefree numbers are  1, 2, 3, 5, 6, 7.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Squarefree numbers
#' print(Squarefree(30))
#'
#' @rdname A005117
#' @aliases A005117
#' @export
Squarefree <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(1)
  if (n>1){
    iter = 1
    tgt  = as.bigz(1)
    while (iter<n){
      tgt = tgt + 1
      if (is.Squarefree(tgt)){
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

#' @keywords internal
#' @noRd
is.Squarefree <- function(n){
  if (abs(asNumeric(1-n))<sqrt(.Machine$double.eps)){
    return(TRUE)
  } else if ((abs(sqrt(asNumeric(n))-round(sqrt(asNumeric(n))))<sqrt(.Machine$double.eps))){
    return(FALSE)
  } else {
    divisors = gmp_divisors_proper(n)
    if (length(divisors)==1){
      return(TRUE)
    } else {
      ldivisors = length(divisors)
      for (i in 2:ldivisors){
        tgt = sqrt(asNumeric(divisors[i]))
        if ((abs(tgt-round(tgt)))<sqrt(.Machine$double.eps)){
          return(FALSE)
          break
        }
      }
    }
    return(TRUE)
  }
}
