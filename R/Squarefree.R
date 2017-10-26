#' Squarefree numbers
#'
#' Under OEIS \href{https://oeis.org/A005117}{A005117}, a \emph{Squarefree} number is
#' a number that are not divisible by a square of a smaller integer greater than 1. First 6 Squarefree numbers are  1, 2, 3, 5, 6, 7.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Squarefree numbers
#' first30 = Squarefree(30)
#'
#' ## print without trailing 0's.
#' print(first30, drop0trailing = TRUE)
#'
#' @rdname A005117
#' @aliases A005117
#' @export
Squarefree <- function(n, Rmpfr=TRUE, PrecisionBits=496){
  ## Preprocessing for 'n'
  if ((length(n)!=1)||(abs(n-round(n))>sqrt(.Machine$double.eps))||(n<0)){
    stop("* Zsequence : input 'n' should be a positive integer.")
  }
  n = as.integer(n)

  ## Control over PrecisionBits
  if (!missing(PrecisionBits)){
    if ((length(PrecisionBits)!=1)||(PrecisionBits<2)||(is.infinite(PrecisionBits))||(is.na(PrecisionBits))||(abs(PrecisionBits-round(PrecisionBits))>sqrt(.Machine$double.eps))){
      stop("* Zsequence : input 'PrecisionBits' should be a positive integer >= 2.")
    }
    PrecisionBits = as.integer(PrecisionBits)
  }

  ## Main Computation : first, compute in Rmpfr form
  output = mpfrArray(c(1), PrecisionBits)
  if (n>1){
    iter = 1
    tgt  = mpfr(1, PrecisionBits)
    while (iter<n){
      tgt = tgt + 1
      if (is.Squarefree(tgt, PrecisionBits)){
        output = append(output, tgt)
        iter = iter + 1
      }
    }
  }
  ## Rmpfr
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}

#' @keywords internal
#' @noRd
is.Squarefree <- function(n, PrecisionBits){
  if (abs(1-n)<sqrt(.Machine$double.eps)){
    return(TRUE)
  } else if ((abs(sqrt(n)-round(sqrt(n)))<sqrt(.Machine$double.eps))){
    return(FALSE)
  } else {
    divisors = large_divisors_proper(n, PrecisionBits)
    if (length(divisors)==1){
      return(TRUE)
    } else {
      ldivisors = length(divisors)
      for (i in 2:ldivisors){
        tgt = sqrt(divisors[i])
        if ((abs(tgt-round(tgt)))<sqrt(.Machine$double.eps)){
          return(FALSE)
          break
        }
      }
    }
    return(TRUE)
  }
}
