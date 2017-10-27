#' Palindromic numbers
#'
#' Under OEIS \href{https://oeis.org/A002113}{A002113}, a \emph{Palindromic} number is a number that
#' remains the same when its digits are reversed. First 6 Palindromic numbers in decimal are 0, 1, 2, 3, 4, 5.
#' This function supports various base by specifying the parameter \code{base} but returns are still represented in decimal.
#'
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param base choice of base.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 palindromic number in decimal
#' first30 = Palindromic(30)
#'
#' ## print without trailing 0's.
#' print(first30, drop0trailing = TRUE)
#'
#' @rdname A002113
#' @aliases A002113
#' @export
Palindromic <- function(n, base=10, Rmpfr=TRUE, PrecisionBits=496){
  ## Preprocessing for 'n'
  if ((length(n)!=1)||(abs(n-round(n))>sqrt(.Machine$double.eps))||(n<0)){
    stop("* Zsequence : input 'n' should be a positive integer.")
  }
  n = as.integer(n)

  ## Base
  if ((length(base)!=1)||(abs(base-round(base))>sqrt(.Machine$double.eps))||(n<2)){
    stop("* Zsequence : input 'base' should be a positive integer >= 2.")
  }
  base = as.integer(base)

  ## Control over PrecisionBits
  if (!missing(PrecisionBits)){
    if ((length(PrecisionBits)!=1)||(PrecisionBits<2)||(is.infinite(PrecisionBits))||(is.na(PrecisionBits))||(abs(PrecisionBits-round(PrecisionBits))>sqrt(.Machine$double.eps))){
      stop("* Zsequence : input 'PrecisionBits' should be a positive integer >= 2.")
    }
    PrecisionBits = as.integer(PrecisionBits)
  }

  ## Main Computation : first, compute in Rmpfr form
  output = mpfrArray(rep(0,n), PrecisionBits)
  output[1] = 0
  if (n>1){
    tgt = mpfr(0, PrecisionBits)
    iter= 1
    while (iter < n){
      tgt = tgt + 1
      if (is.Palindromic(tgt, base)){
        iter = iter + 1
        output[iter] = tgt
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
is.Palindromic <- function(n, p){
  strx = large_p_ary(n, p)
  revx = rev(strx)
  if (all(strx==revx)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
