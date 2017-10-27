#' Palindromic squares
#'
#' Under OEIS \href{https://oeis.org/A002779}{A002779}, a \emph{Palindromic square} is a number that is
#' both Palindromic and Square. First 6 such numbers are 0, 1, 4, 9, 121, 484. It uses only the base 10 decimals.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 10 palindromic squares
#' first10 = Palindromic.Squares(10)
#'
#' ## print without trailing 0's.
#' print(first10, drop0trailing = TRUE)
#'
#' @rdname A002779
#' @aliases A002779
#' @export
Palindromic.Squares <- function(n, Rmpfr=TRUE, PrecisionBits=496){
  ## Preprocessing for 'n'
  if ((length(n)!=1)||(abs(n-round(n))>sqrt(.Machine$double.eps))||(n<0)){
    stop("* Zsequence : input 'n' should be a positive integer.")
  }
  n = as.integer(n)

  ## Base
  base = as.integer(10)

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
      tgt  = tgt + 1
      tgt2 = (tgt^2)
      if (is.Palindromic(tgt2, base)){
        iter = iter + 1
        output[iter] = tgt2
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
is.Square <- function(n){
  tgt = sqrt(n)
  if (abs(n-round(n))>sqrt(.Machine$double.eps)){
    return(FALSE)
  } else {
    return(TRUE)
  }
}
