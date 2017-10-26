#' Odious numbers
#'
#' Under OEIS \href{https://oeis.org/A000069}{A000069}, an \emph{Odious} number has an odd number of 1's in its binary expansion.
#' First 6 Odious numbers are   	1, 2, 4, 7, 8, 11.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Odious numbers
#' first20 = Odious(20)
#'
#' ## print without trailing 0's.
#' print(first20, drop0trailing = TRUE)
#'
#' @seealso \code{\link{Evil}}
#' @rdname A000069
#' @aliases A000069
#' @export
Odious <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  output = mpfrArray(rep(0,n), PrecisionBits)
  output[1] = 1
  if (n>1){
    tgt = mpfr(1, PrecisionBits)
    iter = 1
    while (iter<n){
      tgt = tgt + 1
      if (!is.Evil(tgt, PrecisionBits)){
        iter = iter+1
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
