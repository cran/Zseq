#' Factorial numbers
#'
#' Under OEIS \href{https://oeis.org/A000142}{A000142}, a \emph{Factorial} is the product of all positive integers smaller than or equal to the number.
#' First 6 such numbers are   	1, 1, 2, 6, 24, 120
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 10 Factorials
#' first10 = Factorial(10)
#'
#' ## print without trailing 0's.
#' print(first10, drop0trailing = TRUE)
#'
#' @rdname A000142
#' @aliases A000142
#' @export
Factorial <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  for (i in 2:n){
    output[i] = factorialMpfr((i-1), PrecisionBits)
  }

  ## Rmpfr
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}
