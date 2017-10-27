#' Alternating Factorial numbers
#'
#' Under OEIS \href{https://oeis.org/A005165}{A005165}, an \emph{Alternating Factorial} is the absolute value of the alternating sum of the
#' first \code{n} factorials of positive integers. First 6 such numbers are  	0, 1, 1, 5, 19, 101.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 5 Alternating Factorial numbers
#' first5 = Factorial.Alternating(5)
#'
#' ## print without trailing 0's.
#' print(first5, drop0trailing = TRUE)
#'
#' @seealso \code{\link{Factorial}}
#' @rdname A005165
#' @aliases A005165
#' @export
Factorial.Alternating <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  if (n==1){
    output[1] = mpfr(0, PrecisionBits)
  }
  if (n==2){
    output[1] = mpfr(0, PrecisionBits)
    output[2] = mpfr(1, PrecisionBits)
  }
  if (n>2){
    output[1] = mpfr(0, PrecisionBits)
    output[2] = mpfr(1, PrecisionBits)
    for (i in 3:n){
      output[i] = factorialMpfr((i-1), PrecisionBits)-output[i-1]
    }
  }

  ## Rmpfr
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}


