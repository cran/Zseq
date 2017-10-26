#' Triangular numbers
#'
#' Under OEIS \href{https://oeis.org/A000217}{A000217}, a \emph{Triangular} number counts objects arranged in an equilateral triangle.
#' First 6 Triangular numbers are 0, 1, 3, 6, 10, 15.
#'
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Triangular numbers
#' first20 = Triangular(20)
#'
#' ## print without trailing 0's.
#' print(first20, drop0trailing = TRUE)
#'
#' @rdname A000217
#' @aliases A000217
#' @export
Triangular <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  for (i in 1:n){
    output[i] = chooseMpfr(mpfr(i, PrecisionBits), 2)
  }


  ## Rmpfr
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}
