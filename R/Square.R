#' Square numbers
#'
#' Under OEIS \href{https://oeis.org/A000290}{A000290}, a \emph{Square} number is
#' \deqn{A_n = n^2}
#' for \eqn{n\ge 0}. First 6 Square numbers are  	0, 1, 4, 9, 16, 25.
#'
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Square numbers
#' first20 = Square(20)
#'
#' ## print without trailing 0's.
#' print(first20, drop0trailing = TRUE)
#'
#' @rdname A000290
#' @aliases A000290
#' @export
Square <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  output = mpfrArray(seq(from=0,to=(n-1),by=1), PrecisionBits)^2

  ## Rmpfr
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}
