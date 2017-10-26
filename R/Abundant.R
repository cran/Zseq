#' Abundant numbers
#'
#' Under OEIS \href{https://oeis.org/A005101}{A005101}, an \emph{abundant} number is
#' a number whose proper divisors sum up to the extent greater than the number itself. First
#' 6 abundant numbers are 12, 18, 20, 24, 30, 36.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Abundant numbers
#' first30 = Abundant(30)
#'
#' ## print without trailing 0's.
#' print(first30, drop0trailing = TRUE)
#'
#' @seealso \code{\link{Deficient}}, \code{\link{Perfect}}
#' @rdname A005101
#' @aliases A005101
#' @export
Abundant <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  output = mpfrArray(rep(0, n), PrecisionBits)
  iter   = 0
  tgt    = 10
  while (iter<n){
    tgt = tgt + 1 # update the tgt number
    if (sum(large_divisors_proper(tgt, PrecisionBits)) > tgt){
      iter = iter + 1
      output[iter] = tgt
    }
  }

  ## Rmpfr
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}


