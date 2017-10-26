#' Deficient numbers
#'
#' Under OEIS \href{https://oeis.org/A005100}{A005100}, a \emph{deficient} number is
#' a number whose proper divisors sum up to the extent smaller than the number itself. First
#' 6 abundant numbers are 	1, 2, 3, 4, 5, 7
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Deficient numbers
#' first30 = Deficient(30)
#'
#' ## print without trailing 0's.
#' print(first30, drop0trailing = TRUE)
#'
#' @seealso \code{\link{Abundant}}, \code{\link{Perfect}}
#' @rdname A005100
#' @aliases A005100
#' @export
Deficient <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  iter   = 0
  tgt    = 1
  while (iter<(n-1)){
    tgt = tgt + 1 # update the tgt number
    if  (sum(large_divisors_proper(tgt, PrecisionBits)) < tgt){
      iter = iter + 1
      output[iter+1] = tgt
    }
  }

  ## Rmpfr
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}

