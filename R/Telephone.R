#' Telephone numbers
#'
#' Under OEIS \href{https://oeis.org/A000085}{A000085}, a \emph{Telephone} number - also known as \emph{Involution} number - is counting the number
#' of connection patterns in a telephone system with \code{n} subscribers, or in a more mathematical term, the number of self-inverse permutations on
#' \code{n} letters. First 6 Telephone numbers are  	1, 1, 2, 4, 10, 26,
#'
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Regular numbers
#' first20 = Regular(20)
#'
#' ## print without trailing 0's.
#' print(first20, drop0trailing = TRUE)
#'
#' @rdname A000085
#' @aliases A000085 Involution
#' @export
Telephone <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  if (n>=2){
    output[2] = 1
  }
  if (n>2){
    for (i in 3:n){
      output[i] = output[i-1] + (i-2)*output[i-2] # here the multiplier (i-2) is not identical to OEIS : just for starting index issue.
    }
  }

  ## Rmpfr
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}
