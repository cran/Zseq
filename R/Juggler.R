#' Juggler sequence
#'
#' Under OEIS \href{https://oeis.org/A094683}{A094683}, a \emph{Juggler} sequence is an integer-valued
#' sequence that starts with a nonnegative number iteratively follows that \eqn{J_{k+1}=floor(J_k^{1/2})} if \eqn{J_k} is even, or \eqn{J_{k+1}=floor(J_k^{3/2})} if odd.
#' No first 6 terms are given since it all depends on the starting value.
#'
#' @param start the starting nonnegative integer.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector recording the sequence of unknown length a priori.
#'
#' @examples
#' ## let's start from 9 and show the sequence
#' print(Juggler(9), drop0trailing=TRUE)
#'
#' @rdname A094683
#' @aliases A094683
#' @export
Juggler <- function(start, Rmpfr=TRUE, PrecisionBits=496){
  ## Preprocessing for 'start'
  if ((length(start)!=1)||(abs(start-round(start))>sqrt(.Machine$double.eps))||(start<0)){
    stop("* Zsequence : input 'start' should be a nonnegative integer.")
  }
  start = as.integer(start)

  ## Control over PrecisionBits
  if (!missing(PrecisionBits)){
    if ((length(PrecisionBits)!=1)||(PrecisionBits<2)||(is.infinite(PrecisionBits))||(is.na(PrecisionBits))||(abs(PrecisionBits-round(PrecisionBits))>sqrt(.Machine$double.eps))){
      stop("* Zsequence : input 'PrecisionBits' should be a positive integer >= 2.")
    }
    PrecisionBits = as.integer(PrecisionBits)
  }

  ## Main Computation : first, compute in Rmpfr form
  if (start==0){
    output = mpfrArray(0, PrecisionBits)
  } else if (start==1){
    output = mpfrArray(1, PrecisionBits)
  } else {
    output = mpfrArray(start, PrecisionBits)
    start  = mpfr(start, PrecisionBits)
    while (start>1){
      if (start%%2==0){
        start = floor(sqrt(start))
        output = append(output, start)
      } else {
        start = floor((sqrt(start)^3))
        output = append(output, start)
      }
    }
  }


  ## Rmpfr
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}
