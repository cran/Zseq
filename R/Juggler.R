#' Juggler sequence
#'
#' Under OEIS \href{https://oeis.org/A094683}{A094683}, a \emph{Juggler} sequence is an integer-valued
#' sequence that starts with a nonnegative number iteratively follows that \eqn{J_{k+1}=floor(J_k^{1/2})} if \eqn{J_k} is even, or \eqn{J_{k+1}=floor(J_k^{3/2})} if odd.
#' No first 6 terms are given since it all depends on the starting value.
#'
#' @param start the starting nonnegative integer.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector recording the sequence of unknown length a priori.
#'
#' @examples
#' ## let's start from 9 and show the sequence
#' print(Juggler(9))
#'
#' @rdname A094683
#' @aliases A094683
#' @export
Juggler <- function(start, gmp=TRUE){
  ## Preprocessing for 'start'
  if ((length(start)!=1)||(abs(start-round(start))>sqrt(.Machine$double.eps))||(start<0)){
    stop("* Zsequence : input 'start' should be a nonnegative integer.")
  }
  start = as.integer(start)

  ## Main Computation : first, compute in Rmpfr form
  if (start==0){
    output = as.bigz(0)
  } else if (start==1){
    output = as.bigz(1)
  } else {
    output = as.bigz(start)
    start  = as.integer(start)
    while (start>1){
      if (start%%2==0){
        start = (floor(sqrt(start)))
        output = append(output, as.bigz(start))
      } else {
        start = (floor((sqrt(start)^3)))
        output = append(output, as.bigz(start))
      }
    }
  }


  ## Rmpfr
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}
