#' Largest value for Juggler sequence
#'
#' Under OEIS \href{https://oeis.org/A094716}{A094716}, the \emph{Largest value for Juggler sequence} is the largest value in trajectory of
#' a sequence that starts from \code{n}. First 6 terms are 0, 1, 2, 36, 4, 36 that \code{n} starting from 0 is conventional choice.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 10 numbers of largest values for Juggler sequences
#' first10 = Juggler.Largest(10)
#'
#' ## print without trailing 0's.
#' print(first10, drop0trailing = TRUE)
#'
#' @seealso \code{\link{Juggler}}
#' @rdname  A094716
#' @aliases  A094716
#' @export
Juggler.Largest <- function(n, Rmpfr=TRUE, PrecisionBits=496){
  ## Preprocessing for 'start'
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
  if (n==2){
    output[2] = 1
  }
  if (n>=2){
    output[2] = 1
    for (i in 3:n){
      output[i] = max(Juggler(i-1))
    }
  }

  ## Rmpfr
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}
