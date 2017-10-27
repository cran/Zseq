#' Number of steps for Juggler sequence
#'
#' Under OEIS \href{https://oeis.org/A007320}{A007320}, a \emph{Number of steps for Juggler sequence} literally counts the number of steps
#' required for a sequence that starts from \code{n}. First 6 terms are  	0, 1, 6, 2, 5, 2 that \code{n} starting from 0 is conventional choice.
#' Note that when it counts \emph{number of steps}, not the length of the sequence including the last 1.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 10 numbers of steps for Juggler sequences
#' first10 = Juggler.Nsteps(10)
#'
#' ## print without trailing 0's.
#' print(first10, drop0trailing = TRUE)
#'
#' @seealso \code{\link{Juggler}}
#' @rdname  A007320
#' @aliases  A007320
#' @export
Juggler.Nsteps <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  for (i in 2:n){
    output[i] = length(Juggler(i))-1
  }

  ## Rmpfr
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}
