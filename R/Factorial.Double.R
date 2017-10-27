#' Double Factorial numbers
#'
#' Under OEIS \href{https://oeis.org/A000165}{A000165} and \href{https://oeis.org/A001147}{A001147}, a \emph{Double Factorial} is the factorial of numbers with same parity.
#' For example, if \eqn{n=5}, then \eqn{n!!=5*3*1}.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#' @param odd a logical; \code{TRUE} for double factorial of odd numbers, \code{FALSE} for even numbers.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 10 Factorials
#' first10 = Factorial(10)
#'
#' ## print without trailing 0's.
#' print(first10, drop0trailing = TRUE)
#'
#' @seealso \code{\link{Factorial}}
#' @rdname A000165
#' @aliases A000165 A001147
#' @export
Factorial.Double <- function(n, Rmpfr=TRUE, PrecisionBits=496, odd=TRUE){
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

  ## odd
  if (!is.logical(odd)){
    stop("* Zsequence : input 'odd' should be a logical variable.")
  }

  ## Main Computation : first, compute in Rmpfr form
  output = mpfrArray(rep(0,n), PrecisionBits)
  if (odd){
    output[1] = 1
    for (i in 2:n){
      output[i] = output[i-1]*(2*i-3)
    }
  } else {
    output[1] = 1
    if (n>=2){
      output[2] = 2
    }
    if (n>=3){
      for (i in 3:n){
        output[i] = output[i-1]*(2*i-2)
      }
    }
  }
  output[1] = 1
  for (i in 2:n){

  }

  ## Rmpfr
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}
