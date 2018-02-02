#' Double Factorial numbers
#'
#' Under OEIS \href{https://oeis.org/A000165}{A000165} and \href{https://oeis.org/A001147}{A001147}, a \emph{Double Factorial} is the factorial of numbers with same parity.
#' For example, if \eqn{n=5}, then \eqn{n!!=5*3*1}.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param odd a logical; \code{TRUE} for double factorial of odd numbers, \code{FALSE} for even numbers.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 10 odd Factorials
#' print(Factorial(10))
#'
#' @seealso \code{\link{Factorial}}
#' @rdname A000165
#' @aliases A000165 A001147
#' @export
Factorial.Double <- function(n, gmp=TRUE, odd=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## odd
  if (!is.logical(odd)){
    stop("* Zsequence : input 'odd' should be a logical variable.")
  }

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  if (odd){
    output[1] = as.bigz(1)
    for (i in 2:n){
      output[i] = output[i-1]*(as.bigz(2*i-3))
    }
  } else {
    output[1] = as.bigz(1)
    if (n>=2){
      output[2] = as.bigz(2)
    }
    if (n>=3){
      for (i in 3:n){
        output[i] = output[i-1]*(as.bigz((2*i-2)))
      }
    }
  }


  ## gmp
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}
