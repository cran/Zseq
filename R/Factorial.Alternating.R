#' Alternating Factorial numbers
#'
#' Under OEIS \href{https://oeis.org/A005165}{A005165}, an \emph{Alternating Factorial} is the absolute value of the alternating sum of the
#' first \code{n} factorials of positive integers. First 6 such numbers are  	0, 1, 1, 5, 19, 101.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 5 Alternating Factorial numbers
#' print(Factorial.Alternating(5))
#'
#' @seealso \code{\link{Factorial}}
#' @rdname A005165
#' @aliases A005165
#' @export
Factorial.Alternating <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  if (n==1){
    output[1] = as.bigz(0)
  }
  if (n==2){
    output[1] = as.bigz(0)
    output[2] = as.bigz(1)
  }
  if (n>2){
    output[1] = as.bigz(0)
    output[2] = as.bigz(1)
    for (i in 3:n){
      output[i] = gmp::factorialZ(as.bigz(i-1))-output[i-1]
    }
  }

  ## Rmpfr
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}


