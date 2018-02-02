#' Fibonacci numbers
#'
#' Under OEIS \href{https://oeis.org/A000045}{A000045}, the \emph{n}th \emph{Fibonnaci} number is given as
#' \deqn{F_n = F_{n-1} + F_{n-2}}
#' where the first 6 entries are 0, 1, 1, 2, 3, 5 with \eqn{n\ge 0.}
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Fibonacci numbers
#' print(Fibonacci(30))
#'
#' @rdname A000045
#' @aliases A000045
#' @export
Fibonacci <- function(n, gmp=FALSE){
  ## Preprocessing for 'n'
  n = check_n(n)
  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))

  if (n==1){
    output[1] = as.bigz(0)
    if (!gmp){output=as.integer(output)}
    return(output)
  } else {
    output[1] = as.bigz(0)
    output[2] = as.bigz(1)
    if (n==2){
      if (!gmp){
        output = as.integer(output)
      }
      return(output)
    } else {
      for (i in 3:n){
        output[i] = output[i-1]+output[i-2]
      }
      if (!gmp){
        output = as.integer(output)
      }
      return(output)
    }
  }
}

