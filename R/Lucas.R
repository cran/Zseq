#' Lucas numbers
#'
#' Under OEIS \href{https://oeis.org/A000032}{A000032}, the \emph{n}th \emph{Lucas} number is given as
#' \deqn{F_n = F_{n-1} + F_{n-2}}
#' where the first 6 entries are 2, 1, 3, 4, 7, 11.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Lucas numbers
#' print(Lucas(30))
#'
#' @rdname A000032
#' @aliases A000032
#' @seealso \code{\link{Fibonacci}}
#' @export
Lucas <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  first6 = as.bigz(c(2,1,3,4,7,11))
  if (n<=6){
    output = first6[1:n]
  } else {
    output[1:6] = first6
    for (i in 7:n){
      output[i] = output[i-1]+output[i-2]
    }
  }

  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}

