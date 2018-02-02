#' Bell numbers
#'
#' Under OEIS \href{https://oeis.org/A000110}{A000110}, the \emph{n}th \emph{Bell} number is
#' the number of ways to partition a set of \code{n} labeled elements,
#' where the first 6 entries are 1, 1, 2, 5, 15, 52.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Bell numbers and print
#' print(Bell(30))
#'
#' @rdname A000110
#' @aliases A000110
#' @export
Bell <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## First 6 arguments
  output = gmp::as.bigz(numeric(n))
  first6 = gmp::as.bigz(c(1, 1, 2, 5, 15, 52))
  if (n<=6){
    output = first6[1:n]
  } else {
    output[1:6] = first6
    for (i in 7:n){
      coeffs = gmp::chooseZ((i-2), seq(from=0,to=(i-2),by=1))
      output[i] = sum(output[1:(i-1)]*coeffs)
    }
  }

  ## return
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}
