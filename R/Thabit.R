#' Thabit numbers
#'
#' Under OEIS \href{https://oeis.org/A055010}{A055010}, the \emph{n}th \emph{Thabit} number is given as
#' \deqn{A_n = 3*2^{n-1}-1}
#' where the first 6 entries are  	0, 2, 5, 11, 23, 47 with \eqn{A_0 = 0.}
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Thabit numbers
#' print(Thabit(30))
#'
#' @rdname A055010
#' @aliases A055010
#' @export
Thabit <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  if (n>1){
    output[2:n] =(3*(2^as.bigz(seq(from=0,to=(n-2),by=1)))-1)
  }

  ## Rmpfr
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}
