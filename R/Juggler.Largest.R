#' Largest value for Juggler sequence
#'
#' Under OEIS \href{https://oeis.org/A094716}{A094716}, the \emph{Largest value for Juggler sequence} is the largest value in trajectory of
#' a sequence that starts from \code{n}. First 6 terms are 0, 1, 2, 36, 4, 36 that \code{n} starting from 0 is conventional choice.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 10 numbers of largest values for Juggler sequences
#' print(Juggler.Largest(10))
#'
#' @seealso \code{\link{Juggler}}
#' @rdname  A094716
#' @aliases  A094716
#' @export
Juggler.Largest <- function(n, gmp=TRUE){
  ## Preprocessing for 'start'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
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
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}
