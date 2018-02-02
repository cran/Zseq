#' Padovan numbers
#'
#' Under OEIS \href{https://oeis.org/A000931}{A000931}, the \emph{n}th \emph{Padovan} number is given as
#' \deqn{F_n = F_{n-2} + F_{n-3}}
#' where the first 6 entries are 1, 0, 0, 1, 0, 1.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Padovan numbers
#' print(Padovan(30))
#'
#' @rdname A000931
#' @aliases A000931
#' @export
Padovan <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  first6 = as.bigz(c(1,0,0,1,0,1))
  if (n<=6){
    output = first6[1:n]
  } else {
    output[1:6] = first6
    for (i in 7:n){
      output[i] = output[i-2]+output[i-3]
    }
  }

  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}

