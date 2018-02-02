#' Motzkin numbers
#'
#' Under OEIS \href{https://oeis.org/A001006}{A001006}, a \emph{Motzkin} number for a given \emph{n} is the number
#' of ways for drawing non-intersecting chords among \emph{n} points on a circle,
#' where the first 7 entries are 1, 1, 2, 4, 9, 21, 51.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Motzkin numbers
#' print(Motzkin(30))
#'
#' @rdname A001006
#' @aliases A001006
#' @export
Motzkin <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(1:n)

  if (n==1){
    output[1] = as.bigz(1)
    if (!gmp){output=as.integer(output)}
    return(output)
  } else {
    output[1] = as.bigz(1)
    output[2] = as.bigz(1)
    if (n==2){
      if (!gmp){
        output = as.integer(output)
      }
      return(output)
    } else {
      for (i in 3:n){
        m = (i-1)
        output[i] = (((2*m)+1)*output[i-1]/(m+2))+(output[i-2]*((3*m)-3)/(m+2))
      }
      if (!gmp){
        output = as.integer(output)
      }
      return(output)
    }
  }
}

