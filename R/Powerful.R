#' Powerful numbers
#'
#' Under OEIS \href{https://oeis.org/A001694}{A001694}, a \emph{Powerful} number is a positive integer such that
#' for every prime \eqn{p} dividing the number, \eqn{p^2} also divides the number. First
#' 6 powerful numbers are 1, 4, 8, 9, 16, 25.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Powerful numbers
#' print(Powerful(20))
#'
#' @rdname A001694
#' @aliases A001694
#' @export
Powerful <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  output[1] = as.bigz(1)
  if (n>1){
    tgt = as.bigz(2)
    iter = 1
    while (iter<n){
      tgt = tgt + 1
      if (is.Powerful(tgt)){
        iter = iter+1
        output[iter] = tgt
      }
    }
  }
  ## Rmpfr
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}

# 7. large_ispowerful
#' @keywords internal
#' @noRd
is.Powerful <- function(n){
  theframe = gmp_primefactorization(n)
  if (min(theframe$multiplicity)>=2){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
