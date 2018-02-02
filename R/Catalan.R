#' Catalan numbers
#'
#' Under OEIS \href{https://oeis.org/A000108}{A000108}, the \emph{n}th \emph{Catalan} number is given as
#' \deqn{C_n = \frac{(2n)!}{(n+1)!n!}}
#' where the first 6 entries are 1, 1, 2, 5, 14, 42 with \eqn{n\ge 0.}
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Catalan numbers
#' print(Catalan(30))
#'
#' @rdname A000108
#' @aliases A000108 Segner
#' @export
Catalan <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(1:n)
  for (i in 1:n){
    output[i] = Catalan.single(round(i-1))
  }

  ## Rmpfr
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}

# Works with zero-based numbering
#' @keywords internal
#' @noRd
Catalan.single <- function(n){
  if (n==0){
    return(1)
  } else if (n==1){
    return(1)
  } else {
    sequpper = prod(as.bigz(seq(from=2,to=n,by=1)+n))
    seqlower = prod(as.bigz(seq(from=2,to=n,by=1)))
    return(sequpper/seqlower)
  }
}

