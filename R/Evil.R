#' Evil numbers
#'
#' Under OEIS \href{https://oeis.org/A001969}{A001969}, an \emph{Evil} number has an even number of 1's in its binary expansion.
#' First 6 Evil numbers are  	0, 3, 5, 6, 9, 10.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Evil numbers
#' print(Evil(20))
#'
#' @seealso \code{\link{Odious}}
#' @rdname A001969
#' @aliases A001969
#' @export
Evil <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  output[1] = as.bigz(0)
  if (n>1){
    tgt = as.bigz(1)
    iter = 1
    while (iter<n){
      tgt = tgt + 1
      if (is.Evil(tgt)){
        iter = iter+1
        output[iter] = tgt
      }
    }
  }
  ## gmp
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}


#' @keywords internal
#' @noRd
is.Evil <- function(n){
  return(length(which(gmp_binarize(n)==1))%%2==0)
}
