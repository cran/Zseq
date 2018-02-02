#' Regular numbers
#'
#' Under OEIS \href{https://oeis.org/A051037}{A051037}, a \emph{Regular} number - also known as 5-smooth - is a positive integer that
#' even divide powers of 60, or equivalently, whose prime divisors are only 2,3, and 5. First 6 Regular numbers are 1, 2, 3, 4, 5, 6.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Regular numbers
#' print(Regular(20))
#'
#' @rdname A051037
#' @aliases A051037
#' @export
Regular <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  output[1] = as.bigz(1)
  if (n>1){
    tgt = as.bigz(1)
    iter = 1
    while (iter<n){
      tgt = tgt + 1
      if (is.Regular(tgt)){
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

#' @keywords internal
#' @noRd
is.Regular <- function(n){
  pdivs = as.integer(gmp_divisors_prime(n))
  tgtps = as.integer(c(2,3,5))
  if (length(pdivs)<3){
    return(all(pdivs%in%tgtps))
  } else if (length(pdivs)==3){
    if (all(sort(pdivs)==sort(tgtps))){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}
