#' Unusual numbers
#'
#' Under OEIS \href{https://oeis.org/A064052}{A064052}, an \emph{Unusual} number is a natural number whose largest prime factor is strictly greater than square root of the number.
#' First 6 Unusual numbers are 2, 3, 5, 6, 7, 10.
#'
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Unusual numbers
#' print(Unusual(20))
#'
#' @rdname A064052
#' @aliases A064052
#' @export
Unusual <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  output[1] = 2
  if (n>1){
    tgt = as.bigz(2)
    iter = 1
    while (iter<n){
      tgt = tgt + 1
      if (is.Unusual(tgt)){
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
is.Unusual <- function(n){
  maxpfac = max(gmp_divisors_prime(n))
  if (maxpfac>sqrt(gmp::asNumeric(n))){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

