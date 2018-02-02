#' Extravagant numbers
#'
#' Under OEIS \href{https://oeis.org/A046760}{A046760}, an \emph{Extravagant} number has less digits
#' than the number of digits in its prime factorization including exponents. First 6 Extravagant numbers are   	4, 6, 8, 9, 12, 18. Though
#' it doesn't matter which base we use, here we adopt only a base of 10.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Extravagant numbers
#' print(Extravagant(20))
#'
#' @seealso \code{\link{Frugal}}, \code{\link{Equidigital}}
#' @rdname A046760
#' @aliases A046760 Wasteful
#' @export
Extravagant <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  output[1] = as.bigz(4)
  if (n>1){
    tgt = as.bigz(4)
    iter = 1
    while (iter<n){
      tgt = tgt + 1
      if (is.Extravagant(tgt)){
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
is.Extravagant <- function(tgt){
  doriginal = countdigit_bigz(tgt)

  # 2. prime factorization
  pfactor = gmp_primefactorization(tgt)
  primes  = pfactor$primes
  counts  = pfactor$multiplicity

  dfactored = sum(unlist(lapply(primes, countdigit_bigz)))+sum(unlist(lapply(counts, countdigit_bigz)))

  if (doriginal < dfactored){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
