#' Equidigital numbers
#'
#' Under OEIS \href{https://oeis.org/A046758}{A046758}, an \emph{Equidigital} number has equal digits
#' as the number of digits in its prime factorization including exponents. First 6 Equidigital numbers are 1, 2, 3, 5, 7, 10. Though
#' it doesn't matter which base we use, here we adopt only a base of 10.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Equidigital numbers
#' print(Equidigital(20))
#'
#' @seealso \code{\link{Frugal}}, \code{\link{Extravagant}}
#' @rdname A046758
#' @aliases A046758
#' @export
Equidigital <- function(n, gmp=TRUE){
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
      if (is.Equidigital(tgt)){
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
is.Equidigital <- function(tgt){
  # 1. original digits
  doriginal = countdigit_bigz(tgt)

  # 2. prime factorization
  pfactor = gmp_primefactorization(tgt)
  primes  = pfactor$primes
  counts  = pfactor$multiplicity

  dfactored = sum(unlist(lapply(primes, countdigit_bigz)))+sum(unlist(lapply(counts, countdigit_bigz)))


  #
  # dfactored = 0
  # for (i in 1:length(pfactor$primes)){
  #   dfactored = dfactored + floor(log10(pfactor$primes[i]))+1
  #   if (pfactor$multiplicity[i]!=1){
  #     dfactored = dfactored + floor(log10(pfactor$multiplicity[i]))+1
  #   }
  # }

  if (doriginal==dfactored){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
countdigit_bigz <- function(x){
  if (!is.bigz(x)){
    x = as.bigz(x)
  }
  if (x==1){
    return(0)
  } else {
    return(length(unlist(strsplit(as.character(x),""))))
  }
}
