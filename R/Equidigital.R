#' Equidigital numbers
#'
#' Under OEIS \href{https://oeis.org/A046758}{A046758}, an \emph{Equidigital} number has equal digits
#' than the number of digits in its prime factorization including exponents. First 6 Equidigital numbers are 1, 2, 3, 5, 7, 10. Though
#' it doesn't matter which base we use, here we adopt only a base of 10.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Equidigital numbers
#' first20 = Equidigital(20)
#'
#' ## print without trailing 0's.
#' print(first20, drop0trailing = TRUE)
#'
#' @seealso \code{\link{Frugal}}, \code{\link{Extravagant}}
#' @rdname A046758
#' @aliases A046758
#' @export
Equidigital <- function(n, Rmpfr=TRUE, PrecisionBits=496){
  ## Preprocessing for 'n'
  if ((length(n)!=1)||(abs(n-round(n))>sqrt(.Machine$double.eps))||(n<0)){
    stop("* Zsequence : input 'n' should be a positive integer.")
  }
  n = as.integer(n)

  ## Control over PrecisionBits
  if (!missing(PrecisionBits)){
    if ((length(PrecisionBits)!=1)||(PrecisionBits<2)||(is.infinite(PrecisionBits))||(is.na(PrecisionBits))||(abs(PrecisionBits-round(PrecisionBits))>sqrt(.Machine$double.eps))){
      stop("* Zsequence : input 'PrecisionBits' should be a positive integer >= 2.")
    }
    PrecisionBits = as.integer(PrecisionBits)
  }

  ## Main Computation : first, compute in Rmpfr form
  output = mpfrArray(rep(0,n), PrecisionBits)
  output[1] = 1
  if (n>1){
    tgt = mpfr(1, PrecisionBits)
    iter = 1
    while (iter<n){
      tgt = tgt + 1
      if (is.Equidigital(tgt, PrecisionBits)){
        iter = iter+1
        output[iter] = tgt
      }
    }
  }


  ## Rmpfr
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}



#' @keywords internal
#' @noRd
is.Equidigital <- function(tgt, PrecisionBits){
  doriginal = floor(log10(tgt))+1

  pfactor = large_primefactorization(tgt, PrecisionBits)
  dfactored = 0
  for (i in 1:length(pfactor$primes)){
    dfactored = dfactored + floor(log10(pfactor$primes[i]))+1
    if (pfactor$multiplicity[i]!=1){
      dfactored = dfactored + floor(log10(pfactor$multiplicity[i]))+1
    }
  }

  if (doriginal==dfactored){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
