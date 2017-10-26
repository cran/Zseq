#' Frugal numbers
#'
#' Under OEIS \href{https://oeis.org/A046759}{A046759}, a \emph{Frugal} number has more digits
#' than the number of digits in its prime factorization including exponents. First 6 Frugal numbers are  	125, 128, 243, 256, 343, 512. Though
#' it doesn't matter which base we use, here we adopt only a base of 10.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 5 Frugal numbers
#' first5 = Frugal(5)
#'
#' ## print without trailing 0's.
#' print(first5, drop0trailing = TRUE)
#'
#' @seealso \code{\link{Extravagant}}, \code{\link{Equidigital}}
#' @rdname A046759
#' @aliases A046759 Economical
#' @export
Frugal <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  output[1] = 125
  if (n>1){
    tgt = mpfr(126, PrecisionBits)
    iter = 1
    while (iter<n){
      tgt = tgt + 1
      if (is.Frugal(tgt, PrecisionBits)){
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
is.Frugal <- function(tgt, PrecisionBits){
  doriginal = floor(log10(tgt))+1

  pfactor = large_primefactorization(tgt, PrecisionBits)
  dfactored = 0
  for (i in 1:length(pfactor$primes)){
    dfactored = dfactored + floor(log10(pfactor$primes[i]))+1
    if (pfactor$multiplicity[i]!=1){
      dfactored = dfactored + floor(log10(pfactor$multiplicity[i]))+1
    }
  }

  if (doriginal > dfactored){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

