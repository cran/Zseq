#' Unusual numbers
#'
#' Under OEIS \href{https://oeis.org/A064052}{A064052}, an \emph{Unusual} number is a natural number whose largest prime factor is strictly greater than square root of the number.
#' First 6 Unusual numbers are 2, 3, 5, 6, 7, 10.
#'
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Unusual numbers
#' first20 = Unusual(20)
#'
#' ## print without trailing 0's.
#' print(first20, drop0trailing = TRUE)
#'
#' @rdname A064052
#' @aliases A064052
#' @export
Unusual <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  output[1] = 2
  if (n>1){
    tgt = mpfr(2, PrecisionBits)
    iter = 1
    while (iter<n){
      tgt = tgt + 1
      if (is.Unusual(tgt, PrecisionBits)){
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
is.Unusual <- function(n, PrecisionBits){
  maxpfac = max(large_divisors_prime(n, PrecisionBits))
  if (maxpfac>sqrt(n)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

