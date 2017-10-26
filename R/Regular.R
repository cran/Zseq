#' Regular numbers
#'
#' Under OEIS \href{https://oeis.org/A051037}{A051037}, a \emph{Regular} number - also known as 5-smooth - is a positive integer that
#' even divide powers of 60, or equivalently, whose prime divisors are only 2,3, and 5. First 6 Regular numbers are 1, 2, 3, 4, 5, 6.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Regular numbers
#' first20 = Regular(20)
#'
#' ## print without trailing 0's.
#' print(first20, drop0trailing = TRUE)
#'
#' @rdname A051037
#' @aliases A051037
#' @export
Regular <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
      if (is.Regular(tgt, PrecisionBits)){
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
is.Regular <- function(n, PrecisionBits){
  pdivs = as.integer(large_divisors_prime(n, PrecisionBits))
  tgtps = c(2,3,5)
  if (length(pdivs)<3){
    return(all(pdivs%in%tgtps))
  } else {
    return((all(pdivs%in%tgtps))&&(all(tgtps%in%pdivs)))
}
}
