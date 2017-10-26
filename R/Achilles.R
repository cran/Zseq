#' Achilles numbers
#'
#' Under OEIS \href{https://oeis.org/A052486}{A052486}, an \emph{Achilles} number is a number
#' that is \emph{powerful} but \emph{not perfect}. First 6 Achilles numbers are 72, 108, 200, 288, 392, 432.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 3 Achilles numbers
#' first3 = Achilles(3)
#'
#' ## print without trailing 0's.
#' print(first3, drop0trailing = TRUE)
#'
#' @rdname A052486
#' @aliases A052486
#' @export
Achilles <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  output[1] = 72
  if (n>1){
    tgt = mpfr(100, PrecisionBits)
    iter = 1
    while (iter<n){
      tgt = tgt + 1
      if (is.Achilles(tgt, PrecisionBits)){
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
is.Achilles <- function(tgt, PrecisionBits){
  # cond1 : powerful
  cond1 = (is.Powerful(tgt, PrecisionBits))
  # cond2 : gcd(exponents)=1
  pfactor = large_primefactorization(tgt, PrecisionBits)
  if (length(pfactor$multiplicity)==1){
    cond2 = TRUE
  } else {
    cond2 = (large_gcd_mpfrArray(pfactor$multiplicity, PrecisionBits)==1)
  }
  cond3 = (length(pfactor$multiplicity)>1)

  if (cond1&&cond2&&cond3){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
