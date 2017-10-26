#' Powerful numbers
#'
#' Under OEIS \href{https://oeis.org/A001694}{A001694}, a \emph{Powerful} number is a positive integer such that
#' for every prime \eqn{p} dividing the number, \eqn{p^2} also divides the number. First
#' 6 powerful numbers are 1, 4, 8, 9, 16, 25.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 20 Powerful numbers
#' first20 = Powerful(20)
#'
#' ## print without trailing 0's.
#' print(first20, drop0trailing = TRUE)
#'
#' @rdname A001694
#' @aliases A001694
#' @export
Powerful <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
    tgt = mpfr(2, PrecisionBits)
    iter = 1
    while (iter<n){
      tgt = tgt + 1
      if (is.Powerful(tgt, PrecisionBits)){
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

# 7. large_ispowerful
#' @keywords internal
#' @noRd
is.Powerful <- function(n, PrecisionBits){
  theframe = large_primefactorization(n, PrecisionBits)
  if (min(theframe$multiplicity)>=2){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
