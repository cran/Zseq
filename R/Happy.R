#' Happy numbers
#'
#' Under OEIS \href{https://oeis.org/A007770}{A007770}, a \emph{Happy} number is defined by the process that starts from
#' arbitrary positive integer and replaces the number by the sum of the squares of each digit until the number is 1.
#' First 6 Happy numbers are 1, 7, 10, 13, 19, 23.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 happy numbers
#' first30 = Happy(30)
#'
#' ## print without trailing 0's.
#' print(first30, drop0trailing = TRUE)
#'
#' @rdname A007770
#' @aliases A007770
#' @export
Happy <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
    iter= 1
    while (iter < n){
      tgt = tgt + 1
      if (is.Happy(tgt)){
        iter = iter + 1
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
is.Happy <- function(n){
  if (n==1){
    return(TRUE)
  } else if (n==4){
    return(FALSE)
  } else {
    return(is.Happy(sum(large_p_ary(n, 10)^2)))
  }
}
