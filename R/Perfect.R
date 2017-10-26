#' Perfect numbers
#'
#' Under OEIS \href{https://oeis.org/A000396}{A000396}, a \emph{Perfect} number is
#' a number whose proper divisors sum up to the extent equal to the number itself. First
#' 6 abundant numbers are	6, 28, 496, 8128, 33550336, 8589869056.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 7 Perfect numbers
#' first7 = Perfect(10)
#'
#' ## print without trailing 0's.
#' print(first7, drop0trailing = TRUE)
#'
#' @seealso \code{\link{Deficient}}, \code{\link{Abundant}}
#' @rdname A000396
#' @aliases A000396
#' @export
Perfect <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  first10 = mpfr(c( 	6, 28, 496, 8128, 33550336, 8589869056, 137438691328, 2305843008139952128, 2658455991569831744654692615953842176, 191561942608236107294793378084303638130997321548169216), PrecisionBits)
  if (n<=10){
    output = first10[1:n]
  } else {
    output[1:10] = first10
    iter = 10
    tgt  = mpfr(191561942608236107294793378084303638130997321548169216, PrecisionBits)
    while (iter < n){
      tgt = tgt + 1
      if (sum(large_divisors_proper(tgt, PrecisionBits))==tgt){
        iter = as.integer(iter + 1)
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
is.Perfect <- function(tgt, PrecisionBits){
  pdivs = large_divisors_proper(tgt, PrecisionBits)
  if (abs(sum(pdivs)-tgt)<sqrt(.Machine$double.eps)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
