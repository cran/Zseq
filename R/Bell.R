#' Bell numbers
#'
#' Under OEIS \href{https://oeis.org/A000110}{A000110}, the \emph{n}th \emph{Bell} number is
#' the number of ways to partition a set of \code{n} labeled elements,
#' where the first 6 entries are 1, 1, 2, 5, 15, 52.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Bell numbers
#' first30 = Bell(30)
#'
#' ## print without trailing 0's.
#' print(first30, drop0trailing = TRUE)
#'
#' @rdname A000110
#' @aliases A000110
#' @export
Bell <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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

  ## First 6 arguments
  output = mpfrArray(rep(0,n), PrecisionBits)
  first6 = mpfr(c(1, 1, 2, 5, 15, 52), PrecisionBits)
  if (n<=6){
    output = first6[1:n]
  } else {
    output[1:6] = first6
    for (i in 7:n){
      coeffs = chooseMpfr((i-2), seq(from=0,to=(i-2),by=1))
      output[i] = sum(output[1:(i-1)]*coeffs)
    }
  }

  ## return
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}
