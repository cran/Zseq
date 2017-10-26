#' Thabit numbers
#'
#' Under OEIS \href{https://oeis.org/A055010}{A055010}, the \emph{n}th \emph{Thabit} number is given as
#' \deqn{A_n = 3*2^{n-1}-1}
#' where the first 6 entries are  	0, 2, 5, 11, 23, 47 with \eqn{A_0 = 0.}
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Thabit numbers
#' first30 = Thabit(30)
#'
#' ## print without trailing 0's.
#' print(first30, drop0trailing = TRUE)
#'
#' @rdname A055010
#' @aliases A055010
#' @export
Thabit <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  if (n>1){
    output[2:n] =(3*(2^mpfrArray((1:(n-1))-1, PrecisionBits))-1)
  }


  ## Rmpfr
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}
