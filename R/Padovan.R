#' Padovan numbers
#'
#' Under OEIS \href{https://oeis.org/A000931}{A000931}, the \emph{n}th \emph{Padovan} number is given as
#' \deqn{F_n = F_{n-2} + F_{n-3}}
#' where the first 6 entries are 1, 0, 0, 1, 0, 1.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Padovan numbers
#' first30 = Padovan(30)
#'
#' ## print without trailing 0's.
#' print(first30, drop0trailing = TRUE)
#'
#' @rdname A000931
#' @aliases A000931
#' @export
Padovan <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  first6 = mpfr(c(1, 0, 0, 1, 0, 1), PrecisionBits)
  if (n<=6){
    output = first6[1:n]
  } else {
    output[1:6] = first6
    for (i in 7:n){
      output[i] = output[i-2]+output[i-3]
    }
  }

  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}

