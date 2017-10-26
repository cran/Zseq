#' Fibonacci numbers
#'
#' Under OEIS \href{https://oeis.org/A000045}{A000045}, the \emph{n}th \emph{Fibonnaci} number is given as
#' \deqn{F_n = F_{n-1} + F_{n-2}}
#' where the first 6 entries are 0, 1, 1, 2, 3, 5 with \eqn{n\ge 0.}
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Fibonacci numbers
#' first30 = Fibonacci(30)
#'
#' ## print without trailing 0's.
#' print(first30, drop0trailing = TRUE)
#'
#' @rdname A000045
#' @aliases A000045
#' @export
Fibonacci <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  output = mpfrArray(rep(0,6), PrecisionBits)

  if (n==1){
    output[1] = 0
    if (!Rmpfr){output=as.integer(output)}
    return(output)
  } else {
    output[1] = 0
    output[2] = 1
    if (n==2){
      if (!Rmpfr){
        output = as.integer(output)
      }
      return(output)
    } else {
      for (i in 3:n){
        output[i] = output[i-1]+output[i-2]
      }
      if (!Rmpfr){
        output = as.integer(output)
      }
      return(output)
    }
  }
}

