#' Motzkin numbers
#'
#' Under OEIS \href{https://oeis.org/A001006}{A001006}, a \emph{Motzkin} number for a given \emph{n} is the number
#' of ways for drawing non-intersecting chords among \emph{n} points on a circle,
#' where the first 7 entries are 1, 1, 2, 4, 9, 21, 51.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Motzkin numbers
#' first30 = Motzkin(30)
#'
#' ## print without trailing 0's.
#' print(first30, drop0trailing = TRUE)
#'
#' @rdname A001006
#' @aliases A001006
#' @export
Motzkin <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  output = mpfrArray(1:n, PrecisionBits)

  if (n==1){
    output[1] = 1
    if (!Rmpfr){output=as.integer(output)}
    return(output)
  } else {
    output[1] = 1
    output[2] = 1
    if (n==2){
      if (!Rmpfr){
        output = as.integer(output)
      }
      return(output)
    } else {
      for (i in 3:n){
        m = (i-1)
        output[i] = (((2*m)+1)*output[i-1]/(m+2))+(output[i-2]*((3*m)-3)/(m+2))
      }
      if (!Rmpfr){
        output = as.integer(output)
      }
      return(output)
    }
  }
}

