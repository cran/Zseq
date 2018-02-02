#' Palindromic numbers
#'
#' Under OEIS \href{https://oeis.org/A002113}{A002113}, a \emph{Palindromic} number is a number that
#' remains the same when its digits are reversed. First 6 Palindromic numbers in decimal are 0, 1, 2, 3, 4, 5.
#' This function supports various base by specifying the parameter \code{base} but returns are still represented in decimal.
#'
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param base choice of base.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 palindromic number in decimal
#' print(Palindromic(30))
#'
#' @rdname A002113
#' @aliases A002113
#' @export
Palindromic <- function(n, base=10, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Base
  if ((length(base)!=1)||(abs(base-round(base))>sqrt(.Machine$double.eps))||(n<2)){
    stop("* Zsequence : input 'base' should be a positive integer >= 2.")
  }
  base = as.integer(base)

  ## Main Computation : first, compute in gmp form
  output = as.bigz(numeric(n))
  output[1] = 0
  if (n>1){
    tgt = as.bigz(0)
    iter= 1
    while (iter < n){
      tgt = tgt + 1
      if (is.Palindromic(tgt, base)){
        iter = iter + 1
        output[iter] = tgt
      }
    }
  }

  ## Rmpfr
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}



#' @keywords internal
#' @noRd
is.Palindromic <- function(n, p){
  strx = gmp_p_ary(n, p)
  revx = rev(strx)
  if (all(strx==revx)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
