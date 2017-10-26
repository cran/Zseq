#' Catalan numbers
#'
#' Under OEIS \href{https://oeis.org/A000108}{A000108}, the \emph{n}th \emph{Catalan} number is given as
#' \deqn{C_n = \frac{(2n)!}{(n+1)!n!}}
#' where the first 6 entries are 1, 1, 2, 5, 14, 42 with \eqn{n\ge 0.}
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 Catalan numbers
#' first30 = Catalan(30)
#'
#' ## print without trailing 0's.
#' print(first30, drop0trailing = TRUE)
#'
#' @rdname A000108
#' @aliases A000108 Segner
#' @export
Catalan <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  for (i in 1:n){
    output[i] = Catalan.single(round(i-1), PrecisionBits)
  }

  ## Rmpfr
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}

# Works with zero-based numbering
#' @keywords internal
#' @noRd
Catalan.single <- function(n,PrecisionBits){
  if (n==0){
    return(1)
  } else if (n==1){
    return(1)
  } else {
    sequpper = prod(mpfr(seq(from=2,to=n,by=1)+n, PrecisionBits))
    seqlower = prod(mpfr(seq(from=2,to=n,by=1), PrecisionBits))
    return(sequpper/seqlower)
  }
}

