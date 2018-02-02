#' Achilles numbers
#'
#' Under OEIS \href{https://oeis.org/A052486}{A052486}, an \emph{Achilles} number is a number
#' that is \emph{powerful} but \emph{not perfect}. First 6 Achilles numbers are 72, 108, 200, 288, 392, 432.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 3 Achilles numbers and print
#' print(Achilles(3))
#'
#' @rdname A052486
#' @aliases A052486
#' @export
Achilles <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = gmp::as.bigz(numeric(n))
  output[1] = as.bigz(72)
  if (n>1){
    tgt = as.bigz(100)
    iter = 1
    while (iter<n){
      tgt = tgt + 1
      if (is.Achilles(tgt)){
        iter = iter+1
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
is.Achilles <- function(tgt){
  # cond1 : powerful
  cond1 = (is.Powerful(tgt))
  # cond2 : gcd(exponents)=1
  pfactor = gmp_primefactorization(tgt)
  if (length(pfactor$multiplicity)==1){
    cond2 = TRUE
  } else {
    cond2 = (gmp_gcd_mpfrArray(pfactor$multiplicity)==1)
  }
  cond3 = (length(pfactor$multiplicity)>1)

  if (cond1&&cond2&&cond3){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
