#' Happy numbers
#'
#' Under OEIS \href{https://oeis.org/A007770}{A007770}, a \emph{Happy} number is defined by the process that starts from
#' arbitrary positive integer and replaces the number by the sum of the squares of each digit until the number is 1.
#' First 6 Happy numbers are 1, 7, 10, 13, 19, 23.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 30 happy numbers
#' print(Happy(30))
#'
#' @rdname A007770
#' @aliases A007770
#' @export
Happy <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output = as.bigz(numeric(n))
  output[1] = as.bigz(1)
  if (n>1){
    tgt = as.bigz(1)
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
  if (!gmp){
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
    return(is.Happy(sum(gmp_p_ary(n, 10)^2)))
  }
}
