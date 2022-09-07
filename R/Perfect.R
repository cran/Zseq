#' Perfect numbers
#'
#' Under OEIS \href{https://oeis.org/A000396}{A000396}, a \emph{Perfect} number is
#' a number whose proper divisors sum up to the extent equal to the number itself. First
#' 6 abundant numbers are	6, 28, 496, 8128, 33550336, 8589869056.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' \dontrun{
#' ## generate first 7 Perfect numbers
#' print(Perfect(10))
#' }
#'
#' @seealso \code{\link{Deficient}}, \code{\link{Abundant}}
#' @rdname A000396
#' @aliases A000396
#' @export
Perfect <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  output  = as.bigz(numeric(n))
  first10 = as.bigz(c("6", "28", "496", "8128", "33550336", "8589869056", "137438691328", "2305843008139952128", "2658455991569831744654692615953842176", "191561942608236107294793378084303638130997321548169216"))
  if (n<=10){
    output = first10[1:n]
  } else {
    output[1:10] = first10
    iter = 10
    tgt  = as.bigz("191561942608236107294793378084303638130997321548169216")
    while (iter < n){
      tgt = tgt + 1
      if (sum(gmp_divisors_proper(tgt))==tgt){
        iter = as.integer(iter + 1)
        output[iter] = tgt
      }
    }
  }
  ## gmp
  if (!gmp){
    output = as.integer(output)
  }
  return(output)
}


#' @keywords internal
#' @noRd
is.Perfect <- function(tgt){
  pdivs = gmp_divisors_proper(tgt)
  if (sum(pdivs)==tgt){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
