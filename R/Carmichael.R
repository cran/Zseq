#' Carmichael numbers
#'
#' Under OEIS \href{https://oeis.org/A002997}{A002997}, a \emph{Carmichael} number is
#' a composite number \eqn{n} such that
#' \deqn{b^{n-1} = 1 (mod n)}
#' for all integers \eqn{b} which are relatively prime to \eqn{n}. First 6 Carmichael numbers are  	561, 1105, 1729, 2465, 2821, 6601.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param gmp a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 3 Carmichael numbers
#' print(Carmichael(3))
#'
#' @rdname A002997
#' @aliases A002997
#' @export
Carmichael <- function(n, gmp=TRUE){
  ## Preprocessing for 'n'
  n = check_n(n)

  ## Main Computation : first, compute in Rmpfr form
  first30 = gmp::as.bigz(c(561, 1105, 1729, 2465, 2821, 6601, 8911, 10585, 15841, 29341, 41041, 46657, 52633, 62745, 63973, 75361, 101101, 115921, 126217, 162401, 172081, 188461, 252601, 278545, 294409, 314821, 334153, 340561, 399001, 410041))
  output  = gmp::as.bigz(numeric(n))
  if (n<=30){
    output = first30[1:n]
  } else {
    iter = 30
    tgt  = as.bigz(410041+1)
    while (iter<n){
      tgt = tgt + 1
      if (is.Carmichael(tgt)){
        output = append(output, tgt)
        iter   = iter + 1
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
is.Carmichael <- function(n){
  # cond 1 : it should be a composite number.
  cond1 = (!gmp_isprime(n))
  # cond 2 : square free
  cond2 = (is.Squarefree(n))
  # cond 3 : n-1 divisible by p-1 for all prime divisors
  cond3 = TRUE
  primedivisors = gmp_divisors_prime(n)
  for (i in 1:length(primedivisors)){
    p_1 = primedivisors[i]-1
    if ((n-1)%%(p_1)!=0){
      cond3 = FALSE
      break
    }
  }

  if (cond1&&cond2&&cond3){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
