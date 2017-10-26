#' Carmichael numbers
#'
#' Under OEIS \href{https://oeis.org/A002997}{A002997}, a \emph{Carmichael} number is
#' a composite number \eqn{n} such that
#' \deqn{b^{n-1} = 1 (mod n)}
#' for all integers \eqn{b} which are relatively prime to \eqn{n}. First 6 Carmichael numbers are  	561, 1105, 1729, 2465, 2821, 6601.
#'
#' @param n the number of first \code{n} entries from the sequence.
#' @param Rmpfr a logical; \code{TRUE} to use large number representation, \code{FALSE} otherwise.
#' @param PrecisionBits a positive integer for precision bits larger than 2.
#'
#' @return a vector of length \code{n} containing first entries from the sequence.
#'
#' @examples
#' ## generate first 3 Carmichael numbers
#' first3 = Carmichael(3)
#'
#' ## print without trailing 0's.
#' print(first3, drop0trailing = TRUE)
#'
#' @rdname A002997
#' @aliases A002997
#' @export
Carmichael <- function(n, Rmpfr=TRUE, PrecisionBits=496){
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
  first33 = mpfr(c(561, 1105, 1729, 2465, 2821, 6601, 8911, 10585, 15841, 29341, 41041, 46657, 52633, 62745, 63973, 75361, 101101, 115921, 126217, 162401, 172081, 188461, 252601, 278545, 294409, 314821, 334153, 340561, 399001, 410041, 449065, 488881, 512461), PrecisionBits)
  output = mpfrArray(rep(0,n), PrecisionBits)
  if (n<=33){
    output = first33[1:n]
  } else {
    iter = 33
    tgt  = mpfr(512461+1, PrecisionBits)
    while (iter<n){
      tgt = tgt + 1
      if (is.Carmichael(tgt, PrecisionBits)){
        output = append(output, tgt)
        iter   = iter + 1
      }
    }
  }

  ## Rmpfr
  if (!Rmpfr){
    output = as.integer(output)
  }
  return(output)
}

#' @keywords internal
#' @noRd
is.Carmichael <- function(n, PrecisionBits){
  # cond 1 : it should be a composite number.
  cond1 = (!large_isprime(n))
  # cond 2 : square free
  cond2 = (is.Squarefree(n, PrecisionBits))
  # cond 3 : n-1 divisible by p-1 for all prime divisors
  cond3 = TRUE
  primedivisors = large_divisors_prime(n, PrecisionBits)
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
