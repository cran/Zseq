
# LIST --------------------------------------------------------------------
# 00. check_n                : n should be a positive integer (for length)
# 01. gmp_divisors_proper    : find all proper divisors
# 02. gmp_isprime            : check if the number is prime
# 03. gmp_primesToN          : generate prime numbers up to N
#    NOTE that those 2 and 3 are very slow, should be slow.
#    Consider about porting AKS algorithm (http://yves.gallot.pagesperso-orange.fr/src/aks_gmp.html)
# 04. gmp_divisors_prime     : find all prime divisors
# 05. gmp_primefactors       : returns prime factors only
# 06. gmp_primefactorization : returns a list of prime factors and their correponding multiplicity
# 07. gmp_gcd                : using Recursive Formula
# 08. gmp_binarize
# 09. gmp_p_ary


# 0. ----------------------------------------------------------------------
#' @keywords internal
#' @noRd
check_n <- function(n){
  if ((length(n)!=1)||(abs(n-round(n))>sqrt(.Machine$double.eps))||(n<=0)||(is.na(n))||(is.infinite(n))){
    stop("* Zseq : input 'n' should be a finite positive integer.")
  } else {
    return(as.integer(n))
  }
}


# 1. ----------------------------------------------------------------------
#' @keywords internal
#' @noRd
gmp_divisors_proper <- function(n){
  if (!is.bigz(n)){
    n = as.bigz(n)
  }
  pfactor = gmp_primefactorization(n)
  if (max(pfactor$primes)==n){
    return(1)
  } else {
    primes = pfactor$primes
    counts = as.integer(pfactor$multiplicity)
    nprime = length(primes)


    if (nprime==1){
      vector = ((primes[1])^seq(from=0,to=(counts[1]-1),by=1))
      return(vector)
    } else {
      vector = ((primes[1])^seq(from=0,to=counts[1],by=1))
      for (i in 2:nprime){
        current = primes[i]^seq(from=0,to=counts[i],by=1)
        vector  = gmp_update_two(vector,current)
      }
      idxlogical = (vector!=n)
      output     = vector[idxlogical]
      return(output)
    }
  }
}
gmp_update_two <- function(vec1,vec2){
  output = vec1
  for (i in 1:length(vec2)){
    output = append(output, vec1*vec2[i])
  }
  return(unique(output))
}

# 2. ----------------------------------------------------------------------
#' @keywords internal
#' @noRd
gmp_isprime <- function(n){
  if (!is.bigz(n)){
    n = as.bigz(n)
  }
  if (gmp::isprime(n)==2){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# 3. ----------------------------------------------------------------------
#' @keywords internal
#' @noRd
gmp_primesToN <- function(N){
  if (!is.bigz(N)){
    N = as.bigz(N)
  }
  if (N<2){
    stop("* large_primesToN : N should be larger than 2.")
  }
  output = gmp::as.bigz(2)
  tgt    = gmp::as.bigz(3)
  while (tgt < N){
    if (gmp_isprime(tgt)){
      output = append(output, tgt)
    }
    tgt = gmp::nextprime(tgt)
  }
  return(output)
}

# 4. ----------------------------------------------------------------------
#' @keywords internal
#' @noRd
gmp_divisors_prime <- function(n){
  if (!is.bigz(n)){
    n = as.bigz(n)
  }
  # output = mpfrArray(2, PrecisionBits)
  # i = mpfr(3, PrecisionBits)
  # iter = 1
  # while (i <= (n/2)){
  #   if (n%%i==0){
  #     if (large_isprime(i)){
  #       iter = iter+1
  #       output = append(output, i)
  #     }
  #   }
  #   i = i+1
  # }
  # return(output)
  output = unique(gmp::factorize(n))
  return(output)
}

# 5. ----------------------------------------------------------------------
#' @keywords internal
#' @noRd
gmp_primefactors <- function(n){
  if (!is.bigz(n)){
    n = as.bigz(n)
  }
  #   1. first find prime divisors
  targets  = unique(gmp_divisors_proper(n))
  logprime = unlist(lapply(targets, gmp_isprime))

  #   2. select only the primes
  output = targets[logprime]

  #   3. append the largest ones
  if (gmp_isprime(n)==TRUE){
    output = append(output, n)
  }
  return(output)
}


# 6. ----------------------------------------------------------------------
#' @keywords internal
#' @noRd
gmp_primefactorization <- function(n){
  if (!is.bigz(n)){
    n = as.bigz(n)
  }
  # 1. compute factorization
  vecfactor = gmp::factorize(as.bigz(n))
  uniques   = unique(vecfactor)
  nunique   = length(uniques)

  # 2. set up outputs
  counts    = rep(0,nunique)
  for (i in 1:nunique){
    counts[i] = length(which(vecfactor==uniques[i]))
  }

  # 3. return output
  output = list()
  output$primes = uniques
  output$multiplicity = counts
  return(output)
}


# 7. ----------------------------------------------------------------------
#' @keywords internal
#' @noRd
gmp_gcd <- function(a, b){
  if (b==0){
    return(a)
  } else {
    return(gmp_gcd(b, a%%b))
  }
}
#' @keywords internal
#' @noRd
gmp_gcd_mpfrArray <- function(input){
  if (!is.bigz(input)){
    input = as.bigz(input)
  }
  n = length(input)
  val = gmp_gcd(input[1], input[2])
  if (n >= 3){
    for (i in 3:n){
      val = gmp_gcd(val, input[i])
    }
  }
  return(val)
}


# 8. ----------------------------------------------------------------------
#' @keywords internal
#' @noRd
gmp_binarize <- function(n){
  if (!is.bigz(n)){
    n = as.bigz(n)
  }
  output = as.bigz(unlist(strsplit(as.character(n,b=2),"")))
  return(output)
}


# 9. ----------------------------------------------------------------------
#' @keywords internal
#' @noRd
gmp_p_ary <- function(n, pval){
  if (!is.bigz(n)){
    n = as.bigz(n)
  }
  pval   = as.integer(pval)
  output = as.bigz(unlist(strsplit(as.character(n,b=pval),"")))
  return(output)
}


