
# LIST --------------------------------------------------------------------
# 01. large_divisors_proper    : find all proper divisors
# 02. large_isprime            : check if the number is prime
# 03. large_primesToN          : generate prime numbers up to N
#    NOTE that those 2 and 3 are very slow, should be slow.
#    Consider about porting AKS algorithm (http://yves.gallot.pagesperso-orange.fr/src/aks_gmp.html)
# 04. large_divisors_prime     : find all prime divisors
# 05. large_primefactors       : returns prime factors only
# 06. large_primefactorization : returns a list of prime factors and their correponding multiplicity
# 07. large_gcd                : using Recursive Formula
# 08. large_binarize
# 09. large_p_ary
#  ------------------------------------------------------------------------
# 1. large_divisors_proper
#' @keywords internal
#' @noRd
large_divisors_proper <- function(n, PrecisionBits){
  output = mpfrArray(1, PrecisionBits)
  i = mpfr(2, PrecisionBits)
  iter = 1
  while (i <= (n/2)){
    if (n%%i==0){
      iter = iter+1
      output = append(output, i)
    }
    i = i+1
  }
  return(output)
}

# 2. large_isprime
#' @keywords internal
#' @noRd
large_isprime <- function(n){
  if (n<=1){
    return(FALSE)
  } else if (n<=3){
    return(TRUE)
  } else if ((n%%3==0)||(n%%2==0)){
    return(FALSE)
  }
  i = 5
  while (i<=sqrt(n)){
    if (((n%%i)==0)||((n%%(i+2))==0)){
      return(FALSE)
    }
    i = i+6
  }
  return(TRUE)
}

# 3. large_primesToN
#' @keywords internal
#' @noRd
large_primesToN <- function(N, PrecisionBits){
  if (N<2){
    stop("* large_primesToN : N should be larger than 2.")
  }
  output = mpfrArray(2, PrecisionBits)
  tgt    = mpfr(3, PrecisionBits)
  while (tgt < N){
    if (large_isprime(tgt)){
      output = append(output, tgt)
    }
    tgt = tgt + 2
  }
  return(output)
}


# 4. large_divisors_prime
#' @keywords internal
#' @noRd
large_divisors_prime <- function(n, PrecisionBits){
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
  output = unique(large_primefactors(n, PrecisionBits))
  return(output)
}

# 5. large_primefactors
#' @keywords internal
#' @noRd
large_primefactors <- function(n, PrecisionBits){
  output = mpfrArray(0, PrecisionBits)
  while (n%%2==0){
    output = append(output, mpfr(2, PrecisionBits))
    n = n/2
  }
  i = 3
  while (i<=sqrt(n)){
    while (n%%i==0){
      output = append(output, mpfr(i, PrecisionBits))
      n = n/i
    }
    i = i+2
  }
  if (n>2){
    output = append(output, mpfr(n, PrecisionBits))
  }
  return(sort(output[2:(length(output))]))
}


# 6. large_primefactorization
#' @keywords internal
#' @noRd
large_primefactorization <- function(n, PrecisionBits){
  interim = large_primefactors(n, PrecisionBits)
  if (length(interim)==length(unique(interim))){
    multiplicity = mpfrArray(rep(1,length(interim)), PrecisionBits)
  } else {
    uinterim = unique(interim)
    multiplicity = mpfrArray(rep(0,length(uinterim)), PrecisionBits)
    for (i in 1:length(uinterim)){
      multiplicity[i] = sum((interim==uinterim[i]))
    }
  }

  output = list()
  output$primes = unique(interim)
  output$multiplicity = multiplicity
  return(output)
}

# 7. large_gcd : using Recursive Formula
#' @keywords internal
#' @noRd
large_gcd <- function(a, b, PrecisionBits){
  if (b==0){
    return(a)
  } else {
    return(large_gcd(b, a%%b, PrecisionBits))
  }
}
#' @keywords internal
#' @noRd
large_gcd_mpfrArray <- function(input, PrecisionBits){
  n = length(input)
  val = large_gcd(input[1], input[2], PrecisionBits)
  if (n >= 3){
    for (i in 3:n){
      val = large_gcd(val, input[i], PrecisionBits)
    }
  }
  return(val)
}

# 8. large_binarize
#' @keywords internal
#' @noRd
large_binarize <- function(n, PrecisionBits){
  if (n==0){
    return(0)
  } else {
    outvec = rep(0,as.integer(floor(log2(n)))+1)
    loutvec= length(outvec)
    for (i in 1:(loutvec)){
      outvec[loutvec-i+1] = asNumeric(n%%2)
      n = floor(n/2)
    }
    return(outvec)
  }
}

# 9. large_nary
#' @keywords internal
#' @noRd
large_p_ary <- function(n, p){
  if (n==0){
    return(0)
  } else {
    outvec = 0
    while (n!=0){
      outvec = append(outvec, asNumeric(n%%p))
      n      = floor(n/p)
    }
    # logp = log(mpfr(p, PrecisionBits))
    # outvec = rep(0,as.integer(floor(log(n)/logp))+1)
    # loutvec= length(outvec)
    # for (i in 1:(loutvec)){
    #   outvec[loutvec-i+1] = asNumeric(n%%p)
    #   n = floor(n/p)
    # }
    outvec = outvec[2:length(outvec)]
    return(rev(outvec))
  }
}


