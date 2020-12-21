#' Williams Transformation
#'
#' \code{WGLP} is Williams transformations of linearly transformed good lattice points. This provides a choice of design B.
#'
#' Under the L1-distance, Wang et al. (2018,Theorem 2) constructed an LH(s,s-1) when s is a prime, where LH(s,s-1) is a Latin hypercube of s runs for s-1 factors. If s is not
#' a prime,  the Williams transformation can generate designs with s runs and phi(s) factors,
#' where phi(s) is the Euler function, that is, the number of positive integers smaller than and coprime to s.
#' @param s  The run size of design B, where s is a prime.
#' @return The return value is an LH(s,s-1) when s is a prime or LH(s, phi(s)) when s is not a prime.
#' @examples
#' # Note that WGLP(7) produces an equi-distant LH(7,6)
#' B <- WGLP(7)
#' B <- WGLP(13)
#' @references Wenlong Li, Min-Qian Liu and Boxin Tang (2021). A method of constructing maximin distance designs.   \emph{Biometrika}, published online. <doi:10.1093/biomet/asaa089>
#' @export

WGLP <- function(s){
  'GLP method'
  # s <- 11
  N <- s
  'obtain positive integers less than or equal to N that are coprime to N '
  #library('numbers')
  h <- NULL
  for(i in 1:(N-1)){
    if(numbers::coprime(N,i)){h <- c(h,i)}
  }

  k <- 1:N
  D <- outer(k, h, "*")%%N  # D: 11*10 LHD with x_ij=i*j (mod 11)
  'Leave-one-out method'
  # D <- D[-N,]-1
  'Find best b for L1-distance of Williams transformation'
  Eb.max <- function(D){  # L1-distance of Williams transformation
    b <- 0:(N-1)
    N <- dim(D)[1]
    f <- function(i){
      x <- (D + b[i])%%N
      return(   d1(ifelse(x < N/2, 2*x, 2*(N-x)-1)) )
    }

    b.max <- which.max(apply(matrix(1:N, N,1), 1,  f))
    x <- (D + b[b.max])%%N
    return( ifelse(x < N/2, 2*x, 2*(N-x)-1) )
  }
  WTD <- Eb.max(D) # Db.o : Db.optimal
  WTD
}


d1xy <- function(xy){ # L1 distance between two rows
  x <- xy[1,]
  y <- xy[2,]
  n <- length(x)
  d1 <- sum( abs(x-y) )
  d1
}

d2xy <- function(xy){ # L2 distance between two rows
  x <- xy[1,]
  y <- xy[2,]
  n <- length(x)
  d2 <- sum( (x-y)^2 )
  d2
}
