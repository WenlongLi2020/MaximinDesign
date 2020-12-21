#'   Designs from Proposition 3
#'
#' Let s = 2^k2 be a power of 2, \code{Prop3} is to construct a U(N,s^n), design D, where N = s^k1 and n = 2^(k2-1)*(N-1)/(2^k2-1) in Proposition 3.
#'
#' @param s The number of levels
#' @param k1 As N = s^k1 is the run size of design D, k1 decides N.
#' @return The return value is a U(N,s^n), design D
#' @examples
#' # To construct designs in Proposition 3, we gives three examples.
#' library(MaximinDesign)
#' D <- Prop3(s = 4, k1 = 2); d2_eff(D)
#' D <- Prop3(s = 8, k1 = 2); d2_eff(D)
#' D <- Prop3(s = 16, k1 = 2); d2_eff(D)
#'
#' @references Wenlong Li, Min-Qian Liu and Boxin Tang (2021). A method of constructing maximin distance designs.   \emph{Biometrika}, published online. <doi:10.1093/biomet/asaa089>
#' @export
Prop3 <- function(s,k1){ # s = 2^k2 is a power of 2
  # s <- 4; k1 <- 2
  A <- SaturatedOA(s = s, k = k1)
  # library(numbers)
  B <- B_Prop3( length( numbers::primeFactors(s) ) - 2 )
  D <- MaximinD(A, B)
  D
}


B_Prop3 <- function(i){
  D0 <- cbind(rep(c(0,1),each=2), rep(c(0,1), 2))
  R0 <- cbind(c(2,1),c(-1,2))
  Q0 <- cbind(c(1,0),c(0,-1))
  Q00 <- Q0
  ST <- function(i){ # ST: Sun and Tang (2017)
    for(j in 1:i){
      R1 <- cbind(rbind(2*R0,Q0), rbind(-Q0,2*R0))
      Q1 <- kronecker(Q00,Q0)
      D1 <- cbind(rbind(D0,D0), rbind(D0, (D0+1)%%2))
      Q0 <- Q1
      R0 <- R1
      D0 <- D1
    }
    L <- (D1-1/2)%*%R1
    L
  }
  if(i==0){D <- (D0-1/2)%*%R0 } else {D <- ST(i)}
  D <- D+max(D)
  D
}



