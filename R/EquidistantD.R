#'  Equi-distant design
#'
#' \code{EquidistantD} constructs an equi-distant design, and then provides a choice of design B.
#'
#' Under the L1-distance, Wang et al. (2018,Theorem4) showed that an equi-distant LH(s,s), a Latin hypercube of s runs for s factors, can be constructed if 2s + 1 is a prime.
#' @param s  The run size of design B, if 2s + 1 is a prime, e.g.,  s=3,5,8,9,11,....
#' @return The return value is equi-distant LH(s,s).
#' @examples
#' # EquidistantD(s) for s=3,5,8,9,11,...
#' B <- EquidistantD(3)
#' B <- EquidistantD(5)
#' @references Wenlong Li, Min-Qian Liu and Boxin Tang (2021). A method of constructing maximin distance designs.   \emph{Biometrika}, published online. <doi:10.1093/biomet/asaa089>
#' @export

EquidistantD <- function(s){
  #N <- 3
  N <- 2*s+1 # An equi-distant LH(s,s) exists if 2s + 1 is a prime.

  k <- 1:N
  h <- 1:(N-1)
  x <- outer(k, h, "*")%%N  # D: 11*10 LHD with x_ij=i*j (mod 11)
  WD <- ifelse(x < N/2, 2*x, 2*(N-x))
  WD2 <- WD/2
  n <- (N-1)/2
  equiD <- WD2[1:n,1:n]
  equiD <- equiD-1
  equiD
}


