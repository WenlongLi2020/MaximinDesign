#'   Designs from Proposition 2
#'
#' If s is a prime, \code{Prop2} is to construct a U(N,s^n), design D, where N = s^k and n = s^k - 1 in Proposition 2, as shown in Table 2.
#' For example, s=5,7,11,13,17,19,...
#' @param s The number of levels
#' @param k As N = s^k is the run size of design D, k decides N.
#' @return The return value is a U(N,s^n), design D
#' @examples
#' # To construct designs in Proposition 2, as shown in Table 2, we gives two examples.
#' library(MaximinDesign)
#' D <- Prop2(s = 5, k = 2); # d1_eff(D)
#' D <- Prop2(s = 7, k = 2); d1_eff(D)
#' @references Wenlong Li, Min-Qian Liu and Boxin Tang (2021). A method of constructing maximin distance designs.   \emph{Biometrika}, published online. <doi:10.1093/biomet/asaa089>
#' @export
Prop2 <- function(s,k){
  # s <- 4; k <- 2
  A <- SaturatedOA(s = s, k = k)
  B <- WGLP(s)
  D <- MaximinD(A, B)
  D
}

