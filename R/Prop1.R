#'   Designs from Proposition 1
#'
#' If s is a prime power and 2s - 1 is a prime, \code{Prop1} is to construct a U(N,s^n), design D, where N = s^k and n = s^k − 1 in Proposition 1, as shown in Table 2.
#' For example, s=4, 9, 16,...
#'
#' @param s The number of levels
#' @param k As N = s^k is the run size of design D, k decides N.
#' @return The return value is a U(N,s^n), design D
#' @examples
#' # To construct designs in Proposition 1, as shown in Table 2, we gives three examples.
#' library(MaximinDesign)
#' D <- Prop1(s = 4, k = 2); d1_eff(D)
#' D <- Prop1(s = 9, k = 2); d1_eff(D)
#' D <- Prop1(s = 16, k = 2); d1_eff(D)
#'
#' @references Wenlong Li, Min-Qian Liu and Boxin Tang (2021). A method of constructing maximin distance designs.   \emph{Biometrika}, published online. <doi:10.1093/biomet/asaa089>
#' @export
Prop1 <- function(s,k){
  # s <- 4; k <- 2
  A <- SaturatedOA(s = s, k = k)
  B <- rbind(EquidistantD(s-1)+1, 0)
  D <- MaximinD(A, B)
  D
}


