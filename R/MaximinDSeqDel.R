#'  Maximin distance design
#'
#' \code{MaximinDSeqDel} is obtain a maximin distance design U(N,s^((n1-j)*n2)), a design with N = s^k runs, (n1-j)*n2 factors, and s levels.
#' We sequentially delete the first j columns in a saturated orthogonal array with n1=(N-1)/(s-1) columns according to Yates order and
#'   take the remaining n = n1 - j columns as A. Using this A in combination with the equi-distant design B,
#'   LH(s,n2), we obtain a U(N,s^{(n1-j)*n2}) for j = 0,1,.... See Example 2 for an illustration.
#'
#' \code{MaximinDSeqDel} gives a design D with B chosen to be equi-distant under the L1 distance.
#'  For practice use, we take s = 3, 5, 7, 8, 9, 11,... and k=2,3,4,...
#'
#' For j=0, MaximinDSeqDel(s,k,0) constructs equi-distant maximin designs from Theorem 2, as shown in Table 1
#'
#' For j>0, MaximinDSeqDel(s,k,j) is to delete the first j*n2 columns from MaximinDSeqDel(s,k,0), an equi-distant maximin design.
#' See Example 2 for more details.
#'
#' For j=(s^(k-1)-1)/(s-1), MaximinDSeqDel(s,k,j) provides an N X N design D in  Corollary 2, as shown in Table 3.
#'
#'
#' @param s The number of levels
#' @param k  As N = s^k is the run size of design D, k decides N. For example, taking s=5 and k=2 give N=25.
#' @param j  The number of columns that is sequentially deleted in a saturated orthogonal array according to Yates order such that the resulting design has (n1-j)*n2 factors.
#' @return The return value is a design U(N,s^{(n1-j)*n2}), a design with N runs, (n1-j)*n2 factors, and s levels.
#' @examples
#'  # For practice use, we take s = 3, 5, 7, 8, 9, 11,....
#'  # To construct designs in Example 2, we gives several examples.
#'
#'  library(MaximinDesign)
#'  # For j=0, MaximinDSeqDel(5,3,0) gives an equi-distant maximin designs
#'  # from Theorem 2, as shown in Table 1
#'  MaximinDSeqDel(5,3,0)
#'
#'  # For j=1,2,3,4,5,..., MaximinDSeqDel(5,3,j) is to delete the first 5j columns
#'  # from a design of 125 runs and 155 factors.
#'  MaximinDSeqDel(5,3,1)
#'
#'  # For j=6, MaximinDSeqDel(5,3,6) gives a design of 125 runs and 125 factors.
#'  MaximinDSeqDel(5,3,6)
#'
#' @references Wenlong Li, Min-Qian Liu and Boxin Tang (2021). A method of constructing maximin distance designs.   \emph{Biometrika}, published online. <doi:10.1093/biomet/asaa089>
#' @export


MaximinDSeqDel <- function(s,k,j){
  # s <- 5; k <- 3; j <- 1
  S <- SaturatedOA(s = s, k = k)
  ifelse(s==7, B <- WGLP(s), B <- EquidistantD(s) )
  ifelse(j==0, A <- S, A <- S[,-(1:j)])
  D <- MaximinD(A, B)
  D
}

