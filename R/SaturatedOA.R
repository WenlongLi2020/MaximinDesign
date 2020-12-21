#' Saturated orthogonal array
#'
#' \code{SaturatedOA} is to generate a saturated orthogonal array OA(N, n1, s, 2) with N = s^k runs and n1 = (s^k-1)/(s-1) columns, where these n1 columns are arranged in Yates order.
#' Important reminder: Before you first run \code{SaturatedOA}, please install Matlab soft and find a folder called ``Matlab'' in folder ``man'' in R package  \code{MaximinDesign} in SCAN or GitHub, and then put this folder to local disk (D:).
#'
#' @param s The number of levels, where s is a prime power.
#' @param k As N=s^k is the run size of saturated orthogonal array, k decides N.
#' @return The return value is an saturated orthogonal array OA(N, n1, s, 2), a design with N=s^k runs, n1 = (s^k-1)/(s-1) factors, and s levels from 0,1,...,s − 1.
#' @examples
#' A <- SaturatedOA(3,2)
#' @references Wenlong Li, Min-Qian Liu and Boxin Tang (2021). A method of constructing maximin distance designs.   \emph{Biometrika}, published online. <doi:10.1093/biomet/asaa089>
#' @export
SaturatedOA <- function(s,k){ #
  # library(R.matlab)
  # library(matlabr)
  R.matlab::writeMat("D:/Matlab/input.mat", s = s, k = k)
  matlabr::run_matlab_script("D:/Matlab/MatlabSaturatedOA.m")
  mat <- R.matlab::readMat("D:/Matlab/OA.mat")
  A <- mat$A
  A
}


