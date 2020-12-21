#' The L1-distance
#'
#' Calculate the L1-distance of a design when the design whose levels are from 0,1, ..., s-1
#' @param D  A design
#' @return The return value is the L1-distance of a design.
#' @examples
#' library(MaximinDesign)
#' D <- EquidistantD(3)
#' d1(D)
#' @references Wenlong Li, Min-Qian Liu and Boxin Tang (2021). A method of constructing maximin distance designs.   \emph{Biometrika}, published online. <doi:10.1093/biomet/asaa089>
#' @export
d1 <- function(D){
  N <- dim(D)[1]
  ncom <- choose(N, 2)
  labcom <- utils::combn(N, 2)
  L1 <- numeric(N)
  times <-  1:ncom
  res <- sapply(times, function(i){r <- d1xy(D[labcom[,i],]); r})
  L1_distance <- min(res)
  L1_distance
}


#' The L1-distance efficiency
#'
#' Calculate the L1-distance efficiency of a U-type design D whose levels are from 0,1, ..., s-1, where D is said to be U-type if the number of every level appears equally often in every column.
#' @param D  A U-type design
#' @return The return value is the L1-distance efficiency of design D.
#' @examples
#' library(MaximinDesign)
#' D <- EquidistantD(3)
#' d1_eff(D)
#' @export
#'
d1_eff <- function(D){
  N <- nrow(D) # runs
  n <- ncol(D) # columns
  s <- max(D) + 1 # levels
  L1 <- d1(D)
  upper <- N*(s^2-1)*n/(3*N*s-3*s)
  deff <- L1/upper
  deff
}

#' The L1-distance
#'
#' Calculate the L1-distance of a design when the design is scaled into [0,1]^n
#' @param D  A design
#' @return The return value is the L1-distance of design D.
#' @examples
#' library(MaximinDesign)
#' D <- EquidistantD(3)
#' d1_01(D)
#' @export

d1_01 <- function(D){
  # D <- cbind(c(0:3), c(1:4),c(1,2,1,3))
  D <- apply(D, MARGIN = 2, FUN = function(D) (D - min(D))/diff(range(D)))
  N <- dim(D)[1]
  ncom <- choose(N, 2)
  labcom <- utils::combn(N, 2)
  times <-  1:ncom
  res <- sapply(times, function(i){r <- d1xy(D[labcom[,i],]); r})
  L1_distance <- min(res)
  L1_distance
}



