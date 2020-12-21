#'  Maximin distance design
#'
#' \code{MaximinD} is a method of constructing maximin distance designs
#'
#' Specifically, we construct a U-type design D by replacing the \emph{u}th level of A by the  (\emph{u} + 1)th row of B for \emph{u} = 0,1,...,s - 1,  where D is said to be U-type if the number of every level appears equally often in every column.
#' @param A  The initial U-type design design U(N,s^n1), a design with N runs, n1 factors, and s levels 0,1,...,s - 1.
#' @param B  The initial U-type design design U(s,q^n2), a design with s runs, n2 factors, and s levels 0,1,...,q - 1.
#' @return The return value is a U-type design U(N,q^(n1*n2)), a design with N runs, n1*n2 factors, and q levels from 0,1,...,q - 1.
#' @examples
#' library(MaximinDesign)
#'
#' A <- SaturatedOA(s = 3, k = 2)
#' B <- EquidistantD(3)
#' D <- MaximinD(A, B)
#'
#' # To obtain the results in Table 5 in Example 2, we give the following code.
#' library(MaximinDesign)
#' s <- 5
#' k <- 3
#' S <- SaturatedOA(s, k); n1 <- ncol(S)
#' B <- EquidistantD(s)
#' SequentialDel <-  function(n){D <- MaximinD(S[,(n1-n+1):n1], B); r <- d1_eff(D);r}
#' col_lab <- 31:19
#' cbind(s*col_lab, apply(as.matrix(col_lab), 1, SequentialDel))
#'
#'
#'
#' @references Wenlong Li, Min-Qian Liu and Boxin Tang (2021). A method of constructing maximin distance designs.   \emph{Biometrika}, published online. <doi:10.1093/biomet/asaa089>
#' @export


MaximinD <- function(A,B){
  B <- matrix_char(B) # B is the Bchar
  N <- nrow(A)
  m <- ncol(A) # The number of columns for A
  n <- length(B)
  k <- length(unlist(strsplit(B[1]," "))) # Obtain column size of B
  k
  #j <- 1
  D <- as.vector(A)  # rearrange A by column as a vector
  D.new <- vector('list', length(D))
  n0 <- length(which(D==0))
  lab <- matrix(NA, n, n0)
  for(i in 1:n){
    # i <- 1
    lab[i,] <- which(D==(i-1)) # record the location of D==levels 0,1,...,(n-1)
    D.new[lab[i,]] <- B[i]# replacing the symbols 1,...,n by b_1j,...,bnj

  }
  D.new <- matrix(D.new,N, m, byrow = F) # matrix('list',n,k) # The list file for SOA A.
  E <- matrix(0, N, k*m)
  for(i in 1:N){
    for(j in 1:m){
      strs <- strsplit(unlist(D.new[i,j]), split = " ")
      c.temp <- as.numeric(unlist(strs))
      E[i, (k*(j-1)+1):(k*j)] <- c.temp
    }
  }
  D <- E
  D
}


paste_row_char <- function(a){
  # a <- B[1,]
  n <- length(a)
  b <- a[1]
  for(i in 2:n){
    b <- paste( b, a[i], sep = " ")
  }
  b
}

matrix_char <- function(B){
  i <- 1:nrow(B)
  B <- sapply(i,  function(i){ paste_row_char(B[i,])} )
  B
}

