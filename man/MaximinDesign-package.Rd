\name{MaximinDesign-package}
\alias{MaximinDesign-package}
\docType{package}
\title{MaximinDesign package}
\description{
This package contains functions for generating maximin distance designs.

Important reminder: Before you first run our R codes, please install Matlab soft and find a folder called ``Matlab'' in folder ``man'' in R package \code{MaximinDesign} in SCAN or GitHub, and then put this folder to local disk (D:). Combining this and R function \code{\link{SaturatedOA}} give a saturated orthogonal array, which plays a supporting role in constrcting maximin distance designs.

The \code{\link{MaximinD}} is the basic function using the direct replacement method. Applying this function \code{\link{MaximinD}}, we provide easy-to-use functions called \code{\link{MaximinDSeqDel}}, \code{\link{Prop1}}, \code{\link{Prop2}} and \code{\link{Prop3}}, where \code{\link{MaximinDSeqDel}} allows a design with flexible columns to be constructed.
}
\details{
\tabular{ll}{
Package: \tab MaximinDesign\cr
Type: \tab Package\cr
Version: \tab 1.0-1\cr
Date: \tab 2020-12-17\cr
License: \tab LGPL-1.1\cr
Depends: \tab R (>= 3.6.3) \cr
}
The designs are generated following the Biometrika paper "A method of constructing maximin distance designs".

Important functions in this package are as follows:

\code{\link{MaximinD}} is the basic function that constructs a maximin distance design D using two small designs A and B via the direct replacement method. Specifically,  this function constructs a U-type design D by replacing the \emph{u}th level of A by the  (\emph{u} + 1)th row of B for \emph{u} = 0,1,...,s − 1.
\code{\link{SaturatedOA}} is to generate design A, a OA(N, n1, s, 2) with N = s^k runs and n1 = (s^k-1)/(s-1) columns, where these n1 columns are arranged in Yates order.
\code{\link{EquidistantD}} and \code{\link{WGLP}} provide two good choices of design B.

For practical use, applying the basic function \code{\link{MaximinD}}, we provide easy-to-use functions called \code{\link{MaximinDSeqDel}}, \code{\link{Prop1}}, \code{\link{Prop2}} and \code{\link{Prop3}}. We explains them in the following.

1. \code{\link{MaximinDSeqDel}} is obtain a maximin distance design U(N,s^((n1-j)*n2)), a design with N = s^k runs, (n1-j)*n2 factors, and s levels. We sequentially delete the first j columns in a saturated orthogonal array  with n1=(N-1)/(s-1) columns according to Yates order and take the remaining n = n1 − j columns as A. Using this A in combination with the equi-distant design B, LH(s,n2), we obtain a U(N,s^(n1-j)*n2) for j = 0,1,.... See Example 2 for an illustration as follows.
\code{MaximinDSeqDel} gives a design D with B chosen to be equi-distant under the L1 distance, where s = 3, 5, 7, 8, 9, 11,... and k=2,3,4,...


1) For j=0, MaximinDSeqDel(s,k,0) constructs equi-distant maximin designs from Theorem 2, as shown in Table 1

2) For j>0, MaximinDSeqDel(s,k,j) is to delete the first j*n2 columns from MaximinDSeqDel(s,k,0), an equi-distant maximin design. See Example 2 for more details.

3) For j=(s^(k-1)-1)/(s-1), MaximinDSeqDel(s,k,j) provides an N X N design D in Corollary 2, as shown in Table 3.

2.  If s is a prime power and 2s − 1 is a prime,  \code{\link{Prop1}} is to construct a U(N,s^n), design D, where N = s^k and n = s^k − 1 in Proposition 1, as shown in Table 2.

3. If s is a prime, \code{\link{Prop2}} is to construct a U(N,s^n), design D, where N = s^k and n = s^k − 1 in Proposition 2, as shown in Table 2.

4. Let s = 2^k2 be a power of 2, \code{\link{Prop3}} is to construct a U(N,s^n), design D, where N = s^k1 and n = 2^(k2-1)*(N-1)/(2^k2-1) in Proposition 3.




At last, in order to evaluate the performance of a design, we present the following measures.

a) \code{\link{d1_eff}} and \code{\link{d2_eff}} is to obtain the L1-
and L2-distance efficiencies of a design whose levels are from 0,1, ..., s-1.

b) \code{\link{d1}} and \code{\link{d2}} is to obtain the L1-
and L2-distances of a design whose levels are from 0,1, ..., s-1.

c) \code{\link{d1_01}} and \code{\link{d2_01}} is to obtain the L1-
and L2-distances of a design when the design is scaled into [0,1]^n.
}
\author{
Wenlong Li, Min-Qian Liu and Boxin Tang

Maintainer: Wenlong Li <wenlongli@mail.nankai.edu.cn>
}
\references{
Wenlong Li, Min-Qian Liu and Boxin Tang (2021). A method of constructing maximin distance designs.   \emph{Biometrika}, published online. <doi:10.1093/biomet/asaa089>
}

\keyword{ package }

