\name{spearman.test}
\alias{spearman.test}
\concept{Spearman's correlation coefficient}
\concept{Spearman's rho}

\title{Spearman's rank correlation test with precomputed null distribution}

\description{
  This function is a modification of the part of the function \code{cor.test()}, which
  evaluates Spearman's rank correlation test. Besides the two approximations
  used in \code{cor.test(,method="spearman")}, which are AS89 and the t-distribution,
  this function allows to use precomputed null distribution for \code{n <= 22}.
}

\usage{
spearman.test(x, y,
    alternative = c("two.sided", "less", "greater"),
    approximation = c("exact", "AS89", "t-distribution"))
}

\arguments{
  \item{x, y, alternative}{have the same meaning as in \code{cor.test}.
  See the corresponding help page.}
  \item{approximation}{selection of the method to approximate the null distribution}
}

\value{
  A list with class \code{"htest"} with the same structure as the value
  of the function \code{cor.test(method="spearman")}. Except of the 
  p-value, also the contents is identical.
}

\details{
Calculation of the null distribution of Spearman's rank correlation statistics
is exponentially hard in \code{n}. This package uses precomputed exact distribution
for \code{n <= 22}. The computation was done using Ryser's formula applied to
an appropriate monomial permanent as described in
\cite{
M.A. van de Wiel and A. Di Bucchianico,
Fast computation of the exact null distribution of Spearman's rho and Page's L statistic
for samples with and without ties, J. Stat. Plann. Inf. 92 (2001), pp. 133-145. 
}
using code written by the author of the package. The resulting distributions are
identical to those computed by an independent program kindly provided by M.A. van de Wiel.
}

\examples{
x <- 1:10
y <- c(5:1, 6, 10:7)
out1 <- spearman.test(x, y)
out2 <- spearman.test(x, y, approximation="AS89")
out3 <- cor.test(x, y, method="spearman")
out1$p.value # [1] 0.05443067  this is the exact value
out2$p.value # [1] 0.05444507  approximation obtained from AS89
out3$p.value # [1] 0.05444507  dtto (R-2.9.0 and older versions have 0.05169460 here)
}

\keyword{htest}

