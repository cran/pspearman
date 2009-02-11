\name{spearman.test}
\alias{spearman.test}
\concept{Spearman correlation coefficient}
\concept{Spearman's rho}

\title{Spearman's rank correlation test with improved accuracy}

\description{
  This function is a modified copy of the part of the function cor.test(),
  which evaluates Spearman's rank correlation test. There are two modifications
  described below, which improve the accuracy of the p-value.
}

\usage{
spearman.test(x, y,
         alternative = c("two.sided", "less", "greater"),
         method = "spearman",
         exact = NULL, conf.level = 0.95, \dots)
}

\arguments{
  \item{x, y, alternative, exact, conf.level, \dots}{have exactly
  the same meaning as in \code{cor.test}. See the corresponding help page.}
  \item{method}{defaults to \code{"spearman"} and this should not be changed.}
}

\value{
  A list with class \code{"htest"} with the same structure as the value
  of the function \code{cor.test(method="spearman")}. Except of the 
  p-value, also the contents is identical.
}

\details{
  In cases, where the correlation is positive, AS 89 is called with S+2 instead
  of S+1, which is used in \code{cor.test}. Since AS 89 assumes an even input,
  this increases accuracy.

  Function \code{cor.test} uses AS 89 only if \code{n <= 1290} and less
  accurate approximation using Student's t distribution otherwise.
  The code of \code{cor.test} contains a comment \code{n*(n^2 - 1) does not overflow}
  at the line with the test \code{n <= 1290}, which possibly tries to explain the reason
  for this restriction. However, this comment is probably related to older versions
  of the code. Currently, \code{n} is used in the C code after conversion to
  \code{double} and no integer overflow occurs for \code{n > 1290}.
  Hence, the function \code{spearman.test} uses AS 89 in all cases, unless the
  user explicitly specifies \code{exact=FALSE}.
}

\examples{
x <- 1:10
y <- c(5:1,6,10:7)
out1 <- cor.test(x,y,method="spearman")
out2 <- spearman.test(x,y)
# out1$p.value = 0.05169460
# out2$p.value = 0.05444507
# the correct p-value in this case is 0.05443067
c(out1$p.value, out2$p.value)
out2$p.value <- out1$p.value
# except of the p-value, the output is identical
identical(out1,out2)

# for negative correlation, there is no difference
out3 <- cor.test(x,-y,method="spearman")
out4 <- spearman.test(x,-y)
identical(out3,out4)
}

\keyword{htest}

