# This file was created using file src/library/stats/R/cor.test.R
# from the R package, http://www.R-project.org

spearman.test <-
function(x, y, alternative = c("two.sided", "less", "greater"),
         method = "spearman", exact = NULL, conf.level = 0.95, ...)
{
    alternative <- match.arg(alternative)
    method <- match.arg(method)
	stopifnot(method == "spearman")
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

    if(length(x) != length(y))
        stop("'x' and 'y' must have the same length")
    OK <- complete.cases(x, y)
    x <- x[OK]
    y <- y[OK]
    n <- length(x)

    PVAL <- NULL
    NVAL <- 0
    conf.int <- FALSE

    if(n < 2)
        stop("not enough finite observations")
    PARAMETER <- NULL
    TIES <- (min(length(unique(x)), length(unique(y))) < n)
    method <- "Spearman's rank correlation rho"
    if (is.null(exact))
        exact <- TRUE
    names(NVAL) <- "rho"
    r <- cor(rank(x), rank(y))
    ESTIMATE <- c(rho = r)
    if(!is.finite(ESTIMATE)) {  # all x or all y the same
        ESTIMATE[] <- NA
        STATISTIC <- c(S = NA)
        PVAL <- NA
    }
    else {
        ## Use the test statistic S = sum(rank(x) - rank(y))^2
        ## and AS 89 for obtaining better p-values than via the
        ## simple normal approximation.
        ## In the case of no ties, S = (1-rho) * (n^3-n)/6.
        pspearman <- function(q, n, lower.tail = TRUE) {
            if(exact) # call of AS 89
                .C("prho",
                   as.integer(n),
                   as.double(round(q) + 2*lower.tail),
                   p = double(1),
                   integer(1),
                   as.logical(lower.tail),
                   PACKAGE = "stats")$p
            else { # for large n: asymptotic t_{n-2}
        	r <- 1 - 6 * q / (n*(n^2-1)) # careful for overflow
        	pt(r / sqrt((1 - r^2)/(n-2)), df = n-2,
        	   lower.tail= !lower.tail)
            }
        }
        q <- (n^3 - n) * (1 - r) / 6
        STATISTIC <- c(S = q)
        if(TIES && exact){
            exact <- FALSE
            warning("Cannot compute exact p-values with ties")
        }
        PVAL <-
            switch(alternative,
                   "two.sided" = {
                       p <- if(q > (n^3 - n) / 6)
                           pspearman(q, n, lower.tail = FALSE)
                       else
        		   pspearman(q, n, lower.tail = TRUE)
        	       min(2 * p, 1)
        	   },
        	   "greater" = pspearman(q, n, lower.tail = TRUE),
        	   "less" = pspearman(q, n, lower.tail = FALSE))
    }

    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = as.numeric(PVAL),
                 estimate = ESTIMATE,
                 null.value = NVAL,
                 alternative = alternative,
                 method = method,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    RVAL
}

