\name{rec}
\alias{rec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Recon the data. 
}
\description{
Presents a summary of a dataframe. Results can be stored for later use. Now with the option of silencing the output, and simply returning a line stating the number of cases and parameters in the dataset. 
}
\usage{
rec(df1, details = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df1}{
is a dataframe. 
}
  \item{details}{
boolean variable, value defaults to TRUE. If set to FALSE, will not return the table describing each of the parameters. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Displays the number of variables and cases, then a tabular description of the variables. The table may be stored for later use. 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
hellhound
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (df1, details = TRUE) 
{
    if (isTRUE(details)) {
        cases <- length(df1[, 1])
        paras <- length(df1)
        cat("Data has: ", cases, " cases and ", paras, "parameters. \n")
        varnames <- names(df1)
        n_NA <- colSums(is.na(df1))
        n_NA <- data.frame(n_NA)
        n_NA <- n_NA[, 1]
        varClass <- c()
        varMin <- c()
        varMn <- c()
        varMd <- c()
        varMax <- c()
        for (i in df1) {
            var_class <- class(i)
            varClass <- c(varClass, var_class)
            if (var_class == "numeric" | var_class == "integer") {
                var_min <- min(i, na.rm = T)
                varMin <- c(varMin, var_min)
                var_mn <- mean(i, na.rm = T)
                varMn <- c(varMn, var_mn)
                var_md <- median(i, na.rm = T)
                varMd <- c(varMd, var_md)
                var_max <- max(i, na.rm = T)
                varMax <- c(varMax, var_max)
            }
            else {
                varMin <- c(varMin, NA)
                varMn <- c(varMn, NA)
                varMd <- c(varMd, NA)
                varMax <- c(varMax, NA)
            }
        }
        dfx <- data.frame(varnames, varClass, n_NA, varMin, varMn, 
            varMd, varMax)
        colnames(dfx) <- c("Variable", "Class", "NAs", "Min", 
            "Mean", "Median", "Max")
        return(dfx)
    }
    else {
        cases <- length(df1[, 1])
        paras <- length(df1)
        cat("Data has: ", cases, " cases and ", paras, "parameters. \n")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
