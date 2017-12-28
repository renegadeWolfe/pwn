\name{dlprime}
\alias{dlprime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Primes those annoying variables that have a detection limit and have been keyed in as such. 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This is a slightly more advanced version of dlsplit. 
dlprime will return the dataframe with the target variable as a numeric vector, and a second variable, taking the name "var_detect", which will have one of 3 values: 2,1, 0 and -1. 
}
\usage{
dlprime(df1, varX)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df1}{
%%     ~~Describe \code{df1} here~~
	is a dataframe with the variable in question. 
}
  \item{varX}{
%%     ~~Describe \code{varX} here~~
	is column name of the variable in question, entered as a character string. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
dlprime will return the dataframe with the target variable as a numeric vector, and a second variable, taking the name "var_detect", which will have one of 3 values: 1, 0 and -1. 
2 indicates a value above the highest reliable detection limit. 
1 indicates an actual value.
0 indicates a value that's below the detection limit.
-1 indicates an actual value, but is below the worst detection limit in the data entered. 
All of which will be tacked to the back of the dataframe. 
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

id <- c(1:5)
alpha <- c(0.015, "<0.003", 0.0025, 0.007, "<0.003")
bravo <- c(0.002, "<0.003", 0.007, 0.125, ">1")
x <- data.frame(id,alpha,bravo)

x <- dlprime(x,"alpha")
x <- dlprime(x,"bravo")

## The function is currently defined as
function (df1, varX) 
{
    red <- as.character(df1[, varX])
    varNme <- as.character(varX)
    varNme_less <- paste(varNme, "detect", sep = "_")
    red.1stChar <- substr(red, 1, 1)
    dl_lim <- ifelse(red.1stChar == "<", 0, 1)
    red <- as.numeric(sub("<", "", red))
    blue <- data.frame(red, dl_lim)
    x <- subset(blue, blue$dl_lim == 0, drop = T)
    dls <- max(x$red)
    blue$dl_lim <- ifelse(blue$red < dls & blue$dl_lim != 0, 
        -1, blue$dl_lim)
    red <- blue$red
    dl_lim <- blue$dl_lim
    df1[, varX] <- NULL
    df1$red <- red
    names(df1)[names(df1) == "red"] <- varNme
    df1$detect_lim <- dl_lim
    names(df1)[names(df1) == "detect_lim"] <- varNme_less
    return(df1)
	}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ detection }
\keyword{ limit }% __ONLY ONE__ keyword per line
