\name{fillVec}
\alias{fillVec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fill in the blanks when people can't be bothered to. 
}
\description{
This function is meant to fill in a vector of what would be repeated values but for the fact that the person who created the original table (usually in excel or something like that) didn't fill in
}
\usage{
fillVec(broken_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{broken_list}{
A vector of values with gaps you want to plug. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A filled-in vector. 
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
a <- c("a",NA,"bravo",NA,NA,"charlie",NA,NA,NA)
fillVec(a)
## The function is currently defined as
function (broken_list) 
{
    curr <- c()
    a <- c()
    for (i in broken_list) {
        if (is.na(i)) {
            a <- c(a, curr)
        }
        else {
            curr <- i
            a <- c(a, curr)
        }
    }
    return(a)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
