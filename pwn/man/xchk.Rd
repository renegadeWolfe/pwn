\name{xchk}
\alias{xchk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Compares 2 character vectors and returns what's different. May be used for either case ids or column names or whatever else you want to cross check
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Compares 2 vectors and returns the differences.
}
\usage{
xchk(red, blu)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{red}{
%%     ~~Describe \code{red} here~~
is a vector. 
}
  \item{blu}{
%%     ~~Describe \code{blu} here~~
is a vector. 
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
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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

a <- c("alpha","bravo","charlie","delta","echo")
b <- c("alpha","bravo","charlie","echo","foxtrot")

xchk(a,b)

# if checking cases: 
# xchk(df1$id,df2$id) # or whatever your dataframes and/or unique case identifiers are called.

# if checking columns: 
# xchk(names(df1),names(df2))

## The function is currently defined as
function (red, blu) 
{
    red <- as.character(red)
    blu <- as.character(blu)
    purple <- unique(c(red, blu))
    df0 <- data.frame(purple)
    df0$red_missing <- !(purple \%in\% red)
    df0$blu_missing <- !(purple \%in\% blu)
    red_patch <- as.vector(df0[df0$red_missing == T, 1])
    blu_patch <- as.vector(df0[df0$blu_missing == T, 1])
    if (length(red_patch) > 0 & length(blu_patch) > 0) {
        cat("Values missing in the FIRST vector:\n")
        for (i in red_patch) {
            cat(i, "\n")
        }
        cat("\nValues missing in the SECOND vector:\n")
        for (i in blu_patch) {
            cat(i, "\n")
        }
    }
    else if (length(red_patch) == 0 & length(blu_patch) > 0) {
        cat("NO values missing in the first vector.\n")
        cat("\nValues missing in the SECOND vector:\n")
        for (i in blu_patch) {
            cat(i, "\n")
        }
    }
    else if (length(red_patch) > 0 & length(blu_patch) == 0) {
        cat("Values missing in the FIRST vector:\n")
        for (i in red_patch) {
            cat(i, "\n")
        }
        cat("No values missing in the second vector.\n")
    }
    else {
        cat("Cases match.\n")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
