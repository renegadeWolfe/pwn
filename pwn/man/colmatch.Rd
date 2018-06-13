\name{colmatch}
\alias{colmatch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify overlapping columns in 2 dataframes. 
}
\description{
This function is meant to flag overlapping columns in 2 dataframes. Helps weed out duplicate variables when merging data
}
\usage{
colmatch(red, blue)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{red}{
	is a dataframe.
}
  \item{blue}{
	is a dataframe.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Displays the names of overlapping columns. 
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
alpha <- c(1:5)
bravo <- c(6:10) 
charlie <- c(11:15)
delta <- c(16:20)
echo <- c(21:25)

df1 <- data.frame(alpha,bravo,charlie,delta)
df1a <- data.frame(alpha,bravo,delta)
df2 <- data.frame(alpha,bravo,delta,echo)

colmatch(df1,df1a)
colmatch(df2,df1)

## The function is currently defined as
function (red, blue) 
{
    redNmes <- names(red)
    blueNmes <- names(blue)
    RED <- data.frame(redNmes \%in\% blueNmes)
    RED$redNmes <- redNmes
    red1 <- subset(RED, RED$redNmes..in..blueNmes == "TRUE", 
        drop = T)
    x <- red1$redNmes
    cat("The following columns are common to both datasets:\n", 
        x, "\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ compare } 
\keyword{ match } 
\keyword{ column }% __ONLY ONE__ keyword per line
