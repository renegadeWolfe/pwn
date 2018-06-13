\name{dlsplit}
\alias{dlsplit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Splits the "<" sign from those annoying variables that have a detection limit and have been keyed in as such.  
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Cleans those unusable variables that have a detection limit, and were keyed in with the "<" sign. R will read such variables as a factor. 
This will return the variable in question as a numeric and another variable that consists solely of the "<" sign
}
\usage{
dlsplit(df1, varX)
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
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
It is I, LeClerc!
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
bravo <- c(0.002, "<0.003", 0.007, 0.125, ">0.5")
x <- data.frame(id,alpha,bravo)

x <- dlsplit(x,"alpha")
x <- dlsplit(x,"bravo")

## The function is currently defined as
function (df1, varX) 
{
		red <- as.character(df1[,varX])
		varNme <- as.character(varX)
		varNme_less <- paste(varNme,"ND",sep="_")
		red.1stChar <- substr(red,1,1)
		red.lessthan <- ifelse(red.1stChar=="<","<",
										ifelse(red.1stChar==">",">",NA))
		red <- sub("<","",red)# remove less than sign.
		red <- as.numeric(sub(">","",red))# remove more than sign. 
		df1[,varX] <- NULL
		df1$var.nd <- red.lessthan
		names(df1)[names(df1) == 'var.nd'] <- varNme_less
		df1$red <- red
		names(df1)[names(df1) == 'red'] <- varNme
		return(df1)
		}
}	

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ detection }
\keyword{ limit }
\keyword{ split }
\keyword{ fix }% __ONLY ONE__ keyword per line
