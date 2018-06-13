\name{chkmate}
\alias{chkmate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cross check data.  
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
A less painful way of comparing what you once believed to be identical variables in 2 different data sets. Specify how your data identifier, the variable in question and the acceptable difference threshold
}
\usage{
chkmate(id1, var1, id2, var2, threshold)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id1}{
%%     ~~Describe \code{id1} here~~
	The case identifier for the first data set. 
}
  \item{var1}{
%%     ~~Describe \code{var1} here~~
	The variable in question from the first data set. 
}
  \item{id2}{
%%     ~~Describe \code{id2} here~~
	The case identifier in the second data set. 
}
  \item{var2}{
%%     ~~Describe \code{var2} here~~
	The variable in question from the second data set. 
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
A value between 0-1, indicating the acceptable difference threshold. I wouldn't recommend going higher than 10% (0.1). 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
Returns a dataframe contain the id and values which violate the specified difference threshold.
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
alpha <- c(0.015, 0.35, 0.0025, 0.007, 0.125)

df0 <- data.frame(id,alpha)
df1 <- data.frame(id,alpha)
df1$alpha[2] <- 0.385# let's just increase this value by 10%
df1$alpha[5] <- NA

chkmate(df0$id,df0$alpha,df1$id,df1$alpha,0.05)
chkmate(df0$id,df0$alpha,df1$id,df1$alpha,0.1)

## The function is currently defined as
function (id1, var1, id2, var2, threshold) 
{
    if (missing(threshold)) {
        threshold <- 0.1
    }
    if (is.factor(var1) | is.factor(var2) | is.character(var1) | 
        is.character(var2)) {
        df1 <- data.frame(id1, var1)
        df2 <- data.frame(id2, var2)
        dfx <- merge(df1, df2, by.x = "id1", by.y = "id2", all = T)
        colnames(dfx) <- c("id", "data1", "data2")
        dfx$data1 <- as.character(dfx$data1)
        dfx$data2 <- as.character(dfx$data2)
        dfx$chk <- ifelse(dfx$data1 == dfx$data2, 0, 1)
        dfx$chk <- ifelse(is.na(dfx$data1) & is.na(dfx$data2), 
            0, dfx$chk)
    }
    else {
        df1 <- data.frame(id1, var1)
        df2 <- data.frame(id2, var2)
        dfx <- merge(df1, df2, by.x = "id1", by.y = "id2", all = T)
        colnames(dfx) <- c("id", "data1", "data2")
        accept.diff <- (dfx[, 2] + dfx[, 3])/2 * threshold
        dfx$chk <- ifelse(abs(dfx[, 2] - dfx[, 3]) > accept.diff, 
            1, 0)
        dfx$chk <- ifelse(is.na(dfx$chk), 1, dfx$chk)
        dfx$chk <- ifelse(is.na(dfx$data1) & is.na(dfx$data2), 
            0, dfx$chk)
    }
    flag <- subset(dfx, dfx$chk == "1")
    flag$chk <- NULL
    return(flag)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
