\name{recomp}
\alias{recomp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Recompile data from 2 data sets. 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function takes 2 data frames and compiles them into a single one. This has nothing to do with rbind or merge. Rather, it's for the unfortunate times where you realize that 2 data sets you believed to be the same weren't. 

}
\usage{
recomp(red, blu)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{red}{
%%     ~~Describe \code{red} here~~
red is a dataframe with a case identifying column "id". It is also the slightly favoured dataset, and has to be specified first. 
}
  \item{blu}{
%%     ~~Describe \code{blu} here~~
blu is a dataframe that was supposed to be the same as red, but contains data red doesn't. Like red, it should also contain a case identifying column called "id".
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
For this function to work, both your dataframes need to have the exact same columns, arranged in the exact same order. They should also both have a column called "id", containing unique case identifiers. 
If you lack any of the above (identical columns and an "id" column), this function will fail. Also, be sure to check the output. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a single dataframe, compiled from the 2 that you specified. 
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
x1 <- c(1:5)
x2 <- c(6:10)
x3 <- c(11:15)
x4 <- c(16:20)
x5 <- c(21:25)

# what you thought you had 2 copies of
dfx <- data.frame(rbind(x1,x2,x3,x4,x5))
colnames(dfx) <- c("alpha","bravo","charlie","delta","echo")

dfx$id <- row.names(dfx)
print(dfx)

x4 <- c(47,48,49,NA,51)
dfA <- data.frame(rbind(x1,x2,x3,x4,x5))
colnames(dfA) <- c("alpha","bravo","charlie","delta","echo")
dfA$id <- row.names(dfA)
print(dfA)

# if there's missing cases 
x1 <- c(31:35)
dfz <- data.frame(rbind(x1,x3,x4,x5))
colnames(dfz) <- c("alpha","bravo","charlie","delta","echo")
dfz$id <- row.names(dfz)
print(dfz)
# say we favour dfz 
datadawg <- recomp(dfz,dfx)
print(datadawg)

## The function is currently defined as
function (red, blu) 
{
    f0 <- function(x, y) {
        a <- ifelse(!is.na(x), x, y)
        return(a)
    }
    n.red <- red[, "id"]
    n.blu <- blu[, "id"]
    purple <- unique(c(n.red, n.blu))
    df0 <- data.frame(purple)
    df0$red_missing <- !(purple \%in\% red$id)
    df0$blu_missing <- !(purple \%in\% blu$id)
    red_patch <- as.vector(df0[df0$red_missing == T, 1])
    blu_patch <- as.vector(df0[df0$blu_missing == T, 1])
    if (length(red_patch) > 0 & length(blu_patch) > 0) {
        red2 <- red[-c(1:length(red[, 1])), ]
        blank_row <- rep(NA, length(red2))
        for (i in c(1:length(red_patch))) {
            red2 <- rbind(red2, blank_row)
        }
        colnames(red2) <- names(red)
        red2$id <- red_patch
        red <- rbind(red, red2)
        red <- red[order(red$id), ]
        # print(red)
        blu2 <- blu[-c(1:length(blu[, 1])), ]
        blank_row <- rep(NA, length(blu2))
        for (i in c(1:length(blu_patch))) {
            blu2 <- rbind(blu2, blank_row)
        }
        colnames(blu2) <- names(blu)
        blu2$id <- blu_patch
        blu <- rbind(blu, blu2)
        blu <- blu[order(blu$id), ]
        # print(blu)
        dfx <- mapply(f0, red, blu)
    }
    else if (length(red_patch) == 0 & length(blu_patch) > 0) {
        blu2 <- blu[-c(1:length(blu[, 1])), ]
        blank_row <- rep(NA, length(blu2))
        for (i in c(1:length(blu_patch))) {
            blu2 <- rbind(blu2, blank_row)
        }
        colnames(blu2) <- names(blu)
        blu <- rbind(blu, blu2)
        blu <- blu[order(blu$id), ]
        dfx <- mapply(f0, red, blu)
    }
    else if (length(red_patch) > 0 & length(blu_patch) == 0) {
        red2 <- red[-c(1:length(red[, 1])), ]
        blank_row <- rep(NA, length(red2))
        for (i in c(1:length(red_patch))) {
            red2 <- rbind(red2, blank_row)
        }
        colnames(red2) <- names(red)
        red2$id <- red_patch
        red <- rbind(red, red2)
        red <- red[order(red$id), ]
        # print(red)
        dfx <- mapply(f0, red, blu)
    }
    else {
        dfx <- mapply(f0, red, blu)
    }
    return(dfx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
