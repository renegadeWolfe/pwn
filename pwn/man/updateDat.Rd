\name{updateDat}
\alias{updateDat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Block updates a dataframe. 
}
\description{
Say you have a dataset that consists of 50 cases. You lack data for a specific subset of those cases (say, the 10 most recent cases) - for whatever reason, said data exists, but in another file
This function will make combining that data less painful. 
}
\usage{
updateDat(df0, updat, idvar)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df0}{
%%     ~~Describe \code{df0} here~~
	The original dataset (the one that needs updating). 
}
  \item{updat}{
%%     ~~Describe \code{updat} here~~
	The dataframe containing the data you want to add. 
}
  \item{idvar}{
%%     ~~Describe \code{idvar} here~~
	A case identifier, to both datasets, with the same colname. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
	A single dataframe with the updates patched in. 
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
r1 <- c(1:10)
r1N <- c("alpha","bravo","charlie","delta","echo","foxtrot","golf","hotel","lima","juliet")
df1 <- data.frame()
for(i in 1:5){
df1 <- rbind(df1,r1)
}
r2 <- c(NA,NA,NA,4:20)
for(i in 1:5){
df1 <- rbind(df1,r2)
}

colnames(df1) <- r1N
df1$id <- c("I","II","III","IV","V","VI","VII","VIII","IX","X")

r3 <- c(3:1)
newDat <- data.frame()
for(i in 1:5){
newDat <- rbind(newDat,r3)
}

colnames(newDat) <- r1N[1:3]
newDat$id <- c("VI","VII","VIII","IX","X")


# To patch newDat into df1: 
updateDat(df1,newDat,"id")

## The function is currently defined as
function (df0, updat, idvar) 
{
    df0N <- names(df0)
    updatN <- names(updat)
    colToUpdate <- df0N[df0N \%in\% updatN]
    colToUpdate <- colToUpdate[!colToUpdate \%in\% idvar]
    updatN <- ifelse(updatN == idvar, updatN, paste(updatN, "new", 
        sep = "_"))
    colnames(updat) <- updatN
    dfx <- merge(df0, updat, by.x = idvar, by.y = idvar, all = T)
    for (i in colToUpdate) {
        dfx[, i] <- ifelse(!is.na(dfx[, paste(i, "new", sep = "_")]), 
            dfx[, paste(i, "new", sep = "_")], dfx[, i])
    }
    dfx <- dfx[, -grep("_{1}new", colnames(dfx))]
    return(dfx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }% use one of  RShowDoc("KEYWORDS")
\keyword{ data }% __ONLY ONE__ keyword per line
