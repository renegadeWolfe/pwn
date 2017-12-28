\name{colpatch}
\alias{colpatch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Column Patch
}
\description{
colpatch takes 2 dataframes and fills in whatever columns are missing. If rbind isn't working and it says your dataframes have the same number of columns, run colchk to see what's different.
}
\usage{
colpatch(red, blue)
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
Returns a single datafame that is really a rbind of the 2, with missing columns patched, where necessary. 
If the columns are identical, it does a simple rbind and returns the result. Columns will be arranged with reference to the first dataframe in the input. 
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
df2 <- data.frame(alpha,bravo,charlie,echo)

dfx <- colpatch(df1,df1a)
head(dfx)

dfy <- colpatch(df1,df2)
head(dfy)

## The function is currently defined as
function (red, blue) 
{
    blue_lacks <- names(red) \%in\% names(blue)
    red_lacks <- names(blue) \%in\% names(red)
    if (all(blue_lacks == T) & all(red_lacks == T)) {
				cat("Column names are identical.\n No columns will be added for either dataframe. \n")
        out <- rbind(red, blue)
        return(out)
    }
    else if (!all(blue_lacks == T) & !all(red_lacks == T)) {
        not_in_blue <- names(red) \%in\% names(blue)
        redN <- names(red)
        blue_mia <- data.frame(redN, not_in_blue)
        blue_mia <- subset(blue_mia, blue_mia[, 2] == F)
        blue_mia <- blue_mia[, 1]
        blue_r <- length(blue[, 1])
        Nblue_mia <- length(blue_mia)
        not_in_red <- names(blue) \%in\% names(red)
        blueN <- names(blue)
        red_mia <- data.frame(blueN, not_in_red)
        red_mia <- subset(red_mia, red_mia[, 2] == F)
        red_mia <- red_mia[, 1]
        red_r <- length(red[, 1])
        Nred_mia <- length(red_mia)
        Blue_needs <- data.frame(matrix(NA, nrow = blue_r, ncol = Nblue_mia))
        colnames(Blue_needs) <- blue_mia
        blue <- cbind(blue, Blue_needs)
        Red_needs <- data.frame(matrix(NA, nrow = red_r, ncol = Nred_mia))
        colnames(Red_needs) <- red_mia
        red <- cbind(red, Red_needs)
        out <- rbind(red, blue)
        return(out)
    }
    else if (!all(blue_lacks == T) & all(red_lacks == T)) {
        not_in_blue <- names(red) \%in\% names(blue)
        redN <- names(red)
        blue_mia <- data.frame(redN, not_in_blue)
        blue_mia <- subset(blue_mia, blue_mia[, 2] == F)
        blue_mia <- blue_mia[, 1]
        blue_r <- length(blue[, 1])
        Nblue_mia <- length(blue_mia)
        Blue_needs <- data.frame(matrix(NA, nrow = blue_r, ncol = Nblue_mia))
        colnames(Blue_needs) <- blue_mia
        blue <- cbind(blue, Blue_needs)
        out <- rbind(red, blue)
        return(out)
    }
    else if (all(blue_lacks == T) & !all(red_lacks == T)) {
        not_in_red <- names(blue) \%in\% names(red)
        blueN <- names(blue)
        red_mia <- data.frame(blueN, not_in_red)
        red_mia <- subset(red_mia, red_mia[, 2] == F)
        red_mia <- red_mia[, 1]
        red_r <- length(red[, 1])
        Nred_mia <- length(red_mia)
        Red_needs <- data.frame(matrix(NA, nrow = red_r, ncol = Nred_mia))
        colnames(Red_needs) <- red_mia
        red <- cbind(red, Red_needs)
        out <- rbind(red, blue)
        return(out)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ column }
\keyword{ patch }% __ONLY ONE__ keyword per line
