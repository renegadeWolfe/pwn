dlprime <-
function(df1,varX){
red <- as.character(df1[,varX])
varNme <- as.character(varX)
varNme_less <- paste(varNme,"detect",sep="_")
red.1stChar <- substr(red,1,1)
dl_lim <- ifelse(red.1stChar=="<",0,
					ifelse(red.1stChar==">",2,1))
red <- sub("<","",red)
red <- as.numeric(sub(">","",red))
blue <- data.frame(red,dl_lim)
x <- subset(blue,blue$dl_lim == 0,drop=T)
dls <- max(x$red)
blue$dl_lim <- ifelse(blue$red<dls & blue$dl_lim!=0,-1, blue$dl_lim)
# return(blue)
red <- blue$red
dl_lim <- blue$dl_lim
df1[,varX] <- NULL
df1$red <- red
names(df1)[names(df1) == 'red'] <- varNme
df1$detect_lim <- dl_lim
names(df1)[names(df1) == 'detect_lim'] <- varNme_less
return(df1)
}
