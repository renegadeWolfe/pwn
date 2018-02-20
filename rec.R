rec <- function(df1,v=TRUE){
if(isTRUE(v)){
cases <- length(df1[,1])
paras <- length(df1)
cat("Data has: ", cases," cases and ",paras,"parameters. \n" )
varnames <- names(df1)
n_NA <- colSums(is.na(df1))
n_NA <- data.frame(n_NA)
n_NA <- n_NA[,1]
varClass <- c()
varMin <- c()
varMn <- c()
varMd <- c()
varMax <- c()
for(i in df1){
var_class <- class(i)
varClass <- c(varClass,var_class)
if(var_class=="numeric" | var_class=="integer"){
var_min <- min(i,na.rm=T)
varMin <- c(varMin,var_min)
var_mn <- mean(i,na.rm=T)
varMn <- c(varMn,var_mn)
var_md <- median(i,na.rm=T)
varMd <- c(varMd,var_md)
var_max <- max(i,na.rm=T)
varMax <- c(varMax,var_max)
} else{
varMin <- c(varMin,NA)
varMn <- c(varMn,NA)
varMd <- c(varMd,NA)
varMax <- c(varMax,NA)
}
}
dfx <- data.frame(varnames,varClass,n_NA,varMin,varMn,varMd,varMax)
colnames(dfx) <- c("Variable","Class","NAs","Min","Mean","Median","Max")
return(dfx)} else{
cases <- length(df1[,1])
paras <- length(df1)
cat("Data has: ", cases," cases and ",paras,"parameters. \n" )
}
}
