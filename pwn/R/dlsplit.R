dlsplit <-
function(df1,varX){
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
