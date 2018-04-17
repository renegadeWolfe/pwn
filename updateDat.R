# This function was written to make it easier to bulk update data in a dataframe. 

updateDat <- function(df0,ctd2,idvar){
df0N <- names(df0)
ctd2N <- names(ctd2)
colToUpdate <- df0N[df0N %in% ctd2N]
colToUpdate <- colToUpdate[! colToUpdate %in% idvar]
ctd2N <- ifelse(ctd2N==idvar,ctd2N,paste(ctd2N,"new",sep="_"))
colnames(ctd2) <- ctd2N
dfx <- merge(df0,ctd2,by.x=idvar,by.y=idvar,all=T)
for(i in colToUpdate){
	dfx[,i] <- ifelse(!is.na(dfx[,paste(i,"new",sep="_")]), dfx[,paste(i,"new",sep="_")], dfx[,i])
}
dfx <- dfx[,-grep("_{1}new",colnames(dfx))]
return(dfx)
}
