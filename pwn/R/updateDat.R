updateDat <-
function(df0,updat,idvar){
df0N <- names(df0)
updatN <- names(updat)
colToUpdate <- df0N[df0N %in% updatN]
colToUpdate <- colToUpdate[! colToUpdate %in% idvar]
updatN <- ifelse(updatN==idvar,updatN,paste(updatN,"new",sep="_"))
colnames(updat) <- updatN
dfx <- merge(df0,updat,by.x=idvar,by.y=idvar,all=T)
for(i in colToUpdate){
	dfx[,i] <- ifelse(!is.na(dfx[,paste(i,"new",sep="_")]), dfx[,paste(i,"new",sep="_")], dfx[,i])
}
dfx <- dfx[,-grep("_{1}new",colnames(dfx))]
return(dfx)
}
