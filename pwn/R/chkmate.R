chkmate <- function(id1,var1,id2,var2,threshold){
dfx <- merge(df1,df2,by.x="id1",by.y="id2",all=T)
flag <- subset(dfx,dfx$chk=="1")
return(flag)
}