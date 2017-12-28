chkmate <- function(id1,var1,id2,var2,threshold){df1 <- data.frame(id1,var1)df2 <- data.frame(id2,var2)
dfx <- merge(df1,df2,by.x="id1",by.y="id2",all=T)colnames(dfx) <- c("id","data1","data2")accept.diff <- (dfx[,2]+dfx[,3])/2 * thresholddfx$chk <- ifelse(abs(dfx[,2]-dfx[,3]) > accept.diff,1,0)dfx$chk <- ifelse(is.na(dfx$chk),1,dfx$chk)
flag <- subset(dfx,dfx$chk=="1")flag$chk <- NULL
return(flag)
}