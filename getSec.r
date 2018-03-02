
# clean up the DURATION, and return the duration in _SECONDS_. 
getSec <- function(d){
d1 <- gsub("hrs*\\s*","_",d) # at this point, anything with hours has a | in it. Anything that just has mins doesn't. NA remains. 
rat <- data.frame(d1)
rat$d1 <- as.character(rat$d1)
rat$got_hrs <- ifelse(grepl("_",rat$d1),1,0)
rat$got_val <- ifelse(is.na(rat$d1),0,1)
rat$d2 <- ifelse(!is.na(rat$d1),paste("_",rat$d1,sep=""),NA)
rat$d3 <- ifelse(rat$got_hrs==0 & rat$got_val==1, paste("0",rat$d2,sep=""),rat$d1)
d3 <- rat$d3
d3 <- gsub("\\s*mins*\\s*","",d3)
d3 <- gsub("_$","_0",d3)
d3 <- ifelse(is.na(d3),"NA_NA",d3)

txtCnct <-textConnection(d3)
red <- read.delim(txtCnct,sep="_",header=F)
close(txtCnct)
colnames(red) <- c("hrDuration","minDuration")

secDuration <- red$hrDuration*3600 + red$minDuration*60
return(secDuration)
}