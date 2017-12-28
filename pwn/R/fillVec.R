fillVec <- function(broken_list){
curr <- c()
a <- c()
for(i in broken_list){
if(is.na(i)){
	a <- c(a,curr)
} else{
	curr <- i 
	a <- c(a,curr)
}
}
return(a)
}