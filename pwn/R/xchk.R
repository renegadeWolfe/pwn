xchk <- function(red,blu){
	red <- as.character(red)
	blu <- as.character(blu)
	purple <- unique(c(red,blu))
	df0 <- data.frame(purple)
	df0$red_missing <- !(purple %in% red) # not in red
	df0$blu_missing <- !(purple %in% blu) # not in blu
	red_patch <- as.vector(df0[df0$red_missing==T,1])
	blu_patch <- as.vector(df0[df0$blu_missing==T,1]) 
	if(length(red_patch)>0 & length(blu_patch)>0){
		cat("Values missing in the FIRST vector:\n")
		for(i in red_patch){
			cat(i,"\n")
		}
		cat("\nValues missing in the SECOND vector:\n")
		for(i in blu_patch){
			cat(i,"\n")
		}
	} else if(length(red_patch)==0 & length(blu_patch)>0){
		cat("NO values missing in the first vector.\n")
		cat("\nValues missing in the SECOND vector:\n")
		for(i in blu_patch){
			cat(i,"\n")
		}
	} else if(length(red_patch)>0 & length(blu_patch)==0){
		cat("Values missing in the FIRST vector:\n")
		for(i in red_patch){
			cat(i,"\n")
		}
		cat("No values missing in the second vector.\n")
	} else {cat("Cases match.\n")}
}