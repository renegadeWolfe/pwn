recomp <-
function(red,blu){# red is slightly favoured.
	f0 <- function(x,y){# !!! function is local !!! 
		a <- ifelse(!is.na(x),x,y)
		return(a)
	}
	n.red <- red[,"id"]
	n.blu <- blu[,"id"]
	purple <- unique(c(n.red,n.blu))
	# print(names(red))
	df0 <- data.frame(purple)
	df0$red_missing <- !(purple %in% red$id) # not in red
	df0$blu_missing <- !(purple %in% blu$id) # not in blu
	red_patch <- as.vector(df0[df0$red_missing==T,1])
	blu_patch <- as.vector(df0[df0$blu_missing==T,1]) 
	if(length(red_patch)>0 & length(blu_patch)>0){ # both datasets have missing cases
		# = PATCH RED =
		red2 <- red[-c(1:length(red[,1])),] # declares an empty dataframe 
 		blank_row <- rep(NA,length(red2))
		for(i in c(1:length(red_patch))){
			red2 <- rbind(red2,blank_row)
		}
		colnames(red2) <- names(red)
		red2$id <- red_patch
		red <- rbind(red,red2) # put in missing cases 
		red <- red[order(red$id),] # sort by id. 
		# print(red) # CORRECT TO HERE, AT LEAST.	
		# - end of red patch -
		# = PATCH BLU = 
		blu2 <- blu[-c(1:length(blu[,1])),] # declares an empty dataframe 
 		blank_row <- rep(NA,length(blu2))
		for(i in c(1:length(blu_patch))){
			blu2 <- rbind(blu2,blank_row)
		}
		colnames(blu2) <- names(blu)
		blu2$id <- blu_patch
		blu <- rbind(blu,blu2) # put in missing cases 
		blu <- blu[order(blu$id),] # sort by id. 
		print(blu)
		# - end of blu patch - 
		dfx <- mapply(f0,red,blu) # puts the data together. 
	} else if(length(red_patch)==0 & length(blu_patch)>0){ # only blu has missing cases
#		cat("No cases missing in the first dataset.\n")
#		cat("Cases missing in the SECOND dataset:", blu_patch, "\n") 
		# = PATCH BLU = 
		blu2 <- blu[-c(1:length(blu[,1])),] # declares an empty dataframe 
 		blank_row <- rep(NA,length(blu2))
		for(i in c(1:length(blu_patch))){
			blu2 <- rbind(blu2,blank_row)
		}
		colnames(blu2) <- names(blu)
		blu <- rbind(blu,blu2) # put in missing cases 
		blu <- blu[order(blu$id),] # sort by id. 
		# - end of blu patch - 
		dfx <- mapply(f0,red,blu) # puts the data together. 
	} else if(length(red_patch)>0 & length(blu_patch)==0){# only red has missing cases
		# = PATCH RED =
		red2 <- red[-c(1:length(red[,1])),] # declares an empty dataframe 
 		blank_row <- rep(NA,length(red2))
		for(i in c(1:length(red_patch))){
			red2 <- rbind(red2,blank_row)
		}
		colnames(red2) <- names(red)
		red2$id <- red_patch
		red <- rbind(red,red2) # put in missing cases 
		red <- red[order(red$id),] # sort by id. 
		# print(red)
		# - end of red patch -
		dfx <- mapply(f0,red,blu) # puts the data together. 
	} else{dfx <- mapply(f0,red,blu)}
	return(dfx)
}
