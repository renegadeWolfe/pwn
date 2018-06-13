colpatch <-
function(red,blue){
blue_lacks <- names(red)%in%names(blue)#not in blue
red_lacks <- names(blue)%in%names(red)#not in red
	if(all(blue_lacks==T) & all(red_lacks==T)){# datasets have same columns
		cat("Column names are identical.\n No columns will be added for either dataframe. \n")
		out <- rbind(red,blue)
		return(out)
	} else if(!all(blue_lacks==T) & !all(red_lacks==T)){# both datasets have missing columns
		not_in_blue <- names(red)%in%names(blue)
		redN <- names(red)
		blue_mia <- data.frame(redN,not_in_blue)
		blue_mia <- subset(blue_mia,blue_mia[,2]==F)
		blue_mia <- blue_mia[,1]
		blue_r <- length(blue[,1])
		Nblue_mia <- length(blue_mia)
		not_in_red <- names(blue)%in%names(red)
		blueN <- names(blue)
		red_mia <- data.frame(blueN,not_in_red)
		red_mia <- subset(red_mia,red_mia[,2]==F)
		red_mia <- red_mia[,1]
		red_r <- length(red[,1])
		Nred_mia <- length(red_mia)
		Blue_needs <- data.frame(matrix(NA, nrow=blue_r, ncol=Nblue_mia))
		colnames(Blue_needs) <- blue_mia
		blue <- cbind(blue,Blue_needs)
		Red_needs <- data.frame(matrix(NA,nrow=red_r,ncol=Nred_mia))
		colnames(Red_needs) <- red_mia
		red <- cbind(red,Red_needs)
		out <- rbind(red,blue)
		return(out)
	}
 else if(!all(blue_lacks==T) & all(red_lacks==T)){# missing in second dataframe
		not_in_blue <- names(red)%in%names(blue)
		redN <- names(red)
		blue_mia <- data.frame(redN,not_in_blue)
		blue_mia <- subset(blue_mia,blue_mia[,2]==F)
		blue_mia <- blue_mia[,1]
		blue_r <- length(blue[,1])
		Nblue_mia <- length(blue_mia)
		Blue_needs <- data.frame(matrix(NA, nrow=blue_r, ncol=Nblue_mia))
		colnames(Blue_needs) <- blue_mia
		blue <- cbind(blue,Blue_needs)
		out <- rbind(red,blue)
		return(out)
	} else if(all(blue_lacks==T) & !all(red_lacks==T)){# missing in first dataframe
		not_in_red <- names(blue)%in%names(red)
		blueN <- names(blue)
		red_mia <- data.frame(blueN,not_in_red)
		red_mia <- subset(red_mia,red_mia[,2]==F)
		red_mia <- red_mia[,1]
		red_r <- length(red[,1])
		Nred_mia <- length(red_mia)
		Red_needs <- data.frame(matrix(NA,nrow=red_r,ncol=Nred_mia))
		colnames(Red_needs) <- red_mia
		red <- cbind(red,Red_needs)
		out <- rbind(red,blue)
		return(out)
	}
}
