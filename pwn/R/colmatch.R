colmatch <-
function(red,blue){
	redNmes <- names(red)
	blueNmes <- names(blue)
	RED <- data.frame(redNmes %in% blueNmes)
	RED$redNmes <- redNmes
        red1 <- subset(RED, RED$redNmes..in..blueNmes == "TRUE", drop = T)
	x <- red1$redNmes
	cat("Common columns:\n",x,"\n")

}
