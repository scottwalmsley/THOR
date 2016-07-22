rotateTHOR = function (mat){
	
	mdata = cbind(mat$IDX,mat$x,mat$y);
	colnames(mdata) = c("IDX","x","y");
	
	mdata = as.data.frame(mdata);
	mdata = mdata[order(mdata$x, mdata$y),]

	### ROTATION matrix
	theta = 2 * pi / 8;   ## 45degree
	cc = cos(theta)
	ss = sin(theta)
	rot.matrix = matrix(c(cc, ss, -ss, cc), 2, 2)
	
	rdata = as.matrix(mdata[,2:3]) %*% rot.matrix
	rdata = as.data.frame(rdata)
	colnames(rdata) = c("x", "y")

	return = list(M = rdata);

}
