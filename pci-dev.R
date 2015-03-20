# General function for plotting points and whiskers for ci
# It expects a data frame arranged as:
# data.frame[x, y, lcl, ucl]
# names are unimportant: the function extracts by index
p.ci = function(d = NULL, col = "blue", pch = 16, cex = 1, lwd = 0.5, tipln = 0.15, offset = 0, v = T) {

	if(v) {
		x = d[,1] + offset
		y = d[,2]
		y.lcl = d[,3]
		y.ucl = d[,4]

		# plot the points
		points(y ~ x, col = col, pch = pch, cex = cex)
		
		# add vertical bars for CI
		segments(x0 = x, y0 = y.lcl, x1 = x, y1 = y.ucl,
				 col = col,
				 lwd = lwd)
				 
		# Now horizontal line-end bars
		# lower
		segments(x0 = x - tipln,
				 y0 = y.lcl,
				 x1 = x + tipln,
				 y1 = y.lcl,
				 col = col,
				 lwd = lwd )
		# upper
		segments(x0 = x - tipln,
				 y0 = y.ucl,
				 x1 = x + tipln,
				 y1 = y.ucl,
				 col = col,
				 lwd = lwd )		
	} # end vertical

	# not vertical reverses X & Y
	if(!v) {
		x = d[,2]
		y = d[,1] + offset
		x.lcl = d[,3]
		x.ucl = d[,4]

		# plot the points
		points(y ~ x, col = col, pch = pch, cex = cex)
		
		# add horizontal bars for CI
		segments(x0 = x.lcl, y0 = y, x1 = x.ucl, y1 = y,
				 col = col,
				 lwd = lwd)
				 
		# Now horizontal line-end bars
		# lower
		segments(x0 = x.lcl,
				 y0 = y - tipln,
				 x1 = x.lcl,
				 y1 = y + tipln,
				 col = col,
				 lwd = lwd )
		# upper
		segments(x0 = x.ucl,
				 y0 = y - tipln,
				 x1 = x.ucl,
				 y1 = y + tipln,
				 col = col,
				 lwd = lwd )
	} # end vertical
	
} # end p.ci()
