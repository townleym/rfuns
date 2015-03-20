hbar = function(xrange = c(0, 360), yrange = c(30, 60), xby = 30, yby = 5, invbg = F, vgrid = F, cex = 0.85) {

	# xrange = c(56, 80) ; xby = 2
	# yrange = c(105, 245) ; yby = 15
	# invbg = T ; vgrid = F
	# cex = 0.85
	
	if(invbg) {
		ax.col = "gray45"
		box.col = "gray98"
		bg.col = "gray92"
	} else {
		ax.col = "gray35"
		box.col = "gray92"
		bg.col = "transparent"		
	}
	
	# This is the lagnappe, a little extra padding
	# on each side, top, and bottom	
	x.lp = min(xrange) * 0.015
	y.lp = min(yrange) * 0.015

	# Used to define the horizontal limits of the bars
	# and the overall plot region
	xl = xrange[1] - x.lp
	xr = xrange[2] + x.lp
	yb = yrange[1] - y.lp
	yt = yrange[2] + y.lp

	hline = 2
	tick.wd = 2

	# below are for counting axis ticks/intervals intelligently		
	hmy = floor(max(yrange) / yby)
	lmy = ceiling(min(yrange) / yby)
	
	hmx = floor(max(xrange) / xby)
	lmx = ceiling(min(xrange)/ xby)
	
	yats = lmy:hmy * yby
	xats = lmx:hmx * xby
	
	# this is weird, just get the
	# odd-numbered from the list of 
	# y 'at' values
	start.ind = (1:(length(yats) / 2) * 2)	- 1
	starts = yats[start.ind]
	
	# I think we have enough to draw the plot region

	# par(bg = "transparent")
	par(bg = bg.col, 
		col.lab = ax.col, 
		col = ax.col,
		col.axis = ax.col,
		col.main = ax.col,
		col.sub = ax.col
		)
	plot.new()
	plot.window(xlim = xrange + c(-1,1) * x.lp, ylim = yrange + c(-1,1) * y.lp)

	# loop through and draw the gray bars
	
	for (i in starts)
	polygon(x = c(xl, xr, xr, xl), 
			y = c(i, i, i + yby, i + yby), 
			col = box.col, 
			border = box.col)
	
	axis(side = 1, at = xats, 
		col.ticks = box.col, 
		col = box.col, 
		col.axis = ax.col, 
		line = -1.15, 
		lwd = 0,
		lwd.ticks = tick.wd,
		cex.axis = cex,
		las = 1)
		
	axis(side = 2, at = yats, 
		col.ticks = box.col, 
		col = ax.col, 
		col.axis = ax.col, 
		line = -1.25, 
		lwd = 0, 
		lwd.ticks = tick.wd,
		cex.axis = cex,
		las = 1)
	
	# Add lines to top/bottom of bounding boxes
	# abline(h = yats, col = box.col, lwd = 2)

		n = length(yats)
		xleft = rep(xl, n)
		xright = rep(xr, n)
		segments(x0 = xleft, y0 = yats, x1 = xright, y1 = yats, lty = 1, lwd = hline, col = box.col)
	
	}
