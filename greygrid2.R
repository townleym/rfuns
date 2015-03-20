greygrid2 = function(x = NA, y = NA, xrange = c(0, 360), yrange = c(30, 60), xby = 30, yby = 5, vline = T, hline = T, bg.col = "gray98", cex = 0.85) {
	
	# # uncomment to debug
	# x = NA
	# y = NA
	# xrange = c(2, 4) ; xby = 1
	# yrange = c(1, 4) ; yby = 1
	# greybg = T
	# cex = 0.85

	if(length(x) > 1 & length(y) > 1) {
		xrange = c(min(x), max(x))
		yrange = c(min(y), max(y))
	}
	
		
	# hard-coded colors		
	ax.col = "gray45" 		# axis labels and titles
	grid.col = "gray98"		# grid lines
	box.col = "gray90"		# background color for plot region


	# This is the lagnappe, a little extra padding
	# on each side, top, and bottom	
	x.lp = (max(xrange) - min(xrange)) * 0.015
	y.lp = (max(yrange) - min(yrange)) * 0.015
	

	# Used to define the limits of the background
	# rectangle in the plotting area, and the grid lines
	xl = xrange[1] - x.lp
	xr = xrange[2] + x.lp
	yb = yrange[1] - y.lp
	yt = yrange[2] + y.lp

	# line widths for grid lines	
	vline = 1
	hline = 2
	tick.wd = 2

	# below are for counting axis ticks/intervals intelligently	
	if(length(x) > 1 & length(y) > 1) {
		xats = pretty(x)
		yats = pretty(y)
	} else {
		hmy = floor(max(yrange) / yby)
		lmy = ceiling(min(yrange) / yby)
		
		hmx = floor(max(xrange) / xby)
		lmx = ceiling(min(xrange)/ xby)
		
		yats = lmy:hmy * yby
		xats = lmx:hmx * xby		
	}
	
	# Set global plotting options
	par(bg = bg.col, 
		col.lab = ax.col, 
		col = ax.col,
		col.axis = ax.col,
		col.main = ax.col,
		col.sub = ax.col
		)

	# initialize & draw
	plot.new()
	plot.window(xlim = xrange + c(-1,1) * x.lp, ylim = yrange + c(-1,1) * y.lp)
	
	# background rectangle for plotting area
	rect(xl, yb, xr, yt, col = box.col, border = "transparent")	

	# only draw gridlines if specified (default is T for both)
	if(hline) {
		n = length(yats)
		xleft = rep(xl, n)
		xright = rep(xr, n)
		segments(x0 = xleft, y0 = yats, x1 = xright, y1 = yats, lty = 1, lwd = hline, col = grid.col)
	}

	if(vline) {
		n = length(xats)
		ymin = rep(yb, n)
		ymax = rep(yt, n)
		segments(x0 = xats, y0 = ymin, x1 = xats, y1 = ymax, lty = 2, lwd = vline, col = grid.col)
	}

	# abline(v = xats, lwd = vline, lty = 2, col = grid.col)
	# abline(h = yats, lwd = hline, col = grid.col)

	# finally, plot axes
	axis(side = 1, at = xats, 
		 col.ticks = box.col, 
		 col = ax.col, 
		 col.axis = ax.col, 
		 line = -1, 
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
	
} # end greygrid2()

