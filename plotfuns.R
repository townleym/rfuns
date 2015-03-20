analytics = c("#325da7", "#f68c06", "#f7f7f7", "#ffffff", "#181818")

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
	
	# Add lines to top/bottom of bounding boxes
	# abline(h = yats, col = box.col, lwd = 2)

		n = length(yats)
		xleft = rep(xl, n)
		xright = rep(xr, n)
		segments(x0 = xleft, y0 = yats, x1 = xright, y1 = yats, lty = 1, lwd = hline, col = box.col)
	
} # end hbar()

# hbar(xrange = c(0,366), yrange = c(30, 80), xby = 30, yby = 5, col = "gray92")
# hbar(xrange = c(0,366), yrange = c(30, 80), xby = 30, yby = 2, col = "gray92")

# A simpler function that plots a grey plot region with gridlines colored
# the same as the device region background

greygrid = function(x = NA, y = NA, xrange = c(0, 360), yrange = c(30, 60), xby = 30, yby = 5, vline = T, hline = T, greybg = T, cex = 0.85) {
	
	if(length(x) > 1 & length(y) > 1) {
		xpad = (max(x) - min(x)) * 0.015
		ypad = (max(y) - min(y)) * 0.015
		xrange = c(min(x) - xpad, max(x) + xpad)
		yrange = c(min(y) - ypad, max(y) + ypad)
		
		xby = pretty(x)
		yby = pretty(y)
	}
	
	# uncomment to debug
	# xrange = c(0, 4) ; xby = 1
	# yrange = c(0, 4) ; yby = 1
	# greybg = T
	# cex = 0.85
		
	# hard-coded colors		
	ax.col = "gray45" 		# axis labels and titles
	grid.col = "gray98"		# grid lines
	box.col = "gray90"		# background color for plot region
	# background for device region
	bg.col = ifelse(greybg, "gray98", "white")


	# This is the lagnappe, a little extra padding
	# on each side, top, and bottom	
	x.lp = min(xrange) * 0.015
	y.lp = min(yrange) * 0.015

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
	hmy = floor(max(yrange) / yby)
	lmy = ceiling(min(yrange) / yby)
	
	hmx = floor(max(xrange) / xby)
	lmx = ceiling(min(xrange)/ xby)
	
	yats = lmy:hmy * yby
	xats = lmx:hmx * xby
	
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
	
} # end greygrid()


# This function plots points and draws vertical CI lines
# It expects a data frame arranged as:
# data.frame[x, y, lcl, ucl]
# names are unimportant: the function extracts by index
p.vci = function(d = NULL, col = "blue", pch = 16, cex = 1, lwd = 0.5, tipln = 0.15, hadj = 0) {

	x = d[,1] + hadj
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
} # end p.vci()

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

# The older (perhaps) uglier version of greygrid()
greygrid.old = function(xrange = c(0, 360), yrange = c(30, 60), xby = 30, yby = 5) {

	xrange = c(56, 80) ; xby = 2
	yrange = c(105, 245) ; yby = 15
	
	vline = 1
	hline = 2
	tick.wd = 2
		
	ax.col = "gray45"
	box.col = "gray98"
	bg.col = "gray92"

	hmy = floor(max(yrange) / yby)
	lmy = ceiling(min(yrange) / yby)
	
	hmx = floor(max(xrange) / xby)
	lmx = ceiling(min(xrange)/ xby)
	
	yats = lmy:hmy * yby
	xats = lmx:hmx * xby
	
	par(bg = bg.col, 
		col.lab = ax.col, 
		col = ax.col,
		col.axis = ax.col,
		col.main = ax.col,
		col.sub = ax.col
		)
	plot.new()
	plot.window(xlim = xrange, ylim = yrange)

	axis(side = 1, at = xats, col.ticks = box.col, col = ax.col, col.axis = ax.col, line = -0.5, lwd = 0, lwd.ticks = tick.wd)
	axis(side = 2, at = yats, col.ticks = box.col, col = ax.col, col.axis = ax.col, line = -0.95, lwd = 0, lwd.ticks = tick.wd)

	abline(v = xats, lwd = vline, lty = 2, col = box.col)
	abline(h = yats, lwd = hline, col = box.col)
		
	
} # end greygrid.old()

# A probably-not very generalizable dotchart
s.dot = function(vals, nticks = 7, lmarspc = 6, ylabels = NULL, x.lab = NULL, xrange = NULL, titletext = NULL) {
    
    
#     vals
#     ylabels = citylabels
#     x.lab = "LQ"
#     xrange = c(0,3)
#     titletext = "title"
#     nticks = 7
#     lmarspc = 2

    # this is kinda pointless, but I don't feel like
    # taking it out.
    yf = 10
    y.ats = 1:length(vals) * yf
    whys = 1:max(y.ats)

    # Define x-axis range (if given)
    if (is.null(xrange)) {
        xrange = c(min(floor(vals)), max(ceiling(vals)))        
    }

    # Define y-axis range
    y.ex = max(whys) * 0.035
    yrange = c(min(whys) - (yf + y.ex), max(whys) + y.ex)

    x.ats = 1 / 2 * 0:nticks
    
    plot.new()
    par(mar = c(5, 4 + lmarspc, 4, 2) + 0.1)
    par(usr = c(xrange, yrange)) 

#     plot(y = whys, x = rep(0, max(whys)), col = "white", 
#          bty = "n", 
#          xlab = "", 
#          ylab = "", 
#          yaxt = "n", 
#          xaxt = "n",
#          xlim = xrange
#     )

    points(y = y.ats, x = vals, pch = 20, cex = 0.85)
    abline(h = y.ats, lty = 2, lwd = 0.5, col = "grey")
        
    axis(side = 2, at = y.ats, las = 1, 
         labels = ylabels, 
         cex.axis = 0.5, 
         line = 0, 
         col = "grey", 
         col.ticks = "grey")
    axis(side = 1, at = x.ats, cex.axis = 0.75, line = -1,
         col = "grey", 
         col.ticks = "grey")
    
    xat = 1
    segments(x0 = xat, y0 = min(whys), x1 = xat, y1 = max(whys), col = "grey")
    
    # abline(v = 1, col = "grey", lwd = 0.5)
    title(main = titletext)
    title(xlab = x.lab, line = 1.5)
} # end matt.dot function

###############################################################################
# 
# This is a rather stupid function written to save a bunch of lines of code
# It draws (true) dot charts with space between the y-axis and the plot area
# that can be filled with some annotation.
#
# The following need to be sorted for the chart to look nice
# A descending sort puts the largest values at the top of the chart
#
# Data: 
# vals:     The x values (it plots horizontally only), the quantity of interest
# annot:    The values that will go in the margin between the y-axis and plot
# y.lab:    The y axis labels (for each point/annotation)
# 
# Chart options
# titletext:    A string that will be the main title
# f:        A factor used to fix the upper/lower Y plot limits
# lmarspc:  A number of addtional lines that the y-axis will be shifted left
#           (to leave space for the annotation)
# rightshift:   The number of X values to shift the apparent zero for the plot
#               area to the right (this is what acommodates the annotations)
# dotval:   How much each horizontal dot should be worth
# v.cex:    character expansion parameter for all vertical elements
# annot.lab:    A string/label to put at the top of the annotations
# mnote:    A marginal note that will go on the bottom right of the chart
#
###############################################################################
dotchart.ann = function(vals = NULL, 
                        annot = NULL,
                        titletext = "title",
                        x.lab = "X values",
                        y.lab = NULL,
                        f = 50000, 
                        lmarspc = 2, 
                        rightshift = 10000, 
                        dotval = 10000, 
                        v.cex = 0.5, 
                        annot.lab = NULL,
                        mnote = NULL) {

    # The x plot values will be the data values
    # This is just to set the dimensions of the 
    # plotting area and specify the x-axis tickmarks
    x.axis.points = pretty(vals)
    xmin = floor((min(vals) / f)) * f
    xmax = ceiling((max(vals) / f)) * f
    x.range = c(xmin - rightshift, xmax)

    # This two-layer thingy is in case I want to do something
    # different with the y values (like make them every 10)
    # or something stupid like that
    yats = 1:length(vals)
    whys = 1:max(yats)
    # add 5% to the y range for the limits of the plot area
    y.ex = ceiling(max(whys) * 0.05)
    y.range = c(min(whys) - y.ex, max(whys) + y.ex)

    plot.new()
    
    par(mar = c(5, 4 + lmarspc, 4, 2) + 0.1) # add a little left margin for the long city labels
    par(usr = c(x.range, y.range))

    # Plot an empty box. There are more clever ways to do this, but this is kinda
    # quick & dirty
    plot(y = whys, x = rep(0, max(whys)), col = "white", 
         bty = "n", 
         xlab = "", 
         ylab = "", 
         yaxt = "n", 
         xaxt = "n",
         xlim = x.range
    )
    
    # Add vertical lines for readability
    abline(v = x.axis.points + rightshift, col = "grey", lwd = v.cex)
    
    # Add horizontal dashed lines
    # Rather than using the segments(function), draw a dot every 10,000
    xx = (round(0:(xmax/dotval)) * dotval) + rightshift
    for (i in yats) {
        points(y = rep(i, length(xx)), x = xx, pch = ".", col = "grey")
    }
    
    # Now plot the first points
    points(y = whys, x = vals + rightshift, pch = 20, cex = 0.85)
    
    axis(side = 2, at = yats, las = 1, 
         labels = y.lab, 
         cex.axis = v.cex, 
         line = 0, 
         col = "grey", 
         col.ticks = "grey")
    
    options(scipen = 5) # do fixed (vs scientific) notation for the axis
    
    axis(side = 1, at = x.axis.points + rightshift, cex.axis = 0.75, line = -0.5, 
         col = "grey", 
         col.ticks = "grey",
         labels = x.axis.points)
    
    # add a vertical "zero" line
    xat = rightshift
    # segments(x0 = xat, y0 = min(whys), x1 = xat, y1 = max(whys), col = "grey")
    abline(v = rightshift, col = "grey", lwd = v.cex)

    title(main = titletext)
    title(xlab = x.lab, line = 2.5)
    
    # text(x = rep(rightshift / 3, 55), y = 1:55, labels = vals2[,"LQ1"], cex = 0.4)
    text(x = rep(min(x.range), length(annot)), y = yats, labels = annot, cex = v.cex)
    text(x = min(x.range), y = max(yats) + (y.ex / 2), labels = annot.lab, cex = v.cex)

    if (!is.null(mnote)) {
        mtext(mnote, side = 1, adj = 1, cex = 0.65, line = 2.5)
    }
    
} # end dotchart.ann


###############################################################################
# 
# Improved greygrid that can take X & Y values and compute the rest
#
# If X & Y vectors are specified, then xrange, yrange, xby, & yby are
# overwritten.
#
# 
###############################################################################

greygrid2 = function(x = NA, y = NA, xrange = c(0, 360), yrange = c(30, 60), xby = 30, yby = 5, vline = T, hline = T, bg.col = "gray98", cex = 0.85) {
	
	# # uncomment to debug
	# x = NA
	# y = NA
	# xrange = c(2, 4) ; xby = 1
	# yrange = c(1, 4) ; yby = 1
	# cex = 0.85
	# hline = T
	# vline = T
	# bg.col = "gray98"
	
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
	# x.lp = min(xrange) * 0.015
	# y.lp = min(yrange) * 0.015
	x.lp = (max(xrange) - min(xrange)) * 0.015
	y.lp = (max(yrange) - min(yrange)) * 0.015	

	# Used to define the limits of the background
	# rectangle in the plotting area, and the grid lines
	xl = xrange[1] - x.lp
	xr = xrange[2] + x.lp
	yb = yrange[1] - y.lp
	yt = yrange[2] + y.lp

	# line widths for grid lines	
	vlinesize = 1
	hlinesize = 2
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
	par(usr = c(xl, xr, yb, yt))		

	# background rectangle for plotting area
	rect(xl, yb, xr, yt, col = box.col, border = "transparent")	
	
	# Bunch of troubleshooting shiz	
	# points(xrange[1], yrange[1], pch = 15, cex = 30, col = "black")
	# points(xl, yb, pch = 20, cex = 30, col = "purple")
	# axis(side = 1, at = xl:xr)
	# axis(side = 2, at = yb:yt)
	# points(xl, yb, pch = 20, cex = 1, col = "black")	
	# abline(v = 2)
	# mtext("line-1", side = 1, line = -1)
	# mtext("line0", side = 1, line = 0)
	# mtext("line1", side = 1, line = 1)
	# legend("bottomleft", legend = "fuckyou")



	# only draw gridlines if specified (default is T for both)
	if(hline) {
		n = length(yats)
		xleft = rep(xl, n)
		xright = rep(xr, n)
		segments(x0 = xleft, y0 = yats, x1 = xright, y1 = yats, lty = 1, lwd = hlinesize, col = grid.col)
	}

	if(vline) {
		n = length(xats)
		ymin = rep(yb, n)
		ymax = rep(yt, n)
		segments(x0 = xats, y0 = ymin, x1 = xats, y1 = ymax, lty = 2, lwd = vlinesize, col = grid.col)
	}

	# abline(v = xats, lwd = vline, lty = 2, col = grid.col)
	# abline(h = yats, lwd = hline, col = grid.col)

	# finally, plot axes
	axis(side = 1, at = xats, 
		 col.ticks = box.col, 
		 col = ax.col, 
		 col.axis = ax.col, 
		 line = 0, 
		 lwd = 0, 
		 lwd.ticks = tick.wd,
		 cex.axis = cex,
		 las = 1)
		 
	axis(side = 2, at = yats, 
		 col.ticks = box.col, 
		 col = ax.col, 
		 col.axis = ax.col, 
		 line = 0, 
		 lwd = 0, 
		 lwd.ticks = tick.wd,
		 cex.axis = cex,
		 las = 1)
	
} # end greygrid2()


###############################################################################
# 
# Quickscatter
#
# An X-Y scatter with something other than the default colors
# and the option to draw a lowess or abline (with Beta coef)
#
# 
###############################################################################

quickscatter = function(predictor, outcome, line = c("lowess", "straight"), labs = F) {

	xlabel = deparse(substitute(predictor))
	ylabel = deparse(substitute(outcome))

	pointcol = "#F1A340"
	linecol = "#998EC3"

	plot(outcome ~ predictor, col = pointcol, xlab = xlabel, ylab = ylabel, bty = "n")
	# title(main = xlabel, line = -2, cex = 0.5)
	if (line == "lowess") {
		points(lowess(x = predictor, y = outcome), type = "l", lwd = 2, col = linecol)	
	}
	if (line == "straight") {
		ln = lm(outcome ~ predictor)
		beta = signif(coef(ln)[[2]], 3)		
		abline(ln, lwd = 2, col = linecol)
		legend("topleft", legend = bquote(beta == .(beta)), bty = "n", cex = 0.85)
	}

	# throw the variable name right into the middle of the plot
	if (labs) {
		par(usr = c(0, 1, 0, 1))
		text(x = 0.5, y = 0.5, xlabel, col = "gray40")		
	}

}
## End quickscatter

###############################################################################
# 
# Quickpair
#
# An extension of quickscatter that will plot multiple colors and multiple 
# lines for an outcome conditioned on a binary categorical variable
#
# Depends on greygrid
# Colors from the RColorBrewer "PuOr" palette. Sue me.
# 
# Accepts a predictor, outcome, binary factor variable
# 
###############################################################################

quickpair = function(predictor, outcome, strat, line = c("lowess", "straight"), labs = F) {
	
	# predictor = data$gpa
	# outcome = data$uni.gpa
	# strat = data$f_sex
	
	xlabel = deparse(substitute(predictor))
	ylabel = deparse(substitute(outcome))

	pointcol1 = "#F1A340"
	pointcol2 = "#998EC3"
	linecol1 = "#E66101"
	linecol2 = "#5E3C99"

	linelab1 = unique(strat)[1]
	linelab2 = unique(strat)[2]

	greygrid2(x = predictor, y = outcome)
	point.pal = ifelse(strat == levels(strat)[1], pointcol1, pointcol2)
	
	points(outcome ~ predictor, col = point.pal)
	title(xlab = xlabel, ylab = ylabel)
	legend("bottomright", pch = 1, col = c(pointcol1, pointcol2), legend = levels(strat), cex = 0.75, box.col = "gray45")
	
	# stratify predictor & outcome variables
	strat1 = which(strat == linelab1)
	strat2 = which(strat == linelab2)
	
	# title(main = xlabel, line = -2, cex = 0.5)
	if (line == "lowess") {
		points(lowess(x = predictor[strat1], y = outcome[strat1]), type = "l", lwd = 2, col = linecol1)	
		points(lowess(x = predictor[strat2], y = outcome[strat2]), type = "l", lwd = 2, col = linecol2)	
	}

	if (line == "straight") {
		ln1 = lm(outcome[strat1] ~ predictor[strat1])
		beta = signif(coef(ln1)[[2]], 3)		
		abline(ln1, lwd = 2, col = linecol1)
	
		# Legend becomes a hassle
		xleg = min(predictor) + (min(predictor) * 0.01)
		yleg = max(outcome) - (max(outcome) * 0.005)
		
		legend(xleg, yleg, legend = bquote(beta[1] == .(beta)), bty = "n", cex = 0.75, text.col = linecol1)

		ln2 = lm(outcome[strat2] ~ predictor[strat2])
		beta = signif(coef(ln2)[[2]], 3)		
		abline(ln2, lwd = 2, col = linecol2)
		
		# Second legend
		yleg = max(outcome) - (max(outcome) * 0.025)

		legend(xleg, yleg, legend = bquote(beta[2] == .(beta)), bty = "n", cex = 0.75, text.col = linecol2)
	}

	# throw the variable name right into the middle of the plot
	if (labs) {
		par(usr = c(0, 1, 0, 1))
		text(x = 0.5, y = 0.5, xlabel, col = "gray40")		
	}

}
