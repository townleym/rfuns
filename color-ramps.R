###

parchment = colorRamp(c("cornsilk1", "cornsilk4"), bias = 1, space = "Lab", interpolate = "linear")
parchment(0:10 / 10)
rgb(parchment(0:10 / 10), max = 255)

parch2 = colorRampPalette(c("cornsilk1", "cornsilk4"), bias = 1, space = "Lab", interpolate = "linear")
parch2(10)[1]

# mismo

### Ok, now for real ###

parch = colorRampPalette(c("cornsilk1", "cornsilk2"), bias = 1, space = "Lab", interpolate = "linear")
parch2(20)[1]

ivory = colorRampPalette(c("ivory1", "ivory2"), bias = 1, space = "Lab", interpolate = "linear")

seashell = colorRampPalette(c("seashell1", "seashell2"), bias = 1, space = "Lab", interpolate = "linear")

snow = colorRampPalette(c("snow1", "snow2"), bias = 1, space = "Lab", interpolate = "linear")

wheat = colorRampPalette(c("wheat1", "wheat3"), bias = 1, space = "Lab", interpolate = "linear")

xrange = c(0,8) ; yrange = c(0,10)

par(bg = "transparent")
plot.new()
plot.window(xlim = xrange, ylim = yrange)

runner = function(ramp, xtnt = 20, y = 0) {
	# colramp = ramp(xtnt)[1:xtnt]
	colramp = ramp[1:xtnt]
	
	for (x in 0:xtnt) {
	rect(x, y, x+1, y+1, col = colramp[x+1], border = colramp[x+1])	
	}
}

runner(parch, y = 0)
runner(ivory, y = 2)
runner(seashell, y = 4)
runner(snow, y = 6)
runner(wheat, y = 8)


xtnt = 20
y = 0
for (x in 0:xtnt) {
	rect(x, y, x+1, y+1, col = parch(xtnt)[x+1], border = parch(xtnt)[x+1])	
}

xtnt = 20
y = 2
for (x in 0:xtnt) {
	rect(x, y, x+1, y+1, col = ivory(xtnt)[x+1], border = ivory(xtnt)[x+1])	
}

# rect(0, 0, 1, 1, col = parch2(10)[1], border = parch2(10)[1])
# rect(1, 0, 2, 1, col = parch2(10)[2], border = parch2(10)[2])
# rect(2, 0, 3, 1, col = parch2(10)[3], border = parch2(10)[3])


# Ok, naganna functionalize this now:

pdf(file = "darkaccent.pdf", width = 8, height = 8)
	xrange = c(0,8) ; yrange = c(0,8)

	xl = 0:3 * 2
	xr = 1:4 * 2
	yb = rep(6, 4)
	yt = rep(8, 4)
			
	par(bg = "transparent")
	plot.new()
	plot.window(xlim = xrange, ylim = yrange)

	# dark	
	dark.pal = brewer.pal(n = 8, name = "Dark2")
	pal = dark.pal
	
	rect(xl, yb, xr, yt, col = pal[1:4], border = "transparent")
	text(x = xl+1, y = yb + 1, labels = pal[1:4])
		
	yb = yb - 2
	yt = yt - 2

	rect(xl, yb, xr, yt, col = pal[5:8], border = "transparent")
	text(x = xl+1, y = yb + 1, labels = pal[5:8])

	# dark	
	accent.pal = brewer.pal(n = 8, name = "Accent")
	pal = accent.pal
	
	yb = yb - 2
	yt = yt - 2

	rect(xl, yb, xr, yt, col = pal[1:4], border = "transparent")
	text(x = xl+1, y = yb + 1, labels = pal[1:4])
		
	yb = yb - 2
	yt = yt - 2

	rect(xl, yb, xr, yt, col = pal[5:8], border = "transparent")
	text(x = xl+1, y = yb + 1, labels = pal[5:8])

dev.off()

color.runner = function(ramp) {
	
	xrange = c(0,8) ; yrange = c(0,8)

	xl = 0:3 * 2
	xr = 1:4 * 2
	yb = rep(6, 4)
	yt = rep(8, 4)
			
	par(bg = "transparent")
	plot.new()
	plot.window(xlim = xrange, ylim = yrange)
	
	# colramp = ramp(xtnt)[1:xtnt]
	colramp = ramp[1:xtnt]
	dark.pal = brewer.pal(n = 8, name = "Dark2")
	cols = dark.pal[1:4]
	
	rect(xl, yb, xr, yt, col = dark.pal[1:4], border = "transparent")
	
	for (x in 0:xtnt) {
	rect(x, y, x+1, y+1, col = colramp[x+1], border = colramp[x+1])	
	}
}
