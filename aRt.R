set.seed(2181977)


x = rnorm(n = 3, mean = 0.5, sd = 0.25)
y = rnorm(n = 3, mean = 0.5, sd = 0.25)

plot.new()
points(x, y, pch = 19, cex = 10, col = "blue")
pal = tourmaline
random.bubbles = function(bubbles = 3, minsize = 10, maxsize = 50, pal) {

	rand.col = sample(1:length(pal), bubbles, replace = F)
	bubblesize = runif(n = bubbles, min = minsize, max = maxsize)	

	x = rnorm(n = bubbles, mean = 0.5, sd = 0.1)
	y = rnorm(n = bubbles, mean = 0.5, sd = 0.1)

	par(mar = c(0,0,0,0) + 0.2)
	plot.new()
	points(x, y, pch = 19, cex = sort(bubblesize, decreasing = T), col = pal[rand.col])
	
}


# color palettes: www.color-hex.com
# Tourmaline
tourmaline = c("#a8ecdd", "#a7f7b0", "#ad845f", "#ccf8d0", "#988197")

# Forest flowers
ff = c("#f0d6f2", "#cc859b", "#6d9c88", "#1b583e", "#4b0e21")
random.bubbles(pal = ff)

# Robin Sparkles
rs = c("#6a9a62", "#4b74d4", "#c25d74", "#804b64", "#fac280")
random.bubbles(pal = rs)

# Old Clover
oc = c("#abb88e", "#a0a97f", "#838b75", "#968f7e", "#988177")
random.bubbles(pal = oc)

# Pastel Rainbow
pr = c("#ffb3ba", "#ffdfba", "#ffffba", "#baffc9", "#bae1ff")

# Fruity place
fp = c("#daff7d", "#b2ef9b", "#8c86aa", "#81559b", "#7e3f8f")

# Purple Midnight
pm = c("#3d2a3e", "#5c3358", "#694661", "#d3db53", "#d8a035")

# vin
vin = c("#401f3e", "#3f2e56", "#453f78", "#759aab", "#faf2a1")

# Minimal water
water = c("#6cab9d", "#319c9a", "#2b6a83", "#243f78", "#131b47")

# Berry Blast
bb = c("#673c4f", "#7f557d", "#726e97", "#7698b3", "#83b5d1")

# soft rainbow
sr = c("#9ad2cb", "#d3ecb0", "#f7f9be", "#ebd494", "#ae6665")

# the shore
shore = c("#3f7cac", "#95afba", "#bdc4a7", "#d5e1a3", "#e2f89c")

# http://www.color-hex.com/color-palette/6427
turret = c('#6e4474', '#ffb9b9', '#d5deff', '#ade1dd', '#92d7b8')

# http://www.color-hex.com/color-palette/6166
tired = c('#c5f2ba', '#fbacc0', '#df86b3', '#9351b8', '#511883')

# http://www.color-hex.com/color-palette/6438
flatshare = c('#678c9f', '#4db3b3', '#d0a177', '#dfca8b', '#eee4c7')

# http://www.color-hex.com/color-palette/6453
diver = c('#c9f2c2', '#7edfc4', '#24c7bb', '#058590', '#072a5a')

# http://www.color-hex.com/color-palette/6312
bythewindow = c('#7ed4db', '#9dc3cc', '#c9ab85', '#ea9c67', '#f48164')

pal = function(col, border = "light gray", ...) {
    n <- length(col)
    plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
      axes = FALSE, xlab = "", ylab = "", ...)
 rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border) }
pal(turret)

#########################################################################
# Watch it
for (i in 1:20) {
	random.bubbles(bubbles = 3, minsize = 20, maxsize = 50, pal = pal.turret)
	Sys.sleep(0.5)	
}


#########################################################################
# Alpha channels

	x = rnorm(n = bubbles, mean = 0.5, sd = 0.1)
	y = rnorm(n = bubbles, mean = 0.5, sd = 0.1)

	par(mar = c(0,0,0,0) + 0.2)

	col1 = "#83b5d17e"
	col2 = "#673c4f7e"
		
	plot.new()	
	points(0.5, 0.5, pch = 19, cex = 30, col = col2)

	as.hexmode(55)

	plot.new()
	col1 = "#83b5d1"
	col2 = "#673c4f"

	alpha2 = 200
	col2 = paste0(col2, as.hexmode(alpha2))
		
	alpha1 = 50
	col1 = paste0(col1, as.hexmode(alpha1))
	x = 0.5 ; y = 0.5
	off.x = 0.1 ; off.y = 0.2
	points(x - off.x, y - off.y, pch = 19, cex = 30, col = col2)
	points(x + off.x, y + off.y, pch = 19, cex = 30, col = col1)

# plot a bubble in a random spot, then add a little random noise for each
# remaining bubble and plot 

# Also adds a bit of random noise to the starting alpha value for 
# transparency
offset.bubbles = function(bubbles = 3, minsize = 10, maxsize = 50, alpha = 100, a.noise = 25, randomstart = T, pal) {

	# alpha noise
	a.noise.dev = round(rnorm(bubbles, mean = alpha, sd = a.noise))
	constrain = function(x) {
		ifelse(x > 255 | x < 0, "ff", as.character(as.hexmode(x)))	
		}
	# ensure 0 <= alpha <= 255	
	alphas = sapply(a.noise.dev, constrain)
	
	# random colors
	rand.col = sample(1:length(pal), bubbles, replace = T)
	
	# Add alpha channel
	a.pal = unlist(Map(paste0, pal[rand.col], alphas))
	
	# random start point
	if (randomstart) {
		# random normal
		x.start = rnorm(n = 1, mean = 0.5, sd = 0.15) 
		y.start = rnorm(n = 1, mean = 0.5, sd = 0.15)
		
		# random uniform
		# x.start = runif(n = 1, min = 0.2, max = 0.8)
		# y.start = runif(n = 1, min = 0.2, max = 0.8)
		
		# random normal offset
		x.off = rnorm(n = bubbles, mean = 0, sd = 0.15)
		y.off = rnorm(n = bubbles, mean = 0, sd = 0.15)
		
		x = x.start + x.off
		y = y.start + y.off
		
	} else {
		# random normal
		# x = rnorm(n = bubbles, mean = 0.5, sd = 0.1)
		# y = rnorm(n = bubbles, mean = 0.5, sd = 0.1)
		
		# random uniform
		x = runif(n = bubbles, min = 0.05, max = 0.95)
		y = runif(n = bubbles, min = 0.05, max = 0.95)
		
	}
	
	# random bubble size
	bubblesize = runif(n = bubbles, min = minsize, max = maxsize)	

	par(mar = c(0,0,0,0) + 0.2)
	plot.new()

	points(x, y, pch = 19, cex = sort(bubblesize, decreasing = T), col = a.pal)
	
}

for (i in 1:20) {
	offset.bubbles(bubbles = 10, minsize = 10, maxsize = 30, alpha = 200, a.noise = 10, randomstart = F, pal = pal.flatshare)
	Sys.sleep(0.25)	
}

## Offset boxes
offset.boxes = function(bubbles = 3, minsize = 10, maxsize = 50, alpha = 100, a.noise = 25, randomstart = T, pal) {

	# alpha noise
	a.noise.dev = round(rnorm(bubbles, mean = alpha, sd = a.noise))
	constrain = function(x) {
		ifelse(x > 255 | x < 0, "ff", as.character(as.hexmode(x)))	
		}
	# ensure 0 <= alpha <= 255	
	alphas = sapply(a.noise.dev, constrain)
	
	# random colors
	rand.col = sample(1:length(pal), bubbles, replace = T)
	
	# Add alpha channel
	a.pal = unlist(Map(paste0, pal[rand.col], alphas))
	
	# random start point
	if (randomstart) {
		# random normal
		x.start = rnorm(n = 1, mean = 0.5, sd = 0.15) 
		y.start = rnorm(n = 1, mean = 0.5, sd = 0.15)
		
		# random uniform
		# x.start = runif(n = 1, min = 0.2, max = 0.8)
		# y.start = runif(n = 1, min = 0.2, max = 0.8)
		
		# random normal offset
		x.off = rnorm(n = bubbles, mean = 0, sd = 0.15)
		y.off = rnorm(n = bubbles, mean = 0, sd = 0.15)
		
		x = x.start + x.off
		y = y.start + y.off
		
	} else {
		# random normal
		# x = rnorm(n = bubbles, mean = 0.5, sd = 0.1)
		# y = rnorm(n = bubbles, mean = 0.5, sd = 0.1)
		
		# random uniform
		x = runif(n = bubbles, min = 0.05, max = 0.95)
		y = runif(n = bubbles, min = 0.05, max = 0.95)
		
	}
	
	# random bubble size
	bubblesize = runif(n = bubbles, min = minsize, max = maxsize)	

	par(mar = c(0,0,0,0) + 0.2)
	plot.new()

	points(x, y, pch = 15, cex = sort(bubblesize, decreasing = T), col = a.pal)
	
}
## End offset boxes

for (i in 1:10) {
	offset.boxes(bubbles = 17, minsize = 5, maxsize = 30, alpha = 200, a.noise = 10, randomstart = F, pal = pal.vin)
	Sys.sleep(0.5)	
}

for (i in 1:10) {
	offset.bubbles(bubbles = 17, minsize = 5, maxsize = 30, alpha = 200, a.noise = 10, randomstart = T, pal = turret.ramp(10))
	Sys.sleep(0.5)	
}

setwd('~/Documents/aRt')

svg(filename = "shore1.svg", height = 9, width = 9, pointsize = 6)
offset.bubbles(pal = shore, minsize = 70, maxsize = 120, alpha = 200)
dev.off()

# for reproducibility	
seed = 5161969
set.seed(seed)
	
for (i in 1:50) {
	filename = paste0("gen/vin", i, ".svg")
	svg(filename = filename, height = 9, width = 9, pointsize = 6)
	offset.bubbles(bubbles = 3, minsize = 80, maxsize = 150, alpha = 200, a.noise = 10, randomstart = T, pal = vin)
	dev.off()
	# Sys.sleep(0.5)	
}


# blog topper
for (i in 1:50) {
	filename = paste0("gen/blog", i, ".png")
	png(filename = filename, width = 640, height = 80, pointsize = 6, bg = "transparent", type = "cairo")
	offset.bubbles(bubbles = 80, minsize = 5, maxsize = 20, alpha = 200, a.noise = 10, randomstart = F, pal = tourmaline)
	dev.off()
	# Sys.sleep(0.5)	
}
