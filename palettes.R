# Hopefully generic...
palette.plotter = function(palette, border = "grey80") {
	whys = length(palette)
	xleft = rep(0, length(whys))
	xright = rep(1, length(whys))
	yvec = 0:whys / whys
	ybottom = yvec[-length(yvec)]
	ytop = yvec[-1]
	
	plot.new()
	rect(xleft, ybottom, xright, ytop, col = palette, border = border)
	text(x = rep(0.5, whys), y = ybottom + (1/whys/2), labels = as.character(palette), offset = 0, col = "grey30", cex =0.85)	
	title(main = paste0("Palette: ", substitute(palette)))
}


pal = function(col, border = "light gray", ...) {
    n <- length(col)
    plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
      axes = FALSE, xlab = "", ylab = "", ...)
 rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border) }
# pal(pal.turret)


# http://www.color-hex.com/color-palette/6427
pal.turret = c('#6e4474', '#ffb9b9', '#d5deff', '#ade1dd', '#92d7b8')
#palette.plotter(pal.turret)

# http://www.color-hex.com/color-palette/6166
pal.tired = c('#c5f2ba', '#fbacc0', '#df86b3', '#9351b8', '#511883')
# palette.plotter(pal.tired)

# http://www.color-hex.com/color-palette/6438
pal.flatshare = c('#678c9f', '#4db3b3', '#d0a177', '#dfca8b', '#eee4c7')
# palette.plotter(pal.flatshare)

# http://www.color-hex.com/color-palette/6453
pal.diver = c('#c9f2c2', '#7edfc4', '#24c7bb', '#058590', '#072a5a')
# palette.plotter(pal.diver)

# http://www.color-hex.com/color-palette/6312
pal.bythewindow = c('#7ed4db', '#9dc3cc', '#c9ab85', '#ea9c67', '#f48164')
# palette.plotter(pal.bythewindow)

pal.tourmaline = c("#a8ecdd", "#a7f7b0", "#ad845f", "#ccf8d0", "#988197")
# palette.plotter(pal.tourmaline)

# Forest flowers
pal.ff = c("#f0d6f2", "#cc859b", "#6d9c88", "#1b583e", "#4b0e21")
# palette.plotter(pal = pal.ff)

# Robin Sparkles
pal.rs = c("#6a9a62", "#4b74d4", "#c25d74", "#804b64", "#fac280")
# palette.plotter(pal = pal.rs)

# Old Clover
pal.oc = c("#abb88e", "#a0a97f", "#838b75", "#968f7e", "#988177")
# palette.plotter(pal = pal.oc)

# Pastel Rainbow
pal.pr = c("#ffb3ba", "#ffdfba", "#ffffba", "#baffc9", "#bae1ff")
# palette.plotter(pal.pr)

# Fruity place
pal.fp = c("#daff7d", "#b2ef9b", "#8c86aa", "#81559b", "#7e3f8f")
# palette.plotter(pal.fp)

# Purple Midnight
pal.pm = c("#3d2a3e", "#5c3358", "#694661", "#d3db53", "#d8a035")
# palette.plotter(pal.pm)

# vin
pal.vin = c("#401f3e", "#3f2e56", "#453f78", "#759aab", "#faf2a1")
# palette.plotter(pal.vin)

# Minimal water
pal.water = c("#6cab9d", "#319c9a", "#2b6a83", "#243f78", "#131b47")
# palette.plotter(pal.water)

# Berry Blast
pal.bb = c("#673c4f", "#7f557d", "#726e97", "#7698b3", "#83b5d1")
# palette.plotter(pal.bb)

# soft rainbow
pal.sr = c("#9ad2cb", "#d3ecb0", "#f7f9be", "#ebd494", "#ae6665")
# palette.plotter(pal.sr)

# the shore
pal.shore = c("#3f7cac", "#95afba", "#bdc4a7", "#d5e1a3", "#e2f89c")
# palette.plotter(pal.shore)



# Now try color ramps
parch = colorRampPalette(c("cornsilk1", "cornsilk2"), bias = 1, space = "Lab", interpolate = "linear")
parch(20)[1]

ivory = colorRampPalette(c("ivory1", "ivory2"), bias = 1, space = "Lab", interpolate = "linear")

seashell = colorRampPalette(c("seashell1", "seashell2"), bias = 1, space = "Lab", interpolate = "linear")

snow = colorRampPalette(c("snow1", "snow2"), bias = 1, space = "Lab", interpolate = "linear")

wheat = colorRampPalette(c("wheat1", "wheat3"), bias = 1, space = "Lab", interpolate = "linear")

# palette.plotter(seashell(10))

turret.ramp = colorRampPalette(c("#6e4474", "#92d7b8"), bias = 1, space = "Lab", interpolate = "linear")

# palette.plotter(turret.ramp(10))
