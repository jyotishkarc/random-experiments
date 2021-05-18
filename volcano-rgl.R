z <- 2 * volcano
x <- 10 * (1:nrow(z))
y <- 10 * (1:ncol(z))

zlim <- range(z)
zlen <- zlim[2] - zlim[1] + 1

col <- terrain.colors(zlen)[z-zlim[1]+1]

rgl.open()
rgl.surface(x, y, z, color=col, back="lines")
rgl.bg(color = "white")
