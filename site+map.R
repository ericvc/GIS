####                             Study Site Map                             ####
library(raster)
library(maps)
library(rworldmap)
library(scales) #colour alpha

world = getMap(resolution = "high")
africa = subset(world, world$REGION=="Africa")
kenya = as(getData("GADM", country="KE", level=1),"SpatialLinesDataFrame")
laikipia = getData("GADM", country="KE", level=1)[kenya@data$NAME_1 == "Laikipia",]

pdf("figures/site+map.pdf",5,6, family = "Palatino")
crds = c(36.9, 0.28)
par(mar=c(1,1,1,1)-0.95)
mapcol = alpha("#A8A381",0.75)
bgcol = alpha("#799FB0",0.45)
#darken map background color to highlight laikpia
mapcolRGB = col2rgb(mapcol)
mapcolRGB_dark <- mapcolRGB/1.2
mapcol2 <- rgb(t(mapcolRGB_dark), maxColorValue=255)
plot(africa, xlim=c(34,45), ylim=c(-1.5,1.5), lwd=2, bg=bgcol, col=mapcol)
labels. = c("Kenya","Tanzania","Ethiopia","Somalia","INDIAN OCEAN","EQUATOR")
xs = c(38,35.5,40.25,43,42.5,44.4)
ys = c(1.25,-4.75,6.1,2.3,-3.5,-0.27)
l = length(labels.)
text(x=xs[1:(l-2)], y=ys[1:(l-2)], labels.[1:(l-2)], cex=1.45, font=2)
text(x=xs[l-1], y=ys[l-1], labels.[l-1], cex=1.5, font=3, srt=45)
text(x=xs[l], y=ys[l], labels.[l], cex=0.8, font=3)
box()
plot(laikipia, col=mapcol2, lwd=0.5, add=TRUE)
abline(h=0, lwd=1.5, lty=2) #equator
points(crds[1], crds[2], pch=4, col="black", cex=1.5)
#-Map scale
len2 = (200*(1/111))/2 #Map scale options-------------------------------------------
xcen = 44
ycen = -5.8
fontSize = 1.1
fontFamily = "serif"
xx <- c(c(xcen-len2,(xcen-len2)+.100), c((xcen-len2)+.100,xcen-len2) )
xx2 <- c(c((xcen-len2)+.100,xcen), c(xcen,(xcen-len2)+.100) )
xx3 <- c(c(xcen+len2,xcen), c(xcen,xcen+len2))
yy <- c(ycen, ycen, ycen, ycen)#----------------------------------------------------
segments(x0 = xx, y0 = yy, x1 = xx3, y1=yy, col = "black",lwd=1) #Main bar
segments((xcen-len2), ycen, (xcen-len2), ycen+.10, lwd=1.4, col="black") #off-center tick
segments((xcen), ycen, (xcen), ycen+.10, lwd=1.4, col="black") #left tick
segments((xcen+len2), ycen, (xcen+len2), ycen+.10, lwd=1.4, col="black") #right tick
text((xcen-len2), ycen-.2500, "0", cex=fontSize, col="black", font=1, family=fontFamily) #off-center label
text(xcen, ycen-.2500, "100", cex=fontSize, col="black", font=1, family=fontFamily) #Center label
text(xcen+len2, ycen-.250, "200", cex=fontSize, col="black", font=1, family=fontFamily) #Right label
text(xcen+0.1, ycen-.65, "kilometers", cex=fontSize, col="black", font=2, family=fontFamily) #Units label
dev.off()
