##blah

library(raster)
library(maptools)
library(RColorBrewer)
library(rgdal)
library(scales)
library(rasterVis)

setwd("~/Leopard Analysis")
r = raster("~/Leopard Analysis/Environmental Variables/Habitat Categorical/Habitat_Classification_Cover_FULL_v2.tif")
varNames = c("Open", "Grass", "Cover", "River", "Roads", 
             "Human", "Glade", "Glade Edge", "Escarpment", "Lugga", "Water")
colvec = colorRampPalette(rev(brewer.pal(8, "Set2")))(length(unique(getValues(r))))
colvec2 = c("sandybrown", "lightgoldenrod", "green4", "darkorchid",
            "black", "red","darkslategray1", "darkslategray4",
            "maroon1", "orange2", "slateblue")
colvec3 = c("sandybrown", "#EFCC6B", "green4", "#66C2A5", "black", 
            "red", "#0CAB76", "#176E43","#CF948B", "#CE9C76", "#66C2A5")
roads = readShapeLines("Workspace/Shapefiles/Roads/roads.shp")
escar = readShapePoly("Workspace/Shapefiles/Escarpment/Escarpment_Draft.shp")
elev = raster("Environmental Variables/Elevation_(derived from  SRTM).tif")
proj4string(escar) <- "+proj=longlat"
escar2 = spTransform(escar, CRS(proj4string(r)))

#luggas = readShapeLines("Workspace/Shapefiles/Lugga/lugga.shp")
par(mfrow=c(1,1))
# plot(r, col=colvec)
# plot(r, col=colvec2)
#load vector data

#ex = drawExtent()
# class       : Extent 
# xmin        : 259542.1 
# xmax        : 273272 
# ymin        : 26249.53 
# ymax        : 45037.86 
ex = extent(c(xmin=258842.1, xmax=270072, ymin=26249.53, ymax=43037.86))

par(mar=c(1,1,1,1)-0.75)
png("Figures/habmap+high+res.png", width = 8.5, height=11, units="in", res=256)
image(crop(r,ex), col=sapply(colvec3,function(x) alpha(x,0.8)), axes=FALSE, xlab="", ylab="", asp=1)
plot(crop(escar2,ex), add=TRUE, col="#CF948B", lwd=0.0001)
plot(crop(roads,ex), add=TRUE, col="black", lwd=2)
  #-Map scale
  len2 = 5000/2 #Prep--------------------------------------------------------
  xcen = 262100
  ycen = 27187
  fontSize = 1.6
  fontFamily = "serif"
  xx <- c(c(xcen-len2,(xcen-len2)+1000), c((xcen-len2)+1000,xcen-len2) ) #for 0 - 1 km
  xx2 <- c(c((xcen-len2)+1000,xcen), c(xcen,(xcen-len2)+1000) )
  xx3 <- c(c(xcen+len2,xcen), c(xcen,xcen+len2))
  yy <- c(ycen-75, ycen-75, ycen+75, ycen+75)#------------------
  polygon(xx, yy, col = "black", border = "white", lwd=1.5) #Main bar (1/3)
  polygon(xx3, yy, col = "black", border = "white", lwd=1.5) #Main bar (3/3)
  polygon(xx2, yy, col = "white", border = "white", lwd=1.5) #Main bar (2/3)
  #segments((xcen-len2)+1000, ycen-10, xcen, ycen+10, lwd=2.4, col="white") #off-center tick
  text((xcen-len2)+1000, ycen-300, "1", cex=fontSize-0.4, col="black", font=1, family=fontFamily) #off-center label
  text(xcen, ycen-300, "2.5", cex=fontSize-0.4, col="black", font=1, family=fontFamily) #Center label
  segments(xcen-len2, ycen-100, xcen-len2, ycen+300, lwd=1.5, col="white", family=fontFamily) #Left tick
  text(xcen-len2, ycen-300, "0", cex=fontSize-0.4, col="black", font=1, family=fontFamily) #Left label
  segments(xcen+len2, ycen-100, xcen+len2, ycen+300, lwd=1.5, col="white", family=fontFamily) #Right tick
  text(xcen+len2, ycen-300, "5", cex=fontSize-0.4, col="black", font=1, family=fontFamily) #Right label
  text(xcen, ycen-600, "kilometers", cex=fontSize-0.2, col="black", font=1, family=fontFamily) #Units label
  # text(xcen, ycen-800, "kilometers", cex=fontSize-0.5, col="white", font=2) #Units label

  #-Legend
  #plot.new()
  #-vector data
 # plot(luggas, add=TRUE, col=colvec3[which(varNames=="Lugga")])
  par(family=fontFamily)
  legend(x=267600 ,y=ycen+3800, legend = varNames[-length(varNames)], cex=fontSize-0.5, 
         pt.cex = fontSize+0.3, ncol=1, x.intersp = 0.65,
         pch=15, col = colvec3, bg = "gray90")
dev.off()
  
rng =range(r[])
# Translate floating point to integer-byte

r[] <- as.integer((r[] / rng[2]) * 256)
r2 = projectRaster(r, crs = CRS("+proj=longlat"), method="ngb")
writeRaster(r2, "~/Leopard Analysis/DemByteImg.tif", overwrite = TRUE) # normalized DEM data

# Create a PNG version of our byte /Tif image
# writeGDAL can not create a PNG, so we use 
# GDAL library routines to ceate, and then save
# as a .png image, a temporary dataset.

DemTif <- GDAL.open("~/Leopard Analysis/DemByteImg.tif")
xx <- copyDataset(DemTif, driver = "PNG")
saveDataset(xx, "Mpala_map.png")
DemPng <- readGDAL("Mpala_map.png") # na proper 'integer' PNG file

# Preprocessing step: create a special SpatialGrid object
# for display in GoogleEarth

DemPngGK <- GE_SpatialGrid(DemPng)

# Generate the KML 'wrapper' for the png file.
# 'GE-compatible' PNG file is now a complex of three files:
# .png, .kml, and .png.aux.xml files.

kmlOverlay(DemPngGK, "HabitatImageOverlay.kml", "Mpala_map.png", name = "Mpala_Habitat_Map")

#
library(plotKML)

r2 = projectRaster(r, crs = CRS("+proj=longlat"), method = "ngb")
KML(r2, "~/Desktop/habmap", col=colvec2, overwrite=TRUE, maxpixels=ncell(r2), blur=10)

r3 = r
r3 = r - 1
plot3D(crop(elev,ex), drape=crop(r,ex), col=colvec3, maxpixels=ncell(r), zfac=0.2)


#PWC

library(raster)
library(maptools)
library(RColorBrewer)
library(rgdal)

setwd("~/Leopard Analysis")
r = raster("~/Leopard Analysis/Environmental Variables/Local Predictors/percent_cover_full.tif")

rng =range(r[])
# Translate floating point to integer-byte

r[] <- as.integer((r[] / rng[2]) * 256)
r2 = projectRaster(r, crs = CRS("+proj=longlat"))
writeRaster(r2, "~/Leopard Analysis/Cover_temp.tif", overwrite = TRUE) # normalized DEM data
# Create a PNG version of our byte /Tif image
# writeGDAL can not create a PNG, so we use
# GDAL library routines to ceate, and then save
# as a .png image, a temporary dataset.

DemTif <- GDAL.open("Cover_temp.tif")
xx <- copyDataset(DemTif, driver = "PNG")
saveDataset(xx, "Mpala_cover_map.png")
DemPng <- readGDAL("Mpala_cover_map.png") # na proper 'integer' PNG file

# Preprocessing step: create a special SpatialGrid object
# for display in GoogleEarth

DemPngGK <- GE_SpatialGrid(DemPng, col=terrain.colors(100))

# Generate the KML 'wrapper' for the png file.
# 'GE-compatible' PNG file is now a complex of three files:
# .png, .kml, and .png.aux.xml files.

kmlOverlay(DemPngGK, "HabitatImageOverlay.kml", "Mpala_cover_map.png", name = "Mpala_Cover_Map")

#
library(plotKML)

r2 = projectRaster(r, crs = CRS("+proj=longlat"), method = "ngb")
KML(r2, "~/Desktop/map.kml", col=rev(topo.colors(100)), overwrite=TRUE, maxpixels=ncell(r2), blur=20)