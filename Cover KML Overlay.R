##blah

library(raster)
library(maptools)
library(RColorBrewer)
library(rgdal)

setwd("~/Leopard Analysis")
r = raster("~/Leopard Analysis/Environmental Variables/Habitat Categorical/Habitat_Classification_Cover_FULL.tif")
varNames = c("Open", "Grass", "Cover", "River", "Roads", 
             "Human", "Glade", "GladeEdge", "Escarpment", "Lugga", "Water")
colvec = colorRampPalette(rev(brewer.pal(8, "Set2")))(length(unique(getValues(r))))
colvec2 = c("sandybrown", "lightgoldenrod", "green4", "darkorchid",
            "black", "red","darkslategray1", "darkslategray4",
            "maroon1", "orange2", "slateblue")
colvec3 = c("sandybrown", "#EFCC6B", "green4", "#66C2A5", "#7D6027", 
            "red", "#0CAB76", "#176E43","#CF948B", "#CE9C76", "#66C2A5")
par(mfrow=c(1,3))
plot(r, col=colvec)
plot(r, col=colvec2)
plot(r, col=colvec3, axes=FALSE, legend=FALSE, box=FALSE)

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

DemPngGK <- GE_SpatialGrid(DemPng, col=colvec)

# Generate the KML 'wrapper' for the png file.
# 'GE-compatible' PNG file is now a complex of three files:
# .png, .kml, and .png.aux.xml files.

kmlOverlay(DemPngGK, "HabitatImageOverlay.kml", "Mpala_map.png", name = "Mpala_Habitat_Map")

#
library(plotKML)

r2 = projectRaster(r, crs = CRS("+proj=longlat"), method = "ngb")
KML(r2, "~/Desktop/map.kml", col=colvec3, overwrite=TRUE, maxpixels=ncell(r2), blur=10)


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
KML(r2, "~/Desktop/map.kml", col=rev(topo.colors(100)), overwrite=TRUE, maxpixels=ncell(r2), blur=10)

