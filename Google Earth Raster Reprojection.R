#Reproject vegetation, terrain, etc. rasters into Google Earth projection format
#for overlay.
setwd("~/Leopard Analysis/")
load("Workspace/Analysis.R")
library(raster)
library(maptools)
p4sUTM = proj4string(leopards_UTM)
p4sLL = proj4string(leopards)
p4sGE = "+init=epsg:4326"

file = "Environmental Variables/Vegetation Selection/prop.woodycover.020.meters.tif" #raster to reproject
fileName = "MPALA+PC20"
r = raster(file)
pr = projectRaster(r, crs=CRS(p4sGE))

# writeRaster(pr,
#             filename = paste("Environmental Variables/Google Earth/Google+Earth","_",fileName,".img", sep=""), 
#             format="HFA"
#             )

contourLines(pr, levels = c(0.1,0.2,0.3))
sg = as(pr, "SpatialGridDataFrame")
im = as.image.SpatialGridDataFrame(sg)
cl = contourLines(im, levels=c(0.1, 0.2, 0.3))
sldf <- ContourLines2SLDF(cl)
#plot(sldf, add=TRUE)
writeSpatialShape(sldf, 
                  fn = paste("Environmental Variables/Google Earth/Google+Earth","_",fileName,".shp", sep="")
                  )
