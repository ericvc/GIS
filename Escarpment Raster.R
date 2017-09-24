#I will create a separate script for the escarpment raster and shapefile data
#rather than putting the code in the "Leopard Environment Raster Creation.R" file
#because the methods are quite different and time consuming.

library(raster)
library(e1071)
library(maptools)
library(rgdal)

setwd("~/Leopard Analysis/")

Sys.sleep(2) #Give 'rgdal' package time to load.
#Load polygons covering regions for training data
train_polys = readOGR("KML files/Escarpment.kml", layer = "Escarpment")
p4s_ll = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Load environmental data
slope_raster = raster("Environmental Variables/PercentSlope_(derived from  SRTM).tif")
tpi_raster = raster("Environmental Variables/TPI_(derived from  SRTM).tif")
veg_raster = raster("Environmental Variables/Habitat Type/ProportionWoodyCover_309x307.tif")
veg_raster2 = crop(projectRaster(veg_raster, tpi_raster), y = tpi_raster)
grs_raster = raster("Environmental Variables/Habitat Type/ProportionGrassCover_309x307.tif")
grs_raster2 = crop(projectRaster(veg_raster, tpi_raster), y = tpi_raster)
gis_data = stack(slope_raster, tpi_raster)#, veg_raster2, grs_raster2)

trainData = extract(gis_data, train_polys)
trainData2 = do.call("rbind", trainData)
escarpment = rep(c(1,1,1,1,0,0,0), times=sapply(trainData,nrow))
svmModel = svm(x=trainData2, y=as.factor(escarpment))

fullData = getValues(gis_data)
modelPreds = predict(svmModel, fullData)
cells = as.numeric(names(modelPreds))
vals = as.numeric(modelPreds)
r = tpi_raster
r[] = NA; names(r) = "Data"
r[][cells] = (vals-1)
plot(r)

#The algorithm looks like it correctly classified the escarpment, but there are many
#small areas that need to be 'filtered' out. It will be easier to convert these to polygons
#and then work with them
rcont = rasterToContour(r, nlevels=1)
plot(rcont)

library(PBSmapping)

rpolyset = SpatialLines2PolySet(rcont)
rpoly = PolySet2SpatialPolygons(rpolyset)

polys = unlist(lapply(rpoly@polygons, function(X) slot(X,"Polygons") ) )
poly_size = sapply(polys, function(Z) slot(Z,"area"))
hist(poly_size, breaks=100)
poly_true = polys
poly_list = list()
for(p in 1:length(poly_true)){
  poly_list[[p]] = Polygons(list(Polygon(coords = poly_true[[p]]@coords)), 
                            paste("Escarpment_",p,sep="")
                            )
}

sppolys = SpatialPolygons(poly_list, 
                          proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
                          )
plot(sppolys) #Looks good! (October 16th, 2015). Only minor editing required!
polyIDs = sapply(sppolys@polygons, function(X) slot(X, "ID"))
spdf_data = data.frame(polyIDs, row.names=polyIDs)
spdf = SpatialPolygonsDataFrame(sppolys, data = spdf_data)
writeOGR(spdf, dsn = "Workspace/Shapefiles/Escarpment/Escarpment_Draft.shp", overwrite_layer = TRUE,
         layer = "Escarpment", driver="ESRI Shapefile")

#Read in EDITED escarpment file (.kml) from Google Earth work.

esc_kml = readOGR("KML files/Escarpment_EDITED.kml", layer="Escarpment_EDITED")
proj4string(esc_kml) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
esc_reproj = spTransform(esc_kml, 
                         CRS("+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
                         )
writeOGR(esc_reproj, dsn = "Workspace/Shapefiles/Escarpment/", 
         layer="EscarpmentFINAL",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

