#I will create a separate script for the "Cover" raster creation because the methods
#are unique.

setwd("/Volumes/Lockbox/Leopard Analysis/")

library(e1071)
library(raster)
library(maptools)
library(spatstat)
library(RColorBrewer)

varNames = c("Open", "Grass", "Cover", "River", "Roads", 
             "Human", "Glade", "GladeEdge", "Escarpment", "Lugga", "Water")
colvec = colorRampPalette(rev(brewer.pal(8, "Set2")))(length(varNames))

load("Workspace/Analysis.R");load("Workspace/Analysis2.R");load("/Volumes/Lockbox/Monkeys/Monkey Workspace/Monkey.data.Rdata")

#Vegeatation features
#1 = No Data, 2 = Canopy, 3 = Grass 1, 4 = Grass 2, 5 = Bare Ground
hab = raster("Environmental Variables/Habitat Type/QB2011classified_maskedCROPPED.tif")
#Reference raster layer
ref = projectRaster(raster("Environmental Variables/Elevation_(derived from  SRTM).tif"),
                   crs = CRS( proj4string(hab) )
)

resWanted = res(ref)
nCells = resWanted/res(hab)

ex = sapply(list(leopards_UTM,leopards2_UTM.sub,monkey.move), function(z) as.vector(extent(z) ) )
total.extent = extent( c( min(ex[1,]), max(ex[2,]), min(ex[3,]), max(ex[4,]) ) )
total.extent@xmin = 260616

# #Percent woody cover (This code needs maintenance.)
# #for(b in 1:length(buffers)){
#   class.stat <- function(x,...) {
#     require(SDMTools)
#     dim. <- sqrt(length(x))
#     mat. <- matrix(data=x, nrow=dim., ncol=dim., byrow=TRUE)
#     cs <- ClassStat(mat=mat., cellsize=0.6, latlon=FALSE)
#     stat <- cs[cs$class==2,"prop.landscape"]
#     ifelse(length(stat)==0, NA, stat)
# 
#   }
#   b. <- buffers[b]
#   nCells <- round(b./res(hab))
#   agg <- aggregate(hab, fact=nCells, fun=class.stat)
#   agg_rescale = projectRaster(agg, ref)
#   agg_rescale[agg_rescale<0] = NA
# 
# writeRaster(agg_rescale, filename = "Environmental Variables/Habitat Type/ProportionWoodyCover_309x307.tif")

# ref = raster("Environmental Variables/Habitat Type/ProportionWoodyCover_309x307.tif")
# r2 = ref
# r2[ref>=0.2] = 1; r2[ref<0.2] = 0
# plot(r2, col=cm.colors(2))
# 
# coverPSP <- as.psp(polys)
# cellXY <- as.ppp(xyFromCell(ref, cell=1:ncell(ref), spatial=TRUE))
# CoverDist <- raster(ref)
# CoverDist[] <- 0; names(CoverDist) <- ("CoverDist")
# distances <- nncross(X=cellXY, Y=coverPSP)
# values(CoverDist) <- distances$dist
# 
# #Percent grass cover
# #for(b in 1:length(buffers)){
#   class.stat <- function(x,...) {
#     require(SDMTools)
#     dim. <- sqrt(length(x))
#     mat. <- matrix(data=x, nrow=dim., ncol=dim., byrow=TRUE)
#     cs <- ClassStat(mat=mat., cellsize=0.6, latlon=FALSE)
#     stat <- cs[cs$class==2,"prop.landscape"]
#     ifelse(length(stat)==0, NA, stat)
# 
#   }
#   perc.fun = function(x, ...) length(x[x==3|x==4])/length(x)
#   #b. <- buffers[b]
#   nCells <- round(res(ref)/res(hab))
#   agg <- aggregate(hab, fact=nCells, fun=perc.fun)
#   agg_rescale = projectRaster(agg, ref)
# 
# writeRaster(agg_rescale, filename = "Environmental Variables/Habitat Type/ProportionGrassCover_309x307.tif")
# 
# Percent bare ground
# for(b in 1:length(buffers)){
#   class.stat <- function(x,...) {
#     require(SDMTools)
#     dim. <- sqrt(length(x))
#     mat. <- matrix(data=x, nrow=dim., ncol=dim., byrow=TRUE)
#     cs <- ClassStat(mat=mat., cellsize=0.6, latlon=FALSE)
#     stat <- cs[cs$class==2,"prop.landscape"]
#     ifelse(length(stat)==0, NA, stat)
# 
#   }
# perc.fun = function(x, ...) length(x[x==5])/length(x)
# #b. <- buffers[b]
# nCells <- round(res(ref)/res(hab))
# agg <- aggregate(hab, fact=nCells, fun=perc.fun)
# agg_rescale = projectRaster(agg, ref)
# 
# writeRaster(agg_rescale, filename = "Environmental Variables/Habitat Type/ProportionNoCover_309x307.tif")
# 
# 
#1 = No data, 2 = Bare Ground, 3 = Grass, 4 = Cover, 5 = River, 6 = Roads, 7 = Human, 8 = Glade,
#9 = Glade Edge, 10 = Escarpment, 11 = Lugga, 12 = Water

f = list.files("Environmental Variables/Habitat Type/", pattern="309x307", full.names=TRUE)
plant = stack(f)
plant = extend(plant, total.extent)
vals = getValues(plant)

plant15 = plant[[1]]
plant15[] = 2 #Bare ground
plant15[apply(vals,1,function(x)is.na(mean(x)))==TRUE] = 1 #No data
plant15[vals[,1]>vals[,2]] = 3 #Grass
plant15[vals[,3]>=0.15] = 4 #Woody cover
plot(plant15)

plant20 = plant[[1]]
plant20[] = 2 #Bare ground
plant20[apply(vals,1,function(x)is.na(mean(x)))==TRUE] = 1 #No data
plant20[vals[,1]>vals[,2]] = 3 #Grass
plant20[vals[,3]>=0.20] = 4 #Woody cover
plot(plant20)

plant25 = plant[[1]]
plant25[] = 2 #Bare ground
plant25[apply(vals,1,function(x)is.na(mean(x)))==TRUE] = 1 #No data
plant25[vals[,1]>vals[,2]] = 3 #Grass
plant25[vals[,3]>=0.25] = 4 #Woody cover
plot(plant25)

plant30 = plant[[1]]
plant30[] = 2 #Bare ground
plant30[apply(vals,1,function(x)is.na(mean(x)))==TRUE] = 1 #No data
plant30[vals[,1]>vals[,2]] = 3 #Grass
plant30[vals[,3]>=0.30] = 4 #Woody cover
plot(plant30)

plant35 = plant[[1]]
plant35[] = 2 #Bare ground
plant35[apply(vals,1,function(x)is.na(mean(x)))==TRUE] = 1 #No data
plant35[vals[,1]>vals[,2]] = 3 #Grass
plant35[vals[,3]>=0.35] = 4 #Woody cover
plot(plant35)

plant_ver2 = stack(plant15, plant20, plant25, plant30, plant35)
plant_ver2[] = as.factor(plant_ver2[])
plot(plant_ver2)

# plant_polys = rasterToContour(plant[[3]], levels=c(0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5))
# writeOGR(plant_polys, dsn = "Workspace/Shapefiles/Vegetation Cover/",
#          driver="ESRI Shapefile",
#          layer = "Woody_cover", overwrite_layer = TRUE)

#-Now, add the river, roads, glades, glade edges, human areas, escarpment, etc.
#River
cover1 = plant_ver2
riverPSP <- as.psp(readShapeLines("Workspace/Shapefiles/River/river.shp"))
cellXY <- as.ppp(xyFromCell(cover1, cell=1:ncell(cover1), spatial=TRUE))
distances <- nncross(X=cellXY, Y=riverPSP)
cover1[which(80>=distances$dist)] = 5

#Roads
cover2 = cover1
roadsPSP <- as.psp(readShapeLines("Workspace/Shapefiles/Roads/roads.shp"))
cellXY <- as.ppp(xyFromCell(cover2, cell=1:ncell(cover2), spatial=TRUE))
distances <- nncross(X=cellXY, Y=roadsPSP)
cover2[which(20>=distances$dist)] = 6

#Human
cover3 = cover2
human <- readShapePoly("Workspace/Shapefiles/Humans/humans.shp")
p4s_latlong = CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
proj4string(human) = p4s_latlong
p4s_utm = CRS("+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
human = spTransform(human, p4s_utm)
cells <- do.call("rbind", raster::extract(x=cover3, y=human, cellnumbers=TRUE) )[,1]
cover3[cells] = 7

#Glade and Glade Edge
cover4 = cover3
glade <- readShapePoly("Workspace/Shapefiles/Glades/glades.shp")
gladesPPP <- as.ppp(xyFromCell(cover4, cell=cells, spatial=TRUE))
cellXY <- as.ppp(xyFromCell(cover4, cell=1:ncell(cover4), spatial=TRUE))
distances <- nncross(X=cellXY, Y=gladesPPP)
cover4[which(150>=distances$dist)] = 9 #Glade edges
cells <- do.call("rbind", raster::extract(x=cover4, y=glade, cellnumbers=TRUE) )[,1]
cover4[cells] = 8 #Glades

#Add koppes to habitat map.

train_polys = readOGR("KML files/Escarpment.kml")
p4s_ll = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Load environmental data
elev = raster("Environmental Variables/Elevation_(derived from  SRTM).tif")
tpi_raster = terrain(elev, opt = "tpi")
slope_raster = terrain(elev, opt = "slope")
gis_data = stack(slope_raster, tpi_raster)

trainData = extract(gis_data, train_polys)
trainData2 = data.frame(do.call("rbind", trainData))
names(trainData2) = c("slope","tpi")
trainData2$escarpment = rep(c(1,1,1,1,0,0,0), times=sapply(trainData,nrow))
trainData2$escarpment = factor(trainData2$escarpment)

svmTune2 = tune(svm, escarpment ~ slope + tpi,
                data=trainData2,
                #tunecontrol=tune.control(sampling="fix"),
                ranges = list(gamma=seq(0,2, by=0.2), cost=seq(1.5,2.5,by=0.2))
)
plot(svmTune2)
svmTune2
fullData2 = data.frame(getValues(gis_data))
names(fullData2) = c("slope","tpi")
tunedModel2 = svmTune2$best.model
modelPreds2 = predict(tunedModel2, fullData2)
cells = as.numeric(names(modelPreds2[modelPreds2==1]))
r = gis_data[[1]]
r[] = 0
r[cells] = 1
rproj = projectRaster(r, cover4)
cells2 = which(rproj[]==1)
cover5 = cover4
cover5[cells2] = 10

#Luggas
cover6 = cover5
luggaPSP <- as.psp(readShapeLines("Workspace/Shapefiles/Lugga/lugga.shp"))
cellXY <- as.ppp(xyFromCell(cover6, cell=1:ncell(cover6), spatial=TRUE))
distances <- nncross(X=cellXY, Y=luggaPSP)
cover6[which(15>=distances$dist)] = 11

#Water
cover7 = cover6
water <- readShapePoly("Workspace/Shapefiles/Water/water.shp")
cells <- do.call("rbind", raster::extract(x=cover7, y=water, cellnumbers=TRUE) )
cover7[cells] = 12

# #Koppes
# koppe <- readOGR("KML Files/Koppes/Koppes.kml")
# cells <- do.call("rbind", raster::extract(x=cover7, y=koppe, cellnumbers=TRUE) )
# cover7[cells] = 13

levels = seq(15,35,5)
names(cover7) = paste("Habitat_cover_at_", levels,sep="")
for(layer in 1:nlayers(cover7)){
  writeRaster(cover7[[layer]],
              filename = paste("Environmental Variables/Habitat Categorical/",
                               "Habitat Classification_Cover",levels[layer],".tif",sep=""),
              overwrite=TRUE
  )
}


f = list.files("Environmental Variables/Habitat Categorical/", full.names=TRUE)
cover7 = stack(f[6:10])
# # 1 = No data, 2 = Bare Ground, 3 = Grass, 4 = Cover, 5 = River, 6 = Roads, 7 = Human, 8 = Glade,
# #9 = Glade Edge, 10 = Escarpment, 11 = Lugga, 12 = Water
col.vec = c("white", "sandybrown", "lightgoldenrod", "green4", "darkorchid",
            "black", "red","darkslategray1", "darkslategray4",
            "maroon1", "orange2", "slateblue")

#Expand coverage of the habitat map to include outlying areas. I will do this using
#Landsat data and machine learning algorithms.
lsatFiles = list.files("/Volumes/Shoebox/GIS/Landsat_LC81680602014178LGN00/",
                       pattern = ".TIF", full.names=TRUE)
#Stack LANDSAT 7 bands (5,4,3, and 2)
lsatData = stack(lsatFiles[c(4:7,12)])
lsatData.cropped = crop(lsatData, extent(cover7))
lsatData.repro = projectRaster(lsatData.cropped, cover7, method="ngb")

#see https://landsat.usgs.gov/qualityband for QA band code definitions.
useCell = getValues(lsatData.repro[[5]])
cellVals = getValues(cover7[[4]])
set.seed(138)
trainCell = sample(which(cellVals<5 & cellVals>1 & useCell==20480), 3e3, replace=FALSE)
trainData = data.frame(extract(lsatData.repro, trainCell))[,-5]
names(trainData) = c("B2","B3","B4","B5")
trainData$cover = factor(cellVals[trainCell])
#model tuning uses grid approximation to find optimal input values for SVM model.
svmTune = tune(svm, cover ~ B2 + B3 + B4 + B5,
                data=trainData,
                #tunecontrol=tune.control(sampling="fix"),
                ranges = list(gamma=seq(1.3,1.4,0.01), cost=seq(5,6,by=0.1))
                )
#save(svmTune, file="Workspace/svm_habitat_model_tuned.Rdata") #save to hard disk
plot(svmTune) #examine fit
print(svmTune)
tunedModel = svmTune$best.model #get the best fit model from tune object
fullData = data.frame(getValues(lsatData.repro))
names(fullData) = c("B2","B3","B4","B5","QA")
modelPreds = predict(tunedModel, fullData[,-5])
cells = as.numeric(names(modelPreds))
vals = as.numeric(modelPreds)
r = cover7[[4]]
r[] = NA; names(r) = "Data"
r[cells] = vals+1
#r[trainCell] = 10
plot(r)

cover8 = cover7[[4]]
cover8[which(cover8[]==1)] = r[which(cover8[]==1)]
plot(cover8, col=col.vec[-1])

#write product to hard disk.
writeRaster(cover8,
            filename = paste("Environmental Variables/Habitat Categorical/",
                             "Habitat_Classification_Cover_FULL",".tif",sep=""),
            overwrite=TRUE
)

plot(cover8)

