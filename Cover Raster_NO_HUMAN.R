#I will create a separate script for the "Cover" raster creation because the methods
#are unique.
#This raster will not include HUMANS or ROADS as a habitat category because I am attempting
#to discern the underlying patterns of habitat use that vary with proximity to humans.
#and roads.

setwd("~/Leopard Analysis/")

library(e1071)
library(raster)
library(maptools)
library(spatstat)

load("Workspace/Analysis.R");load("Workspace/Analysis2.R");load("~/Monkeys/Monkey Workspace/Monkey.data.Rdata")

#Vegeatation features
#1 = No Data, 2 = Canopy, 3 = Grass 1, 4 = Grass 2, 5 = Bare Ground
hab = raster("Environmental Variables/Habitat Type/QB2011classified_maskedCROPPED.tif")
#Reference raster layer
ref = projectRaster(raster("Environmental Variables/Elevation_(from  SRTM).tif"), 
                   crs = CRS( proj4string(hab) )
)
ref[] = NA; names(ref) = "data"
#
resWanted = res(ref)
nCells = resWanted/res(hab)

ex = sapply(list(leopards_UTM,leopards2_UTM.sub,monkey.move), function(z) as.vector(extent(z) ) )
total.extent = extent( c( min(ex[1,]), max(ex[2,]), min(ex[3,]), max(ex[4,]) ) )
total.extent@xmin = 260616

#Percent woody cover (This code needs maintenance.)
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

#Percent grass cover
#for(b in 1:length(buffers)){
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

#Percent bare ground
#for(b in 1:length(buffers)){
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

# #Roads
# cover2 = cover1
roads <- readShapeLines("Workspace/Shapefiles/Roads/roads.shp")
# roadsPSP <- as.psp(readShapeLines("Workspace/Shapefiles/Roads/roads.shp"))
# cellXY <- as.ppp(xyFromCell(cover2, cell=1:ncell(cover2), spatial=TRUE))
# distances <- nncross(X=cellXY, Y=roadsPSP)
# cover2[which(20>=distances$dist)] = 6
# 
# #Human
# cover3 = cover2
human <- readShapePoly("Workspace/Shapefiles/Humans/humans.shp")
p4s_latlong = CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
proj4string(human) = p4s_latlong
p4s_utm = CRS("+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
human = spTransform(human, p4s_utm)
# cells <- do.call("rbind", raster::extract(x=cover3, y=human, cellnumbers=TRUE) )[,1]
# cover3[cells] = 7
# plot(cover3)

#Glade and Glade Edge
cover4 = cover1
glade <- readShapePoly("Workspace/Shapefiles/Glades/glades.shp")
gladesPPP <- as.ppp(xyFromCell(cover4, cell=cells, spatial=TRUE))
cellXY <- as.ppp(xyFromCell(cover4, cell=1:ncell(cover4), spatial=TRUE))
distances <- nncross(X=cellXY, Y=gladesPPP)
cover4[which(150>=distances$dist)] = 9 #Glade edges
cells <- do.call("rbind", raster::extract(x=cover4, y=glade, cellnumbers=TRUE) )[,1]
cover4[cells] = 8 #Glades

#Escarpment
cover5 = cover4
escarpment = readOGR("Workspace/Shapefiles/Escarpment/EscarpmentFINAL.shp", "EscarpmentFINAL")
cells <- do.call("rbind", raster::extract(x=cover5, y=escarpment, cellnumbers=TRUE) )[,1]
cover5[cells] = 10
plot(cover5)

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
plot(cover7, col=col.vec)

#Koppe
cover8 = cover7
koppe = readOGR("Workspace/Shapefiles/Koppes/koppes.shp", "koppes")
cells <- do.call("rbind", raster::extract(x=cover8, y=koppe, cellnumbers=TRUE) )[,1]
cover8[cells] = 13
plot(cover8)
 
levels = seq(15,35,5)
names(cover8) = paste("Habitat_cover_at_", levels,sep="")
for(layer in 1:nlayers(cover7)){ 
  writeRaster(cover8[[layer]], 
              filename = paste("Environmental Variables/Habitat Categorical/",
                               "Habitat Classification_Cover_NO_HUMANS",levels[layer],".tif",sep=""),
              overwrite=TRUE
  )
}
# 
pdf("~/Desktop/CoverClassification_NO_HUMANS.pdf", 16, height = 22)
plot(cover8[[3]], col=col.vec)
plot(roads, lwd=1.5, col="black", add=TRUE)
plot(human, lwd=1.5, col="red", add=TRUE)
dev.off()

writeOGR(classes, "Workspace/Shapefiles/Vegetation Cover", "HabitatClassification_NO_HUMANS", 
         driver="ESRI Shapefile", overwrite_layer=TRUE)


f = list.files("Environmental Variables/Habitat Categorical/", full.names=TRUE, pattern="NO_HUMANS")
cover7 = stack(f)
# # 1 = No data, 2 = Bare Ground, 3 = Grass, 4 = Cover, 5 = River, 6 = Roads, 7 = Human, 8 = Glade,
# #9 = Glade Edge, 10 = Escarpment, 11 = Lugga, 12 = Water
col.vec = c("white", "sandybrown", "lightgoldenrod", "green4", "darkorchid", 
            "black", "red","darkslategray1", "darkslategray4",
            "maroon1", "orange2", "slateblue")

#Expand coverage of the habitat map to include outlying areas. I will do this using
#Landsat data and machine learning algorithms.
lsatFiles = list.files("~/GIS/Landsat_LC81680602014178LGN00/", pattern = ".TIF", full.names=TRUE)
#Stack vegetation bands (5,4,3)
lsatData = stack(lsatFiles[c(5:7)])
lsatData.repro = projectRaster(lsatData, cover7)
lsatData.cropped = crop(lsatData.repro, extent(cover7))
crds = xyFromCell(cover7, cell = 1:ncell(cover7))
coverClass = extract(cover7[[3]], crds)
trainData = extract(lsatData.cropped, crds)
trainData2 = data.frame(coverClass, trainData)
trainData3 = trainData2[trainData2[,1]==2|trainData2[,1]==3|trainData2[,1]==4,]
svmModel = svm(x=trainData3[,-1], y=as.factor(trainData3[,1]))
save(svmModel, file="Workspace/svm_habitat_model.Rdata")
fullData = getValues(lsatData.cropped)
modelPreds = predict(svmModel, fullData)
cells = as.numeric(names(modelPreds))
vals = as.numeric(modelPreds)
r = cover7[[3]]
r[] = NA; names(r) = "Data"
r[][cells] = vals+1
plot(r)

cover8 = cover7[[3]]
cover8[which(cover8[]==1)] = r[which(cover8[]==1)]
plot(cover8, col=col.vec[-1])

writeRaster(cover8, 
            filename = paste("Environmental Variables/Habitat Categorical/",
                             "Habitat_Classification_Cover_NO_HUMANS",".tif",sep=""),
            overwrite=TRUE
)

# levels = seq(15,35,5)
# names(cover8) = paste("Habitat_cover_at_", levels,sep="")
# for(layer in 1:nlayers(cover8)){ 
#   writeRaster(cover8[[layer]], 
#               filename = paste("Environmental Variables/Habitat Categorical/",
#                                "Habitat_Classification_Cover_NO_HUMANS",levels[layer],".tif",sep=""),
#               overwrite=TRUE
#   )
# }


coverX = cover8
coverX[coverX[]!=4] = 0
cover_polys = list()
for(l in 1:nlayers(coverX)) cover_polys[[l]] = rasterToContour(coverX[[l]])
writeOGR(cover_polys[[3]], dsn = "Workspace/Shapefiles/Vegetation Cover/", 
         driver="ESRI Shapefile",
         layer = "Woody_cover_full", overwrite_layer = TRUE)



#Isolate polygons for each variable. These will be used for visuals and GE.
png(filename = "~/Desktop/Habitat_NO_HUMANS.png", width = 12, height = 12, res = 480, units="in")
plot(cover8, col=col.vec[-1])
dev.off()