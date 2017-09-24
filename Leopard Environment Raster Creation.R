#------------------(1) Load Packages and Data into Workspace--------------------

library(rethinking)
library(adehabitatHR)
library(adehabitatLT)
library(chron)
library(raster)
require(sp)
require(maptools)
library(rgdal)
library(spatstat) #for distance calculations
#library(rgeos)
#require(SDMTools)

#Set the working directory to the Google Drive folder
setwd('~/Leopard Analysis/')
load('Workspace/Analysis.R') #Loads data in an object called 'leopards'
load('Workspace/Analysis2.R') #Loads data in an object called 'leopards2'
load('~/Monkeys/Monkey Workspace/Monkey.data.Rdata') #Loads data in an object called 'monkey.move'
data = leopards_UTM@data

p4s <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") #Used to project raster files.
p4s_ll <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") #Used to project raster files.
# ex = sapply(list(leopards_UTM,leopards2_UTM.sub,monkey.move), function(z) as.vector(extent(z) ) )
# ex = sapply(list(leopards,leopards2.sub, spTransform(monkey.move, CRSobj = p4s_ll)), function(z) as.vector(extent(z) ) )
# total.extent = extent( c( min(ex[1,]), max(ex[2,]), min(ex[3,]), max(ex[4,]) ) )
ref = projectRaster(raster("Environmental Variables/Habitat Categorical/Habitat_Classification_Cover_FULL_w_Koppe.tif"), 
                    crs = p4s_ll)
total.extent = extent(ref)
##Physical features
#Source: http://www.cgiar-csi.org/data/srtm-90m-digital-elevation-database-v4-1
dem = raster("Environmental Variables/RAW Files/SRTM_1arcSec_30m_global/SRTM_30m_global_laikipia.tif")
dem2 = crop(dem, total.extent)
#Create aspect, elevation, slope (%), TPI, and TRI rasters.
elev = projectRaster(dem2, crs = p4s)
aspect = terrain(elev, opt = "aspect")
slope = terrain(elev, opt = "slope")
tpi = terrain(elev, opt = "TPI")
tri = terrain(elev, opt = "TRI")

physVar <- stack(elev, slope, tpi, tri, aspect)
#Transform variables to UTM coordinate system.
names(physVar) <- c("Elevation","Slope","TPI","Ruggedness", "Aspect")
plot(physVar, asp=1)

for(i in 1:nlayers(physVar)){ 
  writeRaster(physVar[[i]], 
              overwrite=TRUE,
              paste("Environmental Variables/",names(physVar)[i],"_(derived from  SRTM).tif", sep=""
              )
  )
}

#---------------(2) Estimate UD, RD, and ID from the leopard data---------------
dat = data[,c("utm.easting","utm.northing","local.time","Name","state")]
dat2 = leopards2_UTM.sub@data[,c("Easting","Northing","local.time","individual.local.identifier")]
dat2$state = NA
dat2[,1:2] = leopards2_UTM.sub@coords #Coordinates in data.frame are fucked.
names(dat2) = names(dat)
dat3 = data.frame(rbind(droplevels(dat),droplevels(dat2)))
dat4 = dat3[complete.cases(dat3[,1:4]),]
dat4$Name[dat4$Name=="Chumvi2"] = "Chumvi"
dat4$Name = as.character(dat4$Name)
dat5 = droplevels(dat4[dat4$Name!="Morani",])

leop_tr <- as.ltraj(xy=dat5[,c("utm.easting", "utm.northing")], infolocs = data.frame(state=dat5$state/16),
                    date=dat5$local.time, 
                    id=dat5$Name
)

refdate = strptime("2014-01-01 01:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "Africa/Nairobi")
leop_trNA = setNA(leop_tr, date.ref = refdate, dt = 0.25, units = "hour")
leop_tr0 = sett0(leop_trNA, date.ref = refdate, dt = 0.25, units = "hour")

dt <- 32 #Maximum time in minutes between relocations to be used in these analyses.
dd <- 50 #Maximum distance relocation for an to leave it's current location (map units, meters).

#Below, we will estimate the diffusion coefficient D for the biased random bridge.
D_est <- BRB.D(leop_tr0, Tmax=(dt*60), Lmin=dd) #Estimate diffusion coefficient
#save.image("id_est.Rdata")
D_est

#grid on which to estimate densities
grd = SpatialPixels(SpatialPoints(raster("Environmental Variables/Habitat Categorical/Habitat_Classification_Cover_FULL.tif")))

Start <- Sys.time()
ud <- BRB(leop_tr, D=D_est, Tmax=dt*60, Lmin=dd, type="UD", grid = grd,
          hmin=50, 
          radius = 75, maxt=dt*60)
End <- Sys.time()
difftime(End,Start, units="hours")
save(ud, file="Workspace/utilization_distribution_ver2.Rdata")

## ID and RD calculations will take a long time.
Start <- Sys.time()
#Calculate intensity distribution
id <- BRB(leop_tr, D=D_est, Tmax=dt*60, Lmin=dd, type="ID", grid = grd,
          hmin=50, 
          radius = 75, maxt=dt*60)
save(id, file="Workspace/intensity_distribution_ver2.Rdata")

#Calculate recursion distribution
rd <- BRB(leop_tr, D=D_est, Tmax=dt*60, Lmin=dd, type="RD", grid = grd,
          hmin=50, 
          radius = 75, maxt=dt*60)
save(rd, file="Workspace/recursion_distribution_ver2.Rdata")
End <- Sys.time()
difftime(End, Start, units="hours")


# #---------------(3) Create rasters for environmental variables  ----------------
# 
# #Vegeatation features
# #1 = No Data, 2 = Canopy, 3 = Grass 1, 4 = Grass 2, 5 = Bare Ground
# # habitat <- raster("Environmental Variables/RAW Files/QB2011classified.img")
# # hab <- crop(habitat, extent(leopards_UTM)+1e4) #Crop to reduce memory load.
# # rm(habitat) #Remove from workspace/memory.
# # writeRaster(hab, filename="Environmental Variables/Habitat Type/QB2011classified_maskedCROPPED.tif", format="GTiff")
# hab <- raster("Environmental Variables/Habitat Type/QB2011classified_maskedCROPPED.tif")


# #buffers <- c(30,60,100)
# for(b in 1:length(buffers)){
  
  # pc.fun <- function(x,...) length(x[x==2])/length(x)
  # b. <- buffers[b]  
  # nCells <- round(b./res(hab)[1])
  # agg <- aggregate(hab, fact=nCells, fun=pc.fun)
  # fileDir <- "Environmental Variables/Habitat Type/"; fileName <- paste("woodyCover",b.,".tif",sep="")
  # fileName <- paste(fileDir,fileName, sep="")
  # writeRaster(agg, file=fileName, format="GTiff", overwrite=TRUE)
  
# }

# #buffers <- c(30,60,100)

# #Edge density
# S <- Sys.time()
# for(b in 1:length(buffers)){
  
  # class.stat <- function(x,...) {
    
    # dim. <- sqrt(length(x))
    # mat. <- matrix(data=x, nrow=dim., ncol=dim., byrow=TRUE)
    # cs <- ClassStat(mat=mat., cellsize=0.6, latlon=FALSE)
    # stat <- cs[cs$class==2,"edge.density"]
    # ifelse(length(stat)==0, NA, stat)
  # }
  
  # b. <- buffers[b]  
  # nCells <- round(b./res(hab)[1])
  # agg <- aggregate(hab, fact=nCells, fun=class.stat)
  # fileDir <- "Environmental Variables/Habitat Type/"; fileName <- paste("edgeDensity",b.,".tif",sep="")
  # fileName <- paste(fileDir,fileName, sep="")
  # writeRaster(agg, file=fileName, format="GTiff", overwrite=TRUE)
  
# progbar(b, 1, length(buffers), starttime=S, update.interval=1)
# }

# #Percent woody cover
# for(b in 1:length(buffers)){
  # class.stat <- function(x,...) {
    
    # dim. <- sqrt(length(x))
    # mat. <- matrix(data=x, nrow=dim., ncol=dim., byrow=TRUE)
    # cs <- ClassStat(mat=mat., cellsize=0.6, latlon=FALSE)
    # stat <- cs[cs$class==2,"prop.landscape"]
    # ifelse(length(stat)==0, NA, stat)
    
  # }
  # b. <- buffers[b]  
  # nCells <- round(b./res(hab)[1])
  # agg <- aggregate(hab, fact=nCells, fun=class.stat)
  # fileDir <- "Environmental Variables/Habitat Type/"; fileName <- paste("pcWoodyCover",b.,".tif",sep="")
  # fileName <- paste(fileDir,fileName, sep="")
  # writeRaster(agg, file=fileName, format="GTiff", overwrite=TRUE)
# }

# #Landscape shape index.
# for(b in 1:length(buffers)){
  
  # class.stat <- function(x,...) {
    
    # dim. <- sqrt(length(x))
    # mat. <- matrix(data=x, nrow=dim., ncol=dim., byrow=TRUE)
    # cs <- ClassStat(mat=mat., cellsize=0.6, latlon=FALSE)
    # stat <- cs[cs$class==2,"landscape.shape.index"]
    # ifelse(length(stat)==0, NA, stat) 
  # }
  
  # b. <- buffers[b]  
  # nCells <- round(b./res(hab)[1])
  # agg <- aggregate(hab, fact=nCells, fun=class.stat)
  # fileDir <- "Environmental Variables/Habitat Type/"; fileName <- paste("shapeIndex",b.,".tif",sep="")
  # fileName <- paste(fileDir,fileName, sep="")
  # writeRaster(agg, file=fileName, format="GTiff", overwrite=TRUE)

# }

##Human landscape featurs (River, Roads, and Buildings)

#All features are to be combined into a single object (e.g., 'roads')

#-River
#Read in river locations obtained from Google Earth & convert to UTM.
riverShp = spTransform(readOGR("KML files/River/River.kml", "River"), p4s)
writeLinesShape(riverShp, fn="Workspace/Shapefiles/River/river.shp")
showWKT("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84",file="Workspace/Shapefiles/River/river.prj")
#Distance to River raster object.
riverPSP <- as.psp(readShapeLines("Workspace/Shapefiles/River/river.shp"))
cellXY <- as.ppp(xyFromCell(physVar[[1]], cell=1:ncell(physVar[[1]]), spatial=TRUE))
RiverDist <- raster(physVar[[1]])
RiverDist[] <- 0; names(RiverDist) <- ("RiverDist")
distances <- nncross(X=cellXY, Y=riverPSP)
values(RiverDist) <- distances$dist

#Create raster where 1 = river present and 0 = river absent

River <- raster(ncol=ncol(physVar[[1]]), nrow=nrow(physVar[[1]])) #Create comparable raster
extent(River) <- extent(physVar[[1]]) #Change extent
proj4string(River) <- p4s #Project
River[] <- 0
cells <- do.call("rbind", raster::extract(x=River, y=riverShp, cellnumbers=TRUE) )[,1]
River[cells] <- 1 #Cell where the river is present are set to 1.

#River habitat layer(s)
levels = seq(20, 100, by=10)
layers = list()
for(l in 1:length(levels)){
  layers[[l]] = RiverDist
  layers[[l]][] = 0
  layers[[l]][][levels[l]>=RiverDist[]] = 1
} 
layersStacked = brick(layers)
names(layersStacked) = paste("River", levels, sep="_")
plot(layersStacked)
polys = stackApply(layersStacked, indices = 1:nlayers(layersStacked), fun = rasterToContour)
writeOGR(river_polys, dsn = "Workspace/Shapefiles/River", "RiverHabitat", 
         driver="ESRI Shapefile", overwrite_layer = TRUE)

#-Roads
roadsShp = spTransform(readOGR("KML files/Roads/Roads.kml", "roads"), p4s)
writeLinesShape(roadsShp, fn="Workspace/Shapefiles/Roads/roads.shp")
showWKT("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84",file="Workspace/Shapefiles/Roads/roads.prj")
#Distance to roads raster object.
roadsPSP <- as.psp(readShapeLines("Workspace/Shapefiles/Roads/roads.shp"))
cellXY <- as.ppp(xyFromCell(physVar[[1]], cell=1:ncell(physVar[[1]]), spatial=TRUE))
RoadDist <- raster(physVar[[1]])
                    RoadDist[] <- 0; names(RoadDist) <- ("RoadDist")
                    distances <- nncross(X=cellXY, Y=roadsPSP)
                    values(RoadDist) <- distances$dist
                    
#Create raster where 1 = roads present and 0 = roads absent
Roads <- raster(ncol=ncol(physVar[[1]]), nrow=nrow(physVar[[1]])) #Create comparable raster
extent(Roads) <- extent(physVar[[1]]) #Change extent
proj4string(Roads) <- p4s #Project
Roads[] <- 0
cells <- do.call("rbind", raster::extract(x=Roads, y=roadsShp, cellnumbers=TRUE) )[,1]
Roads[cells] <- 1 #Cell where the river is present are set to 1.

#-Human Areas
human = readOGR("KML files/Humans/Humans.kml", "humans")
proj4string(humans) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
humans = spTransform(human, p4s)
#Distance to humans raster object.
#Create raster where 1 = humans present and 0 = humans absent
Humans <- raster(ncol=ncol(physVar[[1]]), nrow=nrow(physVar[[1]])) #Create comparable raster
extent(Humans) <- extent(physVar[[1]]) #Change extent
proj4string(Humans) <- p4s #Project
Humans[] <- 0
cells <- do.call("rbind", raster::extract(x=Humans, y=humans, cellnumbers=TRUE) )[,1]
Humans[cells] <- 1 #Cell where the humans are present are set to 1.

humansPPP <- as.ppp(xyFromCell(physVar[[1]], cell=cells, spatial=TRUE))
cellXY <- as.ppp(xyFromCell(physVar[[1]], cell=1:ncell(physVar[[1]]), spatial=TRUE))
HumanDist <- raster(physVar[[1]])
HumanDist[] <- 0; names(HumanDist) <- ("HumanDist")
distances <- nncross(X=cellXY, Y=humansPPP)
values(HumanDist) <- distances$dist

writeSpatialShape(humans, fn = "Workspace/Shapefiles/Humans/humans.shp")
showWKT("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84",file="Workspace/Shapefiles/Humans/humans.prj")

#-Lugga
#Read in lugga locations obtained from Google Earth & convert to UTM.
luggaShp = spTransform(readOGR("KML files/Lugga/Lugga.kml", "lugga"), p4s)
writeLinesShape(roadsShp, fn="Workspace/Shapefiles/Lugga/lugga.shp")
showWKT("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84",file="Workspace/Shapefiles/Lugga/lugga.prj")
#Distance to lugga raster object.
luggaPSP <- as.psp(readShapeLines("Workspace/Shapefiles/lugga/lugga.shp"))
cellXY <- as.ppp(xyFromCell(physVar[[1]], cell=1:ncell(physVar[[1]]), spatial=TRUE))
luggaDist <- raster(physVar[[1]])
luggaDist[] <- 0; names(luggaDist) <- ("luggaDist")
distances <- nncross(X=cellXY, Y=luggaPSP)
values(luggaDist) <- distances$dist

#Create raster where 1 = luggas present and 0 = roads absent
Lugga <- raster(ncol=ncol(physVar[[1]]), nrow=nrow(physVar[[1]])) #Create comparable raster
extent(Lugga) <- extent(physVar[[1]]) #Change extent
proj4string(Lugga) <- p4s #Project
Lugga[] <- 0
cells <- do.call("rbind", raster::extract(x=Lugga, y=luggaShp, cellnumbers=TRUE) )[,1]
Lugga[cells] <- 1 #Cell where the river is present are set to 1.

#-Glades
glades = readOGR("KML files/Glade/Glades.kml", "glades")
proj4string(glades) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
glades = spTransform(glades, p4s)
#Distance to humans raster object.

#Create raster where 1 = humans present and 0 = humans absent
Glades <- raster(ncol=ncol(physVar[[1]]), nrow=nrow(physVar[[1]])) #Create comparable raster
extent(Glades) <- extent(physVar[[1]]) #Change extent
proj4string(Glades) <- p4s #Project
Glades[] <- 0
cells <- do.call("rbind", raster::extract(x=Glades, y=glades, cellnumbers=TRUE) )[,1]
Glades[cells] <- 1 #Cell where the humans are present are set to 1.

gladesPPP <- as.ppp(xyFromCell(physVar[[1]], cell=cells, spatial=TRUE))
cellXY <- as.ppp(xyFromCell(physVar[[1]], cell=1:ncell(physVar[[1]]), spatial=TRUE))
GladeDist <- raster(physVar[[1]])
GladeDist[] <- 0; names(HumanDist) <- ("GladeDist")
distances <- nncross(X=cellXY, Y=gladesPPP)
values(GladeDist) <- distances$dist
writeSpatialShape(glades, fn = "Workspace/Shapefiles/Glades/glades")
showWKT("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84",file="Workspace/Shapefiles/Glades/glades.prj")

#-Water
water = readOGR("KML files/Water/Water.kml", "water")
proj4string(water) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
water = spTransform(water, p4s)
#Distance to water raster object.

#Create raster where 1 = humans present and 0 = humans absent
Water <- raster(ncol=ncol(physVar[[1]]), nrow=nrow(physVar[[1]])) #Create comparable raster
extent(Water) <- extent(physVar[[1]]) #Change extent
proj4string(Water) <- p4s #Project
Water[] <- 0
cells <- do.call("rbind", raster::extract(x=Water, y=water, cellnumbers=TRUE) )[,1]
Water[cells] <- 1 #Cell where the humans are present are set to 1.

waterPPP <- as.ppp(xyFromCell(physVar[[1]], cell=cells, spatial=TRUE))
cellXY <- as.ppp(xyFromCell(physVar[[1]], cell=1:ncell(physVar[[1]]), spatial=TRUE))
WaterDist <- raster(physVar[[1]])
WaterDist[] <- 0; names(WaterDist) <- ("WaterDist")
distances <- nncross(X=cellXY, Y=waterPPP)
values(WaterDist) <- distances$dist
writeSpatialShape(water, fn = "Workspace/Shapefiles/Water/water.shp")
showWKT("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84",file="Workspace/Shapefiles/Water/water.prj")

#-Koppes
koppes = readOGR("KML files/Koppes/Koppes.kml", "Koppes")
proj4string(koppes) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
koppes = spTransform(koppes, p4s)
#Distance to koppes raster object.
#Create raster where 1 = koppes present and 0 = koppes absent
Koppes <- raster(ncol=ncol(physVar[[1]]), nrow=nrow(physVar[[1]])) #Create comparable raster
extent(Koppes) <- extent(physVar[[1]]) #Change extent
proj4string(Koppes) <- p4s #Project
Koppes[] <- 0
cells <- do.call("rbind", raster::extract(x=Koppes, y=koppes, cellnumbers=TRUE) )[,1]
Koppes[cells] <- 1 #Cell where the koppes are present are set to 1.

koppesPPP <- as.ppp(xyFromCell(physVar[[1]], cell=cells, spatial=TRUE))
cellXY <- as.ppp(xyFromCell(physVar[[1]], cell=1:ncell(physVar[[1]]), spatial=TRUE))
KoppeDist <- raster(physVar[[1]])
KoppeDist[] <- 0; names(KoppeDist) <- ("KoppeDist")
distances <- nncross(X=cellXY, Y=koppesPPP)
values(KoppeDist) <- distances$dist

writeSpatialShape(koppes, fn = "Workspace/Shapefiles/Koppes/koppes.shp")
showWKT("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84",file="Workspace/Shapefiles/Koppes/koppes.prj")

rastersToSave <- list(River,RiverDist,Roads,RoadDist,Humans,HumanDist,luggaDist, GladeDist, WaterDist)
filePath <- "Environmental Variables/Local Predictors/"
fileNames <- c("River.tif",
               "RiverDist.tif",
               "Roads.tif",
               "RoadDist.tif",
               "Humans.tif",
               "HumanDist.tif",
               "LuggaDist.tif",
               "GladeDist.tif",
               "WaterDist.tif"
               #,"Escarpment.tif",
            	#,"EscarpmentDist.tif"
               )
for(i in 1:length(rastersToSave)) writeRaster(rastersToSave[[i]], overwrite=TRUE,
                                              filename=paste(filePath,fileNames[i], 
                                                             sep="") 
      ) 

#MCP
n = length(leop_tr0)
level = c(50,90,95,99)
ids = as.character(summary(leop_tr0)$id)
for(i in 1:n){
  xy = leop_tr0[[i]][,1:2][complete.cases(leop_tr0[[i]][,1:2]),]
  dir = "Workspace/Shapefiles/MCP Home Ranges/"
  ID = ids[i]
  sp = SpatialPoints(xy, p4s)
  hr = sapply(level, function(X) mcp(xy = sp, percent = X, unin = "m", "km2") )
  for(k in 1:length(hr)) writeOGR(hr[[k]], dsn = dir, layer = paste(ID,"_",level[k],"_","home_range",sep=""), driver="ESRI Shapefile")
}

#Distance rasters (discrete)

#Human Distance
hdst = raster("Environmental Variables/Local Predictors/HumanDist.tif")
hdst2[] = as.numeric(cut(hdst[], seq(0,max(hdst[]),by=30), right = FALSE))*30
