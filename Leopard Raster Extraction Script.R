##############################################################################
### Script for pairing PPRP leopard movements with ecological variables.   ###
##############################################################################
#
#-Necessary packages. Ensure these packages are installed before running this
#script. Packages can be installed by running:

#install.packages('NAME')
#Call necessary packages.
library(raster)
library(rethinking) #Not critical, but very helpful
library(chron) #This needs to be loaded to make sure that the time variables
library(move) #are understood by R.

#Set the working directory to the Dropbox folder
setwd('~/Leopard Analysis/')
load('Workspace/Leopards.R') #Loads data in an object called 'leopards'

###=================Define extent of research area around Mpala==============###
# 
# 
  Sys.sleep(1)
  mpalaPoly <- extent(leopards) + 0.05
  Sys.sleep(1)
#
# 
# #This polygon will be used later on to crop the raster data obtained from
# #external sources.
# 
###==========================================================================###
###========Convert SRTM Elevation data to Percent Slope and Aspect===========###
#SRTM (NASA Shuttle Radar Topographic Mission) data v4.1
#Data @ http://www.cgiar-csi.org/data/srtm-90m-digital-elevation-database-v4-1
require(SDMTools)

#Read-in elevation data:
elev <- raster('Environmental Variables/RAW Files/SRTM_1arcSec_30m_global//SRTM_30m_global_laikipia.tif')
#object 'elev' is a raster object with one pixel per 30 m. This information comes
#from files stored locally (in your Dropbox folder).

#Now that the raster data has been created, it is time to trim the rasters
#before saving them.

elev_cropped <- crop(elev, mpalaPoly)

percSlope <- terrain(x=elev_cropped, opt="slope", unit="radians") #Estimates % slope from elevation data

aspect <- terrain(elev_cropped, opt="aspect", unit="radians") #Estimates aspect from elevation data

ruggedness <- terrain(elev_cropped, opt="TRI") #Estimates ruggedness index from elevation data

tpi <- terrain(elev_cropped, opt="TPI")

# NOTE: Interpretation of Aspect
# Aspect describes the direction that a slope faces. Obviously, this can have
# important ecological consequences, especially for local vegetation.

# Aspect values and their corresponding compass directions:
# Flat = -1
# North = (0-22.5) or (337.5-360)
# Northeast = (22.5-67.5)
# Northwest = (292.5-337.5)
# East = (67.5-112.5)
# Southeast = (112.5-157.5)
# South = (157.5-202.5)
# Southwest = (202.5-247.5)
# West = (247.5-292.5)
# 
# NOTE: This type of data is circular (i.e., large values like 340 and small 
# values like 10 are actually very close to one another).

writeRaster(elev_cropped, 
            overwrite=TRUE,
            filename='Environmental Variables/Elevation_(from  SRTM).tif')
writeRaster(percSlope, 
            overwrite=TRUE,
            filename='Environmental Variables/PercentSlope_(derived from  SRTM).tif')
writeRaster(aspect, 
            overwrite=TRUE,
            filename='Environmental Variables/Aspect_(derived from  SRTM).tif')
writeRaster(ruggedness, 
            overwrite=TRUE,
            filename='Environmental Variables/TRI_(derived from  SRTM).tif')
writeRaster(tpi, 
            overwrite=TRUE,
            filename='Environmental Variables/TPI_(derived from  SRTM).tif')


#NOTE: The above code doesn't need to be run any more. Please, use the data
#files created immediately above.

###==========================================================================###
###==============Crop MODIS Vegetation Indices to Save Space=================###
# #NASA MODIS data is available from a number of online sources
# #This data was taken from (http://mrtweb.cr.usgs.gov) and includes data products
# #from the MYD13Q1 data set (Vegetation Indices at 250 m / 16 d resolution).
# #For more information go to (https://lpdaac.usgs.gov/products/modis_products_table/myd13q1)
# 
# #Locate files for EVI and NDVI (both vegetation indices)
# 
# 
# #Scale Factor: 0.0001
# #Fill Value: -3000
# #Valid Range: -2000 to 10000
# 
# #Data for each date is given by the directory in which they are stored.
# #The contents of each directory will be made into a single rasterBrick object.
# dates <- dir('Environmental Variables/RAW LDAAC Files/Vegetation')
# 
# S <- Sys.time()
# for(i in 1:length(dates)){
# 
#   files <- list.files( 
#       paste('Environmental Variables/RAW LDAAC Files/Vegetation/',dates[i],
#             sep=""),
#       full.names=TRUE,
#       recursive=TRUE)
#   
#   fileNames <- list.files( 
#     paste('Environmental Variables/RAW LDAAC Files/Vegetation/',dates[i],
#           sep=""),
#     full.names=FALSE,
#     recursive=TRUE)
#   
#   for(j in 1:length(files) ) {
#     
#     r <- raster(files[j])
#     r_crop <- crop(r, mpalaPoly)
#     
#     outDir <- paste('Environmental Variables/Vegetation/',
#                     dates[i],"/",
#                     sep="")
#     dir.create(outDir, showWarnings=FALSE)
# 
#     
#     writeRaster( r_crop, paste(outDir,fileNames[j], sep=""), 
#                  overwrite=TRUE, format="GTiff")
#     
#   }
#   
#   progbar(i, 1, length(dates), starttime=S, 1)
# }

###==========================================================================###
###================Crop MODIS LAI Raster Data to Save Space==================###
# #NASA MODIS data is available from a number of online sources
# #This data was taken from (http://mrtweb.cr.usgs.gov) and includes data products
# #from the MCD15A2 data set (LAI at 1000 m^2 / 4 d resolution).
# 
#  ##Figure out these values...
# #Scale Factor: 0.0001
# #Fill Value: -3000
# #Valid Range: -2000 to 10000
# 
# #Data for each date is given by the directory in which they are stored.
# #The contents of each directory will be made into a single rasterBrick object.
# dates <- dir('Environmental Variables/RAW LDAAC Files/LAI/')
# 
# S <- Sys.time()
# for(i in 1:length(dates)){
#   
#   files <- list.files( 
#     paste('Environmental Variables/RAW LDAAC Files/LAI/',dates[i],
#           sep=""),
#     full.names=TRUE,
#     recursive=TRUE)
#   
#   fileNames <- list.files( 
#     paste('Environmental Variables/RAW LDAAC Files/LAI/',dates[i],
#           sep=""),
#     full.names=FALSE,
#     recursive=TRUE)
#   
#   for(j in 1:length(files) ) {
#     
#     r <- raster(files[j])
#     r_crop <- crop(r, mpalaPoly)
#     
#     outDir <- paste('Environmental Variables/LAI/',
#                     dates[i],"/",
#                     sep="")
#     dir.create(outDir, showWarnings=FALSE)
#     
#     
#     writeRaster( r_crop, paste(outDir,fileNames[j], sep=""), 
#                  overwrite=TRUE, format="GTiff")
#     
#   }
#   
#   progbar(i, 1, length(dates), starttime=S, 1)
# }


#==========================================================================###
#=============Crop MODIS Landcover Class data to Save Space================###
# lcover <- raster('Environmental Variables/RAW LDAAC Files/Landcover/MCD12Q1.A2005001.h21v08.005.2011085005845_Land_Cover_Type_1.tif')
# lcover_cropped <- crop(lcover, mpalaPoly) #Crop the extent of the raster.
# 
# #Write to a file to store locally.
# writeRaster(lcover_cropped, 
#             overwrite=TRUE,
#             filename='Environmental Variables/Land_Cover_Type_(MCD12Q1).tif')

#============================================================================###
#==========Crop MODIS Vegetation Cont Fields data to Save Space==============###
# files <- list.files("Environmental Variables/RAW LDAAC Files/VCF/", 
#                     pattern=".tif", full.names=TRUE)
# vcf <- stack(files)
# vcf_cropped <- crop(vcf, mpalaPoly) #Crop the extent of the raster.
# 
# #Write to a file to store locally.
# writeRaster(vcf_cropped, 
#             format="raster",
#             overwrite=TRUE,
#             filename='Environmental Variables/VCF/Vegetaion_ContinuousFields.grd')


###==========================================================================###
###================Add Columns for Basic Movement Measures===================###

##First, check to make sure everything looks in order.
#Time lag between points should be about 15 minutes.
lag <- time.lag(leopards, "mins") 
lagTimes <- list()
for(i in 1:length(lag)) lagTimes[[i]] <- c(NA, lag[[i]])
lag.mins <- do.call("c", lagTimes)

range(lag.mins, na.rm=TRUE) #There are some crazy lag times. Let's check them out.
#They appear to be legitimate data points. Still, I should make a new column
#as a flag for these long breaks.
idChange <- c()
for(i in 1:(nrow(leopards@data)-1) ) idChange[i] <- ifelse(leopards@data$Name[i]==leopards@data$Name[i+1],0,1)
brkPnts <- sort(c(which(lag.mins>1440)-1, which(idChange==1), nrow(leopards@data))) #Greater than 24 breaks.
nPnts <- length(brkPnts)
reps <- c(brkPnts[1],diff(brkPnts))
leopards@data$TrackID <- leopards_UTM@data$TrackID <- as.factor(rep(1:nPnts, times=reps))

#The data will now have a column with a factor for each separate track.

#Add the lag.mins vector to the data as well.
leopards@data$lag.mins <- lag.mins


##-We can now calculate distance between successive locations.
leg.m <- move::distance(leopards_UTM) #Using the UTM projection so that units are in meters.

#As above with lag times...
legDists <- list()
for(i in 1:length(leg.m)) legDists[[i]] <- c(NA, leg.m[[i]])
leg.meters <- do.call("c", legDists)
#Convenient!

#Add leg distance to the data.
leopards@data$step <- leopards_UTM@data$step <- leg.meters

#Remove time lag and step distances from rows where "Breaks" occurred
#rm.data <- brkPnts+1

#leopards@data[rm.data,"step"]  <- NA
#leopards@data[rm.data,"lag.mins"]  <- NA
#leopards_UTM@data[rm.data,"step"]  <- NA
#leopards_UTM@data[rm.data,"lag.mins"]  <- NA

###==========================================================================###
###===================Add Columns for Phase of the Moon======================###
#install.packages("oce") #Package neeeded for moon calculations.
library(oce)

lat <- leopards@data$y 
lon <- leopards@data$x
t. <-  as.POSIXct(leopards@timestamps, tz ="Africa/Nairobi")

moon <- moonAngle(t., lon, lat)
names(moon) #Variables of interest: altitude, diameter, azimuth, illuminatedFraction

leopards@data$lunarAz <- moon$azimuth
leopards@data$lunarAlt <- moon$altitude
leopards@data$lunarDiam <- moon$diameter
leopards@data$lunarIllumFrac <- moon$illuminatedFraction

#Add column for night/dark phases.


###==========================================================================###
###====================Add Columns for Solar Variables=======================###
library(maptools)

lat <- leopards@data$y 
lon <- leopards@data$x
t. <-  as.POSIXct(leopards@timestamps, tz="UTC")

sun <- solarpos(cbind(lat,lon), dateTime=t.)
names(sun) <- c("SolarAzimuth", "SolarElevation")

leopards@data$SolarAzimuth <- sun[,1]
leopards@data$SolarElevation <- sun[,2]

# ###==========================================================================###
# ###================Extract environmental data from raster files==============###
# 
# #Read-in the cropped GeoTiff files.
# PerCentSlope <- raster('Environmental Variables/PercentSlope_(derived from  SRTM).tif')
# Aspect <- raster('Environmental Variables/Aspect_(derived from  SRTM).tif')
# Elevation <- raster('Environmental Variables/Elevation_(from  SRTM).tif')
# Ruggedness <- raster('Environmental Variables/TRI_(derived from  SRTM).tif')
# VCF <- stack("Environmental Variables/VCF/Vegetaion_ContinuousFields.grd")
# #Landcover <- raster('Environmental Variables/Land_Cover_Type_(MCD12Q1).tif')
# 
# 
# extractCoords <- leopards@data[,c("x", "y")]
# #Elevation
# leopards@data$Elevation <- raster::extract(x=Elevation, y=extractCoords)
# #%Slope values for each animal location.
# leopards@data$Slope <- PS <- raster::extract(x=PerCentSlope, y=extractCoords) 
# #The degree of the slope, rather than %slope, can be derived from 'PS'.
# leopards@data$DegreeSlope <- atan( (PS/100) ) * (180/pi)
# #%Slope values for each animal location.
# leopards@data$Aspect <- raster::extract(x=Aspect, y=extractCoords)
# leopards@data$VCF <- raster::extract(x=VCF, y=extractCoords)[,3]
# 
# #Extract VI data for by date. This is a little bit more complicated.
# binDates <- dir('Environmental Variables/Vegetation/')#Date bins under consideration
# 
# #Custom function 'binIt'
# leopards@data$viBin <- binIt(leopards@data$Julian, bins=binDates)
# 
# VI <- list() #Create list object to store VI values
# for(i in 1:length(binDates)){
#   
#   
#   d. <- leopards@data[,c("x", "y")][ leopards@data$viBin == binDates[i], ]
#   f. <- list.files(paste('Environmental Variables/Vegetation/',binDates[i], sep=""), 
#                    full.names=TRUE, 
#                    recursive=TRUE)
#   rs <- stack(f.)
#   names(rs) <- c("EVI", "NDVI", "Pixel_Reliability", "VI_Quality")
#   VI[[i]] <- raster::extract(rs , d.)
#   
# }
# #Make the list into a data.frame object.
# VId <- data.frame(do.call("rbind", VI))
# #Add vegetation data to the main data.frame.
# leopards@data[,(ncol(leopards@data)+1):(ncol(leopards@data)+ncol(VId))] <- VId
# #Multiply EVI and NDVI columns by their appropriate scale factor (0.0001)
# leopards@data[,c("EVI","NDVI")] <- apply(leopards@data[,c("EVI","NDVI")], c(1,2), 
#                                          function(z) z*0.0001)
# #Done with vegetation indices.
# 
# #Extract LAI data for by date.
# binDates <- dir('Environmental Variables/LAI/')#Date bins under consideration
# 
# #Custom function 'binIt'
# leopards@data$laiBin <- binIt(leopards@data$Julian, bins=binDates)
# 
# LAI <- list() #Create list object to store VI values
# for(i in 1:length(binDates)){
#   
# 
#   d. <- leopards@data[,c("x", "y")][ leopards@data$laiBin == binDates[i], ]
#   f. <- list.files(paste('Environmental Variables/LAI/',binDates[i], sep=""), 
#                    full.names=TRUE, 
#                    recursive=TRUE)
#   rs <- stack(f.)
#   names(rs) <- c("LAI_QC", "LAI", "LAI_StdDev")
#   LAI[[i]] <- raster::extract(rs , d.)
#   
# }
# #Make the list into a data.frame object.
# LAId <- data.frame(do.call("rbind", LAI))
# #Add vegetation data to the main data.frame.
# leopards@data[,(ncol(leopards@data)+1):(ncol(leopards@data)+ncol(LAId))] <- LAId
# #Multiply LAI columns by their appropriate scale factor (0.1)
# leopards@data[,c("LAI")] <- sapply(leopards@data[,c("LAI")], function(z) z*0.1)
# #Done with LAI.

# #Extract landcover classification.
# leopards@data$Land_Cover <- extract(Landcover, extractCoords)
# 

# #All variables have been extracted.

###==========================================================================###
###====================Acquire Maps of the local area========================###
# library(ggmap)
# 
# #Hybrid map
# gmap <- get_map(location=cbind(mean(leopards@data$x), 
#                                mean(leopards@data$y)), 
#                 maptype="terrain", 
#                 zoom=12)
# 
# #Convert map to a less specialized raster format.
# #(http://rstudio-pubs-static.s3.amazonaws.com/16660_7d1ab1b355344578bbacb0747fd485c8.html)
# 
# mgmap <- as.matrix(gmap)
# vgmap <- as.vector(mgmap)
# vgmaprgb <- col2rgb(vgmap)
# gmapr <- matrix(vgmaprgb[1, ], ncol = ncol(mgmap), nrow = nrow(mgmap))
# gmapg <- matrix(vgmaprgb[2, ], ncol = ncol(mgmap), nrow = nrow(mgmap))
# gmapb <- matrix(vgmaprgb[3, ], ncol = ncol(mgmap), nrow = nrow(mgmap))
# rgmaprgb <- brick(raster(gmapr), raster(gmapg), raster(gmapb))
# rm(gmapr, gmapg, gmapb)
# 
# rgmaprgbGM <- rgmaprgb
# 
# projection(rgmaprgbGM) <- CRS("+init=epsg:3857")
# unlist(attr(gmap, which = "bb"))[c(2, 4, 1, 3)]
# rprobextSpDF <- as(extent(unlist(attr(gmap, which = "bb"))[c(2, 4, 1, 3)]), 
#                    "SpatialPolygons")
# projection(rprobextSpDF) <- CRS("+init=epsg:4326")
# rprobextGM <- spTransform(rprobextSpDF, CRS("+init=epsg:3857"))
# rprobextGM@bbox
# extent(rgmaprgbGM) <- c(rprobextGM@bbox[1, ], rprobextGM@bbox[2, ])
# 
# #rgmaprgbGM
# 
# #Now, project into UTM.
# newproj <- proj4string(leopards_UTM)
# new_r <- projectRaster(from=rgmaprgbGM,crs=newproj)#This may take several moments.
# plotRGB(new_r)
# 
# writeRaster(new_r, file = "Maps/Basemap_terrain_zoom12.grd", format = "raster", overwrite = TRUE, 
#             datatype = "INT1U")
# 
#

###==========================================================================###
###============================Wind everything down==========================###
data <- leopards_UTM@data <- leopards@data #Ensure data frames are identical.

#Remove unnecessary items from work space and then save it.
wrk <- ls()
wrk. <- wrk[wrk!="leopards"&wrk!="leopards_UTM"&wrk!="data"]
rm(list=wrk.)
rm(wrk.) + rm (wrk)

save.image("Workspace/Analysis.R")
######################################
# End of script (thanks for playing) #
######################################