# install.packages("BBMM")
# install.packages("PBSmapping")
codedir = "/Library/Frameworks/R.framework/Versions/3.1/Resources/library/ByrneDBBMM/"
library(ByrneDBBMM)
library(move)
library(rgdal)
library(rethinking)
library(stringr)

#------------------(1) Load Packages and Data into Workspace--------------------

#Set the working directory to the Google Drive folder
setwd('~/Leopard Analysis/')
load('Workspace/Analysis.R') #Loads data in an object called 'leopards'
data$Name = leopards@trackId
source("Scripts/move.forud2.R")

# #Chumvi UD Steps
# a1 = split(leopards)[[1]]
# a1.proj = spTransform(a1, CRSobj = "+proj=aeqd", center=TRUE) 
# # m1 = brownian.bridge.dyn(a1.proj, raster = 200, location.error = a1$eobs_horizontal_accuracy_estimate, margin = 11, window.size = 45, ext = .3)
# # save(m1, file="Workspace/Chumvi_dbbmm.Rdata")
# load("Workspace/Chumvi_dbbmm.Rdata")
# range.subset = which(m1@DBMvar@interest==TRUE)
# time.step = timeLag(a1.proj)/60
# move.forud2(m1, range.subset, ts=time.step, ras=5, le=12, lev=c(50,95), path="Step UD/Chumvi/", name="Chumvi",
#             crs = "+proj=utm +zone=37 +ellps=WGS84", ID="1")
# 
# 
# #Haraka UD Steps
# a2 = split(leopards)[[2]]
# a2.proj = spTransform(a2, CRSobj = "+proj=aeqd", center=TRUE) 
# # m2 = brownian.bridge.dyn(a2.proj, raster = 200, location.error = 12, margin = 11, window.size = 45, ext = .5)
# # save(m2, file="Workspace/Haraka_dbbmm.Rdata")
# load("Workspace/Haraka_dbbmm.Rdata")
# range.subset = which(m2@DBMvar@interest==TRUE & m2@DBMvar$lag.mins < 180)
# time.step = timeLag(a2.proj)/60
# move.forud2(m2, range.subset[-c(1:21415)], ts=time.step, ras=5, le=12, lev=c(50,95), path="Step UD/Haraka/", name="Haraka",
#             crs = "+proj=utm +zone=37 +ellps=WGS84", ID="1")
# 
# #Konda UD Steps
# #Konda is not estimating right...
# a3 = split(leopards)[[3]]
# a3.proj = spTransform(a3, CRSobj = "+proj=aeqd", center=TRUE) 
# # m3 = brownian.bridge.dyn(a3.proj, raster = 200, location.error = 12, margin = 11, window.size = 45, ext = .3)
# # save(m3, file="Workspace/Konda_dbbmm.Rdata")
# load("Workspace/Konda_dbbmm.Rdata")
# range.subset = which(m3@DBMvar@interest==TRUE & m3@DBMvar$lag.mins < 180)
# time.step = timeLag(a3.proj)/60
# move.forud2(m3, range.subset, ts=time.step, ras=5, le=12, lev=c(50,95), path="Step UD/Konda/", name="Konda",
#             crs = "+proj=utm +zone=37 +ellps=WGS84", ID="1")
# 
# #Tatu UD Steps
# a4 = split(leopards)[[4]]
# a4.proj = spTransform(a4, CRSobj = "+proj=aeqd", center=TRUE) 
# # m4 = brownian.bridge.dyn(a4.proj, raster = 200, location.error = 12, margin = 11, window.size = 45, ext = .3)
# # save(m4, file="Workspace/Tatu_dbbmm.Rdata")
# range.subset = which(m4@DBMvar@interest==TRUE)
# time.step = timeLag(a4.proj)/60
# move.forud2(m4, range.subset, ts=time.step, ras=5, le=12, lev=c(50,95), path="Step UD/Tatu/", name="Tatu",
#             crs = "+proj=utm +zone=37 +ellps=WGS84", ID="1")


#Models
load("Workspace/Chumvi_dbbmm.Rdata")
load("Workspace/Haraka_dbbmm.Rdata")
load("Workspace/Konda_dbbmm.Rdata")
load("Workspace/Tatu_dbbmm.Rdata")
move.var = list(m1,m2,m1,m4)


#Autocorrelation in space use? Do they have intensive shifts in space use over time?

#Create a function to generate a continuous color palette
# rbPal <- colorRampPalette(c("red","blue"))
# base2 = raster("Environmental Variables/Local Predictors/HumanDist.tif")
# base2 = projectRaster(base2, crs = CRS(proj4string(leopards_UTM)))
# e = poly.extract(base2, cbind(m1@DBMvar$utm.easting, m1@DBMvar$utm.northing))
# col.vec = rbPal(10)[as.numeric(cut(m1@DBMvar$odba,breaks = 20))]
# col.vec2 = sapply(col.vec, function(z) col.alpha(z, alpha = 0.35))
# 
# colv = rbPal(10)[as.numeric(cut(m1@DBMvar$odba, breaks = 20))]
# plot(log(m1@DBMvar@means) ~ e, col=colv, pch=19, cex=0.5)
# cor(m1@DBMvar@means, e, use = "complete.obs")

# d = m1@DBMvar@data
# m = m1@DBMvar@means
# o = m1@DBMvar$odba
# lm_list = list()
# for(s in 1:1) lm_list[[s]] = lm(log(m) ~ e, na.action = "na.omit")
# for(s in 1:1) abline(lm_list[[s]], lwd=3, col="black")

# #Create a function to generate a continuous color palette
# rbPal <- colorRampPalette(c('red','blue'))
# #This adds a column of color values
# # based on the y values
# col.vec = rbPal(n = 15)[as.numeric(cut(m4@DBMvar@means,breaks = 15))]
# col.vec2 = sapply(col.vec, function(z) col.alpha(z, alpha = 0.5))
# plot(m4@DBMvar$utm.easting, m4@DBMvar$utm.northing, col=col.vec2, asp=1, pch=19, cex=.35)

load("Workspace/Analysis2.R")


##---------------------Movement Analysis with Raster Data---------------------##
#1) poly.extract from ALL rasters
#2) Match values with GPS observations
#3) Plot temporal activity around environmental features.

#Define CRS to reproject rasters.
crs. = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"

#Create function to assess occurrences where animals is within threshold distance.
stepUD.habitat = function(x, data, 
                          threshold=c(0,25,50,150,200,250), 
                          lvls=c(50,95), 
                          timeLag=1, 
                          dim=c("glade","human","roads","river","lugga")
                          ){
  obj = list()
  #Raw distances
  lev = rev(lvls)
  for(d in 1:length(dim)){
    x. = x[[ dim[d] ]]
    xx = lapply(x., function(z) lapply(z, function(y) weighted.mean(x = y[,1], w = y[,2], na.rm = TRUE) ) )
    l1 = data.frame(matrix(data = unlist(xx), nrow = length(unlist(xx))/length(lev), ncol = length(lev), byrow = TRUE))
    names(l1) = paste("UD",lev,sep="")
    l2 = lapply(threshold, function(g) ifelse(l1<g, 1, 0) )
    names(l2) = paste(threshold, "meters", sep="_")
    obj[[d]] = l2
  }
  names(obj) = dim
  obj[["data"]] = data.frame(Hour = data$Hour, Julian = data$Julian, Jday = as.integer(data$Julian))
  obj
#   #season object
#   jday = as.integer(data$Julian)
#   #jday = as.integer(seq(1,3.99,length.out = 50))
#   l3 = lapply(l2, function(z) apply(z, 2, function(zz) table(jday, zz)[,2]) )
#   for(l in 1:length(l3)){ 
#     
#     if(length(l3[[l]])==0){ 
#   
#       .d = matrix(rep(0, length(lev)*length(unique(jday))), ncol = length(lev) )
#       colnames(.d) = paste("UD",lev,sep="")
#       l3[[l]] = .d
#   
#     }
}

#Custom function for poly.extracting data with polygons.
poly.extract = function(raster, poly,...){
  r1 = crop(raster, extent(poly)+(.05/111)) #50 meter buffer.
  extract(r1, poly,...)
}

#Read-in raster data

# #Distance to humnan-occupied areas (better name?)
# hdst = raster("Environmental Variables/Local Predictors/HumanDist.tif")
# hdst_ll = projectRaster(hdst, crs = CRS(crs.))
# 
# #Distance to roads
# rddst = raster("Environmental Variables/Local Predictors/RoadDist.tif")
# rddst_ll = projectRaster(rddst, crs = CRS(crs.))
# 
# #Distance to river
# rvdst = raster("Environmental Variables/Local Predictors/RiverDist.tif")
# rvdst_ll = projectRaster(rvdst, crs = CRS(crs.))
# 
# #Distance to glades
# gdst = raster("Environmental Variables/Local Predictors/GladeDist.tif")
# gdst_ll = projectRaster(gdst, crs = CRS(crs.))
# 
# #Distance to luggas
# ldst = raster("Environmental Variables/Local Predictors/LuggaDist.tif")
# ldst_ll = projectRaster(ldst, crs = CRS(crs.))

#------------------------Chumvi - temporal habitat selection--------------------
poly = list.files("Step UD/Chumvi/", full.names = TRUE, recursive = TRUE, pattern = "Chumvi")
x = list.files("Step UD/Chumvi/", full.names = FALSE)
n = m1@DBMvar@window.size-m1@DBMvar@margin-1
polyID = as.numeric(unlist(regmatches(x, gregexpr("[[:digit:]]+", x))))
stepFile = c(rep(NA, times=nrow(m1@DBMvar@data)))
stepFile[polyID] = poly
m1@DBMvar@data$stepFile = stepFile
obs = m1@DBMvar@data[round(m1@DBMvar@data$lag.mins)==15 & !is.na(m1@DBMvar@data$stepFile),]
obs$Hour = as.integer(obs$TimeOfDay)
dsn =  obs$stepFile
# chumvi_stepUD = list()
# layerNames = paste("step_",as.numeric(unlist(regmatches(dsn, gregexpr("[[:digit:]]+", dsn)))),sep="")
# for(i in 1:nrow(obs)){
#   chumvi_stepUD[[i]] = readOGR(dsn = dsn[i], layer = layerNames[i])
# }
# save(chumvi_stepUD, file="Workspace/chumvi_stepUD_polyList.Rdata")
write.csv(obs, "Workspace/Chumvi_stepUD_data.csv")
load("Workspace/chumvi_stepUD_polyList.Rdata")

# c_dist_glade = lapply(chumvi_stepUD, function(z) poly.poly.extract(gdst_ll, z, weights=TRUE) )
# c_dist_human = lapply(chumvi_stepUD, function(z) poly.extract(hdst_ll, z, weights=TRUE) )
# c_dist_roads = lapply(chumvi_stepUD, function(z) poly.extract(rddst_ll, z, weights=TRUE) )
# c_dist_river = lapply(chumvi_stepUD, function(z) poly.extract(rvdst_ll, z, weights=TRUE) )
# c_dist_lugga = lapply(chumvi_stepUD, function(z) poly.extract(ldst_ll, z, weights=TRUE) )
# 
# #Combine all distance objects into a single list object.
# c_dist = list(
#               glade = c_dist_glade,
#               human = c_dist_human,
#               roads = c_dist_roads,
#               river = c_dist_river,
#               lugga = c_dist_lugga
#               )
# save(c_dist, file = "Workspace/Step UD/chumvi_distance.Rdata") #output file
load("Workspace/Step UD/chumvi_distance.Rdata")


#Data point to remove because poly was estimated wierdly...
area = unlist(lapply(chumvi_stepUD, function(z) z@polygons[[1]]@area))
# .obs = obs[-c(2748),]
# .c_dist = list()
# for(i in 1:length(c_dist)){
#   x = c_dist[[i]]
#   x[[2748]] <- NULL
#   .c_dist[[i]] = x
# }
# names(.c_dist) = names(c_dist)

#Now, combine all into a single object for "easy" storage
#[[ feature ]] [[ x_meters ]] [,"UDlevel"]
#[["data"]]$
chumvi_ths = stepUD.habitat(c_dist, data = obs)

#------------------------Haraka - temporal habitat selection--------------------
poly = list.files("Step UD/Haraka/", full.names = TRUE, recursive = TRUE, pattern = "Haraka")
x = list.files("Step UD/Haraka/", full.names = FALSE)
n = m2@DBMvar@window.size-m2@DBMvar@margin-1
polyID = as.numeric(unlist(regmatches(x, gregexpr("[[:digit:]]+", x))))
stepFile = c(rep(NA, times=nrow(m2@DBMvar@data)))
stepFile[polyID] = poly
m2@DBMvar@data$stepFile = stepFile
obs = m2@DBMvar@data[round(m2@DBMvar@data$lag.mins)==15 & !is.na(m2@DBMvar@data$stepFile),]
obs$Hour = as.integer(obs$TimeOfDay)
dsn =  obs$stepFile
haraka_stepUD = list()
layerNames = paste("step_",as.numeric(unlist(regmatches(dsn, gregexpr("[[:digit:]]+", dsn)))),sep="")
# for(i in 1:nrow(obs)){
#   haraka_stepUD[[i]] = readOGR(dsn = dsn[i], layer = layerNames[i])
# }
# save(haraka_stepUD, file="Workspace/haraka_stepUD_polyList.Rdata")
write.csv(obs, "Workspace/Haraka_stepUD_data.csv")
load("Workspace/haraka_stepUD_polyList.Rdata")

# h_dist_glade = lapply(haraka_stepUD, function(z) poly.extract(gdst_ll, z, weights=TRUE) )
# h_dist_human = lapply(haraka_stepUD, function(z) poly.extract(hdst_ll, z, weights=TRUE) )
# h_dist_roads = lapply(haraka_stepUD, function(z) poly.extract(rddst_ll, z, weights=TRUE) )
# h_dist_river = lapply(haraka_stepUD, function(z) poly.extract(rvdst_ll, z, weights=TRUE) )
# h_dist_lugga = lapply(haraka_stepUD, function(z) poly.extract(ldst_ll, z, weights=TRUE) )
# 
# #Combine all distance objects into a single list object.
# h_dist = list(
#   glade = h_dist_glade,
#   human = h_dist_human,
#   roads = h_dist_roads,
#   river = h_dist_river,
#   lugga = h_dist_lugga
# )
# save(h_dist, file = "Workspace/Step UD/haraka_distance.Rdata") #output file
load("Workspace/Step UD/haraka_distance.Rdata")


#Data point to remove because poly was estimated wierdly...
area = unlist(lapply(haraka_stepUD, function(z) z@polygons[[1]]@area))
.obs = obs
.h_dist = list()
for(i in 1:length(h_dist)){
  x = h_dist[[i]]
  x[[2748]] <- NULL
  .h_dist[[i]] = x
}
names(.h_dist) = names(h_dist)

#Now, combine all into a single object for "easy" storage
#[[ feature ]] [[ x_meters ]] [,"UDlevel"]
#[["data"]]$
haraka_ths = stepUD.habitat(h_dist, data = obs)
#------------------------Konda - temporal habitat selection---------------------

poly = list.files("Step UD/Konda/", full.names = TRUE, recursive = TRUE, pattern = "Konda")
x = list.files("Step UD/Konda/", full.names = FALSE)
n = m3@DBMvar@window.size-m3@DBMvar@margin-1
polyID = as.numeric(unlist(regmatches(x, gregexpr("[[:digit:]]+", x))))
stepFile = c(rep(NA, times=nrow(m3@DBMvar@data)))
stepFile[polyID] = poly
m3@DBMvar@data$stepFile = stepFile
obs = m3@DBMvar@data[round(m3@DBMvar@data$lag.mins)==15 & !is.na(m3@DBMvar@data$stepFile),]
obs$Hour = as.integer(obs$TimeOfDay)
dsn =  obs$stepFile
# konda_stepUD = list()
# layerNames = paste("step_",as.numeric(unlist(regmatches(dsn, gregexpr("[[:digit:]]+", dsn)))),sep="")
# for(i in 1:nrow(obs)){
#   konda_stepUD[[i]] = readOGR(dsn = dsn[i], layer = layerNames[i])
# }
# save(konda_stepUD, file="Workspace/konda_stepUD_polyList.Rdata")
write.csv(obs, "Workspace/Konda_stepUD_data.csv")
load("Workspace/konda_stepUD_polyList.Rdata")

# k_dist_glade = lapply(konda_stepUD, function(z) poly.extract(gdst_ll, z, weights=TRUE) )
# k_dist_human = lapply(konda_stepUD, function(z) poly.extract(hdst_ll, z, weights=TRUE) )
# k_dist_roads = lapply(konda_stepUD, function(z) poly.extract(rddst_ll, z, weights=TRUE) )
# k_dist_river = lapply(konda_stepUD, function(z) poly.extract(rvdst_ll, z, weights=TRUE) )
# k_dist_lugga = lapply(konda_stepUD, function(z) poly.extract(ldst_ll, z, weights=TRUE) )
# 
# #Combine all distance objects into a single list object.
# k_dist = list(
#   glade = k_dist_glade,
#   human = k_dist_human,
#   roads = k_dist_roads,
#   river = k_dist_river,
#   lugga = k_dist_lugga
# )
# save(k_dist, file = "Workspace/Step UD/konda_distance.Rdata") #output file
load("Workspace/Step UD/konda_distance.Rdata")

#Data point to remove because poly was estimated wierdly...
area = unlist(lapply(konda_stepUD, function(z) z@polygons[[1]]@area))
.obs = obs[-c(2748),]
.k_dist = list()
for(i in 1:length(k_dist)){
  x = k_dist[[i]]
  x[[2748]] <- NULL
  .k_dist[[i]] = x
}
names(.k_dist) = names(k_dist)

#Now, combine all into a single object for "easy" storage
#[[ feature ]] [[ x_meters ]] [,"UDlevel"]
#[["data"]]$
konda_ths = stepUD.habitat(k_dist, data = obs)

#-------------------------Tatu - temporal habitat selection---------------------
poly = list.files("Step UD/Tatu/", full.names = TRUE, recursive = TRUE, pattern = "Tatu")
x = list.files("Step UD/Tatu/", full.names = FALSE)
n = m4@DBMvar@window.size-m4@DBMvar@margin-1
polyID = as.numeric(unlist(regmatches(x, gregexpr("[[:digit:]]+", x))))
stepFile = c(rep(NA, times=nrow(m4@DBMvar@data)))
stepFile[polyID] = poly
m4@DBMvar@data$stepFile = stepFile
obs = m4@DBMvar@data[round(m4@DBMvar@data$lag.mins)==15 & !is.na(m4@DBMvar@data$stepFile),]
obs$Hour = as.integer(obs$TimeOfDay)
dsn =  obs$stepFile
# tatu_stepUD = list()
# layerNames = paste("step_",as.numeric(unlist(regmatches(dsn, gregexpr("[[:digit:]]+", dsn)))),sep="")
# for(i in 1:nrow(obs)){
#   tatu_stepUD[[i]] = readOGR(dsn = dsn[i], layer = layerNames[i])
# }
# save(tatu_stepUD, file="Workspace/tatu_stepUD_polyList.Rdata")
write.csv(obs, "Workspace/Tatu_stepUD_data.csv")
load("Workspace/tatu_stepUD_polyList.Rdata")

# t_dist_glade = lapply(chumvi_stepUD, function(z) poly.extract(gdst_ll, z, weights=TRUE) )
# t_dist_human = lapply(chumvi_stepUD, function(z) poly.extract(hdst_ll, z, weights=TRUE) )
# t_dist_roads = lapply(chumvi_stepUD, function(z) poly.extract(rddst_ll, z, weights=TRUE) )
# t_dist_river = lapply(chumvi_stepUD, function(z) poly.extract(rvdst_ll, z, weights=TRUE) )
# t_dist_lugga = lapply(chumvi_stepUD, function(z) poly.extract(ldst_ll, z, weights=TRUE) )
# 
# #Combine all distance objects into a single list object.
# t_dist = list(
#   glade = t_dist_glade,
#   human = t_dist_human,
#   roads = t_dist_roads,
#   river = t_dist_river,
#   lugga = t_dist_lugga
# )
# save(t_dist, file = "Workspace/Step UD/tatu_distance.Rdata") #output file
load("Workspace/Step UD/tatu_distance.Rdata")

#Data point to remove because poly was estimated wierdly...
area = unlist(lapply(tatu_stepUD, function(z) z@polygons[[1]]@area))
.obs = obs[-c(2748),]
.t_dist = list()
for(i in 1:length(t_dist)){
  x = t_dist[[i]]
  x[[2748]] <- NULL
  .t_dist[[i]] = x
}
names(.t_dist) = names(t_dist)

#Now, combine all into a single object for "easy" storage
#[[ feature ]] [[ x_meters ]] [,"UDlevel"]
#[["data"]]$
tatu_ths = stepUD.habitat(t_dist, data = obs)

#---------------------Chumvi (2) - temporal habitat selection-------------------
load("Workspace/Chumvi2_dbbmm.Rdata")
a = split(leopards2.sub)[["Chumvi2"]]
tl = c(NA,timeLag(a))
poly = list.files("Step UD/ChumviII", full.names = TRUE, recursive = TRUE)
x = list.files("Step UD/ChumviII", full.names = FALSE)
n = n1@DBMvar@window.size-n1@DBMvar@margin-1
polyID = as.numeric(unlist(regmatches(x, gregexpr("[[:digit:]]+", x))))
stepFile = c(rep(NA, times=nrow(n1@DBMvar@data)))
stepFile[polyID] = poly
n1@DBMvar@data$stepFile = stepFile
obs = n1@DBMvar@data[ round(tl) == 15 & is.na(n1@DBMvar@data$stepFile)==FALSE,]
obs$step = c(NA,distance(a))[round(tl) == 15 & is.na(n1@DBMvar@data$stepFile)==FALSE]
obs$turn = c(NA,turnAngleGc(a),NA)[round(tl) == 15 & is.na(n1@DBMvar@data$stepFile)==FALSE]
time. = as.POSIXlt(obs$local.time)
obs$TimeOfDay = time.$hour + time.$min/60 + time.$sec/60^2
obs$Hour = as.integer(obs$TimeOfDay)
obs$Julian = time.$yday + time.$hour/24 + time.$min/(24*60) + time.$sec/(24*60^2)
obs$Jday = as.integer(obs$Julian)
obs$x = obs$Longitude_deg
obs$y = obs$Latitude_deg
obs$Name = "Chumvi"
dsn =  obs$stepFile
# layerNames = paste("step_",as.numeric(unlist(regmatches(dsn, gregexpr("[[:digit:]]+", dsn)))),sep="")
# Chumvi2_stepUD = list()
# for(i in 1:nrow(obs)){
#   Chumvi2_stepUD[[i]] = readOGR(dsn = dsn[i], layer = layerNames[i])
# }
#save(Chumvi2_stepUD, file="Workspace/Chumvi2_stepUD_polyList.Rdata")
write.csv(obs, "Workspace/Chumvi2_stepUD_data.csv")
load("Workspace/Chumvi2_stepUD_polyList.Rdata")

# c2_dist_glade = lapply(chumvi_stepUD, function(z) poly.extract(gdst_ll, z, weights=TRUE) )
# c2_dist_human = lapply(chumvi_stepUD, function(z) poly.extract(hdst_ll, z, weights=TRUE) )
# c2_dist_roads = lapply(chumvi_stepUD, function(z) poly.extract(rddst_ll, z, weights=TRUE) )
# c2_dist_river = lapply(chumvi_stepUD, function(z) poly.extract(rvdst_ll, z, weights=TRUE) )
# c2_dist_lugga = lapply(chumvi_stepUD, function(z) poly.extract(ldst_ll, z, weights=TRUE) )
# 
# #Combine all distance objects into a single list object.
# c2_dist = list(
#   glade = c2_dist_glade,
#   human = c2_dist_human,
#   roads = c2_dist_roads,
#   river = c2_dist_river,
#   lugga = c2_dist_lugga
# )
# save(c2_dist, file = "Workspace/Step UD/chumvi2_distance.Rdata") #output file
load("Workspace/Step UD/chumvi2_distance.Rdata")

#Data point to remove because poly was estimated wierdly...
area = unlist(lapply(Chumvi2_stepUD, function(z) z@polygons[[1]]@area))
# .obs = obs[-c(2748),]
# .c2_dist = list()
# for(i in 1:length(c2_dist)){
#   x = c2_dist[[i]]
#   x[[2748]] <- NULL
#   .c2_dist[[i]] = x
# }
# names(.c2_dist) = names(c2_dist)

#Now, combine all into a single object for "easy" storage
#[[ feature ]] [[ x_meters ]] [,"UDlevel"]
#[["data"]]$
chumvi2_ths = stepUD.habitat(c2_dist, data = obs)

#------------------------Ewaso - temporal habitat selection---------------------
load("Workspace/Ewaso_dbbmm.Rdata")
a = split(leopards2.sub)[["Ewaso"]]
tl = c(NA,timeLag(a))
poly = list.files("Step UD/Ewaso/", full.names = TRUE, recursive = TRUE)
x = list.files("Step UD/Ewaso/", full.names = FALSE)
n = n2@DBMvar@window.size-n2@DBMvar@margin-1
polyID = as.numeric(unlist(regmatches(x, gregexpr("[[:digit:]]+", x))))
stepFile = c(rep(NA, times=nrow(n2@DBMvar@data)))
stepFile[polyID] = poly
n2@DBMvar@data$stepFile = stepFile
obs = n2@DBMvar@data[ round(tl) == 15 & is.na(n2@DBMvar@data$stepFile)==FALSE,]
obs$step = c(NA,distance(a))[round(tl) == 15 & is.na(n2@DBMvar@data$stepFile)==FALSE]
obs$turn = c(NA,turnAngleGc(a),NA)[round(tl) == 15 & is.na(n2@DBMvar@data$stepFile)==FALSE]
time. = as.POSIXlt(obs$local.time)
obs$TimeOfDay = time.$hour + time.$min/60 + time.$sec/60^2
obs$Hour = as.integer(obs$TimeOfDay)
obs$Julian = time.$yday + time.$hour/24 + time.$min/(24*60) + time.$sec/(24*60^2)
obs$Jday = as.integer(obs$Julian)
obs$x = obs$Longitude_deg
obs$y = obs$Latitude_deg
obs$Name = "Ewaso"
dsn =  obs$stepFile
#layerNames = paste("step_",as.numeric(unlist(regmatches(dsn, gregexpr("[[:digit:]]+", dsn)))),sep="")
#Ewaso_stepUD = list()
#for(i in 1:nrow(obs)){
#  Ewaso_stepUD[[i]] = readOGR(dsn = dsn[i], layer = layerNames[i])
#}
#save(Ewaso_stepUD, file="Workspace/Ewaso_stepUD_polyList.Rdata")
write.csv(obs, "Workspace/Ewaso_stepUD_data.csv")
load("Workspace/Ewaso_stepUD_polyList.Rdata")

# e_dist_glade = lapply(Ewaso_stepUD, function(z) poly.extract(gdst_ll, z, weights=TRUE) )
# e_dist_human = lapply(Ewaso_stepUD, function(z) poly.extract(hdst_ll, z, weights=TRUE) )
# e_dist_roads = lapply(Ewaso_stepUD, function(z) poly.extract(rddst_ll, z, weights=TRUE) )
# e_dist_river = lapply(Ewaso_stepUD, function(z) poly.extract(rvdst_ll, z, weights=TRUE) )
# e_dist_lugga = lapply(Ewaso_stepUD, function(z) poly.extract(ldst_ll, z, weights=TRUE) )
# 
# #Combine all distance objects into a single list object.
# e_dist = list(
#   glade = e_dist_glade,
#   human = e_dist_human,
#   roads = e_dist_roads,
#   river = e_dist_river,
#   lugga = e_dist_lugga
# )
# save(e_dist, file = "Workspace/Step UD/ewaso_distance.Rdata") #output file
load("Workspace/Step UD/ewaso_distance.Rdata")


#Data point to remove because poly was estimated wierdly...
area = unlist(lapply(Ewaso_stepUD, function(z) z@polygons[[1]]@area))
# .obs = obs[-c(2748),]
# .e_dist = list()
# for(i in 1:length(e_dist)){
#   x = e_dist[[i]]
#   x[[2748]] <- NULL
#   .e_dist[[i]] = x
# }
# names(.e_dist) = names(e_dist)

#Now, combine all into a single object for "easy" storage
#[[ feature ]] [[ x_meters ]] [,"UDlevel"]
#[["data"]]$
ewaso_ths = stepUD.habitat(e_dist, data = obs)

#------------------------Limofu temporal habitat selection----------------------
load("Workspace/Limofu_dbbmm.Rdata")
a = split(leopards2.sub)[["Limofu"]]
tl = c(NA,timeLag(a))
poly = list.files("Step UD/Limofu/", full.names = TRUE, recursive = TRUE)
x = list.files("Step UD/Limofu/", full.names = FALSE)
n = n3@DBMvar@window.size-n3@DBMvar@margin-1
polyID = as.numeric(unlist(regmatches(x, gregexpr("[[:digit:]]+", x))))
stepFile = c(rep(NA, times=nrow(n3@DBMvar@data)))
stepFile[polyID] = poly
n3@DBMvar@data$stepFile = stepFile
obs = n3@DBMvar@data[ round(tl) == 15 & is.na(n3@DBMvar@data$stepFile)==FALSE,]
obs$step = c(NA,distance(a))[round(tl) == 15 & is.na(n3@DBMvar@data$stepFile)==FALSE]
obs$turn = c(NA,turnAngleGc(a),NA)[round(tl) == 15 & is.na(n3@DBMvar@data$stepFile)==FALSE]
time. = as.POSIXlt(obs$local.time)
obs$TimeOfDay = time.$hour + time.$min/60 + time.$sec/60^2
obs$Hour = as.integer(obs$TimeOfDay)
obs$Julian = time.$yday + time.$hour/24 + time.$min/(24*60) + time.$sec/(24*60^2)
obs$Jday = as.integer(obs$Julian)
obs$Name = "Limofu"
obs$x = obs$Longitude_deg
obs$y = obs$Latitude_deg
dsn =  obs$stepFile
# layerNames = paste("step_",as.numeric(unlist(regmatches(dsn, gregexpr("[[:digit:]]+", dsn)))),sep="")
# Limofu_stepUD = list()
# for(i in 1:nrow(obs)){
#   Limofu_stepUD[[i]] = readOGR(dsn = dsn[i], layer = layerNames[i])
# }
# save(Limofu_stepUD, file="Workspace/Limofu_stepUD_polyList.Rdata")
write.csv(obs, "Workspace/Limofu_stepUD_data.csv")
load("Workspace/Limofu_stepUD_polyList.Rdata")

# l_dist_glade = lapply(Limofu_stepUD, function(z) poly.extract(gdst_ll, z, weights=TRUE) )
# l_dist_human = lapply(Limofu_stepUD, function(z) poly.extract(hdst_ll, z, weights=TRUE) )
# l_dist_roads = lapply(Limofu_stepUD, function(z) poly.extract(rddst_ll, z, weights=TRUE) )
# l_dist_river = lapply(Limofu_stepUD, function(z) poly.extract(rvdst_ll, z, weights=TRUE) )
# l_dist_lugga = lapply(Limofu_stepUD, function(z) poly.extract(ldst_ll, z, weights=TRUE) )
# 
# #Combine all distance objects into a single list object.
# l_dist = list(
#   glade = l_dist_glade,
#   human = l_dist_human,
#   roads = l_dist_roads,
#   river = l_dist_river,
#   lugga = l_dist_lugga
# )
# save(l_dist, file = "Workspace/Step UD/limofu_distance.Rdata") #output file
load("Workspace/Step UD/limofu_distance.Rdata")

#Data point to remove because poly was estimated wierdly...
area = unlist(lapply(Limofu_stepUD, function(z) z@polygons[[1]]@area))
# .obs = obs[-c(2748),]
# .l_dist = list()
# for(i in 1:length(l_dist)){
#   x = l_dist[[i]]
#   x[[2748]] <- NULL
#   .l_dist[[i]] = x
# }
# names(.l_dist) = names(l_dist)

#Now, combine all into a single object for "easy" storage
#[[ feature ]] [[ x_meters ]] [,"UDlevel"]
#[["data"]]$
limofu_ths = stepUD.habitat(l_dist, data = obs)

#-----------------------Morani - temporal habitat selection---------------------
load("Workspace/Morani_dbbmm.Rdata")
a = split(leopards2.sub)[["Morani"]]
tl = c(NA,timeLag(a))
poly = list.files("Step UD/Morani/", full.names = TRUE, recursive = TRUE)
x = list.files("Step UD/Morani/", full.names = FALSE)
n = n4@DBMvar@window.size-n4@DBMvar@margin-1
polyID = as.numeric(unlist(regmatches(x, gregexpr("[[:digit:]]+", x))))
stepFile = c(rep(NA, times=nrow(n4@DBMvar@data)))
stepFile[polyID] = poly
n4@DBMvar@data$stepFile = stepFile
obs = n4@DBMvar@data[ round(tl) == 15 & is.na(n4@DBMvar@data$stepFile)==FALSE,]
obs$step = c(NA,distance(a))[round(tl) == 15 & is.na(n4@DBMvar@data$stepFile)==FALSE]
obs$turn = c(NA,turnAngleGc(a),NA)[round(tl) == 15 & is.na(n4@DBMvar@data$stepFile)==FALSE]
time. = as.POSIXlt(obs$local.time)
obs$TimeOfDay = time.$hour + time.$min/60 + time.$sec/60^2
obs$Hour = as.integer(obs$TimeOfDay)
obs$Julian = time.$yday + time.$hour/24 + time.$min/(24*60) + time.$sec/(24*60^2)
obs$Jday = as.integer(obs$Julian)
obs$Name = "Morani"
obs$x = obs$Longitude_deg
obs$y = obs$Latitude_deg
dsn =  obs$stepFile
# layerNames = paste("step_",as.numeric(unlist(regmatches(dsn, gregexpr("[[:digit:]]+", dsn)))),sep="")
# Morani_stepUD = list()
# for(i in 1:nrow(obs)){
#   Morani_stepUD[[i]] = readOGR(dsn = dsn[i], layer = layerNames[i])
# }
#save(Morani_stepUD, file="Workspace/Morani_stepUD_polyList.Rdata")
write.csv(obs, "Workspace/Morani_stepUD_data.csv")
load("Workspace/Morani_stepUD_polyList.Rdata")


# m_dist_glade = lapply(Morani_stepUD, function(z) poly.extract(gdst_ll, z, weights=TRUE) )
# m_dist_human = lapply(Morani_stepUD, function(z) poly.extract(hdst_ll, z, weights=TRUE) )
# m_dist_roads = lapply(Morani_stepUD, function(z) poly.extract(rddst_ll, z, weights=TRUE) )
# m_dist_river = lapply(Morani_stepUD, function(z) poly.extract(rvdst_ll, z, weights=TRUE) )
# m_dist_lugga = lapply(Morani_stepUD, function(z) poly.extract(ldst_ll, z, weights=TRUE) )
# 
# #Combine all distance objects into a single list object.
# m_dist = list(
#   glade = m_dist_glade,
#   human = m_dist_human,
#   roads = m_dist_roads,
#   river = m_dist_river,
#   lugga = m_dist_lugga
# )
# save(m_dist, file = "Workspace/Step UD/morani_distance.Rdata") #output file
load("Workspace/Step UD/morani_distance.Rdata")


#Data point to remove because poly was estimated wierdly...
area = unlist(lapply(Morani_stepUD, function(z) z@polygons[[1]]@area))
.obs = obs[-c(2748),]
.m_dist = list()
for(i in 1:length(m_dist)){
  x = m_dist[[i]]
  x[[2748]] <- NULL
  .m_dist[[i]] = x
}
names(.m_dist) = names(m_dist)

#Now, combine all into a single object for "easy" storage
#[[ feature ]] [[ x_meters ]] [,"UDlevel"]
#[["data"]]$
morani_ths = stepUD.habitat(m_dist, data = obs)

#------------------------Mzee - temporal habitat selection----------------------
load("Workspace/Mzee_dbbmm.Rdata")
a = split(leopards2.sub)[["Mzee"]]
tl = c(NA,timeLag(a))
poly = list.files("Step UD/Mzee/", full.names = TRUE, recursive = TRUE)
x = list.files("Step UD/Mzee/", full.names = FALSE)
n = n5@DBMvar@window.size-n5@DBMvar@margin-1
polyID = as.numeric(unlist(regmatches(x, gregexpr("[[:digit:]]+", x))))
stepFile = c(rep(NA, times=nrow(n5@DBMvar@data)))
stepFile[polyID] = poly
n5@DBMvar@data$stepFile = stepFile
obs = n5@DBMvar@data[ round(tl) == 15 & is.na(n5@DBMvar@data$stepFile)==FALSE,]
obs$step = c(NA,distance(a))[round(tl) == 15 & is.na(n5@DBMvar@data$stepFile)==FALSE]
obs$turn = c(NA,turnAngleGc(a),NA)[round(tl) == 15 & is.na(n5@DBMvar@data$stepFile)==FALSE]
time. = as.POSIXlt(obs$local.time)
obs$TimeOfDay = time.$hour + time.$min/60 + time.$sec/60^2
obs$Hour = as.integer(obs$TimeOfDay)
obs$Julian = time.$yday + time.$hour/24 + time.$min/(24*60) + time.$sec/(24*60^2)
obs$Jday = as.integer(obs$Julian)
obs$Name = "Mzee"
obs$x = obs$Longitude_deg
obs$y = obs$Latitude_deg
dsn =  obs$stepFile
# layerNames = paste("step_",as.numeric(unlist(regmatches(dsn, gregexpr("[[:digit:]]+", dsn)))),sep="")
# Mzee_stepUD = list()
# for(i in 1:nrow(obs)){
#   Mzee_stepUD[[i]] = readOGR(dsn = dsn[i], layer = layerNames[i])
# }
# save(Mzee_stepUD, file="Workspace/Mzee_stepUD_polyList.Rdata")
write.csv(obs, "Workspace/Mzee_stepUD_data.csv")
load("Workspace/Mzee_stepUD_polyList.Rdata")

# mz_dist_glade = lapply(Mzee_stepUD, function(z) poly.extract(gdst_ll, z, weights=TRUE) )
# mz_dist_human = lapply(Mzee_stepUD, function(z) poly.extract(hdst_ll, z, weights=TRUE) )
# mz_dist_roads = lapply(Mzee_stepUD, function(z) poly.extract(rddst_ll, z, weights=TRUE) )
# mz_dist_river = lapply(Mzee_stepUD, function(z) poly.extract(rvdst_ll, z, weights=TRUE) )
# mz_dist_lugga = lapply(Mzee_stepUD, function(z) poly.extract(ldst_ll, z, weights=TRUE) )
# 
# #Combine all distance objects into a single list object.
# mz_dist = list(
#   glade = mz_dist_glade,
#   human = mz_dist_human,
#   roads = mz_dist_roads,
#   river = mz_dist_river,
#   lugga = mz_dist_lugga
# )
# save(mz_dist, file = "Workspace/Step UD/mzee_distance.Rdata") #output file
load("Workspace/Step UD/mzee_distance.Rdata")


#Data point to remove because poly was estimated wierdly...
area = unlist(lapply(Mzee_stepUD, function(z) z@polygons[[1]]@area))
.obs = obs[-c(2748),]
.mz_dist = list()
for(i in 1:length(mz_dist)){
  x = mz_dist[[i]]
  x[[2748]] <- NULL
  .mz_dist[[i]] = x
}
names(.mz_dist) = names(mz_dist)

#Now, combine all into a single object for "easy" storage
#[[ feature ]] [[ x_meters ]] [,"UDlevel"]
#[["data"]]$
mzee_ths = stepUD.habitat(mz_dist, data = obs)
