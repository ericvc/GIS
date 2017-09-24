#LEOPARD HOME RANGE CALCULATIONS
library(adehabitatHR)
library(rgdal)
library(maptools)

setwd("~/Leopard Analysis/")
load("Workspace/Analysis.R")
load("Workspace/Analysis2.R")
#load("Workspace/utilization_distribution.Rdata")
p4s = CRS(proj4string(leopards_UTM))

# #Calculate 50, 90, 95, and 99% home range polygons for each individual
names = c(as.character(leopards@data$Name), as.character(leopards2.sub@data$individual.local.identifier))
names[names=="Chumvi2"] = "Chumvi"
ids = sort(unique(names))
# 
# for(i in 1:length(ud)){
#  ID = ids[i]
#  dir = "Workspace/Shapefiles/MKDE Home Ranges/"
#  level = c(50,90,95,99)
#  polys = sapply(level, 
#                      function(z) getverticeshr(ud[[i]], percent = z, unin = "m", unout = "km2") 
#                      )
#  #for(p in 1:length(polys)) proj4string(polys[[p]]) = p4s
#  #Write shapefile
#  for(p in 1:length(polys)) writeSpatialShape(polys[[p]], fn = paste(dir,ID,"_",level[p],"_","home_range",sep=""))
#  #Write proj file
#  for(p in 1:length(level)) showWKT("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84",file=paste(dir,ID,"_",level[p],"_","home_range.prj",sep=""))
#  
# }


#NEED A LOOP FOR EACH LEOPARD'S MCP HOME RANGE (95% only)
data = leopards_UTM@data
dat = data[,c("utm.easting","utm.northing","local.time","Name","state")]
dat2 = leopards2_UTM.sub@data[,c("Easting","Northing","local.time","individual.local.identifier")]
dat2$state = NA
dat2[,1:2] = leopards2_UTM.sub@coords #Coordinates in data.frame are fucked.
names(dat2) = names(dat)
dat3 = data.frame(rbind(droplevels(dat),droplevels(dat2)))
dat4 = dat3[complete.cases(dat3[,1:4]),]
dat4$Name[dat4$Name=="Chumvi2"] <- "Chumvi"
dat4$Name = as.character(dat4$Name)

# xy.sp = SpatialPointsDataFrame(dat4[,c("utm.easting","utm.northing")], data = data.frame(dat4$Name))
# kernel.UDs = kernelUD(xy.sp, grid = 1200, same4all = TRUE)
# saveRDS(kernel.UDs, file = "Workspace/lkde.uds_allLeopards.rds")
kernel.UDs = readRDS("Workspace/lkde.uds_allLeopards.rds")

for(i in 1:length(kernel.UDs)){
  ID = ids[i]
  dir = "Workspace/Shapefiles/LKDE Home Ranges/"
  level = c(50,90,95,99)
  polys = sapply(level, 
                 function(z) getverticeshr(kernel.UDs[[i]], percent = z, unin = "m", unout = "km2") 
  )
  #Write shapefile
  for(p in 1:length(polys)){ 
  #Write output in the form of a .shp file.
  writeSpatialShape(polys[[p]], fn = paste(dir,ID,"_",level[p],"_","home_range",sep=""))
  #Write .prj file (PROJ.4)
  showWKT("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84",file=paste(dir,ID,"_",level[p],"_","home_range.prj",sep=""))
  #Plot preview PDF
  pdf(paste(dir,ID,"_",level[p],"_","home_range.pdf",sep=""),6,6)
  plot(polys[[p]], axes=FALSE)
  dev.off()
  }
}
