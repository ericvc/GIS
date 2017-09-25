##############################################################################
###              Script for visualizing leopard daily paths                ###
##############################################################################
library(rethinking)
library(raster)
library(rasterVis)
library(maptools)
library(adehabitatHR)
library(move)

#Set the working directory to the Dropbox folder
setwd('~/Leopard Analysis/')
source('Scripts/Leopard Helper Functions.R') #Adds specialized fxns for analysis.
load('Workspace/Analysis.R') #Loads data in an object called 'leopards'
locs <- read.csv("locations.csv")
roads <- readShapeLines("Workspace/Shapefiles/Roads/roads.shp")
humans <- readShapePoly("Workspace/Shapefiles/Humans/humans.shp")
river <- readShapeLines("Workspace/Shapefiles/River/river.shp")
#load("Workspace/utilization_distribution.Rdata")

#names(leopards_UTM)
vars <- c("utm.easting","utm.northing", "Julian","SolarElevation", "step", "timestamp","Name","Night","active","state","lag.mins")
a <- split(leopards_UTM)
ID <- names(a)
color <- colorRampPalette(c("slateblue","orange2","orange2","slateblue"))
d <- lapply(a, function(z) z@data[,vars])
base <- raster("Environmental Variables/Elevation_(from  SRTM).tif")
base1 <- projectRaster(base, crs=CRS(proj4string(leopards_UTM)))
base2 = crop(base1, extent(roads))

#Plot all of the movements together...
d. <- do.call("rbind",d)
yday. = as.POSIXlt(d.$timestamp, tz = "UTC")$yday
jul <- sort(unique(yday.))
pdf("Daily_Paths_all.pdf", height=8.5, width=8.5)
for(j in 1:length(jul)){
  d.. <- d.[yday.==jul[j],]
  date <- jul[j] #Current date for frame
  yLim <- c(extent(leopards_UTM)@ymin,extent(leopards_UTM)@ymax)
  xLim <- c(extent(leopards_UTM)@xmin,extent(leopards_UTM)@xmax)
  baseXlim <- c(extent(base2)@xmin, extent(base2)@xmax)
  baseYlim <- c(extent(base2)@ymin, extent(base2)@ymax)
  par(bg="white")
  colvec <- colorRampPalette(colors=c("white","cadetblue"))(255) #Terrain color
  colvec <- sapply(colvec, function(z)col.alpha(z,0.4)) #Add transparency to terrain color.
  colvec2 <- topo.colors(n=4, alpha=0.7) #Animal track colors.
  par(oma=c(3,3,3,3)) # Add margins 
  plot(base1, col = topo.colors(3e3)) #Plot basemap
  axis(3, at=seq(2.6e5, 2.72e5, by=1e3));axis(4, at=seq(2.8e4, 4e4, by=1e3)) #Add axes
  plot(humans, add=TRUE, col=col.alpha("red", alpha=0.35)) #Add human sites
  plot(river, add=TRUE, col="lightblue", lwd=3) #Add river sites.
  plot(roads, add=TRUE, col=col.alpha("red", alpha=0.65), lwd=1.3, lty=2) #Add roads.
  dtd <- len <- name <- c() #Create empty vectors
  for(a in 1:length(unique(d..$Name))){
    index <- which(unique(d..$Name)[a]==ID)
    dtd[a] <- sum(d..$step[d..$Name==ID[index]&d..$active==1], na.rm=TRUE)
    len[a] <- length(d..$step[d..$Name==ID[index]])
    name[a] <- ID[index]
    #Create pch vector to describe time of day.
    pchvec = ifelse(d..$Night==0,19,17)
    cexvec = (log(d..$state)+0.1)/1.4
    linevec = ifelse(d..$lag.mins<30, "gray45", "red")
    #Add GPS points to map.
    lines(d..$utm.easting[d..$Name==ID[index]],d..$utm.northing[d..$Name==ID[index]], lwd=2, col=linevec)
    points(d..$utm.easting[d..$Name==ID[index]], d..$utm.northing[d..$Name==ID[index]],
           asp=1,
           pch=pchvec,
           cex=cexvec,
           col=colvec2[ index ]
    )
    points(d..$utm.easting[d..$Name==ID[index]][1], d..$utm.northing[d..$Name==ID[index]][1], pch=25, bg="green", cex=1)
    points(d..$utm.easting[d..$Name==ID[index]][nrow(d..[d..$Name==ID[index],])], 
           d..$utm.northing[d..$Name==ID[index]][nrow(d..[d..$Name==ID[index],])], pch=24, bg="red", cex=1)
#     if(j>1){
#     d... <- d.[yday.==jul[j-1]&d.$Name==ID[index],c("utm.easting","utm.northing","Night","active")]
#     points(d...$utm.easting, d...$utm.northing, pch=ifelse(d...$Night==0,19,17), cex=0.60,col=col.alpha("gray60",0.5))
#      lines(d...$utm.easting, d...$utm.northing, lwd=.6, col=linevec) 
#     }
    
  }
  #Add lines for coordinate grid
  #hlines = seq(2.8e4, 4e4, by=100); vlines= seq(2.6e5, 2.72e5, by=100)
  #lwdvec = rep(0.25, length(hlines)); colvec = rep("gray43", length(hlines))
  #lwdvec[seq(1,length(lwdvec),by=10)] <- 0.75; colvec[seq(1,length(colvec),by=10)] <- "black"
  #lwdvec[seq(6,length(lwdvec)-5,by=10)] <- 0.5; colvec[seq(1,length(colvec),by=10)] <- "gray20"
  #abline(h=hlines, lwd=lwdvec, col=colvec)
  #abline(v=vlines, lwd=lwdvec, col=colvec)
  mtext("Easting", side=1); mtext("Northing", side=2)
  text(x=locs$Easting, y=locs$Northing, labels=c("",paste(locs$Location[2:3])), pos=4, cex=0.53)
  points(x=locs$Easting, y=locs$Northing, pch=19, cex=c(0,0,0.4))
  legend(x="topright", col=colvec2, pch=15, cex=1.6, legend=ID)
  legend("topleft", legend=date, bty="n", cex=2)
  lab <- paste(name,": ", round(dtd,0)," meters ", "(",len,")",sep="")
  legend("right", legend=lab, bty="n")  
}
dev.off()

#Interesting idea on how to test whether leopards are using the landscape in an
#"energetically costly" manner: compare actual movements of leopards through the 
#landscape with those of randomly generated movements. Look at changes in elevation
#and distance traveled through thick vegetation to determine if leopards were
#moving "optimally". A simple comparison from the randomly-generated distribution
#could be simple enough for a relative comparison (i.e. like a p-value).

#Examine how often leopards patrol (1) the boundaries of their territories and
# (2) the areas of overlap.

#Autocorrelation in space use? Do they have intensive shifts in space use over time?