##############################################################################
###              Script for visualizing leopard daily paths                ###
##############################################################################
library(rethinking)
library(chron)
library(circular)
library(sp)
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
lugga <- readShapeLines("Workspace/Shapefiles/Lugga/lugga.shp")
base2 = raster("Environmental Variables/Elevation_(from  SRTM).tif")
base2 = projectRaster(base2, crs = CRS(proj4string(leopards_UTM)))
#load("Workspace/utilization_distribution.Rdata")

#names(leopards_UTM)
vars <- c("utm.easting","utm.northing", "Julian","SolarElevation", "step", "timestamp","Name","Night","active","state","lag.mins")
a <- split(leopards_UTM)
ID <- names(a)
color <- colorRampPalette(c("slateblue","orange2","orange2","slateblue"))
d <- lapply(a, function(z) z@data[,vars])
#base <- stack("Maps/Basemap_terrain_zoom13.grd")
#base <- projectRaster(base, crs=CRS(proj4string(leopards_UTM)))
#base2 = crop(base, extent(roads))
#hrPolys <- sapply(ud, function(z) getverticeshr(z, percent=99))

# for(i in 1:length(d)){
#   
#   d. <- d[[i]]
#   jul <- unique(d.$Julian)
#   par(mar=c(0,0,5,0)+0.1)
#   pdf(paste(ID[i],".pdf"), height=11, width=8.5)
#   for(j in 1:length(jul)){
#     
#     d.. <- d.[d.$Julian==jul[j],]
#     if(nrow(d..)<86) next
#     dates <- as.Date(d..$timestamp)
#     yLim <- c(extent(a[[i]])@ymin,extent(a[[i]])@ymax)
#     xLim <- c(extent(a[[i]])@xmin,extent(a[[i]])@xmax)
#     baseXlim <- c(extent(base2)@xmin, extent(base2)@xmax)
#     baseYlim <- c(extent(base2)@ymin, extent(base2)@ymax)
#     plotRGB(base2)
#     text(x=mean(baseXlim), y=baseYlim[2], labels=paste(ID[i],dates[j],sep=": "),cex=1.4)
#     text(x=mean(baseXlim),y=baseYlim[1],labels=paste("Distance Traveled = ",round(sum(d..$step, na.rm=TRUE),0)," meters",sep=""),cex=1.4)
#     plot(hrPolys[[i]],add=TRUE, col=col.alpha("gray60",0.15))
#     points(d..$utm.easting, d..$utm.northing,
#          asp=1,
#          pch=19, cex=.8,
#          col=sapply(ifelse(d..$Night==0,"orange2","slateblue"),function(z)col.alpha(z,0.6))
#          )
#     #if(j>1){
#      # d... <- d.[d.$Julian==jul[j-1],c("utm.easting","utm.northing")]
#      # points(d...$utm.easting, d...$utm.northing, pch=19, cex=0.60,col="gray60")
#     #  lines(d...$utm.easting, d...$utm.northing, lwd=.6, col="gray50") 
#     #}
#     plot(humans, add=TRUE, col=col.alpha("red", alpha=0.15))
#     plot(river, add=TRUE, col="blue", lwd=1.2)
#     lines(d..$utm.easting,d..$utm.northing, lwd=.8, col="gray45")
#     points(d..$utm.easting[1], d..$utm.northing[1], pch=25, bg="green", cex=1.3)
#     points(d..$utm.easting[nrow(d..)], d..$utm.northing[nrow(d..)], pch=24, bg="red", cex=1.3)
#     plot(roads, add=TRUE, col=col.alpha("black", alpha=0.75))
#     
#     text(x=locs$Easting, y=locs$Northing, labels=locs$Location, pos=3, cex=0.7)
#      }
#   dev.off()
# }

#3D PLOT HELP CODE

#Sets NA values to min(base2)
summary.elev <- summary(base2[])
setBaseElev <- function(x) {
  x[is.na(x)] <- as.numeric(summary.elev[1])
  return(x)
}
elev3D <- calc(base2, setBaseElev)

#Raster to regular matrix
zData <-round(as.matrix(elev3D),1)

#X and Y for persp()
r = res(elev3D)[1]
x = (r * (1:nrow(zData)))    
y = (r * (1:ncol(zData)))

#Colors for elevation data
ncz = ncol(zData)
nrz = nrow(zData)
zfacet <- (zData[-1, -1] + zData[-1, -ncz] + 
             zData[-nrz, -1] + zData[-nrz, -ncz])/4

#Create color schemes
nbcol <- 255
color <- c("grey", rev(terrain.colors(nbcol)))
facetcol <- cut(zfacet, nbcol+1)

# #Color Ramp
# jet.colors <- colorRampPalette( c("blue", "green") )
# # Generate the desired number of colors from this palette
# nbcol <- 1000
# color <- jet.colors(nbcol)
# # Recode facetValues into color indices
# facetcol <- cut(facetValues, nbcol)

#Final Plot
persp(x, y, z = zData*2, theta = 90, phi = 50,
      col = color[facetcol],
      scale = FALSE, expand = 0.75, 
      ltheta = 0, shade = 0.75, border = NA, lphi = 60,
      box = F, ticktype = "detailed")

#Plot all of the movements together...
d. <- do.call("rbind",d)

pdf("Daily_Paths_all.pdf", height=8.5, width=8.5)
for(j in 1:length(jul)){
  d.. <- d.[which(yday. %in% jul[j]),]
  date <- jul[j] #Current date for frame
  yLim <- c(extent(leopards_UTM)@ymin,extent(leopards_UTM)@ymax)
  xLim <- c(extent(leopards_UTM)@xmin,extent(leopards_UTM)@xmax)
#   baseXlim <- c(extent(leopards_UTM)@xmin, extent(base2)@xmax)
#   baseYlim <- c(extent(base2)@ymin, extent(base2)@ymax)
  par(bg="white")
  colvec <- colorRampPalette(colors=c("lightyellow","lightyellow4"))(255*2) #Terrain color
  #colvec <- sapply(colvec, function(z)col.alpha(z,0.7)) #Add transparency to terrain color.
  colvec2 <- topo.colors(n=4, alpha=0.7) #Animal track colors.
  #par(oma=c(0.1,0.1,0.1,0.1)) # Add margins 
  #raster::persp(base2, scale=0.5, phi = 90, lphi=45)
  plot(base2)
#contour(base2, method = "flattest", add=TRUE, nlevels = 7)
  #axis(3, at=seq(2.6e5, 2.72e5, by=1e3));axis(4, at=seq(2.8e4, 4e4, by=1e3)) #Add axes
  plot(humans, add=TRUE, col=col.alpha("red", alpha=0.35)) #Add human sites
  plot(river, add=TRUE, col="lightblue", lwd=3) #Add river sites.
  plot(roads, add=TRUE, col=col.alpha("red", alpha=0.65), lwd=1.3, lty=2) #Add roads.
  plot(lugga, add=TRUE, col=col.alpha("orange2", alpha=0.65), lwd=1.3, lty=1) #Add luggas.
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
    linevec[is.na(d..$lag.mins==TRUE)] = "gray45"
    #Add GPS points to map.
    lines(d..$utm.easting[d..$Name==ID[index]],d..$utm.northing[d..$Name==ID[index]], lwd=6, col=linevec)
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
#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('red','blue'))
#This adds a column of color values
# based on the y values
col.vec = rbPal(10)[as.numeric(cut(m1@DBMvar@mean,breaks = 10))]

plot(m2@DBMvar$step, m2@DBMvar@means, col=col.vec)

