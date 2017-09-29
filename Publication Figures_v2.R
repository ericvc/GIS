#Load required packages
library(raster)
library(rethinking)
setwd("/Volumes/Data/Leopard Analysis/")
p4s_ll = CRS("+proj=longlat +datum=WGS84") 
p4s_utm <- CRS("+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
conv =  1 / 111320 #Conversion between meters and decimal degrees
#Read data into workspace.
dir = 'Manuscripts/Leopard Alarm Calls/' #path to working directory

data = read.table(paste(dir,"movements.txt", sep=""), header=TRUE)
cameras = read.table(paste(dir,"cameras.txt", sep=""), header=TRUE)
events = unique(data$EVENT)
basemap = raster("Manuscripts/Leopard Alarm Calls/hippo_fromGE_v3.png")
plot(basemap, col=c(1:100/100))
extent. = extent(c(xmin=36.900801, xmax=36.915387, ymin=0.311486, ymax=0.329323))
extent(basemap) = extent.
proj4string(basemap) = CRS("+init=epsg:4326") #Geographic coordinate system

# The handy bit of code below finds all the arguments for the underlying plot functions.
# This is needed to turn off the box and legend in the raster plot.

# showMethods("plot")
# getMethod("plot", c("Raster"))
# getAnywhere(".plotraster2")
# getAnywhere(".rasterImagePlot")
# args(raster:::.rasterImagePlot)

#Define custom arrow function
arrows2 <-  function(XY1, XY2, arrow.length, arrow.width, adj.f=.8,...){
  
  conv =  1 / 111320 #Conversion between meters and decimal degrees
  
  aw = arrow.width*conv
  al = arrow.length*conv
  
  xyA = XY1
  xy5 = XY2
  #Create polygon arrow "tip"
  V = matrix(xy5-xyA)
  V. = norm(V)
  D = matrix(rev(V)*c(-1,1))
  D. = norm(D)
  tip.base = xy5 - (V/V.)*al
  xy1 = xyA - (D/D.)*(aw/2)
  xy2 = xyA + (D/D.)*(aw/2)
  xy3 = tip.base + (D/D.)*(aw/2)
  xy4 = xy3 + (D/D.)*(adj.f*aw)
  xy7 = tip.base - (D/D.)*(aw/2)
  xy6 = xy7 - (D/D.)*(adj.f*aw)
  crds = matrix(c(xy1,xy2,xy3,xy4,xy5,xy6,xy7,xy1), ncol=2, byrow=TRUE)
  polygon(crds[,1],crds[,2],...)
  
}

base = crop(basemap, (extent(basemap)-(.6/111)))
#par(mar=c(0,0,0,0)+0.1, mfrow=c(3,3))
for(e in 1:length(events)){

  d  = droplevels(data[data$EVENT==e,])
  leop  = d[d$TAXON=="Panthera",]
  monk  = d[d$TAXON=="Chlorocebus",]
  
  #-Start of plotting
  fileName = paste("Manuscripts/Leopard Alarm Calls/Figures/Supplementary_Figure",
                   e,unique(data$DATE)[e], sep="_")
  pdf(paste(fileName,".pdf",sep=""))
  plot(base, col=c(1:100/100), legend=FALSE, box=FALSE, axes=FALSE)
  
  #Leopard locations
  for(l in 2:nrow(leop)){
    crds1 = c(leop$LONGITUDE[l-1], leop$LATITUDE[l-1])
    crds2 = c(leop$LONGITUDE[l], leop$LATITUDE[l])
    D = crds2-crds1
    delta = sqrt((D[1])^2+(D[2])^2)
    jig = 10*conv
    L1 = crds1 + (D / norm(matrix(D))) * (delta*0.11)
    L2 = crds2 - (D / norm(matrix(D))) * (delta*0.11)
    if(delta > 50*conv){
      arrows2(XY1=L1, XY2=L2, col=col.alpha("yellow",0.75), adj.f=0.8,
              arrow.length=55, arrow.width=20)
    }
    else points(crds2[1], crds2[2], pch=21, bg=col.alpha("yellow",0.8), cex=1)
    #arrows(crds1[1], crds1[2], crds2[1], crds2[2], col="yellow", lwd=3, length=0.07)
  }
  
  #Primate location
  x = mean(monk$LONGITUDE, na.rm=TRUE)
  y = mean(monk$LATITUDE, na.rm=TRUE)
  points(x,y, pch=21, bg="slateblue", cex=1.75)
  
  #Camera location
  x = cameras[e,"LONGITUDE"]
  y = cameras[e,"LATITUDE"]
  points(x,y, pch=24, bg="orange2", cex=1.75)
  
  #Map scale
  len = 200*conv/2 #Prep--------------------------------------------------------
  xcen = 36.911
  ycen = 0.316
  xx <- c(c(xcen-len,xcen), c(xcen,xcen-len) )
  xx2 <- c(c(xcen,xcen+len), c(xcen+len,xcen) )
  yy <- c(ycen-5*conv, ycen-5*conv, ycen+5*conv, ycen+5*conv)#------------------
  polygon(xx, yy, col = "black", border = "white", lwd=1.3) #Main bar (1)
  polygon(xx2, yy, col = NA, border = "white", lwd=1.3) #Main bar (2)
  #segments(xcen, ycen-10*conv, xcen, ycen+10*conv, lwd=2.4, col="white") #Center tick
  text(xcen, ycen-50*conv, "100", cex=0.9, col="white", font=2) #Center label
  segments(xcen-len, ycen, xcen-len, ycen+20*conv, lwd=1.3, col="white") #Left tick
  text(xcen-len, ycen-50*conv, "0", cex=0.9, col="white", font=2) #Left label
  segments(xcen+len, ycen, xcen+len, ycen+20*conv, lwd=1.3, col="white") #Right tick
  text(xcen+len, ycen-50*conv, "200", cex=0.9, col="white", font=2) #Right label
  text(xcen, ycen-90*conv, "meters", cex=0.8, col="white", font=2) #Units label
  
  dev.off()
}


figures = c(list.files("Manuscripts/Leopard Alarm Calls/Figures/", ".pdf", full.names=TRUE),
            list.files("", recursive=TRUE, pattern="Publication Figures_v2.R") #This script.
#zip figures into archive
zip("Manuscripts/Leopard Alarm Calls/Figures/Manuscript+Supplementary+Figures.zip", 
    files=figures)