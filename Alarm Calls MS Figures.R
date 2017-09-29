setwd("~/Leopard Analysis/")
load("Workspace/Analysis.R") #Leopard data
load("Monkeys/Monkey Workspace/Monkey.data.Rdata") #Primate data
library(raster)
library(GISTools)

#Define CRS objects for spatial transformations.
p4s_ll = CRS(proj4string(leopards))
p4s_UTM = CRS(proj4string(leopards_UTM))
time. = as.POSIXct(monkey.move$study.local.timestamp, tz = "Africa/Nairobi")
monkey.move$Julian = julian(x = time., origin=as.POSIXct("2014-01-01", "Africa/Nairobi"))
monkey.move$Jday = as.integer(monkey.move$Julian)
tod = as.POSIXlt(time.)
monkey.move$TimeOfDay = tod$hour + tod$min/60 + tod$sec/60^2
monkey.move$Hour = as.integer(monkey.move$TimeOfDay)
monkey.move_UTM = spTransform(monkey.move, p4s_UTM)

# files = list.files("~/GIS/", pattern = c("11MAY01073359-P2AS",".tif"), full.names = TRUE)
# hab = list()
# for(i in 1:length(files)) hab[[i]] = raster(files[i])
#Mosaic together
# hab = mosaic(hab[[1]],hab[[2]],hab[[3]],hab[[4]],hab[[5]],
#            hab[[6]],hab[[7]],hab[[8]], 
#            fun=mean
#           )
# writeRaster(hab, file="~/GIS/Mpala_QUICKBIRD.tif", format="GTiff")

#-Run code all together.........................................................
hab  = raster("Manuscripts/Leopard Alarm Calls/hippo_fromGE.jpg")
extent(hab) = extent(c(xmin=264990.45, xmax=269034.39, ymin=34339.13, ymax=36640.41))
proj4string(hab) = p4s_UTM
#...............................................................................

#Vegetation details
#1 = No Data, 2 = Canopy, 3 = Grass 1, 4 = Grass 2, 5 = Bare Ground
veg = raster("Environmental Variables/RAW Files/QB2011classified.img")
dem2 = raster("Environmental Variables/Elevation_(from  SRTM).tif")
habcode = raster("Environmental Variables/")
river = spTransform(readOGR("KML files/HPSS_mask.kml", layer="HPSS_mask.kml"), p4s_UTM)
roads = readShapeLines("Workspace/Shapefiles/Roads/roads.shp", proj4string = p4s_UTM)

#Modified function from 'GISTools' package (map.scale)
map.scale.custom = function (xc, yc, len, units, ndivs, lab, subdiv = 1, tcol = "black", 
                      scol = "black", sfcol = "black", cex.label=1) 
{
  frame = par("usr")
  l <- len
  tic = (frame[4] - frame[3])/100
  ul = l/ndivs
  #if(length(lab)!=(ndivs-1)) warning("Labels not same length as number of divisions")
  for (i in seq(0, ndivs - 1, by = 2)) rect(xc - l/2 + i * 
                                              ul, yc, xc - l/2 + (i + 1) * ul, yc + tic/2, border = NA, 
                                            col = sfcol)
  lines(c(xc - l/2, xc - l/2, xc + l/2, xc + l/2), c(yc + tic, 
                                                     yc, yc, yc + tic), col = scol)
  lines(c(xc - l/2, xc + l/2), c(yc + tic/2, yc + tic/2), col = scol)
  #Add labels
  for (i in 0:ndivs) text(xc - l/2 + ul * i, yc - strheight(i * 
                                                              subdiv) * 0.7, lab[i+1], col = tcol, cex=cex.label)
  text(xc, yc - 2 * strheight(units), units, col = tcol, cex = cex.label)
}

#---Create background for plots

# plot cast shadows on mountain terrain using raster
slope = terrain(dem2, opt = "slope")
aspect = terrain(dem2, opt = "aspect")
hsh = hillShade(slope, aspect, angle = 45, direction = 315)
#hsh2 = crop(hsh, extent(a2[[i]])+1/111)
site = c(E = 267364.81, N = 35237.05)
cam = c(E=267397.5, N=35313.95)
d = 295
site.ex = extent(c(xmin=site["E"]-d, xmax=site["E"]+d, ymin=site["N"]-d, ymax=site["N"]+d))

#-----------------------------------Figure 4------------------------------------

#isolate Tatu's data
##Tatu
t = split(leopards_UTM, leopards_UTM$Name)[["Tatu"]]
p = split(monkey.move_UTM, f = monkey.move_UTM$Group)[["HPPO"]]
load("Workspace/Tatu_dbbmm.Rdata")
load("Workspace/Tatu_acc_state.Rdata")
m = m4@DBMvar

#Time 1 - HAE: 1629, heading = 349.31, n = 7
o = which(as.integer(t$Julian)==125 & t$TimeOfDay>2.6 & t$TimeOfDay < 4.55)
dat1 = t[o,]
o = which(as.integer(p$Julian)==125 & p$TimeOfDay>0 & p$TimeOfDay < 4.55)
dat2 = p[o,]
o = which(as.integer(m$Julian)==125 & m$TimeOfDay>2.6 & m$TimeOfDay < 4.55)
dat3 = m4@DBMvar[o,]
sleep = matrix(unlist(lapply(split(dat2@coords,dat2@trackId), function(z) apply(matrix(z,ncol=2), 2, mean))), ncol=2)

#site.ex = extent(dat1)+100
hab2 = crop(hab, site.ex)
#plot(hab2, col=c("yellow","green","brown"))
#plot(hsh.fig4, col=grey(10:100/100), legend=FALSE, axes=TRUE, xlab="Longitude", ylab="Latitude", cex.lab=0.8)
#pdf("~/Leopard Analysis/Manuscripts/Leopard Alarm Calls/Figure4.pdf", height=10, width = 14)
plot(hab, legend=FALSE, col=grey(10:100/100), axes=FALSE, frame.plot=FALSE, asp=1)
dat1@coords[,1] = dat1@coords[,1]-20 #Adjustment for X coordinates
#text(dat1@coords, labels=c(1:nrow(dat1))) #numbers for each location.
points(cam, pch=19, cex=1.1, col="red") #Plot camera.
#Plot leopard movements
lines(dat1@coords, col="yellow", lwd=2.7, lty=2)
points(dat1@coords, bg="yellow", pch=21, cex=1.1)
points(dat1@coords, bg="yellow", pch=21, cex=1.1) #Start point
points(dat1@coords, bg="yellow", pch=21, cex=1.1) #End point

# #Add movement arrows
# for(i in 1:(nrow(dat1)-1)){
#   if(dat1$step[i+1]>50){
#     crds = dat1@coords[c(i,i+1),]
#     v = as.matrix(crds[2,]-crds[1,])
#     u = v/sqrt(sum(v^2)) #Create vector of unit length.
#     b = 17.5#dat1$step[i+1]/2
#     s2 = dat1[i+1,]$step - b
#     crds[2,] = crds[1,] + t(s2*u)
#     crds[1,] = crds[1,] + t(b*u)
#     arrows(x0 = crds[1,1], 
#            x1 = crds[2,1], 
#            y0 = crds[1,2], 
#            y1 = crds[2,2], 
#            col="yellow", 
#            lwd=5.7, 
#            length=0.075, 
#            angle = 50
#           )
#   }
  #points(x = sleep[1,], y = sleep[2,], pch=19, col="blue", cex=.8)
  text(cam["E"], cam["N"], labels = "Camera", cex=1.15, col="red", pos=1)
}

#Add map scale.
map.scale.custom(site["E"]+175, yc = site["N"]-50, 
                 len = 100, 
                 units = "meters", 
                 ndivs = 2, 
                 lab = c(0,50,100), 
                 tcol = "white", 
                 scol = "white",
                 cex.label = 1
)
dev.off()

#----------------------------Supplement Fig. 1---------------------------------

#Read in data
files = list.files("Manuscripts/Leopard Alarm Calls/Movements/", recursive = TRUE, pattern = '.csv', full.names = TRUE)
dat = crds = list()
for(file in 1:length(files)) dat[[file]] = read.csv(files[file])
for(i in 1:length(dat)) crds[[i]] = SpatialPoints(dat[[i]][,c("utm.easting","utm.northing")], proj4string = p4s_UTM)

L = 1500 #Length of buffer side
site = cbind(X=mean(c(extent(hab)@xmax,extent(hab)@xmin)), Y=mean(c(extent(hab)@ymax,extent(hab)@ymin)), Area=L^2)
siteSP = SpatialPoints(site, proj4string = p4s_UTM)
site.ex = gBuffer(siteSP, width = L, quadsegs = 1, capStyle = "SQUARE")
hab2 = crop(hab, site.ex)

# #Run the following together........
# veg2 = crop(veg, site.ex)
# veg2[veg2==1] = NA #No Data
# veg2[veg2==5] = "A" #Bare ground
# veg2[veg2==3|veg2==4] = "B" #Grass
# veg2[veg2==2] = "C" #Canopy
# #..................................

#!!!!!!!
#Need to extend the range of the map to include 2 of the events.
#!!!!!!!
for(p in 1:length(crds)){
  time = as.Date(dat[[p]]$local.time[1])
  file = paste("~/Leopard Analysis/Manuscripts/Leopard Alarm Calls/SupplementalFigure_",time,".pdf", sep="")
  pdf(file, height=5.75, width = 6)
  plot(hab2, legend=FALSE, col=grey(0:100/100), axes=FALSE, frame.plot=FALSE, asp=1)
  xy = coordinates(crds[[p]])
  lines(xy, col="yellow", lwd=2, lend=0) #Tracks
  d = nrow(xy)
  arrows(x0 = xy[d-1,1], y0 = xy[d-1,2], x1 = xy[d,1], y1 = xy[d,2], 
         length = 0.05, angle = 25, lwd=2, col="yellow")
    map.scale.custom(267565.1, 34737.79,
                     len = 500, 
                     units = "meters", 
                     ndivs = 2, 
                     lab = c(0,250,500), 
                     tcol = "white", 
                     scol = "white",
                     cex.label = .7
    )
  
  #points(x = sleep[1,], y = sleep[2,], pch=19, col="blue", cex=.8)
  #text(cam["E"], cam["N"], labels = "Camera", cex=1.15, col="red", pos=1)
  dev.off()
}
#---------------------------Supplemental Figures--------------------------------

#data=read.table('c:/WorkSpace/Elk&ATV data.txt', header=T)
XY = c("utm.easting","utm.northing")
ssite = c(E=267433, N=35186)
dd = split(leopards)[["Tatu"]]
th = dat[[1]]$Julian[1]+c(-.5,.5)
dd2 = dd[dd$Julian>th[1] & dd$Julian<th[2],]
delx = (dd2$utm.easting - c(NA, dd2$utm.easting[-nrow(dd2)]))
dely = (dd2$utm.northing - c(NA, dd2$utm.northing[-nrow(dd2)]))
delt = dd2$lag.mins
dx = dd2$utm.easting - ssite["E"]; dy = dd2$utm.northing - ssite["N"]
ddist=sqrt( dx^2 + dy^2 )   #distance between leopard and sleeping site 
X1= c(dx/ddist, dy/ddist); X2 = 2*c(dx, dy) ; X3 = 3*ddist*c(dx, dy); X4 = 3*ddist^2*c(dx, dy)

Y=c(delx,dely)
#jday = as.integer()
Jday=c(dd2$Julian,dd2$Julian)
Ind=c(dd2$Name,dd2$Name) #Individual
Ltime=c(data$ltime,data$ltime) #Time since alarm call
ejd=interaction(ID,Jday,Ind)   #identify unique values within groups
cr3=corAR1(form=~Ltime|ejd)

ddat = data.frame(cbind(Y,X1,X2,X3,X4))
ddat = ddat[complete.cases(ddat),]
###################################################################################
#Next fit a linear mixed effect model with serial correlation to equation [8] assuming H1x=H1y=0, 
#since no significant movement between foraging and resting areas was observed during the daytime hours.
####################################################################################
library(nlme)

fit.lme= gls(Y~X1+X2+X3+X4+1, data=ddat)

#lines after summary are output of from the lme model fit
summary(fit.lme)