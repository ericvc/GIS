################################################################################
###               Script for visualizing ALL animal locations                ###
################################################################################
library(chron)
library(circular)
library(sp)
library(maptools)
library(adehabitatHR)
library(move)
library(RColorBrewer)

#Set the working directory to the cloud folder
setwd('~/Leopard Analysis/')

#Load movement locations...
load('Workspace/Analysis.R') #Loads data in an object called 'leopards'
load('Workspace/Analysis2.R') #Loads data in an object called 'leopards2'
#load('/Volumes/Lockbox/Monkeys/Monkey Workspace/') #Loads data in an object called 'monkey.move'
load('/Volumes/Lockbox/Monkeys/Monkey Workspace/Monkey.data.Rdata') #Loads data in an object called 'monkey.move'
#

#---------------leopard data---------------
d1 = leopards_UTM@data
d2 = leopards2_UTM.sub@data
dat = d1[,c("utm.easting","utm.northing","local.time","Name","state")]
dat2 = d2[,c("Easting","Northing","local.time","individual.local.identifier")]
dat2$state = NA
dat2[,1:2] = leopards2_UTM.sub@coords #Coordinates in data.frame are fucked.
names(dat2) <- names(dat)
dat3 = data.frame(rbind(droplevels(dat),droplevels(dat2)), check.names = FALSE)
dat4 = dat3[complete.cases(dat3[,1:4]),]
dat4$Name[dat4$Name=="Chumvi2"] = "Chumvi"
dat4$ID = as.character(dat4$Name)
dat5 = droplevels(dat4[dat4$ID!="Morani",])

leop_tr <- as.ltraj(xy=dat5[,c("utm.easting", "utm.northing")], infolocs = data.frame(state=dat5$state/16),
                    date=dat5$local.time, 
                    id=dat5$ID
)


refdate = strptime("2014-01-01 01:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "Africa/Nairobi")
leop_trNA = setNA(leop_tr, date.ref = refdate, dt = 0.25, units = "hour")
leop_tr0 = sett0(leop_trNA, date.ref = refdate, dt = 0.25, units = "hour")

#---------------primate data---------------

dtemp = monkey.move@data
#Define desired (most locations) individuals as belonging to each group.
dtemp$Taxon = "Chlorocebus"
dtemp$Taxon[dtemp$Group == "CLFF"] <- "Papio"
dtemp$Taxon[dtemp$Group == "MEGG"] <- "Papio"
dtemp$Taxon[dtemp$Group == "SHRT"] <- "Papio"
dtemp$Taxon[dtemp$Group == "LMPY"] <- "Papio"
dtemp$ID = monkey.move@trackId
dat6 = droplevels(dtemp[complete.cases(dtemp[,c("utm.easting","utm.northing","time", "ID","Group","Taxon")]),])

#How many missing locations?
time. = strptime(dat6$time, "%Y-%m-%d %H:%M:%S", "UTC")
time. = as.POSIXct(time., tz = "Africa/Nairobi")
monkey.tr = as.ltraj(xy=dat6[,c("utm.easting", "utm.northing")], typeII = TRUE, id = dat6$ID,
                     date=time.
)

refdate = strptime("2014-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "Africa/Nairobi")
monkey.trNA = setNA(monkey.tr, date.ref = refdate, dt = 0.25, units = "hour")
monkey.tr_t0 = sett0(monkey.trNA, date.ref = refdate, dt = 0.25, units = "hour")
monkey.sub = monkey.tr_t0[c(5,7:9,11:14,18)]

xyt1 = do.call("rbind",lapply(leop_tr0, function(z) z[1:6]))
xyt1$ID = as.character(rep(summary(leop_tr0)$id, times = summary(leop_tr0)$nb.reloc))
xyt1$Taxon = "Panthera"
xyt2 = do.call("rbind",lapply(monkey.sub, function(z) z[1:6]))
xyt2$ID = as.character(rep(summary(monkey.sub)$id, times = summary(monkey.sub)$nb.reloc))
xyt2$Taxon = "Chlorocebus"
xyt2$Taxon[xyt2$ID == "Luna"|xyt2$ID == "Msafiri"|xyt2$ID == "Shujaa"|xyt2$ID == "Yakobo"] <- "Papio"
xyt = rbind(xyt1,xyt2)
xyt$Julian = julian(xyt$date, origin = as.POSIXct("2014-01-01", tz = "Africa/Nairobi"))
time. = as.POSIXlt(xyt$date)
xyt$TimeOfDay = time.$hour + time.$min/60 + time.$sec/60^2
xyt$Jday = as.integer(xyt$Julian)
XYT = xyt[order(xyt$Jday),]
XYT$index = paste(XYT$Jday, XYT$TimeOfDay, sep="_")
XYT$hour.index = paste(XYT$Jday, as.integer(XYT$TimeOfDay), sep="_")

#-------Prepare for plotting------

#pdf("Daily_Paths_all.pdf", height=8.5, width=8.5)
data = XYT#[complete.cases(XYT),]
index = unique(data$index)
ticks = length(index)
data$clock.time = strftime(data$date, format = "%H:%M", tz = "Africa/Nairobi") #24-hour time associated with each tick
data$cal.date = strftime(data$date, format= "%d-%m-%Y", tz="Africa/Nairobi") #Current date during tick

#Diel period variables
crds = SpatialPoints(matrix(apply(XYT[,1:2],2,function(z) mean(z,na.rm=TRUE)),ncol=2), 
                     proj4string = CRS("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0"))
crds2 = spTransform(crds, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0"))
times = unique(data$date)
solarelev = solarpos(crds2, dateTime = times)[,2]
solarazim = solarpos(crds2, dateTime = times)[,1]
diel.period = rep(1,length(solarelev)) #Day
diel.period[solarelev > -18 & solarelev < 0] = 2 #astronomical twilight
diel.period[solarelev<18*-1] = 3 #Night
solarelev2  = solarelev
solarchange = c(1,solarelev[-1] - solarelev[-length(lunarillm)])
solarelev2[solarelev>=0 & solarchange<0] <- 180-(solarelev[solarelev>=0 & solarchange < 0]) #Setting (Quadrant 2)
solarelev2[solarelev<0 & solarchange<0] <- abs((solarelev[solarelev<0 & solarchange < 0])) + 180 #Setting (Quadrant 3)
solarelev2[solarelev<0 & solarchange>0] <- 360-abs((solarelev[solarelev<0 & solarchange > 0])) #Setting (Quadrant 4)
bg.colors = list(50:80/100, 30:50/100, 10:60/100)
library(oce) #For moon data
times_utc = as.POSIXlt(times, "UTC")# format="%Y-%m-%d %H:%M:%S"),tz = "UTC")
crds = coordinates(crds2)
moondata = moonAngle(times_utc, longitude = crds[1], latitude = crds[2])
lunarelev = moondata$altitude #Like 'solarelev'
lunardiam = moondata$diameter #Relative size of moon ('cex')
lunarillm = moondata$illuminatedFraction #Brightness ('alpha')
lunarchange = c(1,lunarillm[-1] - lunarillm[-length(lunarillm)])

#-Taxon plotting SYMBOL variable
data$pch.vec = rep(21, nrow(data))
data$pch.vec[data$Taxon=="Papio"] <- 22
data$pch.vec[data$Taxon=="Panthera"] <- 24

#-Taxon plotting COLOR variable
cols = brewer.pal(3,"Set2")
data$col.vec = rep(cols[1], nrow(data))
data$col.vec[data$Taxon=="Papio"] <- cols[2]
data$col.vec[data$Taxon=="Panthera"] <- cols[3]

#-Alternate ID labels
labels = rep("",nrow(data))
labels[data$ID=="Luna"] = "LI"
labels[data$ID=="Msafiri"] = "ST"
labels[data$ID=="Pimento"] = "HP"
labels[data$ID=="Golden.Gate"] = "BR"
labels[data$ID=="Kasuku"] = "FG"
labels[data$ID=="Yakobo"] = "AI"
labels[data$ID=="Meleaburu"] = "KU"
labels[data$ID=="Konda"] = "KO"
labels[data$ID=="Shujaa"] = "MG"
labels[data$ID=="Haraka"] = "HA"
labels[data$ID=="Chumvi"] = "CH"
labels[data$ID=="Colin.Tait"] = "CT"
labels[data$ID=="Tatu"] = "TA"
labels[data$ID=="Ewaso"] = "EW"
labels[data$ID=="Limofu"] = "LF"
labels[data$ID=="Mzee"] = "MZ"
data$labels = labels
lvls = unique(labels)
lab.cols = c(rep(cols[2],4),rep(cols[1],5), rep(cols[3],6))

#-Habiat use data.
#Step 1) Load data into workspace
#Step 2) Match with 'data'
leop_files = list.files("Workspace/", pattern = "stepUD_data", full.names = TRUE, recursive = TRUE)
#monk_files = list.files("~/Monkeys/Temporal Habitat/", pattern = "habitat_UD50.csv", full.names = TRUE, recursive = TRUE)
#files = c(monk_files,leop_files)
files = leop_files
varNames = c("Open", "Grass", "Cover", "River", "Roads", 
             "Human", "Glade", "GladeEdge", "Escarpment", "Lugga", "Water","Koppe")
vars = c("Julian","Name","TimeOfDay",varNames)
habitat.calc = function(x, index){
  vals = lapply(x, function(X) data.frame("value"=factor(X[,1],2:13), "weight"=X[,2]) )
  vals2 = lapply(vals, function(X) tapply(X$weight, X$value, FUN = sum))
}
datalist = list()
for(f in 1:length(files)){
  temp = read.csv(files[f],header=TRUE)

  if(any(names(temp)=="Julian")==FALSE){
    temptime = as.POSIXlt(temp$time, tz = "Africa/Nairobi")
    temp$Julian = as.numeric(julian(temptime, origin = as.POSIXct("2014-01-01", tz = "Africa/Nairobi")))
  } 
  if(any(names(temp)=="Julian")==TRUE){ 
    temptime = as.POSIXlt(temp$local.time, tz = "Africa/Nairobi")
    temp$Julian = as.numeric(julian(temptime, origin = as.POSIXct("2014-01-01", tz = "Africa/Nairobi")))
  }
  if(any(names(temp)=="Name")==FALSE) temp$Name = temp$individual.local.identifier
  if(any(names(temp)=="Koppe")==FALSE) temp$Koppe = NA
  datalist[[f]] = temp[,vars]
}
habdata = do.call("rbind",datalist)
habdata$Jday = as.integer(habdata$Julian)
habid = rep("",nrow(habdata))
habid[habdata$Name=="Luna"] = "LMPY"
habid[habdata$Name=="Msafiri"] = "SHRT"
habid[habdata$Name=="Pimento"] = "HPPO"
#habid[habdata$Name=="Golden.Gate"] = "BRDG"
#habid[habdata$Name=="Kasuku"] = "FIGG"
habid[habdata$Name=="Yakobo"] = "CLFF"
#habid[habdata$Name=="Meleaburu"] = "KUDU"
habid[habdata$Name=="Konda"] = "KNDA"
habid[habdata$Name=="Shujaa"] = "MEGG"
habid[habdata$Name=="Haraka"] = "HRKA"
habid[habdata$Name=="Chumvi"] = "CHMV"
habid[habdata$Name=="Chumvi2"] = "CHMV"
#habid[habdata$Name=="Colin.Tait"] = "CNTR"
habid[habdata$Name=="Tatu"] = "TATU"
habid[habdata$Name=="Ewaso"] = "EWSO"
habid[habdata$Name=="Limofu"] = "LMFU"
habid[habdata$Name=="Mzee"] = "MZEE"
habdata$id = habid
data$ToD = round(data$TimeOfDay*4)/4
habdata$ToD = round(habdata$TimeOfDay*4)/4
habdata$ToD[habdata$ToD==24] = 0
habindex = paste(habdata$Jday, habdata$ToD, habid,sep="_")
dataindex = paste(data$Jday, data$ToD, data$labels, sep="_")
habcols = colorRampPalette(rev(brewer.pal(8, "Set2")))(length(varNames))

matchvec = match(habindex, dataindex)
matchvec2 = matchvec[!is.na(matchvec)]
newdata <- as.matrix(habdata[,varNames])
newdata[is.na(newdata)] = 0
data[,varNames] = NA
data[matchvec2,varNames] = newdata[which(is.na(matchvec)==FALSE),]

#Circular plotting
#Custom function
circ.func = function(h,k,r,x){
  xx = h + r*cos(x)
  yy = k + r*sin(x)
  data.frame(xx,yy)
}
#Solar symbol color
solarcol = rep("white", nrow(data))
solarcol[diel.period==2] <- "gray50"
solarcol[diel.period==3] <- NA

#Lunar symbol color
lunarcol = sapply(lunarillm, function(z) col.alpha("white",z))

#Load habitat base map
base = raster("Environmental Variables/Habitat Categorical/Habitat_Classification_Cover_FULL_w_Koppe.tif")

#Load and prepare satellite imagery

# sat = raster("Maps/mpala_site_ge_CROPPED.jpg")
# extent(sat) = extent(c(36.816204,36.983602,0.219437,0.436833))
# proj4string(sat) = CRS("+init=epsg:4326")
# sat2 = projectRaster(sat, base)
#writeRaster(sat2,"Maps/Mpala_GE_site_satellite_utm.tif","GTiff")
sat2 = raster("Maps/Mpala_GE_site_satellite_utm.tif")

#--
#----------------------Version 2
#-X Add "created by" in lower corner (or with FFmpeg)
#-X Add color template below each column in habitat figure
#-X Increase size of habitat figure
#-X Remove vervets from habitat figure
#-X axis on habitat figure: increase size and make bold
#-X Gray shading along diag of distance matrix
#-X Reduce size of distance matrix
#-Consider adjusting habitat use with associated figure (+/- a time step)
#-X Moon symbol is ALWAYS shown.

#-Panthera

pp = subset(data, data$Taxon=="Panthera")

test = pp$dist<2000
boxplot(pp$dist[test] ~ pp$ToD[test])

boxplot(pp$Cover[test] ~ factor(pp$ToD[test]))


S = Sys.time()
for(step in 1:ticks){

  
  #--Plotting data--
  
  d = data[data$index==index[step],]
  newpoints = d[,c("x","y")] + t(apply(d[,c("dx","dy")],1,function(z) z/norm(matrix(z,nrow = 2))))*d$dist
  
  #--Black and white habitat map--
  fileID = paste("/Volumes/Media Drive/BW_Habitat_ver2/","bw_hab_img",formatC(step,width = 5,flag="0"),".png",sep="")
  png(fileID, width = 1920, height = 1080, units = "px", res=144)
  
  #-Base plot
  par(mar=c(0,0,0,0)+0.1, mfrow=c(1,2))
  plot(base, #asp =  1.295104,
       col=bg.colors[[ diel.period[step] ]], 
       box=FALSE, legend=FALSE, axes=FALSE
  )
  
  #-Animal positions
  #Add trail
  segments(d$x, d$y, newpoints$x, newpoints$y, col=d$col.vec, lwd=2)
  #Add locations moved to.
  points(newpoints, pch=d$pch.vec, bg=d$col.vec, cex=1.3)
  text(newpoints$x, newpoints$y, labels = d$labels, pos = 3, font=2, col="white", cex=0.5)
  
  #Date
  text(x = 272500, y = 46500, labels = unique(d$cal.date), cex=1.7, col="white", font=2)
  #Time
  text(x = 272500, y = 45500, labels = unique(d$clock.time), cex=1.5, col="white", font=2)
  
  #-Map scale
  len2 = 5000/2 #Prep--------------------------------------------------------
  xcen = 271808
  ycen = 26987
  xx <- c(c(xcen-len2,(xcen-len2)+1000), c((xcen-len2)+1000,xcen-len2) ) #for 0 - 1 km
  xx2 <- c(c((xcen-len2)+1000,xcen+len2), c(xcen+len2,(xcen-len2)+1000) )
  xx3 <- c(c(xcen+len2,xcen), c(xcen,xcen+len2))
  yy <- c(ycen-75, ycen-75, ycen+75, ycen+75)#------------------
  polygon(xx, yy, col = "black", border = "white", lwd=1.5) #Main bar (1/3)
  polygon(xx2, yy, col = "white", border = "white", lwd=1.5) #Main bar (2/3)
  polygon(xx3, yy, col = "black", border = "white", lwd=1.5) #Main bar (3/3)
  #segments((xcen-len2)+1000, ycen-10, xcen, ycen+10, lwd=2.4, col="white") #off-center tick
  text((xcen-len2)+1000, ycen-600, "1", cex=1, col="white", font=2) #off-center label
  text(xcen, ycen-600, "2.5", cex=1, col="white", font=2) #Center label
  segments(xcen-len2, ycen-100, xcen-len2, ycen+300, lwd=1.3, col="white") #Left tick
  text(xcen-len2, ycen-600, "0", cex=1, col="white", font=2) #Left label
  segments(xcen+len2, ycen-100, xcen+len2, ycen+300, lwd=1.3, col="white") #Right tick
  text(xcen+len2, ycen-600, "5", cex=1, col="white", font=2) #Right label
  text(xcen, ycen-1100, "kilometers", cex=0.7, col="white", font=2) #Units label
  
    #Solar position
    h = 259000
    k = 25987
    r = 1000
    x = seq(0,pi/2,by=pi/24)
    crds = circ.func(h,k,r,x)
    lines(crds, col="white", lwd=1.3)
    x = seq(-pi/2,0,by=pi/24)
    crds = circ.func(h,k,r,x) #points to connect
    lines(crds, col="white", lwd=1.3, lty=2) #draw circle
    pos = circ.func(h,k,r,solarelev[step]*(pi/180)) #solar point coordinate
    points(pos, pch=21, bg=solarcol[step], cex=1.6, col="white") #solar point
    text(h,k,"sun", cex=1.1, font=3, col="white") #label
    
    #Lunar position
    h = 261100
    k = 25987
    r = 1000
    x = seq(0,pi/2,by=pi/244)
    crds = circ.func(h,k,r,x)
    lines(crds, col="white", lwd=1.3)
    x = seq(-pi/2,0,by=pi/24)
    crds = circ.func(h,k,r,x) #points to connect
    lines(crds, col="white", lwd=1.3, lty=2) #draw circle
    pos = circ.func(h,k,r,lunarelev[step]*(pi/180)) #lunar point coordinate
    points(pos, pch=21, bg=lunarcol[step], cex=lunardiam[step], col="white") #lunar point
    text(h,k,"moon", cex=1.1, font=3, col="white") #label
  
  #--Add distance matrix plot
  par(mar=c(1,2.25,1,2.25)+0.1, cex.main=0.8, fig=c(0.52,.95,.54,.98), new=TRUE)

  crds = as.matrix(newpoints)
  id = factor(d$labels, levels = lvls)
  xy = matrix(data = rep(NA,2*length(lvls)), ncol=2)
  matchvec = match(id,lvls)
  xy[matchvec[!is.na(matchvec)],] = crds
  dst <- dist(xy, method = "e")
  dst3 <- dst2 <- data.matrix(dst)
  dst3[dst2==0] <- NA
  dst3[dst2>1000] <- NA
  dst4 = sprintf("%0.1f", dst3, cex=0.6)
  dst4[is.na(dst3)] <- ""
  dim <- ncol(dst2)
  ## Use n equally spaced breaks to assign each value to n-1 equal sized bins 
  ii <- cut(dst3, breaks = seq(1,1000, length.out = 20), 
            include.lowest = TRUE)
  ## Use bin indices, ii, to select color from vector of n-1 equally spaced colors
  distcols <- colorRampPalette(c("red", "blue"))(19)[ii]
  distcols = data.frame(matrix(distcols, dim,dim))
  distcols = apply(distcols,1,as.character)
  diagcolors = gray.colors(n = 8, .76, end = .90)
  diagcolors = c(diagcolors,rev(diagcolors))
  xy = expand.grid(1:dim,1:dim)[,2:1]
  plot(xy, type='n', axes=FALSE, xlab="", ylab="")
  title("Distance (m)", font=2, cex.main=0.8)
  
  #Fill grid cells with appropriate color using 'polygon' function
  for(i in 1:16){
    xx = c(i-1,i-1,i,i,i-1) + 0.5
    for(j in 1:16){
      yy = c(j-1,j,j,j-1,j-1) + 0.5
      polygon(xx,yy, col = ifelse(i==j, diagcolors[i],distcols[i,j]), border = NA)
    }
  }
  
  #Add axes and labels.
  text(xy, labels = dst4, cex=0.67, col="white", font=2)
  axis(1, tick = FALSE, 1:dim, lvls, cex.axis = 0.7, las=3, font=2)
  axis(2, tick = FALSE, 1:dim, lvls, cex.axis = 0.7, las=1, font=2)
  
  #Add general grid lines
  ys = xs = 0:16+0.5
  for(l in 1:17){ 
    segments(0, ys[l], 16.5, ys[l], lwd=.2)
    segments(xs[l], 0, xs[l], 16.5, lwd=.2)
  }

  #-Basic data plot
  par(fig=c(.2,.98,.01,.70), new=TRUE, mar=c(0,0,0,0))
  vars = c("x","y","dist")
  temp = matrix(NA, nrow = length(lvls), ncol=length(vars))
  matchvec = match(d$labels, lvls)
  temp[matchvec,] = as.matrix(d[,vars])
  temp2 <- round(data.frame(temp, row.names=lvls),2)
  temp2[-matchvec,] <- "-"
  names(temp2) <- c("Easting", "Northing", "Step_dist_m")
  plot(1,type="n",axes=FALSE,ylab="",xlab="")
  text(1,1, paste(capture.output(temp2), collapse='\n'), family="mono", pos=1, cex=.75, font=2)
  
  #-Habitat use plot
  
  toInclude = c("CLFF", "LMPY", "MEGG", "SHRT", "CHMV", "EWSO", "HRKA", "KNDA", "LMFU", "MZEE", "TATU")
  matchvec2 = match(d$labels[d$labels%in%toInclude], toInclude)
  
  par(fig=c(.75,1,0,.42), new=TRUE, mar=c(0,1,1,0.1))
  plot(xy, type='n', axes=FALSE, xlab="", ylab="", ylim=c(16,0.5))
  #title("Habitat", font=2, cex.main=0.8)
  
  habuse = matrix(data = rep(NA,length(varNames)*length(toInclude)), ncol=length(varNames), nrow=length(toInclude))
  datamat = as.matrix(d[which(d$labels%in%toInclude),varNames])
  habuse[matchvec2,] = datamat
  alphamat = habuse
  alphamat[is.na(habuse)] <- 0
  colmat = habuse
  colmat[] <- rep(habcols, each = length(toInclude))
  colmat2 = colmat[]
  for(rows in 1:nrow(colmat2)){
    for(columns in 1:ncol(colmat2)){
      colmat2[rows,columns] = col.alpha(colmat[rows,columns], alphamat[rows,columns])
    }
  }
  
  #Fill grid cells with appropriate color using 'polygon' function
  for(habs in 1:(length(toInclude)+1)){
   yy = c(habs-1,habs,habs,habs-1,habs-1) + 0.5
    for(indv in 1:length(toInclude)){
      xx = c(indv-1,indv-1,indv,indv,indv-1) + 0.5
      alpha = !is.na(habuse[indv,habs])
      polygon(yy,xx, col = colmat2[indv,habs], border = NA)
    }
  }
  
  #Add axes and labels.
  #text(1:12, rep(11,hab), labels = dst4, cex=0.75, col="white", font=2, srt=45)
  axis(3, tick = FALSE, 1:length(varNames), varNames, cex.axis = 0.7, las=2, font=2, hadj = .25)
  axis(2, tick = FALSE, 1:length(toInclude), toInclude, cex.axis = 0.65, las=1, font=2)
  
  #Add general grid lines
  ys = 0:11+0.5
  xs = 0:13+0.5
  for(l in 1:(length(toInclude)+1)){ 
    segments(0, ys[l], 12.5, ys[l], lwd=.2) #Horizontal lines
  }
  for(e in 1:13){
    segments(xs[e], 0, xs[e], 11.5, lwd=.2) #Vertical lines
  }
  
  #Add comparison template below habitat plot
  hab = length(varNames)
  par(fig=c(.75,1,.05,.1), new=TRUE, mar=c(0,2,2,0.1))
  ycrds = rep(c(0,10,20), each = hab)
  xcrds = 1:hab
  points(xcrds,ycrds[(2*hab+1):(3*hab)], pch=15, col=sapply(habcols, function(z) col.alpha(z,0.25)), cex=2.5) #25 %
  points(xcrds,ycrds[(hab+1):(2*hab)], pch=15, col=sapply(habcols, function(z) col.alpha(z,0.50)), cex=2.5) #50 %
  points(xcrds, ycrds[1:hab], pch=15, col=habcols, cex=2.5) #100 %
  text(c(13.5,13.5,13.5), c(0,10,20), c("100%","50%","25%"), cex=.75)
  #title("Habitat", font=2, cex.main=0.8)
  
  
  dev.off()
  
#   #--Black and white satellite map--
#   #-Base plot
#   fileID = paste("/Volumes/Media Drive/BW_Satellite/","bw_sat_img",formatC(step,width = 5,flag="0"),".png",sep="")
#   png(fileID, width = 1920, height = 1080, units = "px", res=144)
#   par(mar=c(0,0,0,0)+0.1, mfrow=c(1,2))
#   plot(sat2,
#        col=bg.colors[[ diel.period[step] ]], 
#        box=FALSE, legend=FALSE, axes=FALSE
#   )
#   
#   #-Animal positions
#   #Add trail
#   segments(d$x, d$y, newpoints$x, newpoints$y, col=d$col.vec, lwd=2)
#   #Add locations moved to.
#   points(newpoints, pch=d$pch.vec, bg=d$col.vec, cex=1.3)
#   text(newpoints$x, newpoints$y, labels = d$labels, pos = 3, font=2, col="white", cex=0.5)
#   
#   #Date
#   text(x = 272500, y = 46500, labels = unique(d$cal.date), cex=1.4, col="white", font=2)
#   #Time
#   text(x = 272500, y = 45500, labels = unique(d$clock.time), cex=1.3, col="white", font=2)
#   
#   #-Map scale
#   len2 = 5000/2 #Prep--------------------------------------------------------
#   xcen = 271808
#   ycen = 26987
#   xx <- c(c(xcen-len2,(xcen-len2)+1000), c((xcen-len2)+1000,xcen-len2) ) #for 0 - 1 km
#   xx2 <- c(c((xcen-len2)+1000,xcen+len2), c(xcen+len2,(xcen-len2)+1000) )
#   xx3 <- c(c(xcen+len2,xcen), c(xcen,xcen+len2))
#   yy <- c(ycen-75, ycen-75, ycen+75, ycen+75)#------------------
#   polygon(xx, yy, col = "black", border = "white", lwd=1.5) #Main bar (1/3)
#   polygon(xx2, yy, col = "white", border = "white", lwd=1.5) #Main bar (2/3)
#   polygon(xx3, yy, col = "black", border = "white", lwd=1.5) #Main bar (3/3)
#   #segments((xcen-len2)+1000, ycen-10, xcen, ycen+10, lwd=2.4, col="white") #off-center tick
#   text((xcen-len2)+1000, ycen-600, "1", cex=1, col="white", font=2) #off-center label
#   text(xcen, ycen-600, "2.5", cex=1, col="white", font=2) #Center label
#   segments(xcen-len2, ycen-100, xcen-len2, ycen+300, lwd=1.3, col="white") #Left tick
#   text(xcen-len2, ycen-600, "0", cex=1, col="white", font=2) #Left label
#   segments(xcen+len2, ycen-100, xcen+len2, ycen+300, lwd=1.3, col="white") #Right tick
#   text(xcen+len2, ycen-600, "5", cex=1, col="white", font=2) #Right label
#   text(xcen, ycen-1100, "kilometers", cex=0.7, col="white", font=2) #Units label
#   
#   #Solar position
#   h = 259000
#   k = 25987
#   r = 1000
#   x = seq(0,pi/2,by=pi/24)
#   crds = circ.func(h,k,r,x)
#   lines(crds, col="white", lwd=1.3)
#   x = seq(-pi/2,0,by=pi/24)
#   crds = circ.func(h,k,r,x) #points to connect
#   lines(crds, col="white", lwd=1.3, lty=2) #draw circle
#   pos = circ.func(h,k,r,solarelev[step]*(pi/180)) #solar point coordinate
#   points(pos, pch=21, bg=solarcol[step], cex=1.6, col="white") #solar point
#   text(h,k,"sun", cex=1.1, font=3, col="white") #label
#   
#   #Lunar position
#   h = 261100
#   k = 25987
#   r = 1000
#   x = seq(0,pi/2,by=pi/244)
#   crds = circ.func(h,k,r,x)
#   lines(crds, col="white", lwd=1.3)
#   x = seq(-pi/2,0,by=pi/24)
#   crds = circ.func(h,k,r,x) #points to connect
#   lines(crds, col="white", lwd=1.3, lty=2) #draw circle
#   pos = circ.func(h,k,r,lunarelev[step]*(pi/180)) #lunar point coordinate
#   points(pos, pch=21, bg=lunarcol[step], cex=lunardiam[step], col="white") #lunar point
#   text(h,k,"moon", cex=1.1, font=3, col="white") #label
#   
#   #--Add distance matrix plot
#   par(mar=c(1,2.25,1,2.25)+0.1, cex.main=0.8, fig=c(0.52,.98,.52,.98), new=TRUE)
#   
#   crds = as.matrix(newpoints)
#   id = factor(d$labels, levels = lvls)
#   xy = matrix(data = rep(NA,2*length(lvls)), ncol=2)
#   matchvec = match(id,lvls)
#   xy[matchvec[!is.na(matchvec)],] = crds
#   dst <- dist(xy, method = "e")
#   dst3 <- dst2 <- data.matrix(dst)
#   dst3[dst2==0] <- NA
#   dst3[dst2>1000] <- NA
#   dst4 = sprintf("%0.1f", dst3, cex=0.6)
#   dst4[is.na(dst3)] <- ""
#   dim <- ncol(dst2)
#   ## Use n equally spaced breaks to assign each value to n-1 equal sized bins 
#   ii <- cut(dst3, breaks = seq(1,1000, length.out = 20), 
#             include.lowest = TRUE)
#   ## Use bin indices, ii, to select color from vector of n-1 equally spaced colors
#   distcols <- colorRampPalette(c("red", "blue"))(19)[ii]
#   distcols = data.frame(matrix(distcols, dim,dim))
#   distcols = apply(distcols,1,as.character)
#   xy = expand.grid(1:dim,1:dim)[,2:1]
#   plot(xy, type='n', axes=FALSE, xlab="", ylab="")
#   title("Distance (m)", font=2, cex.main=0.8)
#   
#   #Fill grid cells with appropriate color using 'polygon' function
#   for(i in 1:16){
#     xx = c(i-1,i-1,i,i,i-1) + 0.5
#     for(j in 1:16){
#       yy = c(j-1,j,j,j-1,j-1) + 0.5
#       polygon(xx,yy, col = ifelse(i==j,"gray93",distcols[i,j]), border = NA)
#     }
#   }
#   
#   #Add axes and labels.
#   text(xy, labels = dst4, cex=0.75, col="white", font=2)
#   axis(1, tick = FALSE, 1:dim, lvls, cex.axis = 0.65, las=3, font=2)
#   axis(2, tick = FALSE, 1:dim, lvls, cex.axis = 0.65, las=1, font=2)
#   
#   #Add general grid lines
#   ys = xs = 0:16+0.5
#   for(l in 1:17){ 
#     segments(0, ys[l], 16.5, ys[l], lwd=.2)
#     segments(xs[l], 0, xs[l], 16.5, lwd=.2)
#   }
#   
#   #-Basic data plot
#   par(fig=c(.2,.98,.01,.75), new=TRUE, mar=c(0,0,0,0))
#   vars = c("x","y","dist")
#   temp = matrix(NA, nrow = length(lvls), ncol=length(vars))
#   matchvec = match(d$labels,lvls)
#   temp[matchvec[!is.na(matchvec)],] = as.matrix(d[,vars])
#   temp2 <- round(data.frame(temp, row.names=lvls),2)
#   temp2[-matchvec,] <- "-"
#   names(temp2) <- c("Easting", "Northing", "step_len_m")
#   plot(1,type="n",axes=FALSE,ylab="",xlab="")
#   text(1,1, paste(capture.output(temp2), collapse='\n'), family="mono", pos=1, cex=.93, font=2)
#   
#   #-Habitat use plot
#   
#   par(fig=c(.72,.99,.01,.42), new=TRUE, mar=c(3,3,3,1))
#   plot(xy, type='n', axes=FALSE, xlab="", ylab="", ylim=c(16,0.5))
#   #title("Habitat", font=2, cex.main=0.8)
#   
#   id = factor(d$labels, levels = lvls)
#   habuse = matrix(data = rep(NA,length(varNames)*length(lvls)), ncol=length(varNames), nrow=length(lvls))
#   matchvec = match(id,lvls)
#   datamat = as.matrix(d[,varNames])
#   habuse[matchvec[!is.na(matchvec)],] = datamat
#   alphamat = habuse
#   alphamat[is.na(habuse)] <- 0
#   colmat = habuse
#   colmat[] <- rep(habcols, each = length(lvls))
#   colmat2 = colmat[]
#   for(rows in 1:nrow(colmat2)){
#     for(columns in 1:ncol(colmat2)){
#       colmat2[rows,columns] = col.alpha(colmat[rows,columns], alphamat[rows,columns])
#     }
#   }
#   
#   #Fill grid cells with appropriate color using 'polygon' function
#   for(hab in 1:12){
#     yy = c(hab-1,hab,hab,hab-1,hab-1) + 0.5
#     for(indv in 1:16){
#       xx = c(indv-1,indv-1,indv,indv,indv-1) + 0.5
#       alpha = !is.na(habuse[indv,hab])
#       polygon(yy,xx, col = colmat2[indv,hab], border = NA)
#     }
#   }
#   
#   #Add axes and labels.
#   #text(xy, labels = dst4, cex=0.75, col="white", font=2)
#   axis(3, tick = FALSE, 1:length(varNames), varNames, cex.axis = 0.65, las=2, font=2, hadj = .25)
#   axis(2, tick = FALSE, 1:length(lvls), lvls, cex.axis = 0.65, las=1, font=2)
#   
#   #Add general grid lines
#   ys = 0:16+0.5
#   xs = 0:12+0.5
#   for(l in 1:17){ 
#     segments(0, ys[l], 12.5, ys[l], lwd=.2)
#     segments(xs[l], 0, xs[l], 16.5, lwd=.2)
#   }
# 
# dev.off()
#   
}
#-------Prepare plots for video formatting------
library(stringr)
library(lattice)

datevec = paste(data$date)
m = substring(datevec,6,7)
y = substring(datevec,1,4)
dayy = round(as.numeric(days(data$date))*.15)/.15
my = paste(y,m,sep="_")
my2 = paste(y,m,sep="/")
umy = unique(my)
umy2 = unique(my2)
obs = table(data$index, my2)
obs[obs>0] <- 1
obssum = apply(obs,2,sum)
filevec = obssum[order(names(obssum))]


# ffmpeg_commands = paste("ffmpeg -framerate 1/1 -start_number ", startnumber," -v"," ",totframes," ", 
#                         "-f image2 -s 640x480 -i bw_hab_img%05d.png -vcodec libx264 -crf 25 -pix_fmt yuv420p  bw_habitat_",umy[u],
#                         "_projectvis_ver1_012716_HD.mp4", 
#                         sep="")

#-BW Habitat Map
bw_hab_dir = "/Volumes/Media Drive/BW_Habitat_ver2/"
bw_hab_files = list.files(bw_hab_dir, pattern = ".png")
for(u in 1:length(umy)){
  startnumber = (cumsum(filevec)-filevec + 1)[u]
  totframes = filevec[u]
  frameRate = min(900/filevec) #15 minute time video limit (900 sec)
  ffmpeg_commands = paste("ffmpeg -framerate 1/",frameRate," -start_number ", startnumber," -i bw_hab_img%05d.png",
                          " -vframes ", totframes," -c:v libx264 -pix_fmt yuv420p bw_habitat_",umy[u],
                          "_projectvis_ver2_020316_HD.mp4", 
                          sep="")
 
  encode_string = paste( #Pastes together directory change and video encoding commands.
    "cd /Volumes/Med*/BW_Habitat_ver2/",
    ffmpeg_commands
    , sep="\n")
  
system(command = encode_string) #Run commands in terminal.

# #Generate video companion files (defunct).
# pdfFile = paste(bw_hab_dir,"/",umy[u],"_companion_figure.pdf",sep="")
# #pdfFile = "~/Desktop/TEST.pdf"
# mdata = data[fileNum,]
# labels. = factor(mdata$labels, levels = lvls, ordered = TRUE)
# numlabels = rep(NA, length(labels.))
# for(qq in 1:length(lvls)) numlabels[which(lvls[qq]==labels.)] <- qq
# daysOfMonth = paste(days(data$date[fileNum]))
# lday = as.numeric(daysOfMonth[which.max(daysOfMonth)])
# tickmarks.dayaxis = as.integer(seq(1,lday,length.out = 5))
# vidlength = totframes/60
# tickmarks.videoaxis = seq(1,totframes,length.out = 5)
# labels.videoaxis = round(tickmarks.videoaxis/60)
# tickmarks.videoaxis2 = seq(1,lday, length.out = 5)
# 
# pdf(pdfFile, width = 8.5/2, height = 11/2)
# par(fig=c(0,1,0.2,1), cex.main=2, mar=c(5,5,7,2), cex.lab=1.2, cex.axis=1.1)
# tab = table(labels., mdata$Julian)
# plot(numlabels ~ mdata$Julian, axes=FALSE, ylab="", xlab="Day of Month", xlim=c(0,lday), main=umy2[u])
# axis(1, at = tickmarks.dayaxis)
# axis(2, at = 1:16, labels = lvls, las=2)
# par(fig=c(0,1,0.01,0.20), mar=c(5,0,0,0)+0.1, cex.axis=1.2, cex.lab=1.2)
# axis(1, at=tickmarks.videoaxis2, labels = labels.videoaxis)
# mtext("Minute in Slideshow",3)
# dev.off()

}

#-BW Satellite Map
bw_sat_dir = "/Volumes/Shoebox/BW_Satellite"
bw_sat_files = list.files(bw_sat_dir, pattern = ".png")
datevec = paste(data$date)
m = substring(datevec,6,7)
y = substring(datevec,1,4)
dayy = round(as.numeric(days(data$date))*.15)/.15
my = paste(y,m,sep="_")
my2 = paste(y,m,sep="/")
umy = unique(my)
umy2 = unique(my2)
obs = table(data$index, my2)
obs[obs>0] <- 1
obssum = apply(obs,2,sum)
filevec = obssum[order(names(obssum))]
#ffmpeg -framerate 1/1 -i bw_hab_img%05d.png -c:v libx264 -pix_fmt yuv420p bw_habitat_projectvis_ver1_012716.mp4#-c:v 

for(u in 1:length(umy)){
  startnumber = (cumsum(filevec)-filevec + 1)[u]
  totframes = filevec[u]
  frameRate = min(900/filevec) #15 minute time video limit (900 sec)
  ffmpeg_commands = paste("ffmpeg -framerate 1/",frameRate," -start_number ", startnumber," -i bw_sat_img%05d.png",
                          " -vframes ", totframes," -c:v libx264 -pix_fmt yuv420p bw_satellite_",umy[u],
                          "_projectvis_ver2_020316_HD.mp4", 
                          sep="")
  
  encode_string = paste( #Pastes together directory change and video encoding commands.
    "cd /Volumes/Shoebox/BW_Satellite/",
    ffmpeg_commands
    , sep="\n")
  
  system(command = encode_string) #Run commands in terminal.
  
  # #Generate video companion files (defunct).
  # pdfFile = paste(bw_sat_dir,"/",umy[u],"_companion_figure.pdf",sep="")
  # #pdfFile = "~/Desktop/TEST.pdf"
  # mdata = data[fileNum,]
  # labels. = factor(mdata$labels, levels = lvls, ordered = TRUE)
  # numlabels = rep(NA, length(labels.))
  # for(qq in 1:length(lvls)) numlabels[which(lvls[qq]==labels.)] <- qq
  # daysOfMonth = paste(days(data$date[fileNum]))
  # lday = as.numeric(daysOfMonth[which.max(daysOfMonth)])
  # tickmarks.dayaxis = as.integer(seq(1,lday,length.out = 5))
  # vidlength = totframes/60
  # tickmarks.videoaxis = seq(1,totframes,length.out = 5)
  # labels.videoaxis = round(tickmarks.videoaxis/60)
  # tickmarks.videoaxis2 = seq(1,lday, length.out = 5)
  # 
  # pdf(pdfFile, width = 8.5/2, height = 11/2)
  # par(fig=c(0,1,0.2,1), cex.main=2, mar=c(5,5,7,2), cex.lab=1.2, cex.axis=1.1)
  # tab = table(labels., mdata$Julian)
  # plot(numlabels ~ mdata$Julian, axes=FALSE, ylab="", xlab="Day of Month", xlim=c(0,lday), main=umy2[u])
  # axis(1, at = tickmarks.dayaxis)
  # axis(2, at = 1:16, labels = lvls, las=2)
  # par(fig=c(0,1,0.01,0.20), mar=c(5,0,0,0)+0.1, cex.axis=1.2, cex.lab=1.2)
  # axis(1, at=tickmarks.videoaxis2, labels = labels.videoaxis)
  # mtext("Minute in Slideshow",3)
  # dev.off()
  
}

#-DEMO BW Habitat Map
bw_hab_dir = "/Volumes/Shoebox/BW_Habitat_ver2/"
blur_data = ' -filter_complex "[0:v]split=2[v0][v1]; [v0]crop=200:400:999:640,boxblur=10[fg]; [v1][fg]overlay=999:640[v]" -map "[v]" -map 0:a? -c:v libx264 -c:a copy -movflags +faststart '
blur_date = ' -filter_complex "[0:v]split=2[v0][v1]; [v0]crop=225:45:625:60,boxblur=10[fg]; [v1][fg]overlay=625:60[v]" -map "[v]" -map 0:a? -c:v libx264 -c:a copy -movflags +faststart '
for(u in 1:length(umy)){
  startnumber = (cumsum(filevec)-filevec + 1)[u]
  totframes = filevec[u]
  frameRate = min(900/filevec) #15 minute time video limit (900 sec)
  ffmpeg_commands = paste("ffmpeg -framerate 1/",frameRate," -start_number ", startnumber," -i bw_hab_img%05d.png ",
                          #blur_date,
                          blur_data,
                          " -vframes ", totframes," -c:v libx264 -pix_fmt yuv420p DEMO_bw_habitat_",
                          umy[u],
                          "_projectvis_ver2_020316_HD.mp4", 
                          sep="")
  
  encode_string = paste( #Pastes together directory change and video encoding commands.
    "cd /Volumes/Lockbox/BW_Habitat_ver2/",
    ffmpeg_commands
    , sep="\n")
  
  system(command = encode_string) #Run commands in terminal.
  
  # #Generate video companion files (defunct).
  # pdfFile = paste(bw_hab_dir,"/",umy[u],"_companion_figure.pdf",sep="")
  # #pdfFile = "~/Desktop/TEST.pdf"
  # mdata = data[fileNum,]
  # labels. = factor(mdata$labels, levels = lvls, ordered = TRUE)
  # numlabels = rep(NA, length(labels.))
  # for(qq in 1:length(lvls)) numlabels[which(lvls[qq]==labels.)] <- qq
  # daysOfMonth = paste(days(data$date[fileNum]))
  # lday = as.numeric(daysOfMonth[which.max(daysOfMonth)])
  # tickmarks.dayaxis = as.integer(seq(1,lday,length.out = 5))
  # vidlength = totframes/60
  # tickmarks.videoaxis = seq(1,totframes,length.out = 5)
  # labels.videoaxis = round(tickmarks.videoaxis/60)
  # tickmarks.videoaxis2 = seq(1,lday, length.out = 5)
  # 
  # pdf(pdfFile, width = 8.5/2, height = 11/2)
  # par(fig=c(0,1,0.2,1), cex.main=2, mar=c(5,5,7,2), cex.lab=1.2, cex.axis=1.1)
  # tab = table(labels., mdata$Julian)
  # plot(numlabels ~ mdata$Julian, axes=FALSE, ylab="", xlab="Day of Month", xlim=c(0,lday), main=umy2[u])
  # axis(1, at = tickmarks.dayaxis)
  # axis(2, at = 1:16, labels = lvls, las=2)
  # par(fig=c(0,1,0.01,0.20), mar=c(5,0,0,0)+0.1, cex.axis=1.2, cex.lab=1.2)
  # axis(1, at=tickmarks.videoaxis2, labels = labels.videoaxis)
  # mtext("Minute in Slideshow",3)
  # dev.off()
  
}
