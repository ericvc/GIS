####                                                                        ####
####          Activity Patterns and Habitat Selection in Leopards           ####
####                                                                        ####
####                                                                        ####

####(1)                      Set Up The Workspace                           ####
#-Load packages
#GIS
library(raster)
library(maptools)
library(rgdal)
library(spatstat)
#Stats/Plotting
library(HDInterval)
library(ggplot2)

#-Set the working directory.

# load step reference data
setwd("/Volumes/Lockbox/Leopard Analysis/")
useByStep = readRDS("Chapter 1/data/cover+class+by+step.rdata")
d5 = readRDS("Chapter 1/data/stepdata+state.rdata")

# load cell use reference data
useByCell = read.csv("Leopard+Space+Use+by+CellNum_n=7.csv")
useByCell$Name = as.character(useByCell$Name) #factor -> character
useByCell$Name[useByCell$Name=="Chumvi2"] = "Chumvi"

# load raster data
hab = raster("Environmental Variables/Habitat Categorical/Habitat_Classification_Cover_FULL_v2.tif")
cover = raster("Environmental Variables/Local Predictors/percent_cover_full.tif")
cover2 = raster(cover)
cover2[] = cut(cover[], seq(0,1,by=0.1))

# load 95% home range polygons
hrPolyFiles = list.files("Workspace/Shapefiles/MKDE Home Ranges/", 
                         pattern = "95_home_range.shp", full.names = TRUE)[c(1,3,4,7)] #Exclude morani
hrPolys = sapply(hrPolyFiles, readShapePoly)

# extract the percent availability of each habitat class per individual
percHR = lapply(hrPolys, function(x) prop.table(table(raster::extract(hab, x))))

#===============================================================================
#----------------Selection for Woody Cover by Behavioral State------------------
#===============================================================================

#-------------------------------------------------------------------------------
#----------------------Overall Selection for Woody Cover------------------------

use = droplevels(useByCell[useByCell$Name=="Chumvi"|useByCell$Name=="Haraka"
                           |useByCell$Name=="Konda"|useByCell$Name=="Tatu",])
hrs = hrPolys

coverUsed = extract(cover2, use$cell)
coverUsedByCell = tapply(use$weight, list(coverUsed, use$Name), sum)
coverUsedByCell = coverUsedByCell
coverUsedPerc = apply(coverUsedByCell,2,function(x) x / sum(x, na.rm=TRUE) )

coverBootSamples = apply(coverUsedPerc, 1, 
                         function(x) replicate(500, mean(sample(x, size = 7, replace = TRUE))))
coverBootMean = apply(coverBootSamples,2,mean)
coverBootHdi = apply(coverBootSamples,2,function(x) hdi(x,0.9))

## extract the percent availability of each cover class per individual
percHR = do.call(cbind,lapply(hrs, 
                              function(x) prop.table(table(raster::extract(cover2, x)))))
sr = coverUsedPerc / percHR
srBootSamples = apply(sr, 1, 
                      function(x) replicate(500, mean(sample(x, size = 7, replace = TRUE))))
srBootMean = apply(srBootSamples,2,mean)
srBootHdi = apply(srBootSamples,2,function(x) hdi(x,0.9))

#-------------------------------------------------------------------------------
#---------------------Selection for Woody Cover by State------------------------

#-Selection of cover by behavioral state

# stepUDs = sapply(d5$stepFile,
#     function(x){
#       tkml <- getKMLcoordinates(kmlfile=paste(x), ignoreAltitude=TRUE)
#       #make polygon
#       p1 = Polygon(tkml[[1]])
#       #make Polygon class
#       p2 = Polygons(list(p1), ID = paste(x))
#       #make spatial polygons class
#       p3= SpatialPolygons(list(p2),proj4string=CRS("+init=epsg:4326"))
#       p4 = spTransform(p3, proj4string(cover2))
#       return(p4)
#     }
#  )
# useByStep = sapply(stepUDs, function(x){
# 
#   ex <- extract(cover2, x, weights=TRUE)[[1]]
#   tapply(ex[,2], factor(ex[,1], 1:10), sum)
# 
#    }
# )
# saveRDS(useByStep, "Chapter 1/cover+class+by+step.rdata")
useByStep2 = t(useByStep)
useByStep2[is.na(useByStep2)] = 0

useByState = apply(useByStep2,2,function(x) tapply(x, list(paste(d5$Name, d5$state)), sum))
useByStateProp = prop.table(useByState,1)
percHR.expanded = t(percHR)[rep(1:4,each=4),]
srByState = useByStateProp / percHR.expanded

srByState2 = srByState[order(rep(1:4,times=4)),]
srByStateList = list()
srByStateList[[1]] = srByState2[1:4,]
srByStateList[[2]] = srByState2[5:8,]
srByStateList[[3]] = srByState2[9:12,]
srByStateList[[4]] = srByState2[13:16,]

srByStateBootMean = lapply(srByStateList,
                           function(x)
                             apply(x, 2, function(z)
                               mean( replicate(500, mean(
                                 sample(z, size = 7, replace = TRUE)
                               )))) )

useByState2 = useByStateProp[order(rep(1:4,times=4)),]
useByStateList = list()
useByStateList[[1]] = useByState2[1:4,]
useByStateList[[2]] = useByState2[5:8,]
useByStateList[[3]] = useByState2[9:12,]
useByStateList[[4]] = useByState2[13:16,]
useByStateBootMean = lapply(useByStateList,
                            function(x)
                              apply(x[,-c(9:10)], 2, function(z)
                                mean( replicate(500, mean(
                                  sample(z, size = 7, replace = TRUE)
                                )))) )

d5.2 = data.frame(srByStateBootMean)
# d5.3 = d5.2[-c(1,10),]
names(d5.2) = c("Rest", "Encamped", "Search", "Transit")
d5.2$Overall = srBootMean
d5.2$perc.cover = c(0:9) * 10
#Define plotting options
statecols = c(REST="#30BFC7", ENCAMPED="#7F5BD9", SEARCH="#DE8A3C", TRANSIT="#8FC248")
#As a line plot
pdf(
  "Chapter 1/figures/cover+selection+by+state+lineplot.pdf",
  #res = 72,
  width = 10,
  height = 10#,
  #units = "in"
)
bckGrndCol = "gray95"
cols = c(statecols, Overall = "gray15")
P = ggplot(d5.2[-c(1,10),], aes(x = perc.cover))
P +
  geom_line(aes(y = Rest), colour=cols[1], size = 2, show.legend=FALSE) +
  geom_line(aes(y = Encamped), colour=cols[2], size = 2, show.legend=FALSE) +
  geom_line(aes(y = Search), colour=cols[3], size = 2, show.legend=FALSE) +
  geom_line(aes(y = Transit), colour=cols[4], size = 2, show.legend=FALSE) +
  geom_line(aes(y = Overall), colour=cols[5], size = 2.6, linetype = 6, show.legend=FALSE) +
  scale_fill_manual(values = cols,
                    name = "Movement State") +
  geom_hline(yintercept = 1,
             size = 0.4,
             linetype = 2) +
  labs(x = "% Woody Cover", y = "Selection Ratio") +
  theme_bw() +
  theme(
    text = element_text(size = 28, family = "Times"),
    plot.margin = unit(c(2.5, 1.5, 1.5, 1.5) - 1.3, units = "in"),
    panel.grid.major.x = element_line(colour = bckGrndCol, lineend = 3),
    panel.grid.major.y = element_line(colour = bckGrndCol, size = 0),
    panel.grid.minor.y = element_line(colour = "white", size = 0),
    panel.background = element_rect(fill = bckGrndCol),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    plot.title = element_text(
      size = 32,
      face = "bold",
      family = "Times",
      hjust = 0.5,
      vjust = 10
    ),
    axis.text.x = element_text(angle = 0, size = 28),
    axis.text.y = element_text(angle = 0, size = 30),
    axis.title.x = element_text(size = 28, vjust = 0.2, face = "bold"),
    axis.title.y = element_text(size = 28, vjust = 3, face = "bold"),
    legend.title = element_text(
      size = 18,
      face = "bold",
      vjust = -3
    ),
    legend.box  = "vertical"
  ) +
  ggtitle(label = "Selection for Woody Cover\nby Movement State")
dev.off()

#===============================================================================
#--------------Selection for Woody Cover EDGE by Behavioral State---------------
#===============================================================================
#-create raster layer measuring the distance to the edge of woody cover
# edge = raster(cover)
# edge = setValues(edge, values = 0)
# edge[hab[]==4] <- 1
# edgeLines = rasterToContour(edge, level=1)
# # lineCrds = cellFromLine(edge, edgeLines)[[1]]
# # edge2 = raster(edge)
# # edge2[lineCrds] <- 1
# 
# #-Create proximity rasters
# #raster cell point patter
# rPPP <- as.ppp(xyFromCell(edge, cell=1:ncell(edge), spatial=TRUE))
# 
# #proximity to cover edge
# cells = do.call('c', cellFromLine(edge, edgeLines))
# coverXY <- as.ppp(xyFromCell(edge, cell=cells, spatial=TRUE))
# coverDist = raster(edge)
# #coverDist[cells] = 0
# distances <- nncross(X=rPPP, Y=coverXY)
# values(coverDist) <- distances$dist
# coverDist[hab[]==4] <- coverDist[hab[]==4]*-1
# 
# writeRaster(coverDist, 
#             filename = "Environmental Variables/Local Predictors/distance+to+cover+edge.tif", 
#             overwrite=TRUE)
edge = raster("Environmental Variables/Local Predictors/distance+to+cover+edge.tif")
plot(edge, col=terrain.colors(n=255)) #looks good.

#-------------------------------------------------------------------------------
#----------------------Overall Selection for Cover Edge-------------------------
edge2 = raster(edge)
edge2[] <- 0
edge2[hab[]==4] <- 1
defCells = which(edge[]<0 & edge[]>-100)
edge2[defCells] <- 2
plot(edge2, col=terrain.colors(n=255)) #looks good.
use = droplevels(useByCell[useByCell$Name=="Chumvi"|useByCell$Name=="Haraka"
                           |useByCell$Name=="Konda"|useByCell$Name=="Tatu",])
hrs = hrPolys

coverUsed = extract(edge2, use$cell)
coverUsedByCell = tapply(use$weight, list(coverUsed, use$Name), sum)
coverUsedByCell = coverUsedByCell
coverUsedPerc = apply(coverUsedByCell,2,function(x) x / sum(x, na.rm=TRUE) )

coverBootSamples = apply(coverUsedPerc, 1,
                         function(x) replicate(500, mean(sample(x, size = 7, replace = TRUE))))
coverBootMean = apply(coverBootSamples,2,mean)
coverBootHdi = apply(coverBootSamples,2,function(x) hdi(x,0.9))

## extract the percent availability of each cover class per individual
percHR = do.call(cbind,lapply(hrs,
                              function(x) prop.table(table(raster::extract(edge2, x)))))
#index = as.numeric(attributes(coverUsedPerc)$dimnames[[1]]) %in% as.numeric(attributes(percHR)$dimnames[[1]])
sr = coverUsedPerc / percHR
sr

srBootSamples = apply(sr2, 1,
                      function(x) replicate(500, mean(sample(x, size = 7, replace = TRUE))))
srBootMean = apply(srBootSamples,2,mean)
srBootHdi = apply(srBootSamples,2,function(x) hdi(x,0.9))

#-------------------------------------------------------------------------------
#---------------------Selection for Cover Edge by State------------------------

# #-Selection of cover by behavioral state
# 
# stepUDs = sapply(d5$stepFile,
#     function(x){
#       tkml <- getKMLcoordinates(kmlfile=paste(x), ignoreAltitude=TRUE)
#       #make polygon
#       p1 = Polygon(tkml[[1]])
#       #make Polygon class
#       p2 = Polygons(list(p1), ID = paste(x))
#       #make spatial polygons class
#       p3= SpatialPolygons(list(p2),proj4string=CRS("+init=epsg:4326"))
#       p4 = spTransform(p3, proj4string(cover2))
#       return(p4)
#     }
#  )
# useByStep = sapply(stepUDs, function(x){
# 
#   ex <- extract(edge2, x, weights=TRUE)[[1]]
#   tapply(ex[,2], factor(ex[,1], 0:2), sum)
# 
#    }
# )
# saveRDS(useByStep, "Chapter 1/cover+edge+by+step.rdata")
useByStep2 = t(useByStep)
useByStep2[is.na(useByStep2)] = 0

useByState = apply(useByStep2,2,function(x) tapply(x, list(paste(d5$Name, d5$state)), sum))
useByStateProp = prop.table(useByState,1)
percHR.expanded = t(percHR)[rep(1:4,each=4),]
srByState = useByStateProp / percHR.expanded

srByState2 = srByState[order(rep(1:4,times=4)),]
srByStateList = list()
srByStateList[[1]] = srByState2[1:4,]
srByStateList[[2]] = srByState2[5:8,]
srByStateList[[3]] = srByState2[9:12,]
srByStateList[[4]] = srByState2[13:16,]

srByStateBootMean = lapply(srByStateList,
                           function(x)
                             apply(x, 2, function(z)
                               mean( replicate(500, mean(
                                 sample(z, size = 7, replace = TRUE)
                               )))) )

useByState2 = useByStateProp[order(rep(1:4,times=4)),]
useByStateList = list()
useByStateList[[1]] = useByState2[1:4,]
useByStateList[[2]] = useByState2[5:8,]
useByStateList[[3]] = useByState2[9:12,]
useByStateList[[4]] = useByState2[13:16,]
useByStateBootMean = lapply(useByStateList,
                            function(x)
                              apply(x[,-c(9:10)], 2, function(z)
                                mean( replicate(500, mean(
                                  sample(z, size = 7, replace = TRUE)
                                )))) )

d5.2 = data.frame(srByStateBootMean)
# d5.3 = d5.2[-c(1,10),]
names(d5.2) = c("Rest", "Encamped", "Search", "Transit")
d5.2$Overall = srBootMean
d5.2$perc.cover = c(0:9) * 10
#Define plotting options
statecols = c(REST="#30BFC7", ENCAMPED="#7F5BD9", SEARCH="#DE8A3C", TRANSIT="#8FC248")
#As a line plot
pdf(
  "Chapter 1/figures/cover+selection+by+state+lineplot.pdf",
  #res = 72,
  width = 10,
  height = 10#,
  #units = "in"
)
bckGrndCol = "gray95"
cols = c(statecols, Overall = "gray15")
P = ggplot(d5.2[-c(1,10),], aes(x = perc.cover))
P +
  geom_line(aes(y = Rest), colour=cols[1], size = 2, show.legend=FALSE) +
  geom_line(aes(y = Encamped), colour=cols[2], size = 2, show.legend=FALSE) +
  geom_line(aes(y = Search), colour=cols[3], size = 2, show.legend=FALSE) +
  geom_line(aes(y = Transit), colour=cols[4], size = 2, show.legend=FALSE) +
  geom_line(aes(y = Overall), colour=cols[5], size = 2.6, linetype = 6, show.legend=FALSE) +
  scale_fill_manual(values = cols,
                    name = "Movement State") +
  geom_hline(yintercept = 1,
             size = 0.4,
             linetype = 2) +
  labs(x = "% Woody Cover", y = "Selection Ratio") +
  theme_bw() +
  theme(
    text = element_text(size = 28, family = "Times"),
    plot.margin = unit(c(2.5, 1.5, 1.5, 1.5) - 1.3, units = "in"),
    panel.grid.major.x = element_line(colour = bckGrndCol, lineend = 3),
    panel.grid.major.y = element_line(colour = bckGrndCol, size = 0),
    panel.grid.minor.y = element_line(colour = "white", size = 0),
    panel.background = element_rect(fill = bckGrndCol),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    plot.title = element_text(
      size = 32,
      face = "bold",
      family = "Times",
      hjust = 0.5,
      vjust = 10
    ),
    axis.text.x = element_text(angle = 0, size = 28),
    axis.text.y = element_text(angle = 0, size = 30),
    axis.title.x = element_text(size = 28, vjust = 0.2, face = "bold"),
    axis.title.y = element_text(size = 28, vjust = 3, face = "bold"),
    legend.title = element_text(
      size = 18,
      face = "bold",
      vjust = -3
    ),
    legend.box  = "vertical"
  ) +
  ggtitle(label = "Selection for Woody Cover\nby Movement State")
dev.off()

#===============================================================================
#--------------------Maps / Plots of Woody Vegetation Cover---------------------
#===============================================================================
cov = which(hab[]==4)
river = which(hab[]==5)
gladeEdge = which(hab[]==9)
lugga = which(hab[]==10)
escar = which(hab[]==11)

polyLists = list(escar=escar, river=river, lugga=lugga, gladeEdge=gladeEdge, cover=cov)
woodyCoverByHabitat = lapply(polyLists, function(x) extract(cover, x))
names(woodyCoverByHabitat) = c("escar","river","lugga","gladeEdge","cover")

r = ggplot(data.frame(escar=woodyCoverByHabitat$escar), aes(x=1,y=escar))
r + geom_violin() +
  geom_violin(data = data.frame(river=woodyCoverByHabitat$river), aes(x=2, y=river)) +
  geom_violin(data = data.frame(gladeEdge=woodyCoverByHabitat$gladeEdge), aes(x=3, y=gladeEdge)) +
  geom_violin(data = data.frame(lugga=woodyCoverByHabitat$lugga), aes(x=4, y=lugga)) +
  geom_violin(data = data.frame(cover=woodyCoverByHabitat$cover), aes(x=5, y=cover)) +
  geom_hline(yintercept = 0.3, linetype=2)

        