#------------------------Leopard Glade Visitation------------------------------#
setwd("/Volumes/Lockbox/Leopard Analysis/")
cellUse = read.csv("Leopard+Space+Use+by+CellNum_n=7.csv")

#Dimensions [habitat (category), Julian Day, Individual ID]
d = tapply(cellUse$weight, list(cellUse$habitat, cellUse$Jday, cellUse$Name), function(x) sum(x, na.rm = TRUE))
visits = d[8,,]
#identify 'bouts' of glade use.
bouts = visits
bouts[is.na(bouts)] <- 0
bouts[bouts>0] <- 1
#use 'diff' function to identify continuous use from previous time step.
#if used in previous time step, the resulting value is 0.
bouts.rle = apply(bouts, 2, rle)

head(bouts.rle[[1]])
x = bouts.rle$Chumvi

#define glades
library(maptools)
library(spatstat)
library(raster)
r_glade = raster(raster("Environmental Variables/Habitat Categorical/Habitat_Classification_Cover_FULL_w_Koppe.tif"))
gshp = readShapePoly("Workspace/Shapefiles/Glades/glades.shp")
polys = cellFromPolygon(r_glade, gshp)
for(p in 1:length(polys)) if(p!=2) r_glade[polys[[p]]] <- p

# define glade edges
r_edge = raster(r_glade)
for(p in 1:length(polys)){
  if(p!=2){
    
    gladesPPP = as.ppp(xyFromCell(r_edge, cell=polys[[p]], spatial = TRUE))
    cellXY <- as.ppp(xyFromCell(r_edge, cell=1:ncell(r_edge), spatial=TRUE))
    distances <- nncross(X=cellXY, Y=gladesPPP)
    edgeCells = which(distances$dist<100)
    r_edge[edgeCells] <- p
    
    }
}

#identify glades and glade edges
glade_id = extract(r_edge, cellUse$cell)
night = ifelse(cellUse$TimeOfDay > 18.5 | cellUse$TimeOfDay < 5.5, 1, 0)
use = cbind(cellUse, glade_id, night)

#identify 'use'
visits = use[!is.na(glade_id) & night==1,]

attach(visits)
unq.visits = tapply(visits$glade_id, list(Jday, Name), function(x) length(unique(x)))
unq.visits[is.na(unq.visits)] <- 0
write.csv(unq.visits, "~/Desktop/Leop+Glade_unique+visits+atNight.csv")
