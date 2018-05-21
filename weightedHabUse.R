setwd("~/Leopard Analysis")
library(move)
library(rgdal)
library(raster)

weightedHabUse = function (x, range.subset, ts, ras, le, hab) 
{
  object <- x@DBMvar
  outMatrix = matrix(NA, length(range.subset), ncol=11)
  if (length(range.subset) < 2) 
    stop("\nrange.subset must have length >1\n")
  if (length(le) == 1) 
    location.error = rep(c(le), nrow(object))
  if (length(le) > 1) 
    location.error = c(le)
  for (i in range.subset){
    object@interest <- rep(FALSE, nrow(object))
    object@interest[i] <- TRUE
    times = object@data$time[i]
    var = object@means[i]
    extent.compare = cbind(extent(object)[],extent(object[i+1])[])
    n = 1e2
    extent. = c(xmin=min(extent.compare[1,])-n, xmax=max(extent.compare[2,])+n, 
                ymin=min(extent.compare[3,])-n, ymax=max(extent.compare[4,])+n)
    x.out <- brownian.bridge.dyn(object, raster = ras, time.step = ts, bbox = extent.,
                                 location.error = le)
    hab.crop = projectRaster(hab, x.out, method="ngb")
    outMatrix[i-(min(range.subset)-1),] = tapply(x.out[], list(hab.crop[]), sum)
  }
  outMatrix
}

leop_id = c("Chumvi","Chumvi2","Ewaso","Haraka","Konda","Limofu","Mzee","Tatu")
#probability distributions
dbbmm = list.files("Workspace/", pattern="dbbmm", full.names=TRUE)[-7]
#GIS layer
hab = raster("Environmental Variables/Habitat Categorical/Habitat_Classification_Cover_FULL_v3.tif")

use = list()
for(leopard in 1:length(leop_id)){
  d = get(load(dbbmm[leopard]))
  range.subset = which(d@DBMvar@interest==TRUE)
  time.step = 0.25
  use[[leopard]] = weightedHabUse(d, range.subset, ts=time.step, ras=5, hab=hab, le=12)
}
names(use) = leop_id

START <- Sys.time()
saveRDS(use, "weightedHabUse_list.rds")
END <- Sys.time()

difftime(END, START, units = "hours")