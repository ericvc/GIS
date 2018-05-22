setwd("~/Leopard Analysis")
library(move)
library(rgdal)
library(raster)

weightedHabUse = function (x, range.subset, ts, ras, le, h) 
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
    x.out <- brownian.bridge.dyn(object, raster = ras, time.step = ts,
                                 location.error = le)
    outMatrix[i-(min(range.subset)+1),] = tapply(x.out[], list(crop(h, x.out, method="ngb")[]), sum)
    
  }
  outMatrix
}

leop_id = c("Chumvi","Chumvi","Ewaso","Haraka","Konda","Limofu","Mzee","Tatu")
#probability distributions
dbbmm = list.files("Chapter 1/data/", pattern="dbbmm", full.names=TRUE)[-7]
#GIS layer
hab = raster("Environmental Variables/Habitat Categorical/Habitat_Classification_Cover_FULL_v3_STEP.tif")

use = list()
for(leopard in 1:length(leop_id)){
  d = get(load(dbbmm[leopard]))
  range.subset = which(d@DBMvar@interest==TRUE)
  use[[leopard]] = weightedHabUse(d, range.subset, ts=0.25, ras=5, h=hab, le=12)
}
names(use) = leop_id

START <- Sys.time()
saveRDS(use, "weightedHabUse_list.rds")
END <- Sys.time()

#print time to completion
difftime(END, START, units = "hours")

#calculations are done
browseURL("https://www.youtube.com/watch?v=-0Ao4t_fe0I")
