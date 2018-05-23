setwd("~/Leopard Analysis")
library(move)
library(rgdal)
library(raster)

leop_id = c("Chumvi","Chumvi","Ewaso","Haraka","Konda","Limofu","Mzee","Tatu")
#probability distributions
dbbmm = list.files("Chapter 1/data/", pattern="dbbmm", full.names=TRUE)
#GIS layer
hab = raster("Environmental Variables/Habitat Categorical/Habitat_Classification_Cover_FULL_v3.tif")

START <- Sys.time()

for(leopard in 1:length(leop_id)){
  
  d = get(load(dbbmm[leopard]))
  range.subset = which(d@DBMvar@interest==TRUE)
  hab2 = projectRaster(hab, crs = CRS(proj4string(d)), method="ngb")
  object <- d@DBMvar
  outMatrix = matrix(data=NA,nrow=length(range.subset), ncol=11)
  for(i in range.subset){
    if(object@data$step[i+1] < 1500){
      object@interest <- rep(FALSE, nrow(object))
      object@interest[i] <- TRUE
      n = 5e2
      extent.compare = cbind(extent(object[i])[],extent(object[i+1])[])
      extent. = c(xmin=min(extent.compare[1,])-n, xmax=max(extent.compare[2,])+n, 
                  ymin=min(extent.compare[3,])-n, ymax=max(extent.compare[4,])+n)
      h.crop = crop(hab2, extent.)
      x.out <- brownian.bridge.dyn(object, raster = h.crop, ext=c(0.5,0),
                                   bbox = extent.,
                                   location.error = 9)
      outMatrix[i-(min(range.subset)+1),] = tapply(x.out[], list(factor(h.crop[],2:12)), sum)
      print(i)
    }
    else{
      outMatrix[i-(min(range.subset)+1),] = rep(NA,11)
    }
    
  }

  write.csv(outMatrix, file = paste0("Chapter 1/data/weightedHabitatUse_",leop_id[leopard],".csv"))
  
}
 names(use) = leop_id

END <- Sys.time()

#print time to completion
difftime(END, START, units = "hours")
