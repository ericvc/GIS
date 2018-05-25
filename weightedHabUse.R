setwd("Leopard Analysis")
library(move)
library(rgdal)
library(raster)

habVars = c( "Open","Grass","Cover","River","Roads","Human","Glade","GladeEdge",
             "Escarpment","Lugga","Koppe")
leop_id = c("Chumvi","Chumvi2","Ewaso","Haraka","Konda","Limofu","Mzee","Tatu")
#probability distributions
dbbmm = list.files("Chapter 1/data/", pattern="dbbmm", full.names=TRUE)
#GIS layer
hab = raster("Environmental Variables/Habitat Categorical/Habitat_Classification_Cover_FULL_v3.tif")

START <- Sys.time()
use = list()
for (leopard in 1:length(leop_id)) {
  d = get(load(dbbmm[leopard]))
  hab2 = projectRaster(hab, crs = CRS(proj4string(d)), method = "ngb")
  object <- d@DBMvar
  steps = c(move::distance(object), NA)
  timesteps = c(move::timeLag(object), NA)
  range.subset = which(d@DBMvar@interest == TRUE &
                         round(timesteps) == 15 & steps < 1500)
  outMatrix = matrix(data = NA,
                     nrow = length(range.subset),
                     ncol = 11)
  for (i in range.subset) {
    object@interest <- rep(FALSE, nrow(object))
    object@interest[i] <- TRUE
    extent.compare = cbind(extent(object[i])[], extent(object[i + 1])[])
    n <- 1e3
    extent. = c(
      xmin = min(extent.compare[1, ]) - n,
      xmax = max(extent.compare[2, ]) + n,
      ymin = min(extent.compare[3, ]) - n,
      ymax = max(extent.compare[4, ]) + n
    )
    h.crop = crop(hab2, extent.)
    x.out <- tryCatch(
      brownian.bridge.dyn(
        object,
        raster = h.crop,
        ext = c(0.2, 0.2),
        location.error = 9
      ),
      error = function(e) {
        extent.compare1 = cbind(extent(object)[], extent(object[i + 1])[])
        n <- 1e3
        extent. = c(
          xmin = min(extent.compare1[1,]) - n,
          xmax = max(extent.compare1[2,]) + n,
          ymin = min(extent.compare1[3,]) - n,
          ymax = max(extent.compare1[4,]) + n
        )
        h.crop = crop(hab2, extent.)
        brownian.bridge.dyn(
          object,
          raster = h.crop,
          ext = c(0.2, 0.2),
          location.error = 9
        )
        
      }
    )
    outMatrix[which(range.subset == i), ] = tapply(x.out[], list(factor(h.crop[], 2:12)), sum)
    print(paste0(leopard, "_", i))
    
  }
  
  out = data.frame(outMatrix, range.subset, object@data$local.time[range.subset])
  names(out) = c(habVars, "stepID", "local.time")
  write.csv(
    out,
    file = paste0("Chapter 1/data/weightedHabitatUse_", leop_id[leopard], ".csv"),
    row.names = FALSE
  )
  
  use[[leopard]] = out
  
}


names(use) = leop_id
saveRDS(use, file = "Chapter 1/data/weightedHabitatUse_list.rds")

END <- Sys.time()

#print time to completion
difftime(END, START, units = "hours")

#i have ghosts
browseURL("https://www.youtube.com/watch?v=4kNetd_lISI")
