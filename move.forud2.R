move.forud2 = function (x, range.subset, ts, ras, le, lev, crs, path, name,
          ID) 
{
  object <- x@DBMvar
  if (length(range.subset) < 2) 
    stop("\nrange.subset must have length >1\n")
  if (length(le) == 1) 
    location.error = rep(c(le), nrow(object))
  if (length(le) > 1) 
    location.error = c(le)
  for (i in range.subset) {
    object@interest <- rep(FALSE, nrow(object))
    object@interest[i] <- TRUE
    times = object@data$time[i]
    var = object@means[i]
    extent.compare = cbind(extent(object[i])[],extent(object[i+1])[])
    n = 700
    extent. = c(xmin=min(extent.compare[1,])-n, xmax=max(extent.compare[2,])+n, 
                ymin=min(extent.compare[3,])-n, ymax=max(extent.compare[4,])+n)
    x.out <- brownian.bridge.dyn(object, raster = ras, time.step = ts, bbox = extent., ext=.1,
                                               location.error = location.error)
    r <- list(x = seq(x@extent@xmin, x@extent@xmax, by = 1), 
              y = seq(x@extent@ymin, x@extent@ymax, by = 1), probability = x.out@data@values)
    contrs.values = bbmm.contour(r, levels = lev, plot = FALSE)
    output = rasterToContour(x.out, maxpixels = 4e+06, level = contrs.values$Z)
    out = spTransform(output, CRS(crs))
    out = SpatialLines2PolySet(out)
    out = PolySet2SpatialPolygons(out)
    out = as(out, "SpatialPolygonsDataFrame")
    out$time = times
    out$udvals = ifelse(length(rev)==2, paste(rev(lev)), rev(lev)[1])
    out$ID = ID
    outKML = spTransform(out, CRS("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0"))
      # writeOGR(outKML, layer = paste("step",i), dsn = paste(path,name, i,".kml", sep = ""), 
               # driver = "KML")
     writeOGR(outKML, layer = paste("step",i), dsn = paste(path,name, i,".shp", sep = ""), check_exists=FALSE,
               driver = "ESRI Shapefile")
  }
}
#<environment: namespace:moveud>

move.forud2.KML = function (x, range.subset, ts, ras, le, lev, crs, path, name,
          ID) 
{
  object <- x@DBMvar
  if (length(range.subset) < 2) 
    stop("\nrange.subset must have length >1\n")
  if (length(le) == 1) 
    location.error = rep(c(le), nrow(object))
  if (length(le) > 1) 
    location.error = c(le)
  for (i in range.subset) {
    object@interest <- rep(FALSE, nrow(object))
    object@interest[i] <- TRUE
    times = object@timestamps[i]
    var = object@means[i]
    extent.compare = cbind(extent(object[i])[],extent(object[i+1])[])
    n = 500
    extent. = c(xmin=min(extent.compare[1,])-n, xmax=max(extent.compare[2,])+n, 
                ymin=min(extent.compare[3,])-n, ymax=max(extent.compare[4,])+n)
    x.out <- brownian.bridge.dyn(object, raster = ras, time.step = ts, bbox = extent., ext=.1,
                                               location.error = location.error)
    r <- list(x = seq(x@extent@xmin, x@extent@xmax, by = 1), 
              y = seq(x@extent@ymin, x@extent@ymax, by = 1), probability = x.out@data@values)
    contrs.values = bbmm.contour(r, levels = lev, plot = FALSE)
    output = rasterToContour(x.out, maxpixels = 4e+06, level = contrs.values$Z)
    out = spTransform(output, CRS(crs))
    out = SpatialLines2PolySet(out)
    out = PolySet2SpatialPolygons(out)
    out = as(out, "SpatialPolygonsDataFrame")
    out$time = times
    out$stepvar = var
    out$udvals = ifelse(length(rev)==2, paste(rev(lev)), rev(lev)[1])
    out$ID = ID
    outKML = spTransform(out, CRS("+proj=longlat +datum=WGS84"))
    writeOGR(outKML, layer = paste("step",i), dsn = paste(path,name, i,".kml", sep = ""), 
             driver = "KML")
  }
}
#<environment: namespace:moveud>