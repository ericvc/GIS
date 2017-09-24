move.forud.BGB = function (x, range.subset, ts, ras, le, lev, crs, path, name, ext,
                        ID) 
{
  object <- x@var
  if (length(range.subset) < 2) 
    stop("\nrange.subset must have length >1\n")
  if (length(le) == 1) 
    location.error = rep(c(le), nrow(object))
  if (length(le) > 1) 
    location.error = c(le)
  for (i in range.subset) {
    object@segInterest <- rep(FALSE, nrow(object))
    object@segInterest[i] <- TRUE
    times = object@timestamps[i]
    var1 = object@paraSd[i] 
    var2 = object@orthSd[i] 
    x.out <- dynBGB(object, raster = ras, time.step = ts, ext=ext,
                                 locErr = location.error)
    r <- list(x = seq(x@extent@xmin, x@extent@xmax, by = 1), 
              y = seq(x@extent@ymin, x@extent@ymax, by = 1), probability = x.out@data@values)
    contrs.values = bbmm.contour(r, levels = lev, plot = FALSE)
    output = rasterToContour(x.out, maxpixels = 4e+06, level = contrs.values$Z)
    out = spTransform(output, CRS(crs))
    out = SpatialLines2PolySet(out)
    out = PolySet2SpatialPolygons(out)
    out = as(out, "SpatialPolygonsDataFrame")
    out$time = times
    out$paraSD = var1
    out$orthSD = var2
    out$udvals = paste(rev(lev))
    out$ID = ID
    outKML = spTransform(out, CRS("+proj=longlat +datum=WGS84"))
    writeOGR(outKML, layer = paste("step",i), dsn = paste(path,name, i,".kml", sep = ""), 
             driver = "KML")
  }
}
#<environment: namespace:moveud>