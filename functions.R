

#'
#' constructs a point grid around the origin point (wgs84), with a given step (meters), of width/height 2*size+1
point_grid <- function(orig, step, size){
  p0 = st_coordinates(st_transform(orig,etrs89lcc))
  x0 = p0[1,1]; y0 = p0[1,2]
  xcoords = seq(from = x0 - size*step, to = x0 + size*step, by = step)
  ycoords = seq(from = y0 - size*step, to = y0 + size*step, by = step)
  allx = matrix(data=rep(xcoords,length(ycoords)),nrow = length(ycoords), byrow = T)
  ally = matrix(data=rep(ycoords,length(xcoords)),ncol = length(xcoords), byrow = F)
  return(st_transform(st_sfc(st_multipoint(as.matrix(data.frame(X = c(allx), Y=c(ally)))), crs = etrs89lcc), wgs84))
}

