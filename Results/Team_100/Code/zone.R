
polygon <- apply(zones_dict, 1, function(x) {
  multipol <- x[2]
  tmp <- gsub(multipol, pattern = "[^0-9 \\.,]", replacement = "")
  tmp2 <- strsplit(tmp, split=",")
  tmp3 <- lapply(tmp2, strsplit, split = " ")
  tmp4 <- lapply(tmp3, function(x) {
    tt <- sapply(x, function(y) {
      as.numeric(y)
    })
    tt <- cbind(tt, tt[,1])
    tt
  })
  return(tmp4)
}
)

point_in_polygon <- function(point_lon, point_lat, pol_x, pol_y) {
  point.in.polygon(point_lon, point_lat, pol_x, pol_y)
}

library(rgdal)

find_zone <- function(point_lon, point_lat) {
  in_zone <- sapply(polygon, function(x) {
    point_in_polygon(point_lon, point_lat, x[[1]][1, ], x[[1]][2, ])
  }) 
  if (sum(in_zone) == 0) {
    return(-1)
  } 
  return(which.max(in_zone))
}