# 0a. Setup ----------------------------------------------------------------
list_packages <- function() {
  unique(c(
    "here", "piggyback", "qs", "sf", "tidyverse", "cols4all", "spatstat", 
    "stars", "scales", "gdalUtilities", "grid", "stpp", "stopp", 
    "mgcv", "plot3D", "grDevices", "colorspace", "sparr", 
    "conflicted"
  ))
}
is_conflicted_last <- function(pkgs) {
  `%!in%` <- Negate(`%in%`)
  if ("conflicted" %!in% pkgs) {
    stop("conflicted is not in the package")
  }
  stopifnot(
    "Conflicted is not last" = {which(pkgs == "conflicted") == length(pkgs)}
  )
  invisible(TRUE)
}
normalize <- function(x, max = 1) {
  x <- as.numeric(x)
  (x - min(x)) / (max(x) - min(x)) * max
}

# 0b. Bounding boxes for plots --------------------------------------------
define_bb <- function(
    xmin, 
    ymin, 
    xmax, 
    ymax, 
    crs
) {
  bbox <- st_bbox(
    c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), 
    crs = crs
  ) |> st_as_sfc()
  
  if (st_crs(bbox) == st_crs(3003)) {
    return(bbox)
  }
  st_transform(bbox, 3003)
}