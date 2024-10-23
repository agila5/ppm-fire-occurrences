list_packages <- unique(c(
  "piggyback", "qs", "sf", "tidyverse", "viridis", "cols4all", 
  "spatstat", 
  "stars", "scales", "gdalUtilities", "grid", "stpp", "stopp", 
  "mgcv", "plot3D", "grDevices", "colorspace", "sparr", 
  "conflicted"
))
invisible(suppressPackageStartupMessages(
  sapply(list_packages, library, character.only = TRUE)
))
