# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(here)
library(piggyback)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
tar_option_set(format = "qs", packages = list_packages()) # debug goes here
tar_config_set(store = here("_targets"))

# Download the data files if necessary
if (!dir.exists(here("data"))) {
  dir.create(here("data"))
}

pb_download(
  file = c(
    "DL_FIRE_J1V-C2_510187.zip", "confini-regioni.zip", "land-use.zip", "NDVI.zip", 
    "environmental-variables.zip", "INGV-elev.zip"
  ), 
  dest = here("data"), 
  repo = "agila5/ppm-fire-occurrences",
  tag = "v1-data", 
  overwrite = FALSE
)

list(
  # 0a. Setup and basic checks
  tar_target(
    name = packages_names, 
    command = {
      out <- list_packages()
      stopifnot(is_conflicted_last(out))
      out
    }
  ), 
  
  #0b. Bouding boxes
  tar_target(
    name = pantelleria_bbox, 
    command = define_bb(
      xmin = 11.92586, ymin = 36.73438, 
      xmax = 12.05684, ymax = 36.83939, 
      crs = "OGC:CRS84"
    )
  ),
  tar_target(
    name = linosa_bbox, 
    command = define_bb(
      xmin = 12.84838, ymin = 35.85436, 
      xmax = 12.88393, ymax = 35.87595, 
      crs = "OGC:CRS84"
    )
  ),
  tar_target(
    name = lampedusa_bbox, 
    command = define_bb(
      xmin = 12.51730, ymin = 35.49295, 
      xmax = 12.63422, ymax = 35.52931, 
      crs = "OGC:CRS84"
    )
  ), 
  tar_target(
    name = palermo_bbox, 
    command = define_bb(
      xmin = 1871427, xmax = 1890744, 
      ymin = 4219859, ymax = 4240118, 
      crs = 3003
    )
  ), 
  tar_target(
    name = sicily_mainland_bbox, 
    command = define_bb(
      xmin = 1807082, ymin = 4041434, 
      xmax = 2083886 , ymax = 4265502, 
      crs = 3003
    )
  ), 
  
  # 1. Data files
  tar_target(
    name = fires_italy_file, 
    command = here("data", "DL_FIRE_J1V-C2_510187.zip"), 
    format = "file"
  ), 
  tar_target(
    name = confini_regioni_files, 
    command = here("data", "confini-regioni.zip"), 
    format = "file"
  ),
  tar_target(
    name = land_use_files, 
    command = here("data", "land-use.zip"), 
    format = "file"
  ),
  tar_target(
    name = elev_files, 
    command = here("data", "INGV-elev.zip"), 
    format = "file"
  ), 
  tar_target(
    name = env_var_files, 
    command = here("data", "environmental-variables.zip"), 
    format = "file"
  ), 
  tar_target(
    name = NDVI_files, 
    command = here("data", "NDVI.zip"), 
    format = "file"
  ),
  
  # 2. Read-in data
  tar_target(
    name = fires_italy, 
    command = {
      st_read(paste0("/vsizip/", fires_italy_file), quiet = TRUE) |> 
        st_transform(3003) |> 
        mutate(
          ACQ_DATETIME = ymd_hm(paste0(ACQ_DATE, " ", ACQ_TIME))
        )
    }
  ),
  tar_target(
    name = confini_regioni, 
    command = {
      st_read(
        paste0("/vsizip/", confini_regioni_files,"/confini-regioni"), 
        quiet = TRUE
      ) |> 
        st_transform(3003)
    }
  ),
  tar_target(
    name = land_use_raw, 
    command = {
      st_read(
        paste0("/vsizip/", land_use_files, "/land-use"), 
        quiet = TRUE
      )
    }
  ), 
  tar_target(
    name = env_var, 
    command = {
      tmp_dir <- tempdir()
      unzip(env_var_files, exdir = tmp_dir)
      nc_files <- list.files(
        paste0(tmp_dir, "/environmental-variables"), 
        pattern = "\\.nc$", 
        full.names = TRUE
      )
      suppressMessages({
        img_combined <- lapply(
          nc_files, 
          function(x, var) {
            img <- read_ncdf(x, var = var)
            if (grepl("october.nc", x, fixed = TRUE)) {
              img <- img[, , , 2, , drop = TRUE]
            }
            img
          }, 
          var = c("u10", "v10", "d2m", "t2m", "skt", "stl1", "stl2", "stl3", "stl4", "sp", "tp")
        ) |> 
          do.call(c, args = _)
      })
      
      st_crs(img_combined) <- "OGC:CRS84"
      st_warp(img_combined, crs = 3003)
      img_combined
    }
  ),
  tar_target(
    name = env_var_agg_1day, 
    command = {aggregate(env_var, by = "1 day", max)}
  ),
  tar_target(
    name = NDVI_raw, 
    command = {
      tmp_dir <- tempdir(check = TRUE)
      unzip(NDVI_files, exdir = tmp_dir)
      files <- list.files(
        paste0(tmp_dir, "/NDVI"), 
        pattern = "\\.nc$",
        full.names = TRUE
      )
      # Currently, the structure returned by read_stars is an array with
      # dimension 1253 x 1116 and 36 attributes (1 for each NDVI in a 10 days
      # interval). We need to convert it to a 1253 x 1116 x 36 array with 1
      # attribute. See below (NDVI_tidy).
      read_stars(files, quiet = TRUE)
    }
  ),
  
  # The end
  tar_quarto(
    name = results, 
    path = here("reports", "results.qmd"), 
    quiet = FALSE
  ) 
)