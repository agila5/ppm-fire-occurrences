# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(here)
library(piggyback)

# Run the R scripts in the R/ folder with your custom functions:
tar_source(here("R/functions.R"))
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
  overwrite = TRUE, 
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
      img_combined <- st_warp(img_combined, crs = 3003)
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
  # 3. Subsets and data transformations
  tar_target(
    name = land_use_tidy_for_spatstat, 
    command = tidy_land_use(land_use_raw)
  ), 
  tar_target(
    name = land_use_tess, 
    command = {
      owins <- 
        lapply(
          st_geometry(land_use_tidy_for_spatstat), 
          as.owin
        ) |> 
        set_names(
          land_use_tidy_for_spatstat[["Code_18"]]
        )
      tess(tiles = owins)
    }
  ), 
  tar_target(
    name = land_use_tidy, 
    command = {
      land_use_tidy_for_spatstat |> 
        st_set_agr(c(Code_18 = "constant")) |> # remove warning on "st_cast"
        st_cast("POLYGON")
    }
  ), 
  tar_target(
    name = land_use_tidy_union, 
    command = {
      land_use_tidy |> st_geometry() |> st_union() |> st_cast("POLYGON")
    }
  ), 
  tar_target(
    name = mainland, 
    command = {
      idx_mainland <- which.max(st_area(land_use_tidy_union))
      land_use_tidy_union[idx_mainland]
    }
  ), 
  tar_target(
    name = fires_italy_ppp, 
    command = {
      coords <- fires_italy |> st_geometry() |> as.ppp()
      setmarks(coords, st_drop_geometry(fires_italy))
    }
  ), 
  tar_target(
    name = fires_sicily_ppp, 
    command = {
      W = Window(land_use_tess)
      fires_italy_ppp[W]
    }
  ), 
  tar_target(
    name = fires_sicily_sf, 
    command = {
      out <- st_as_sf(fires_sicily_ppp)[-1, ]
      st_crs(out) <- 3003
      out
    }
  ),
  tar_target(
    name = elev, 
    command = {
      orig_tif <- generate_orig_tif(elev_files)
      tif <- orig_tif |> st_downsample(n = c(10, 10)) |> st_warp(crs = 3003)
      rm(orig_tif); gc()
      
      tif <- tif[st_bbox(land_use_tidy_union)]
      tif[land_use_tidy_union]
    }
  ),
  tar_target(
    name = slope, 
    command = {
      # I cannot save orig_tif in targets format, so I need to repeat the
      # previous steps
      orig_tif <- generate_orig_tif(elev_files)
      
      # The following is "new"
      temp_tif <- tempfile(fileext = ".tif")
      write_stars(orig_tif, temp_tif, progress = FALSE)
      temp_slope <- tempfile(fileext = ".tif")
      gdaldem("slope", temp_tif, temp_slope)
      slope <- read_stars(temp_slope)
      slope <- slope |> st_downsample(c(10, 10)) |> st_warp(crs = 3003)
      slope <- slope[st_bbox(land_use_tidy_union)]
      slope[land_use_tidy_union]
    }
  ),
  tar_target(
    name = NDVI_tidy, 
    command = {
      # The online docs say that the NDVI measurements are shared approximately
      # every 10 days
      day_jan <- c(as.Date("2023-01-01"), as.Date("2023-01-11"), as.Date("2023-01-21"))
      
      # The following code is taken from the first vignette of stars package. 
      merge(NDVI_raw) |> 
        setNames("NDVI") |> 
        st_set_dimensions(
          3, 
          values = c(
            day_jan, 
            day_jan + months(1), 
            day_jan + months(2),
            day_jan + months(3), 
            day_jan + months(4),
            day_jan + months(5), 
            day_jan + months(6),
            day_jan + months(7), 
            day_jan + months(8),
            day_jan + months(9),
            day_jan + months(10),
            day_jan + months(11)
          ),
          names = "time"
        ) |> 
        st_warp(crs = 3003, cellsize = c(500, 500))
    }
  ), 
  tar_target(
    name = NDVI_tidy_agg_1month, 
    command = aggregate(NDVI_tidy, by = "months", FUN = mean)
  ), 
  tar_target(
    name = NDVI_tidy_agg_2months, 
    command = aggregate(NDVI_tidy, by = "2 months", FUN = mean)
  ),
  tar_target(
    name = NDVI_and_landuse, 
    command = {
      NDVI_tidy_new <- st_warp(
        NDVI_tidy, 
        st_as_stars(st_bbox(NDVI_tidy), dx = 1500, dy = 1500)
      )
      
      aggregate(
        NDVI_tidy_new, 
        by = land_use_tidy_for_spatstat, 
        FUN = mean, 
        na.rm = TRUE
      )
    }
  ),
  tar_target(
    name = cov_time, 
    command = {
      tmp_dir <- tempdir()
      unzip(env_var_files, exdir = tmp_dir)
      nc_files <- list.files(
        paste0(tmp_dir, "/environmental-variables"), 
        pattern = "\\.nc$", 
        full.names = TRUE
      )
      
      nome_file <- c(
        "january.nc", "february.nc", "march.nc", "april.nc",
        "may.nc", "june.nc", "july.nc",
        "august.nc", "september.nc", "october.nc", "november.nc", 
        "december.nc"
      )
      
      lista <- list()
      for(i in nc_files){
        k <- which(nome_file == basename(i))
        suppressMessages({
          prova <- read_ncdf(i)
        })
        st_crs(prova) <- 4326 
        prova <- st_warp(prova, crs = 3003)
        prova <- prova[mainland]
        
        if (basename(i) == "october.nc"){
          prova2 <- aggregate(prova[, , , 2, , drop = TRUE], max, by = "1 day")[,-1]  |>
            st_apply(c("time"), function(z) max(z, na.rm =TRUE))
        } else {
          prova2 <- aggregate(prova[, , , ], max, by = "1 day")  |>
            st_apply(c("time"), function(z) max(z, na.rm =TRUE))
        }
        
        prova2 <- as.data.frame(prova2)
        prova2$month <- rep(k, nrow(prova2))
        prova2$day <- 1:nrow(prova2)
        lista[[k]] <- prova2
      }
      
      do.call(rbind, lista)
    }
  ), 
  
  # 4. Building the final dataframe that will be used by stopp
  tar_target(
    name = stp_time, 
    command = {normalize(fires_sicily_sf[["ACQ_DATETIME"]], 364)}
  ), 
  tar_target(
    name = stp0, 
    command = {
      stp(cbind(spatstat.geom::coords(fires_sicily_ppp) / 1000, stp_time))
    }
  ),
  tar_target(
    name = dum1, 
    command = {
      mainland_coords <- st_coordinates(mainland)
      set.seed(2)
      dum1 <- rpp(
        npoints = npoints(fires_sicily_ppp) * 7, 
        s.region = mainland_coords[, 1:2] / 1000, 
        t.region = range(stp_time)
      )
      data.frame(x = dum1$xyt[,1], y = dum1$xyt[,2], t =  dum1$xyt[,3])
    }
  ), 
  tar_target(
    name = covs0_spatial, 
    command = {
      elev_im <- spatstat.geom::rescale(as.im(elev), 1000)
      slope_im <- spatstat.geom::rescale(as.im(slope), 1000)
      land_use_f <- as.function(land_use_tess)
      
      covs0_spatial <- rbind(
        cbind(
          land_use_f(stp0$df$x * 1000, stp0$df$y * 1000), 
          slope_im[list(x = stp0$df$x, y = stp0$df$y), drop = FALSE], 
          elev_im[list(x = stp0$df$x, y = stp0$df$y), drop = FALSE] 
        ), 
        cbind(
          land_use_f(dum1$x * 1000, dum1$y * 1000), 
          slope_im[list(x = dum1$x, y = dum1$y), drop = FALSE], 
          elev_im[list(x = dum1$x, y = dum1$y), drop = FALSE]
        )
      )
      colnames(covs0_spatial) <- c("land", "slope", "elev")
      covs0_spatial
    }
  ), 
  tar_target(
    name = pts_combined, 
    command = {
      pts_combined <- rbind(stp0$df, dum1) |> setNames(c("x", "y", "time"))
      pts_combined[, c("x", "y")] <- pts_combined[, c("x", "y")] * 1000 # Since we previously converted m to km
      pts_combined <- st_as_sf(pts_combined, coords = c("x", "y"), crs = 3003)
      pts_combined$time <- (as.Date("2023-01-01") + pts_combined$time) |> round_date(unit = "day")
      pts_combined
    }
  ),
  tar_target(
    name = env_cov_st, 
    command = {
      out <- st_extract(
        x = env_var_agg_1day,
        at = pts_combined,
        time_column = "time"
      )
      st_drop_geometry(out)[, -c(12)]
    }
  ), 
  tar_target(
    name = NDVI_cov_st, 
    command = {
      st_extract(
        x = NDVI_tidy,
        at = pts_combined |> mutate(time = round_date(time, "month")),
        time_column = "time", 
      )
    }
  ), 
  tar_target(
    name = covs_complete, 
    command = {
      out <- cbind(
        x = c(stp0$df$x, dum1$x), 
        y = c(stp0$df$y, dum1$y), 
        t = c(stp0$df$t, dum1$t), 
        covs0_spatial, 
        env_cov_st, 
        NDVI = NDVI_cov_st$NDVI 
      )
      # Rescale elev from m to km
      out$elev <- out$elev / 1000
      out
    }
  ), 
  
  # 5. Modelling
  tar_target(
    name = volume0, 
    command = {
      area(
        Window(fires_sicily_ppp |> spatstat.geom::rescale(1000))
      ) * abs(diff(range(stp0$df$t)))
    }
  ), 
  tar_target(
    name = mod_global, 
    command = {
      # Please notice that stppm is defined in R/functions.R
      stppm(
        X = stp0, 
        formula = ~
          as.factor(land) + NDVI + 
          + elev + slope 
        + u10 + stl2 
        + tp
        , 
        dummy_points = dum1, 
        dati.interpolati = covs_complete[3 + c(1, 2, 3, 4, 10, 13, 14, 15)], 
        obsvol = volume0,  ncube = 5, nxyt = 87
      )
    }
  ),
  tar_target(
    name = mod_local, 
    command = {
      # Please notice that stppm is defined in R/functions.R
      stppm(
        X = stp0, 
        formula = ~
          as.factor(land) + NDVI + 
          + elev + slope 
        + u10 + stl2 
        + tp
        , 
        dummy_points = dum1, 
        dati.interpolati = covs_complete[3 + c(1, 2, 3, 4, 10, 13, 14, 15)], 
        obsvol = volume0,  ncube = 5, nxyt = 87, 
        local = TRUE, 
        verbose = TRUE
      )
    }
  ),
  
  # The end
  tar_quarto(
    name = results, 
    path = here("reports", "results.qmd"), 
    quiet = FALSE
  ) 
)