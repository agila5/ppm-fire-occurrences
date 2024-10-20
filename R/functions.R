# 0a. Setup ----------------------------------------------------------------
list_packages <- function() {
  unique(c(
    "here", "piggyback", "qs", "sf", "tidyverse", "viridis", "cols4all", 
    "spatstat", 
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

# 3. Subsets and data transformations -------------------------------------
tidy_land_use <- function(land_use_raw) {
  land_use_tidy <- land_use_raw |> 
    select(Code_18) |> 
    st_transform(crs = 3003) |> # for spatstat
    st_set_agr(c(Code_18 = "constant")) |> # remove warning on "st_cast"
    st_cast("POLYGON") |> 
    st_make_valid()
  
  # Merge together areas with the same macro code
  land_use_tidy |> 
    mutate(Code_18 = substr(Code_18, 1, 1))|> # Get the macro code
    mutate(
      Code_18 = factor(
        Code_18, 
        labels = c(
          "Artificial surfaces", 
          "Agricultural areas", 
          "Forests", 
          "Water bodies", 
          "Water bodies"
        )
      )
    ) |> 
    group_by(Code_18) |> 
    summarise()
}
generate_orig_tif <- function(paths) {
  temp_folder <- tempdir()
  unzip(paths, exdir = temp_folder)
  tif_paths <- list.files(
    temp_folder, pattern = "\\.tif", recursive = TRUE, 
    full.names = TRUE
  )
  tifs <- lapply(tif_paths, read_stars)
  do.call(st_mosaic, tifs)
}

# 5. Modelling ------------------------------------------------------------
.counting.weights <- function(id, volumes) {
  id <- as.integer(id)
  fid <- factor(id, levels = seq_along(volumes))
  counts <- table(fid)
  w <- volumes[id] / counts[id]
  w <- as.vector(w)
  names(w) <- NULL
  return(w)
}

.default.ncube <- function(X){
  guess.ngrid <- floor((splancs::npts(X) / 2) ^ (1 / 3))
  max(5, guess.ngrid)
}

.grid1.index <- function(x, xrange, nx) {
  i <- ceiling(nx * (x - xrange[1]) / diff(xrange))
  i <- pmax.int(1, i)
  i <- pmin.int(i, nx)
  i
}

.grid.index <- function(x, y, t, xrange, yrange, trange, nx, ny, nt) {
  
  ix <- .grid1.index(x, xrange, nx)
  iy <- .grid1.index(y, yrange, ny)
  it <- .grid1.index(t, trange, nt)
  
  return(list(ix = ix, iy = iy, it = it, index = as.integer((iy - 1) * nx + ix + (it - 1) * nx * ny)))
}

stppm <- function(X, formula, dummy_points, dati.interpolati,
                  ncube = NULL, obsvol, nxyt = NULL, local = FALSE,
                  verbose = TRUE) {
  if (!inherits(X, c("stp", "stpm"))) {
    stop("x should be either of class stp or stpm")
  }

  time1 <- Sys.time()


  if (!is.null(ncube)) {
    if (!is.numeric(ncube)) {
      stop("ncube should be a numeric value")
    } else {
      if (ncube <= 0) {
        stop("ncube should be ncube > 0")
      }
    }
  }

  # X è il processo osservato

  X0 <- X
  X <- X$df

  nX <- nrow(X)

  x <- X[, 1]
  y <- X[, 2]
  t <- X[, 3]

  # definire dummy points come un df con x y e t

  quad_p <- rbind(X[, 1:3], dummy_points)

  xx <- quad_p[, 1]
  xy <- quad_p[, 2]
  xt <- quad_p[, 3]
  win <- spatstat.geom::box3(
    xrange = range(xx), yrange = range(xy),
    zrange = range(xt)
  )

  if (is.null(ncube)) {
    ncube <- .default.ncube(quad_p)
  }
  ncube <- rep.int(ncube, 3)
  nx <- ncube[1]
  ny <- ncube[2]
  nt <- ncube[3]

  if (is.null(nxyt)) nxyt <- nx * ny * nt
  # cubevolume <-  spatstat.geom::volume(win) / nxyt
  cubevolume <- obsvol / nxyt
  volumes <- rep.int(cubevolume, nxyt)

  id <- .grid.index(
    xx, xy, xt, win$xrange, win$yrange, win$zrange,
    nx, ny, nt
  )$index

  w <- .counting.weights(id, volumes)

  Wdat <- w[1:nX]
  Wdum <- w[-c(1:nX)]
  ndata <- nrow(X)
  ndummy <- nrow(dummy_points)

  # dati.interpolati sono i dati delle covariate

  z <- c(rep(1, ndata), rep(0, nrow(dummy_points)))
  y_resp <- z / w
  dati.cov.marks <- data.frame(cbind(y_resp, w, quad_p, dati.interpolati))

  suppressWarnings(mod_global <- try(gam(
    as.formula(paste("y_resp",
      paste(formula, collapse = " "),
      sep = " "
    )),
    weights = w,
    family = poisson,
    data = dati.cov.marks
  ), silent = T))
  summary(mod_global)
  res_global <- coef(mod_global)
  pred_global <- exp(predict(mod_global, newdata = dati.cov.marks[1:nX, ]))

  if (local) {
    nU <- dim(quad_p)[1]
    h_x <- MASS::bandwidth.nrd(x)
    h_y <- MASS::bandwidth.nrd(y)
    h_t <- MASS::bandwidth.nrd(t)
    localwt <- matrix(NA, nrow = nX, ncol = nU)
    if (verbose) {
      cat(paste(
        "\n", "Computing Kernel Densities to the",
        nX, "points", "\n", "\n"
      ))
    }
    for (j in 1:nX) {
      if (verbose) {
        spatstat.geom::progressreport(j, nX)
      }
      localwt[j, ] <- dnorm(xx - x[j], sd = h_x) *
        dnorm(xy - y[j], sd = h_y) * dnorm(xt - t[j], sd = h_t)
    }
    a_s <- localwt * w
    res_local <- matrix(NA, nrow = nX, ncol = length(mod_global$coefficients))
    pred_local <- vector(length = nX)
    if (verbose) {
      cat(paste(
        "\n", "Fitting local model to the", nX, "points",
        "\n", "\n"
      ))
    }
    for (i in 1:nX) {
      if (verbose) {
        spatstat.geom::progressreport(i, nX)
      }
      suppressWarnings(mod_local <- try(glm(as.formula(paste("y_resp",
        paste(formula, collapse = " "),
        sep = " "
      )), weights = a_s[i, ], family = poisson, data = dati.cov.marks), silent = T))
      res_local[i, ] <- mod_local$coefficients
      pred_local[i] <- exp(predict(mod_local, newdata = dati.cov.marks[i, ]))
    }
    res_local <- as.data.frame(res_local)
    colnames(res_local) <- names(res_global)
  }



  time2 <- Sys.time()
  if (local) {
    list.obj <- list(
      IntCoefs = res_global,
      IntCoefs_local = res_local,
      X = X0,
      nX = ndata,
      I = z,
      y_resp = y_resp,
      formula = formula,
      l = as.vector(pred_global),
      l_local = as.vector(pred_local),
      mod_global = mod_global,
      newdata = dati.cov.marks[1:ndata, ],
      ncube = ncube,
      time = paste0(round(as.numeric(difftime(
        time1 = time2,
        time2 = time1,
        units = "sec"
      )), 3), " sec")
    )
    class(list.obj) <- "locstppm"
  } else {
    list.obj <- list(
      IntCoefs = res_global,
      X = X0,
      nX = ndata,
      I = z,
      y_resp = y_resp,
      formula = formula,
      l = as.vector(pred_global),
      mod_global = mod_global,
      newdata = dati.cov.marks[1:ndata, ],
      ncube = ncube,
      time = paste0(round(as.numeric(difftime(
        time1 = time2,
        time2 = time1,
        units = "sec"
      )), 3), " sec")
    )
    class(list.obj) <- "stppm"
  }
  # class(list.obj) <- "stppm"
  return(list.obj)
}
