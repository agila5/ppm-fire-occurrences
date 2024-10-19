# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(here)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
tar_option_set(format = "qs", packages = list_packages()) # debug goes here
tar_config_set(store = here("_targets"))

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
  
  # The end
  tar_quarto(
    name = results, 
    path = here("reports", "results.qmd"), 
    quiet = FALSE
  )
)