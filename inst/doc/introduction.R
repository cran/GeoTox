## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(GeoTox)
library(dplyr)

n <- 250 # Sample size

## -----------------------------------------------------------------------------
set.seed(2357)
geoTox <- GeoTox() |> 
  # Set region and group boundaries (for plotting)
  set_boundaries(region = geo_tox_data$boundaries$county,
                 group  = geo_tox_data$boundaries$state) |> 
  # Simulate populations for each region
  simulate_population(age           = split(geo_tox_data$age, ~FIPS),
                      obesity       = geo_tox_data$obesity,
                      exposure      = split(geo_tox_data$exposure, ~FIPS),
                      simulated_css = geo_tox_data$simulated_css,
                      n             = n) |> 
  # Estimated Hill parameters
  set_hill_params(geo_tox_data$dose_response |>
                    filter(endp == "TOX21_H2AX_HTRF_CHO_Agonist_ratio") |>
                    fit_hill(chem = "casn") |> 
                    filter(!tp.sd.imputed, !logAC50.sd.imputed)) |>
  # Calculate response
  calculate_response() |>
  # Perform sensitivity analysis
  sensitivity_analysis()

geoTox

## ----fig.width = 7, fig.height = 3, fig.align = 'center'----------------------
plot(geoTox)
plot(geoTox, type = "hill")
plot(geoTox, type = "sensitivity")

## -----------------------------------------------------------------------------
set.seed(2357)
geoTox <- GeoTox() |> 
  # Set region and group boundaries (for plotting)
  set_boundaries(region = geo_tox_data$boundaries$county,
                 group  = geo_tox_data$boundaries$state) |> 
  # Simulate populations for each region
  simulate_population(age           = split(geo_tox_data$age, ~FIPS),
                      obesity       = geo_tox_data$obesity,
                      exposure      = split(geo_tox_data$exposure, ~FIPS),
                      simulated_css = geo_tox_data$simulated_css,
                      n             = n) |> 
  # Estimated Hill parameters
  set_hill_params(geo_tox_data$dose_response |>
                    fit_hill(assay = "endp", chem = "casn") |> 
                    filter(!tp.sd.imputed, !logAC50.sd.imputed)) |>
  # Calculate response
  calculate_response() |>
  # Perform sensitivity analysis
  sensitivity_analysis()

geoTox

## ----fig.width = 7, fig.height = 3, fig.align = 'center'----------------------
plot(geoTox)
plot(geoTox, assays = "TOX21_H2AX_HTRF_CHO_Agonist_ratio")
plot(geoTox, type = "hill")
plot(geoTox, type = "sensitivity")
plot(geoTox, type = "sensitivity", assay = "TOX21_H2AX_HTRF_CHO_Agonist_ratio")

## ----fig.width = 7, fig.height = 3, fig.align = 'center'----------------------
plot(geoTox, type = "exposure", ncol = 5)

## ----fig.width = 7, fig.height = 3, fig.align = 'center'----------------------
plot(geoTox, type = "exposure", chem_label = "chnm", ncol = 5)

