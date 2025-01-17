#' GeoTox S3 object
#' 
#' @description
#' An S3 object that can be used to help organize the data and results of a
#' GeoTox analysis.
#'
#' @return a GeoTox S3 object
#' @export
#' 
#' @examples
#' # Use a subset of the package data for demonstration purposes
#' set.seed(2357)
#' n <- 10 # Population size
#' m <- 5 # Number of regions
#' idx <- if (m < 100) sample(1:100, m) else 1:100
#' 
#' geoTox <- GeoTox() |> 
#'   # Set region and group boundaries (for plotting)
#'   set_boundaries(region = geo_tox_data$boundaries$county,
#'                  group  = geo_tox_data$boundaries$state) |> 
#'   # Simulate populations for each region
#'   simulate_population(age           = split(geo_tox_data$age, ~FIPS)[idx],
#'                       obesity       = geo_tox_data$obesity[idx, ],
#'                       exposure      = split(geo_tox_data$exposure, ~FIPS)[idx],
#'                       simulated_css = geo_tox_data$simulated_css,
#'                       n             = n) |> 
#'   # Estimated Hill parameters
#'   set_hill_params(geo_tox_data$dose_response |>
#'                     fit_hill(assay = "endp", chem = "casn") |> 
#'                     dplyr::filter(!tp.sd.imputed, !logAC50.sd.imputed)) |>
#'   # Calculate response
#'   calculate_response() |>
#'   # Perform sensitivity analysis
#'   sensitivity_analysis()
#' 
#' # Print GeoTox object
#' geoTox
#' 
#' # Plot hill fits
#' plot(geoTox, type = "hill")
#' # Plot exposure data
#' plot(geoTox, type = "exposure", ncol = 5)
#' # Plot response data
#' plot(geoTox)
#' plot(geoTox, assays = "TOX21_H2AX_HTRF_CHO_Agonist_ratio")
#' # Plot sensitivity data
#' plot(geoTox, type = "sensitivity")
#' plot(geoTox, type = "sensitivity", assay = "TOX21_H2AX_HTRF_CHO_Agonist_ratio")
GeoTox <- function() {
  structure(
    list(
      par = list(
        n = 1e3,
        IR_params = NULL,
        obesity = list(obes_prev  = "OBESITY_CrudePrev",
                       obes_sd    = "OBESITY_SD",
                       obes_label = "FIPS"),
        exposure = list(expos_mean  = "mean",
                        expos_sd    = "sd",
                        expos_label = "casn"),
        internal_dose = list(time    = 1,
                             BW      = 1,
                             scaling = 1),
        resp = list(max_mult = 1.5)
      )
    ),
    class = "GeoTox")
}

#' @export
print.GeoTox <- function(x, ...) {
  
  names_simulated <- c("age", "IR", "obesity", "C_ext", "C_ss")
  names_computed <- c("D_int", "C_invitro", "resp", "sensitivity")
  names_other <- setdiff(names(x),
                         c(names_simulated, names_computed))
  
  get_info <- function(names) {
    info <- lapply(names, \(name) {
      class <- dim <- ""
      if (is.null(x[[name]])) {
        return(data.frame(Name = name, Class = "", Dim = ""))
      }
      is_list <- inherits(x[[name]], "list")
      if (is_list && length(x[[name]]) > 0) {
        item <- x[[name]][[1]]
      } else if (!is_list) {
        item <- x[[name]]
      } else {
        item <- NULL
      }
      class <- class(item)
      if (any(c("matrix", "data.frame") %in% class)) {
        dim <- paste(dim(item), collapse = " x ")
      } else {
        dim <- length(item)
      }
      if (is_list) {
        dim <- paste0(length(x[[name]]), " x (", dim, ")")
        class <- paste0("list(", class[[1]], ")")
      } else {
        class <- paste(class, collapse = ", ")
      }
      data.frame(Name = name, Class = class, Dim = dim)
    })
    do.call(rbind, info)
  }
  
  info_simulated <- get_info(names_simulated)
  info_simulated <- info_simulated[info_simulated$Class != "", , drop = FALSE]
  info_computed <- get_info(names_computed)
  info_computed <- info_computed[info_computed$Class != "", , drop = FALSE]
  
  cat("GeoTox object\n")
  if (is.null(x$hill_params)) {
    n_assays <- 0
    n_chems <- 0
  } else {
    if ("assay" %in% names(x$hill_params)) {
      n_assays <- length(unique(x$hill_params$assay))
    } else {
      n_assays <- 1
    }
    if ("chem" %in% names(x$hill_params)) {
      n_chems <- length(unique(x$hill_params$chem))
    } else {
      n_chems <- 1
    }
  }
  cat("Assays: ", n_assays, "\n", sep = "")
  cat("Chemicals: ", n_chems, "\n", sep = "")
  if (nrow(info_simulated) > 0) {
    n_regions <- length(x[[info_simulated$Name[1]]])
  } else if (nrow(info_computed) > 0) {
    n_regions <- length(x[[info_computed$Name[1]]])
  } else {
    n_regions <- 0
  }
  cat("Regions: ", n_regions, "\n", sep = "")
  cat("Population: ", x$par$n, "\n", sep = "")
  cat("Data Fields:")
  if (nrow(info_simulated) > 0) {
    cat("\n")
    print(info_simulated, row.names = FALSE, print.gap = 2)
  } else {
    cat(" None\n")
  }
  cat("Computed Fields:")
  if (nrow(info_computed) > 0) {
    cat("\n")
    print(info_computed, row.names = FALSE, print.gap = 2)
  } else {
    cat(" None\n")
  }
  cat("Other Fields:")
  if (length(names_other) > 0) {
    cat(" ", paste(names_other, collapse = ", "), "\n", sep = "")
  } else {
    cat(" None\n")
  }
}

#' @rdname GeoTox
#'
#' @param x GeoTox object.
#' @param type type of plot.
#' @param ... arguments passed to subsequent methods.
#'
#' @seealso [plot_resp], [plot_hill], [plot_exposure], [plot_sensitivity]
#' @export
plot.GeoTox <- function(x,
                        type = c("resp", "hill", "exposure", "sensitivity"),
                        ...) {
  type <- match.arg(type)
  if (type == "resp") {
    if (is.null(x$resp)) {
      stop("No response data found.", call. = FALSE)
    }
    if (is.null(x$boundaries$region)) {
      stop("No region boundary data found.", call. = FALSE)
    }
    dots = list(...)
    metric = dots$metric %||% "GCA.Eff"
    assays = dots$assays
    assay_quantiles = dots$assay_quantiles %||% c("Median" = 0.5)
    assay_summary = dots$assay_summary %||% FALSE
    summary_quantiles = dots$summary_quantiles %||% c("10th percentile" = 0.1)
    df <- resp_quantiles(x$resp,
                         metric = metric,
                         assays = assays,
                         assay_summary = assay_summary,
                         assay_quantiles = assay_quantiles,
                         summary_quantiles = summary_quantiles)
    plot_resp(df,
              region_boundary = x$boundaries$region,
              group_boundary = x$boundaries$group,
              assay_quantiles = assay_quantiles,
              summary_quantiles = summary_quantiles)
  } else if (type == "hill") {
    plot_hill(x$hill_params,
              ...)
  } else if (type == "exposure") {
    dots = list(...)
    chem_label = dots$chem_label %||% x$par$exposure$expos_label
    ncol = dots$ncol %||% 2
    plot_exposure(x$exposure,
                  region_boundary = x$boundaries$region,
                  group_boundary  = x$boundaries$group,
                  chem_label      = chem_label,
                  ncol            = ncol)
  } else if (type == "sensitivity") {
    plot_sensitivity(x, ...)
  }
}
