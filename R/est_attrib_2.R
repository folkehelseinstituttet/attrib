est_attrib_2 <- function(
                         fit,
                         data,
                         exposures) {
  if (length(which(is.na(data))) != 0) {
    stop("The dataset has NA values")
  }

  if (is.null(attr(fit, "fit_fix"))) {
    stop("Fit is missing attribute fit_fix and possibly not computed by fit_attrib") # Maybe a different message, you decide :)
  }

  if (is.null(attr(fit, "response"))) {
    stop("Fit is missing attribute fit_fix and possibly not computed by fit_attrib") # Maybe a different message, you decide :)
  }

  if (length(exposures) == 0) {
    stop("Exposures is empthy")
  }
  for (i in seq_along(exposures)) {
    if (!names(exposures)[i] %in% colnames(data)) {
      stop(glue::glue("Exposure {names(exposures)[i]} is not in the dataset"))
    }
  }


  id <- NULL
  tag <- NULL
  id_row <- NULL

  data_ret_val <- copy(data)
  data_ret_val[, id := 1:.N]

  col_names_orig <- colnames(data)

  data_observed <- copy(data)
  data_observed[, id := 1:.N]
  data_observed$tag <- "observed"
  # data_ret_val_2 = copy(data)
  data_tot <- vector("list", length = length(exposures) + 1)
  data_tot[[1]] <- data_observed
  for (i in seq_along(exposures)) {
    data_reference <- copy(data)
    data_reference[, id := 1:.N]
    data_reference <- data_reference[, glue::glue({
      names(exposures)[i]
    }) := exposures[[i]]]
    data_reference$tag <- as.character(glue::glue("ref_{names(exposures)[i]}"))
    data_tot [[i + 1]] <- data_reference
  }
  data_tot <- rbindlist(data_tot)

  data_tot_ret <- sim_2(fit, data_tot)

  data_ret_val <- data_tot_ret[tag == "observed"]
  setnames(data_ret_val, "expected_mort", "exp_mort_observed")
  # this works but is a bit sslow
  for (i in seq_along(exposures)) {
    data_ret_temp <- data_tot_ret[tag == glue::glue("ref_{names(exposures)[i]}")]
    data_ret_val[data_ret_temp,
      on = c("sim_id", "id"),
      glue::glue("exp_mort_{names(exposures)[i]}={(exposures)[i]}") := data_ret_temp$expected_mort
    ]
  }

  data_ret_val[, tag := NULL]
  data_ret_val[, id_row := NULL]
  return(data_ret_val)
}
