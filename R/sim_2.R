
# With this one it is possibel to have lags/splines in the random effects as well with the restriction that
# they have to be present in the fixed effects as well
sim_2 <- function(
                  fit,
                  data) {
  if (length(which(is.na(data))) != 0) {
    stop("The dataset has NA values")
  }

  if (is.null(attr(fit, "fit_fix"))) {
    stop("Fit is missing attribute fit_fix and possibly not computed by fit_attrib") # Maybe a different message, you decide :)
  }

  if (is.null(attr(fit, "response"))) {
    stop("Fit is missing attribute fit_fix and possibly not computed by fit_attrib") # Maybe a different message, you decide :)
  }

  col_names <- colnames(data)

  n_sim <- 500

  fix_eff <- attr(fit, "fit_fix")
  offset <- attr(fit, "offset")
  response <- attr(fit, "response")
  x <- arm::sim(fit, n.sims = n_sim)

  # get the design matrix for the fixed effects
  data_fix <- stats::model.frame(fix_eff, data = data)
  data_fix_copy <- as.data.table(data_fix)
  data_fix_copy[, (response) := NULL]

  x_fix <- as.data.frame(as.matrix(x@fixef))

  r_names <- rownames(rbind(1, as.matrix(t(data_fix_copy))))
  c_names <- colnames(as.matrix(x@fixef))
  count <- 0
  for (i in (2:(length(r_names) - 1))) {
    # print(i)
    r_cur <- r_names[i]
    c_cur <- c_names[i - count]

    check <- FALSE

    c_check <- stringr::str_replace_all(c_cur, "[:(, =)*/-]", ".")
    r_check <- stringr::str_replace_all(r_cur, "[:(, =)*/-]", ".")
    # print(c_check == r_cur)
    # print(r_cur)
    # print(c_check)

    if (c_check == r_check) {
      check <- TRUE
      next
    }

    split <- strsplit(c_check, "")[[1]]
    if (split[length(split) - 1] == ".") {
      p <- paste0(substr(c_check, 1, (nchar(c_check) - 1)), ".", substr(c_check, nchar(c_check), nchar(c_check)), collapse = NULL)
      if (p == r_check) {
        check <- TRUE
        next
      }
    }

    if (check == FALSE) {
      x_fix <- tibble::add_column(x_fix, extra = 0, .after = (i - 1 + count))
      count <- count + 1
    }
  }

  # multiply it out

  dim(cbind(as.matrix(x_fix), 1))
  dim(rbind(1, as.matrix(t(data_fix_copy))))

  colnames(cbind(as.matrix(x_fix), 1))
  rownames(rbind(1, as.matrix(t(data_fix_copy))))

  # add the offset!!
  if (is.null(offset)) {
    cbind(as.matrix(x_fix)) %*% rbind(1, as.matrix(t(data_fix_copy)))
  } else {
    expected_fix <- cbind(as.matrix(x_fix), 1) %*% rbind(1, as.matrix(t(data_fix_copy)))
  }
  # set up the results for random effects
  expected_ran <- matrix(0, ncol = ncol(expected_fix), nrow = nrow(expected_fix))

  # slowly add in each of the random effects
  i <- j <- k <- 1
  pb <- utils::txtProgressBar(min = 0, max = (length(x@ranef)), style = 3)

  nrow(data_fix_copy)
  nrow(data)
  data_merge <- as.data.frame(cbind(c(data_fix_copy, data)))

  for (i in 1:length(x@ranef)) {
    grouping <- names(x@ranef)[i]
    for (j in 1:dim(x@ranef[[i]])[3]) {
      # print(j)
      variable <- dimnames(x@ranef[[i]])[[3]][j]
      coefficients <- x@ranef[[i]][, , j]
      if (variable == "(Intercept)") {
        # print(dim(expected_ran))
        # print(dim(coefficients[,data[[grouping]]]))
        expected_ran <- expected_ran + coefficients[, data[[grouping]]]
      } else {
        # print(dim(expected_ran))
        # print(dim(coefficients[,data[[grouping]]]))
        # print("non_intercept")

        data_cur <- data[[variable]]
        if (is.null(data_cur)) {
          variable_cur <- stringr::str_replace_all(variable, "[:(, =)*/-]", ".")
          data_cur <- data_fix_copy[[variable_cur]]
        }

        if (is.null(data_cur)) {
          variable_cur <- paste0(substr(variable_cur, 1, (nchar(variable_cur) - 1)), ".",
            substr(variable_cur, nchar(variable_cur), nchar(variable_cur)),
            collapse = NULL
          )
          data_cur <- data_fix_copy[[variable_cur]]
        }

        expected_ran <- expected_ran + coefficients[, data[[grouping]]] %*% diag(data_cur)
      }
    }
    utils::setTxtProgressBar(pb, i)
  }
  # print("loop over")
  # add together the coefficients for the fixed and random effects


  expected <- as.data.table(exp(expected_fix + expected_ran))

  expected_t <- data.table::transpose(expected)
  expected_t$id_row <- 1:nrow(data)
  data$id_row <- 1:nrow(data)

  new_data <- merge(data, expected_t, by = "id_row", all = TRUE)
  new_data <- data.table::melt(new_data, id.vars = c(col_names, "id_row"))

  setnames(new_data, "variable", "sim_id")
  new_data$sim_id <- as.numeric(as.factor(new_data$sim_id))
  setnames(new_data, "value", "expected_mort")

  return(new_data)
}
