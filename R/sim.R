#' Generates simulations of expected mortality by simulating the model coefficients.
#'
#' With the given fit from fit_attrib the function sim, from package arm as described in Gelman, Hill (2012) <doi:10.1017/CBO9780511790942>, is used to generate 500 simulations
#' of all the coefficients, from there respective posterior distributions.
#' This is then used to compute the expected response for all simulations and rows in the input dataset.
#'
# For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param fit A model fit created by fit_attrib
#' @param data The data with either observed values or reference values.
#' @param n_sim Number of simulations
#'
#' @examples
#'
#' response <- "deaths"
#' fixef <- "pr100_ili_lag_1 + sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52)"
#' ranef <- " (pr100_ili_lag_1| season)"
#' offset <- "log(pop)"
#'
#' data <- attrib::data_fake_nation
#'
#' fit <- fit_attrib(data = data, response = response, fixef = fixef, ranef = ranef, offset = offset)
#'
#' n_sim <- 5
#' sim(fit, data, n_sim)
#' @return A dataset with 500 simulations of the expected response for each row in the original dataset.
#' @export
sim <- function(
                fit,
                data,
                n_sim) {
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

  fix_eff <- attr(fit, "fit_fix")
  offset <- attr(fit, "offset")
  response <- attr(fit, "response")
  x <- arm::sim(fit, n.sims = n_sim)

  # get the design matrix for the fixed effects
  data_fix <- stats::model.frame(fix_eff, data = data)
  data_fix_copy <- data.table::as.data.table(data_fix)
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
  pb <- progress::progress_bar$new(total = length(x@ranef) + 3)

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
        expected_ran <- expected_ran + coefficients[, data[[grouping]]] %*% diag(data[[variable]])
      }
    }
    if (interactive()) pb$tick()
  }
  # print("loop over")
  # add together the coefficients for the fixed and random effects


  expected <- as.data.table(exp(expected_fix + expected_ran))

  expected_t <- data.table::transpose(expected)
  expected_t$id_row <- 1:nrow(data)
  data$id_row <- 1:nrow(data)

  if (interactive()) pb$tick()
  new_data <- merge(data, expected_t, by = "id_row", all = TRUE)
  if (interactive()) pb$tick()
  new_data <- data.table::melt(new_data, id.vars = c(col_names, "id_row"))
  if (interactive()) pb$tick()

  setnames(new_data, "variable", "sim_id")
  new_data$sim_id <- as.numeric(as.factor(new_data$sim_id))
  setnames(new_data, "value", "sim_value")

  return(new_data)
}
