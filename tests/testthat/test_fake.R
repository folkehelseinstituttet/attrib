context("attrib")

test_that("Model fit", {

  # n_features <- 5 # incl intercept
  rep <- 2

  pb <- txtProgressBar(min = 0, max = rep, style = 3)
  retval <- vector("list", length = rep)

  response <- "deaths"
  fixef <- " temperature_high +
  pr100_ili_lag_1 +
  sin(2 * pi * (week - 1) / 52) +
  cos(2 * pi * (week - 1) / 52)"

  offset <- "log(pop)"

  # take in the random effects
  ranef <- "(1|location_code) +
  (pr100_ili_lag_1|season)"

  for (i in 1:rep) {
    setTxtProgressBar(pb, i)

    data <- gen_fake_attrib_data()
    suppressWarnings(
      fit <- fit_attrib(
        data = data,
        response = response,
        fixef = fixef,
        ranef = ranef,
        offset = offset
      )
    )
    temp <- colMeans(x = coef(fit)$season, na.rm = TRUE)
    temp <- as.data.frame(temp)
    temp$var <- row.names(temp)
    retval[[i]] <- temp
  }
  retval <- rbindlist(retval)

  results <- retval[, .(
    temp = mean(temp)
  ), keyby = .(
    var
  )]


  testthat::expect_equal(
    round(as.numeric(results$temp, 0)),
    c(-9, 0, 0, 0, 0) # OBSOBS COEFICIANT DEPENDENT!!!!! #obsobs not sure if the oreder will always be the same when using the keyby..
  )
})

test_that("Attributable numbers", {

  # generate data
  data <- gen_fake_attrib_data(2)

  response <- "deaths"
  # take in fixed effects
  fixef <- "temperature_high +
  pr100_ili_lag_1 +
  sin(2 * pi * (week - 1) / 52) +
  cos(2 * pi * (week - 1) / 52)"

  # take in offset
  offset <- "log(pop)"

  # take in the random effects
  ranef <- "(1|location_code) +
  (pr100_ili_lag_1|season)"

  # fit initial model
  suppressWarnings(
    fit <- fit_attrib(
      data = data,
      response = response,
      fixef = fixef,
      ranef = ranef,
      offset = offset
    )
  )
  exposures <- list("pr100_ili_lag_1" = 0, "temperature_high" = 0)
  # data_one <- data[1]
  data <- est_attrib(fit, data, exposures = exposures, n_sim = 500)

  data_copy <- copy(data)
  data_copy <- data_copy[, .(
    attr_pr100_ili_lag_1 = median(`sim_value_exposures=observed` - `sim_value_pr100_ili_lag_1=0`),
    attr_temperature_high = median(`sim_value_exposures=observed` - `sim_value_temperature_high=0`)
  ),
  keyby = .(season, location_code, id, week)
  ]

  # verify that your model is giving you results like you expect
  # influenza
  testthat::expect_gt(mean(data_copy$attr_pr100_ili_lag_1), 0)

  testthat::expect_lt(
    sum(data_copy[week >= 21 & week <= 39]$attr_pr100_ili_lag_1),
    sum(data_copy[week >= 40 | week <= 20]$attr_pr100_ili_lag_1)
  )

  # heat_wave
  testthat::expect_gt(mean(data_copy$attr_temperature_high), 0) # thisi is now for temperature not heatwaves!!


  # general expect more deaths during wintern no mather the cause
  testthat::expect_lt(
    sum(data[week >= 21 & week <= 39]$deaths),
    sum(data[week >= 40 | week <= 20]$deaths)
  )
})


test_that("simmulations", {
  data <- gen_fake_attrib_data(4)

  response <- "deaths"
  # take in the fixed effects
  fixef <- "temperature_high +
  pr100_ili_lag_1 +
  sin(2 * pi * (week - 1) / 52) +
  cos(2 * pi * (week - 1) / 52)"

  offset <- "log(pop)"
  # take in the random effects
  ranef <- "(1|location_code) +
  (pr100_ili_lag_1|season)"

  # ranef <- "(pr100_ili_lag_1|season)"

  suppressWarnings(
    fit <- fit_attrib(data, response = response, fixef = fixef, ranef = ranef, offset = offset)
  )

  exposures <- list("pr100_ili_lag_1" = 0, "temperature_high" = 0)
  est_mort <- est_attrib(fit, data, exposures, n_sim = 500)

  # predict mean
  pred <- exp(lme4:::predict.merMod(fit, data))
  est_mean <- est_mort[, .(observed_value_median = median(`sim_value_exposures=observed`)), keyby = .(id, location_code, week, season, yrwk, pop, deaths)]

  dif_mean <- pred - est_mean$observed_value_median
  mean(dif_mean)
  median(dif_mean)

  testthat::expect_equal(
    round(as.numeric(median(dif_mean), 0)),
    c(0)
  )

  dif_obs <- est_mean$deaths - est_mean$observed_value_median
  mean(dif_obs)
  median(dif_obs)

  testthat::expect_equal(
    round(as.numeric(median(dif_obs), 0)),
    c(0)
  )

  dif <- est_mean$deaths - pred
  median(dif)

  testthat::expect_equal(
    round(as.numeric(median(dif), 0)),
    c(0)
  )
})

# test_that("lag_data", {
#   data <- gen_fake_attrib_data(2)
#
#   response = "deaths"
#   # take in the fixed effects
#   fixef <- "temperature_high +
#   lag_data(x = pr100_ili, lags = 2, ref = 0, by = location_code) +
#   sin(2 * pi * (week - 1) / 52) +
#   cos(2 * pi * (week - 1) / 52)"
#
#   offset <- "log(pop)"
#   # take in the random effects
#   ranef <- "(1|location_code) +(lag_data(x = pr100_ili, lags = 2, ref = 0, by = location_code)|season)"
#
#
#   suppressWarnings(
#     fit <- fit_attrib(data, response = response, fixef = fixef, ranef = ranef, offset = offset)
#   )
#
#   exposures <- list("pr100_ili" = 0, "temperature_high" = 0)
#   #est_mort <- est_mort(fit, data, exposures)
#   sim <- sim_2(fit, data)
#
#   diff <- sim$deaths - sim$expected_mort
#   median(diff)
#
#   est_mort <- est_attrib_2(fit, data, exposures)
#
#   # predict mean
#   pred <- exp(lme4:::predict.merMod(fit, data))
#   est_mean <- est_mort[, .(exp_mort_median = median(exp_mort_observed)), keyby = .(id, location_code, week, season, yrwk, pop, deaths)]
#
#   dif_mean <- pred - est_mean$exp_mort_median
#   mean(dif_mean)
#   median(dif_mean)
#
#   testthat::expect_equal(
#     round(as.numeric(median(dif_mean), 0)),
#     c(0)
#   )
#
#   dif_obs <- est_mean$deaths - est_mean$exp_mort_median
#   mean(dif_obs)
#   median(dif_obs)
#
#   testthat::expect_equal(
#     round(as.numeric(median(dif_obs), 0)),
#     c(0)
#   )
#
#   dif <- est_mean$deaths - pred
#   median(dif)
#
#   testthat::expect_equal(
#     round(as.numeric(median(dif), 0)),
#     c(0)
#   )
#
#
# })
