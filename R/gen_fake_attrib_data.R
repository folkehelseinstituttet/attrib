#' Fake data for mortality in Norway
#'
#' @format
#' \describe{
#' \item{location_code}{Location code of the Norwegian municipalities}
#' \item{week}{Week}
#' \item{season}{Season used for influenza like illnesses}
#' \item{yrwk}{Year and week}
#' \item{x}{Number of weeks from the start of the season}
#' \item{pop}{Population size}
#' \item{pr100_ili}{Per hundred ILI, percentage of consultations diagnosed as influenza like illnesses}
#' \item{pr100_ili_lag_1}{pr100_ili_lag_1}
#' \item{temperature}{ temperature}
#' \item{temperature_high}{temperature_high}
#' \item{deaths}{deaths}
#' }
"data_fake_county"

#' Fake data for mortality in Norway nationally
#'
#' @format
#' \describe{
#' \item{location_code}{Location code}
#' \item{week}{Week}
#' \item{season}{Season used for influenza like illnesses}
#' \item{yrwk}{Year and week}
#' \item{x}{Number of weeks from the start of the season}
#' \item{pop}{Population size}
#' \item{pr100_ili}{Per hundred ILI, percentage of consultations diagnosed as influenza like illnesses}
#' \item{pr100_ili_lag_1}{pr100_ili_lag_1}
#' \item{temperature}{ temperature}
#' \item{temperature_high}{temperature_high}
#' \item{deaths}{deaths}
#' }
"data_fake_nation"

# Generates fake data
#
# This function generates a fake dataset with parameters
# @param n_locations Telling how many locations one wants in the output data, default = 11 the number of municipalities in Norway.

gen_fake_attrib_data <- function(n_locations = 11) {
  yrwk <- NULL
  x <- NULL
  season <- NULL
  level <- NULL
  . <- NULL
  pop <- NULL
  peak_center_influenza <- NULL
  hight_peak <- NULL
  influenza_coef <- NULL
  normal_base <- NULL
  pr100_ili <- NULL
  pr100_ili_lag_1 <- NULL
  pr100_ili_lag_2 <- NULL
  mean_temperature <- NULL
  temperature <- NULL
  temperature_high <- NULL
  temperature_spline_1 <- NULL
  temperature_spline_2 <- NULL
  temperature_spline_3 <- NULL
  mu <- NULL
  deaths <- NULL


  start_date <- as.Date("2009-07-20")
  end_date <- as.Date("2020-07-19")

  location_code <- c("county03", "county11", "county15", "county18", "county30", "county34", "county38", "county42", "county46", "county50", "county54")
  # unique(fhidata::norway_locations_b2020$county_code)
  # location_code <- "norway"
  skeleton <- expand.grid(
    location_code = location_code,
    date = seq.Date(
      from = start_date,
      to = end_date,
      by = 7 # to get a weakly base.
    ),
    stringsAsFactors = FALSE
  )
  setDT(skeleton)

  skeleton[, year := isoyear_n(date)]
  skeleton[, week := isoweek_n(date)]
  skeleton[, yrwk := isoyearweek(date)]
  skeleton[, x := x(week)]
  skeleton[, season := season(yrwk)]


  x_pop <- data.table(
    location_code = c("county03", "county11", "county15", "county18", "county30", "county34", "county38", "county42", "county46", "county50", "county54"),
    pop = c(693494, 479892, 265238, 241235, 1241165, 371385, 419396, 307231, 636531, 468702, 243311)
  )

  # x_pop <- fhidata::norway_population_b2020[level == "county", .(
  #   pop = sum(pop)
  # ), keyby = .(
  #   year,
  #   location_code
  # )]
  skeleton[
    x_pop,
    on = c("location_code"),
    pop := pop
  ]

  # skeleton[, pop := 5367580]
  ######## seasonbased influenza
  #### still without any lag.
  skeleton_season <- unique(skeleton[, c("location_code", "season")])
  skeleton_season[, peak_center_influenza := round(stats::rnorm(.N, mean = 28, sd = 3))]
  skeleton_season[, hight_peak := stats::rnorm(.N, mean = 2, sd = 0.02)]
  skeleton_season[, influenza_coef := stats::rnorm(.N, mean = 0.03, sd = 0.02)]

  skeleton <- merge(
    skeleton,
    skeleton_season,
    by = c("location_code", "season")
  )

  skeleton[, normal_base := stats::dnorm(x, peak_center_influenza, 5)]
  skeleton[, pr100_ili := 10 * 1.2 * hight_peak * normal_base] # something strange doing on her but this gives pr100ili around 2
  skeleton[pr100_ili < 0, pr100_ili := 0] # should there be some more randomness here??

  skeleton[, pr100_ili_lag_1 := shift(pr100_ili, fill = 0), by = c("location_code")]
  skeleton[, pr100_ili_lag_2 := shift(pr100_ili, n = 2L, fill = 0), by = c("location_code")]

  # temperature

  skeleton_weeks_temp <- unique(skeleton[, c("location_code", "week")])
  skeleton_weeks_temp[, mean_temperature := (26 - abs((week - 26)))]
  skeleton_weeks_temp[, mean_temperature := c(skeleton_weeks_temp[(.N - 4):.N]$mean_temperature, skeleton_weeks_temp[1:(.N - 5)]$mean_temperature) - 5]

  skeleton <- merge(
    skeleton,
    skeleton_weeks_temp,
    by = c("location_code", "week")
  )
  skeleton[, temperature := stats::rnorm(
    n = .N,
    mean = mean_temperature, # temperature span between -5,20 on average
    sd = 5
  )]

  skeleton[, temperature_high := 0]
  skeleton[temperature > 20, temperature_high := stats::rbinom(.N, 7, 0.2)]

  # skeleton[, temperature_spline_1 := splines::ns(skeleton$temperature, df=3)[,1]]
  # skeleton[, temperature_spline_2 := splines::ns(skeleton$temperature, df=3)[,2]]
  # skeleton[, temperature_spline_3 := splines::ns(skeleton$temperature, df=3)[,3]]

  # remove covid to pas the tests
  # # covid-19
  # skeleton[, pr100_covid19:= 0]
  # skeleton[date>="2020-03-01", pr100_covid19:= rnorm(.N, mean = 0.0042, sd = 0.001)]
  # skeleton[, pr100_covid19_lag_1 := shift(pr100_covid19, fill = 0), by = c("location_code")]
  # skeleton[, pr100_covid19_lag_2 := shift(pr100_covid19, n= 2L, fill = 0), by = c("location_code")]


  # generate deaths

  skeleton[, mu := exp(-8.8 +
    0.08 * temperature_high +
    # 0.25*influenza_coef * pr100_ili +
    influenza_coef * pr100_ili_lag_1 +
    # 10*pr100_covid19_lag_1 +
    0.02 * sin(2 * pi * (week - 1) / 52) + 0.07 * cos(2 * pi * (week - 1) / 52) + # finn a og b
    # 1*pr100_ili_lag_2 +
    log(pop))]


  skeleton[, deaths := stats::rpois(n = .N, lambda = mu)]


  # fit <- lme4::glmer(deaths ~ (1|location_code) +
  #                      #splines::ns(skeleton$temperature, df=3) +
  #                      temperature_high +
  #                      pr100_ili_lag_1 +
  #                      (pr100_ili_lag_1|season) +
  #                      sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52)+
  #                      offset(log(pop)),
  #                    data = skeleton,
  #                    family = "poisson")
  # summary(fit)
  #
  #
  # death_tot <- skeleton[, .(
  #   death = sum(deaths),
  #   year
  # ), keyby = .(
  #   date
  # )]
  # min(death_tot$death)
  # max(death_tot$death)
  # get unique loctation codes, return n first.

  # fake_data_colums <- c("location_code", "week", "season", "year", "yrwk", "x", "pop", "pr100_ili", "pr100_ili_lag_1", "temperature", "temperature_high", "deaths")
  # data_fake_county <- skeleton[, ..fake_data_colums]
  # save(data_fake_county, file = "data/data_fake_county.rda", compress = "bzip2")
  #
  # data_fake_nation <- data_fake_county[, .(
  #   location_code = "norge",
  #   pop = sum(pop),
  #   pr100_ili = mean(pr100_ili),
  #   pr100_ili_lag_1 = mean(pr100_ili_lag_1),
  #   temperature_high = min(temperature_high),
  #   deaths = sum(deaths)
  # ), keyby = .(
  #     week,
  #     season,
  #     year,
  #     yrwk,
  #     x
  # )]
  #
  # save(data_fake_nation, file = "data/data_fake_nation.rda", compress = "bzip2")
  #
  #

  locations <- unique(skeleton$location_code)
  locations_current <- locations[1:n_locations]
  return(skeleton[location_code %in% locations_current])
}
