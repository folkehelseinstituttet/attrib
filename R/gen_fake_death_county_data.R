#' Fake data for mortalityregistration
#'
#' @format
#' \describe{
#' \item{doe}{Date og event}
#' \item{dor}{Date of registration}
#' \item{location_code}{Location code}
#' }
"data_fake_nowcasting_county_raw"


# Generates fake data
#
# This function generates a fake dataset with parameters
# @param n_locations Telling how many locations one wants in the output data, default = 11 the number of municipalities in Norway.


gen_fake_death_data_county <- function() {
  doe <- NULL
  dor <- NULL
  reg_lag <- NULL
  . <- NULL
  location_code <- NULL
  pop_frac <- NULL
  pop <- NULL
  mu <- NULL
  death_exp <- NULL
  deaths_exp <- NULL
  deaths <- NULL
  location_intercept <- NULL



  start_date <- as.Date("2018-01-01")
  end_date <- as.Date("2020-01-01")

  location_code <- c("county03", "county11", "county15", "county18", "county30", "county34", "county38", "county42", "county46", "county50", "county54")

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

  pop_data <- fhidata::norway_population_by_age_cats(cats = list(c(1:120)))[location_code %in% c("county03",
                                                                                                 "county11",
                                                                                                 "county15",
                                                                                                 "county18",
                                                                                                 "county30",
                                                                                                 "county34",
                                                                                                 "county38",
                                                                                                 "county42",
                                                                                                 "county46",
                                                                                                 "county50",
                                                                                                 "county54") & year == 2021]
  pop_data[, pop_frac:= pop/5312753 ]

  skeleton[, year := isoyear_n(date)]
  skeleton[, week := isoweek(date)]

  ranef <- data.table(location_code = location_code,
             location_intercept = c(-0.18, -0.21, 0.04, 0.13, -0.05, 0.25, 0.10, -0.01, -0.06, -0.4, 0.3))

  skeleton[pop_data, on = c("location_code"), pop_frac:= pop_frac]
  skeleton[ranef, on = c("location_code"), location_intercept := location_intercept]
  skeleton[, mu:=(  16.4+ 0.0425475*sin(2 * pi * (week) / 53) + 0.0801632*cos(2 * pi * (week ) / 53) -0.0124715* year + location_intercept) ]
  skeleton[pop_data, on = c("location_code"), pop:= pop]
  skeleton[, deaths_exp:= round(exp(mu)*pop*1/7)]
  skeleton[, deaths := stats::rpois(.N, deaths_exp)]

  temp_vec <- vector( "list", length = length(skeleton))

  for (i in seq(1:nrow(skeleton))){
    #print(i)
    if (skeleton[i, deaths]>0){
      skeleton_temp <- expand.grid(
        date = skeleton[i, date],
        location_code = skeleton[i, location_code],
        count = seq(1, skeleton[i, deaths], by = 1),
        stringsAsFactors = FALSE
      )
      setDT(skeleton_temp)
      temp_vec[[i]] <- as.data.table(skeleton_temp)
    }
  }

  skeleton_death <- rbindlist(temp_vec)

  skeleton_death[, doe := date]
  skeleton_death[, reg_lag := stats::rpois(.N, 18)]
  skeleton_death[, dor := doe + reg_lag]

  # data_fake_nowcasting_county_raw <- skeleton_death[,.(doe, dor, location_code)]
  # save(data_fake_nowcasting_county_raw, file = "data/data_fake_nowcasting_county_raw.rda", compress = "bzip2")

  return(skeleton_death[,.(doe, dor, location_code)])
}
