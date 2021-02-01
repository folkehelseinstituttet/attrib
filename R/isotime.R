# isoyear_c
# @param date The date of interest

isoyear_c <- function(date = lubridate::today()) {
  yr <- format.Date(date, "%G")
  return(yr)
}

# isoyear_n
# @param date The date of interest

isoyear_n <- function(date = lubridate::today()) {
  yr <- as.numeric(isoyear_c(date))
  return(yr)
}

# isoweek_c
# @param date The date of interest

isoweek_c <- function(date = lubridate::today()) {
  # wk <- data.table::isoweek(date)
  # wk <- formatC(wk, flag = "0", width = 2)
  wk <- format.Date(date, "%V")
  return(wk)
}

# isoweek_n
# @param date The date of interest

isoweek_n <- function(date = lubridate::today()) {
  wk <- as.numeric(isoweek_c(date))
  return(wk)
}

# isoyearweek
# @param date The date of interest

isoyearweek <- function(date = lubridate::today()) {
  return(sprintf("%s-%s", isoyear_n(date), isoweek_c(date)))
}

# start_of_season
# @param yrwk a
# @param start_week the start week of the season

start_of_season <- function(yrwk, start_week = 30) {
  retval <- as.numeric(stringr::str_split(yrwk, "-")[[1]])
  yr <- retval[1]
  wk <- retval[2]

  if (wk >= start_week) {
    start <- glue::glue("{yr}-{start_week}")
  } else {
    start <- glue::glue("{yr-1}-{start_week}")
  }
  return(start)
}

# start_of_year
# @param yrwk a

start_of_year <- function(yrwk) {
  retval <- as.numeric(stringr::str_split(yrwk, "-")[[1]])
  yr <- retval[1]
  wk <- retval[2]

  start <- glue::glue("{yr}-01")

  return(start)
}

season.int <- function(yrwk, start_week = 30) {
  retval <- as.numeric(stringr::str_split(yrwk, "-")[[1]])
  yr <- retval[1]
  wk <- retval[2]

  if (wk >= start_week) {
    start <- glue::glue("{yr}/{yr+1}")
  } else {
    start <- glue::glue("{yr-1}/{yr}")
  }
  return(start)
}


# season
# @param yrwk a
# @param start_week the start week of the season

season <- Vectorize(season.int, vectorize.args = c("yrwk"))

# x from week
# @param week week

x <- function(week) {
  retval <- week
  retval[week >= 30] <- week[week >= 30] - 29
  retval[week < 30] <- week[week < 30] + 23
  retval[week == 53] <- 23.5

  return(retval)
}
