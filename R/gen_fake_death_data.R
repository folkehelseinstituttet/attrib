#' Fake data for mortalityregistration
#'
#' @format
#' \describe{
#' \item{doe}{Date og event}
#' \item{dor}{Date of registration}
#' }
"data_fake_nowcasting_raw"


# Generates fake data
#
# This function generates a fake dataset with parameters
# @param n_locations Telling how many locations one wants in the output data, default = 11 the number of municipalities in Norway.

# gen_fake_death_data <- function() {
#   doe <- NULL
#   dor <- NULL
#   reg_lag <- NULL
#   . <- NULL
#   
#   start_date <- as.Date("2018-01-01")
#   end_date <- as.Date("2020-01-01")
#   
#   
#   skeleton <- expand.grid(
#     count = seq(1, 150, by = 1),
#     date = seq.Date(
#       from = start_date,
#       to = end_date,
#       by = 1 # to get a weakly base.
#     ),
#     stringsAsFactors = FALSE
#   )
#   setDT(skeleton)
#   
#   # skeleton <- expand.grid(
#   #   date = seq.Date(
#   #     from = start_date,
#   #     to = end_date,
#   #     by = 1 # to get a weakly base.
#   #   ),
#   #   stringsAsFactors = FALSE
#   # )
#   # setDT(skeleton)
#   # 
#   # skeleton[, n_death := rnorm(.N, mean = 150, sd = 20)]
#   
#   
#   skeleton[, doe := date]
#   skeleton[, reg_lag := stats::rpois(.N, 28)]
#   skeleton[, dor := doe + reg_lag]
#   
#    # data_fake_death <- skeleton[,.(doe, dor)]
#    # save(data_fake_death, file = "data/data_fake_death.rda", compress = "bzip2")
# 
#   return(skeleton[,.(doe, dor)])
# }


gen_fake_death_data <- function() {
  doe <- NULL
  dor <- NULL
  reg_lag <- NULL
  . <- NULL
  
  start_date <- as.Date("2018-01-01")
  end_date <- as.Date("2020-01-01")
  
  date = seq.Date(
    from = start_date,
    to = end_date,
    by = 1 
  )
  
  temp_vec <- vector( "list", length = length(date))
  n_death <- rep(0, length(date))
  n_death[1] <- round(stats::rnorm(1, mean = 150, sd = 10))
  for (i in seq_along(date)){
    if(i != length(date)){
      deaths <- round(stats::rnorm(1, mean = n_death[i], sd = 5))
      while(deaths <= 0){
        deaths <- round(stats::rnorm(1, mean = n_death[i], sd = 5))
      }
      n_death[i+ 1] = deaths
    }
    
    skeleton <- expand.grid(
      date = date[i],
      count = seq(1, n_death[i], by = 1),
      stringsAsFactors = FALSE
    )
    setDT(skeleton)
    temp_vec[[i]] <- as.data.table(skeleton)
  }
  
  skeleton <- rbindlist(temp_vec)
  
  skeleton[, doe := date]
  skeleton[, reg_lag := stats::rpois(.N, 21)]
  skeleton[, dor := doe + reg_lag]
  
  # data_fake_nowcasting_raw <- skeleton[,.(doe, dor)]
  # save(data_fake_nowcasting_raw, file = "data/data_fake_nowcasting_raw.rda", compress = "bzip2")
  
  return(skeleton[,.(doe, dor)])
}
