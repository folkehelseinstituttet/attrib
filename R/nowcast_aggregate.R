
#' Cleaned fake data for mortalityregistration
#'
#' @format
#' \describe{
#' \item{cut_doe}{First date of every week}
#' \item{n_death}{Number of true deaths this week}
#' \item{n0_0}{Number of registrations within the current week}
#' \item{n0_1}{Number of registrations within the current and previous week}
#' \item{p0_1}{Percentile of registrations within the current and previous week}
#' \item{n0_2}{Number of registrations within the 2 last weeks and the current week}
#' \item{p0_2}{Percentile of registrations within the current and prvious 2 weeks}
#' \item{n0_3}{Number of registrations within the 3 last weeks and the current week}
#' \item{p0_3}{Percentile of registrations within the current and prvious 3 weeks}
#' \item{n0_4}{Number of registrations within the 4 weeks and the current week}
#' \item{p0_4}{Percentile of registrations within the current and prvious 4 weeks}
#' \item{n0_5}{Number of registrations within the 5 weeks and the current week}
#' \item{p0_5}{Percentile of registrations within the current and prvious 5 weeks}
#' \item{n0_6}{Number of registrations within the 6 weeks and the current week}
#' \item{p0_6}{Percentile of registrations within the current and prvious 6 weeks}
#' \item{n0_7}{Number of registrations within the 7 weeks and the current week}
#' \item{p0_7}{Percentile of registrations within the current and prvious 7 weeks}
#' \item{n0_8}{Number of registrations within the 8 weeks and the current week}
#' \item{p0_8}{Percentile of registrations within the current and prvious 8 weeks}
#' \item{n0_9}{Number of registrations within the 9 weeks and the current week}
#' \item{p0_9}{Percentile of registrations within the current and prvious 9 weeks}
#' \item{n0_10}{Number of registrations within the 10 weeks and the current week}
#' \item{p0_10}{Percentile of registrations within the current and prvious 10 weeks}
#' \item{n0_11}{Number of registrations within the 11 weeks and the current week}
#' \item{p0_11}{Percentile of registrations within the current and prvious 11 weeks}
#' \item{n0_12}{Number of registrations within the 12 weeks and the current week}
#' \item{p0_12}{Percentile of registrations within the current and prvious 12 weeks}
#' \item{n0_13}{Number of registrations within the 13 weeks and the current week}
#' \item{p0_13}{Percentile of registrations within the current and prvious 13 weeks}
#' \item{n0_14}{Number of registrations within the 14 weeks and the current week}
#' \item{p0_14}{Percentile of registrations within the current and prvious 14 weeks}
#' \item{n0_15}{Number of registrations within the 15 weeks and the current week}
#' \item{p0_15}{Percentile of registrations within the current and prvious 15 weeks}
#' }
"data_fake_nowcasting_aggregated"



#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param data Dataset containing doe (Date of event) and dor (Date of registation). The columns must have these exact names. 
#' @param aggregation_date Date of aggregation 
#' @param n_week Number of weeks to calculate the percentage of the total registraations. Must be larger og equal to 2 amd smaller than the total number of weeks in the dataset.
#' 
#' @examples
#' \dontrun{
#'
#' data <- attrib::data_fake_death
#' aggregation_date <- as.Date("2020-01-01")
#' n_week <- 52
#' 
#' clean_data <- nowcast_clean(data, aggregation_date, n_week)
#' }
#' @return Cleaned dataset with the percentiles of registered events within the last 52 weeks
#'
#' @export
nowcast_aggregate <- function(
  data,
  aggregation_date,
  n_week) {
  
  doe <- NULL
  dor <- NULL
  cut_doe <- NULL
  n_death <- NULL
  temp_outcome <- NULL
  n0_0 <- NULL
  p0_0 <- NULL
  temp_variable_n <- NULL
  temp_variable_p <- NULL
  . <- NULL
  new_value <- NULL
  # retur only dataset or graphs as well? ## First only dataset! 
  
  
  
  ##### for developing
  
  # data <- gen_fake_death_data()
  # aggregation_date <- as.Date("2020-01-01")
  # n_week <- 15

  ### check og parameters ----
  
  if (! "doe" %in% colnames(data)){
    stop("The dataset does not have the correct column names")
  }
  
  if (! "dor" %in% colnames(data)){
    stop("The dataset does not have the correct column names")
  }
  
  if (! "n_week" > 1){
    stop("n_week is to small" )
  }
  
  #should perhaps have a check for max length as well. 
  
  ### cleaning ----
  d <- data.table::as.data.table(data)
  d <- d[, .(doe, dor)]
  d <- d[dor <= as.Date(cut(aggregation_date, "week"))] # we erase all date for incompleate weeks. 
  d <- d[doe <= as.Date(cut(aggregation_date, "week"))]
  d[, cut_doe := as.Date(cut(doe, "week"))]
  
  
  # count deaths
  
  d_death <- d[ , .(
    "n_death" = .N
  ), keyby = .(
    cut_doe
  )]
  
  d[ d_death, 
     on = "cut_doe",
     n_death := n_death]
  
  retval <- vector("list", length = n_week)
  d_within_week <- d[, .(cut_doe)]
  
  for ( i in 1:n_week){
    temp_d <- d[, .(cut_doe, n_death)]
    temp <- d[dor < (as.Date(cut_doe) + i*7), .(
      temp_outcome_n = .N,
      temp_outcome_p = sum(dor < (as.Date(cut_doe) + i*7))/n_death,
      n_death = n_death),
      keyby = .(cut_doe)]
    
    temp_d[,paste0("n0_", (i-1)) := 0]
    temp_d[,paste0("p0_", (i-1)) := 0]
    temp_d[temp, on= .(cut_doe),  paste0("n0_", (i-1)) := temp_outcome_n]
    temp_d[temp, on= .(cut_doe),  paste0("p0_", (i-1)) := temp_outcome_p]
    
    
    retval[[i ]] <- as.data.frame(temp_d)
    
    
    
    # setnames(temp, "temp_outcome_p", paste0("p0_", (i-1)))
    # setnames(temp, "temp_outcome_n", paste0("n0_", (i-1)))
    # 
    # retval[[i ]] <- as.data.frame(temp)
    #retval[[i]] <- as.data.frame(subset(temp, select = -c(cut_doe) ))
    
  }
  
  d_within_week <- cbind.data.frame(retval)
  d_within_week <- unique(as.data.table(d_within_week))
  # nrow(d_within_week)
  # nrow(unique(d[, .(cut_doe, n_death)]))
  d_within_week <- as.data.table(subset(d_within_week, select = unique(colnames(d_within_week))))
  
  
  # insert NA where we do not have data
  
  d_corrected <- d_within_week[, .(cut_doe, n_death, n0_0)]
  for ( i in 2:n_week){
    
    week_n <- paste0("n0_",(i-1))
    week_p <- paste0("p0_",(i-1))
    d_within_week[, new_value := NA]
    d_within_week[, temp_variable_n := get(week_n)]
    d_within_week[, temp_variable_p := get(week_p)]
    d_within_week[(nrow(d_within_week)-i+2):nrow(d_within_week), temp_variable_n := new_value]
    d_within_week[(nrow(d_within_week)-i+2):nrow(d_within_week), temp_variable_p := new_value]
    d_corrected[ d_within_week, 
                 on = "cut_doe",
                 paste0("n0_",(i-1)) := temp_variable_n]
    d_corrected[ d_within_week, 
                 on = "cut_doe",
                 paste0("p0_",(i-1)) := temp_variable_p]
  }
  
  


    # data_fake_nowcasting_aggregated <- d_corrected
    # save(data_fake_nowcasting_aggregated, file = "data/data_fake_nowcasting_aggregated.rda", compress = "bzip2")L

  
  retval <- d_corrected
  
  return (retval)
}