#' For more details see the help vignette:
#' \code{vignette("nowcast", package="attrib")}
#'
#' @param data_train Data to train on.
#' @param data_predict Data to predict on
#' @param n_sim number of simulations to preform. Default setting is n_sim = 1000
#' @param formula Formula to model
#' @param offsett True or False depending on whetehr there is an offset in the formula or not.
#' @examples
#' data <- data.table::as.data.table(data_fake_nowcasting_aggregated)
#' n_sim <- 1000
#' formula <- paste0("n_death", "~sin(2 * pi * (week) / 53) + cos(2 * pi * (week ) / 53) + year")
#' data_train <- data[cut_doe< "2019-06-30"]
#' data_predict <- data
#' offsett <-FALSE
#' base_line <- baseline_est(data_train, data_predict, formula = formula, offsett = offsett)
#'
#' @return Residualplots for all ncor_i and some evaluationmetrixs for each of them as well as a plot containing credible intervals using the simulation
#'
#'@export
baseline_est_lme4 <- function(data_train, data_predict, n_sim = 1000, response, fixef, ranef, offsett ){

  cut_doe <- NULL
  location_code <- NULL
  . <- NULL
  pop <- NULL
  n_death <- NULL

  # #for developping
  # data <- gen_fake_death_data_county()
  # pop_data <- fhidata::norway_population_by_age_cats(cats = list(c(1:120)))[location_code %in% unique(fhidata::norway_locations_b2020$county_code)]
  # data_agg <-  nowcast_aggregate(data, aggregation_date = as.Date("2019-12-30"), n_week = 12, pop_data = pop_data)
  # data_agg_rel <- data_agg[, .(n_death, week, year, location_code, pop)]
  # n_sim <- 1000
  # response <- "n_death"
  # fixef <- "sin(2 * pi * (week) / 53) + cos(2 * pi * (week ) / 53) + year"
  # ranef <- "(1|location_code)"
  # offset <- "log(pop)"

  data_train <- data_agg_rel
  data_predict <- data_agg_rel[1:(nrow(data_agg_rel)-12)]

  fit <- fit_attrib(data_train, response = response, fixef = fixef, ranef = ranef, offset = offset)

  sim_data <- sim(fit = fit, data = data_predict, n_sim = 1000, PI = TRUE)


  q025 <- function(x){
    return(stats::quantile(x, 0.025))
  }
  q975 <- function(x){
    return(stats::quantile(x, 0.975))
  }

  col_names_new <- colnames(sim_data)
  data.table::setkeyv(sim_data,
                      col_names_new[!col_names_new %in% c("sim_value",
                                                          "sim_id"
                      )])

  aggregated_sim<- sim_data[,unlist(recursive = FALSE,
                                    lapply(.(median = stats::median, q025 = q025, q975 = q975),
                                           function(f) lapply(.SD, f))),
                            by = eval(data.table::key(sim_data)),.SDcols = c("sim_value")]

  #library(ggplot2)
  # q <- ggplot(aggregated_sim[location_code == "county30" & year == 2019],
  #             aes(x = week,
  #                 y = median.sim_value,
  #                 group = as.factor(year),
  #                 colour = as.factor(year)))
  # q <- q + geom_line()
  # q <-q + geom_line(aes(y = n_death), colour = "blue")
  # q <- q + geom_ribbon(aes(x = week, ymin=q025.sim_value,
  #                          ymax=q975.sim_value, fill = as.factor(year)),
  #                      alpha=0.5)
  #
  # q

  retval <- vector(mode = "list")
  retval$simulations <- sim_data
  retval$aggregated <- aggregated_sim
  retval$fit <- fit
  return(retval )

}
