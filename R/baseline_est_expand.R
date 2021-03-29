#' For more details see the help vignette:
#' \code{vignette("nowcast", package="attrib")}
#'
#' @param data_train Data to train on.
#' @param data_predict Data to predict on
#' @param n_sim number of simulations to preform. Default setting is n_sim = 1000
#' @param fixef Fixed effekts
#' @param ranef Random effekts, default is NULL
#' @param offset Offset, can be NULL
#' @examples \donotrun{

#' data_aggregated <- data.table::as.data.table(data_fake_nowcasting_county_aggregated)
#' n_sim <- 10
#' data_train <- data_aggregated[cut_doe< "2019-06-30"]
#' data_predict <- data_aggregated
#' response <- "n_death"
#' fixef <- "1 + sin(2 * pi * (week) / 53) + cos(2 * pi * (week ) / 53) + year"
#' ranef <- "(1|location_code)"
#' offset <- "log(pop)"
#' base<- baseline_est_expand(data_train, data_predict, n_sim, response, fixef, ranef, offset)
#'}
#' @return data
#'
#' @export

baseline_est_expand <- function(data_train, data_predict, n_sim = 1000, response, fixef, ranef = NULL, offset){
  cut_doe <- NULL
  location_code <- NULL
  . <- NULL
  pop <- NULL
  n_death <- NULL



  #for developping
  #
  # data_raw <- gen_fake_death_data_county()
  # pop_data <- fhidata::norway_population_by_age_cats(cats = list(c(1:120)))[location_code %in%
  #                                                                             unique(fhidata::norway_locations_b2020$county_code)]
  #
  # data_aggregated <- nowcast_aggregate(data_raw, lubridate::today(), n_week = 13, pop_data = pop_data)
  # data_aggregated<- data_aggregated[order(cut_doe, location_code)]
  #
  # n_sim <- 1000
  # data_train <- data_aggregated[cut_doe< "2019-06-30"]
  # data_predict <- data_aggregated
  # response <- "n_death"
  # fixef <- "1 + sin(2 * pi * (week) / 53) + cos(2 * pi * (week ) / 53) + year"
  # ranef <- "(1|location_code)"
  # offset <- "log(pop)"

  col_names <- colnames(data_train)

  if(is.null(ranef)){
    col_names <- colnames(data_train)
    formula = paste0(response, "~", fixef, "+", "offset(log(pop))")
    fit <- stats::glm(stats::as.formula(formula), family = "quasipoisson", data = data_train)
    dispersion<- summary(fit)$dispersion


    x<- arm::sim(fit, n_sim)
    sim_models <- as.data.frame(as.matrix(x@coef))
    data_x <- as.data.table(copy(stats::model.frame(formula, data = data_predict)))
    data_x[, n_death:= NULL]

    col_names_sim<-  colnames(sim_models)
    if (!"(Intercept)" %in% colnames(sim_models) ){
      if (!is.null(offset)){
        expected_fix <- cbind(as.matrix( sim_models),1) %*%  rbind(as.matrix(t(data_x)))
      }else{
        expected_fix <- cbind(as.matrix( sim_models)) %*%  rbind(as.matrix(t(data_x)))
      }

    } else{
      if (!is.null(offset)){
        expected_fix <- cbind(as.matrix( sim_models),1) %*%  rbind(1, as.matrix(t(data_x)))
      }else{
        expected_fix <- cbind(as.matrix( sim_models)) %*%  rbind(1, as.matrix(t(data_x)))
      }
    }


    expected <-(stats::rnbinom(length(expected_fix),mu = exp(expected_fix), size = (exp(expected_fix)/(dispersion-1)))) #using a neg bin to draw from a quiasipoison
    dim(expected)<- dim(expected_fix)
    expected <- as.data.table(expected)


    expected_t <- data.table::transpose(expected)
    expected_t$id_row <- 1:nrow(data_predict)
    data_predict$id_row <- 1:nrow(data_predict)

    sim_data <- merge(data_predict, expected_t, by = "id_row", all = TRUE)

    sim_data <- data.table::melt(sim_data, id.vars = c(col_names, "id_row"))


    setnames(sim_data, "variable", "sim_id")
    sim_data$sim_id <- as.numeric(as.factor(sim_data$sim_id))
    setnames(sim_data, "value", "sim_value")

  } else if(!is.null(ranef)){

    fit_neg_bin <- fit_attrib(data_train, response, fixef, ranef, offset, dist_family = "negbin")

    sim_data<- sim(fit_neg_bin, data_predict[, .(n_death, week, year,  cut_doe, location_code, pop)], n_sim = 1000)

    shape<- getME(fit_neg_bin, "glmer.nb.theta")
    sim_data[, sim_value := rnbinom(.N, shape, mu = sim_value)]
    sim_data[, type := "neg_bin"]


  } else{
    return("Something wrong with model input")
  }


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

  summary <- aggregated_sim[, .(mean_death = sum(median.sim_value), n_death = sum(n_death)), keyby = .(year, cut_doe)]
  summary


  summary <- aggregated_sim[, .(mean_death = sum(median.sim_value), n_death = sum(n_death)), keyby = .(year)]
  summary

  # library(ggplot2)
  # q <- ggplot(aggregated_sim[location_code == "county03" & year == 2019],
  #             aes(x = week,
  #                 y = median.sim_value,
  #                 group = as.factor(year),
  #                 colour = as.factor(year)))
  # q <- q + geom_line()
  # q <-q + geom_line(aes(y = n_death))
  # q <- q + geom_ribbon(aes(x = week, ymin=q025.sim_value,
  #                          ymax=q975.sim_value, fill = year),
  #                      alpha=0.5)
  #
  # q
  retval <- vector(mode = "list")
  retval$simulations <- sim_data
  retval$aggregated <- aggregated_sim
  retval$fit <- fit
 return(retval)
}

