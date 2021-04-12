#' For more details see the help vignette:
#' \code{vignette("nowcast", package="attrib")}
#'
#' @param data_train Data to train on.
#' @param data_predict Data to predict on
#' @param n_sim number of simulations to preform. Default setting is n_sim = 1000
#' @param formula Formula to model
#' @param offsett True or False depending on whetehr there is an offset in the formula or not.

#'
#' @return Residualplots for all ncor_i and some evaluationmetrixs for each of them as well as a plot containing credible intervals using the simulation
#'

baseline_est_simple <- function(data_train, data_predict, n_sim = 1000, formula, offsett ){

  cut_doe <- NULL
  location_code <- NULL
  . <- NULL
  pop <- NULL
  n_death <- NULL



  #for developping
#
  # data <- as.data.table(data_fake_nowcasting_aggregated)
  # n_sim <- 1000
  # formula <- paste0("n_death", "~sin(2 * pi * (week) / 53) + cos(2 * pi * (week ) / 53) + year + offset(log(pop))")
  # data_train <- data[cut_doe< "2019-06-30"]
  # data_predict <- data
  # offsett <- TRUE


  col_names <- colnames(data_train)
  fit <- stats::glm(stats::as.formula(formula), family = "quasipoisson", data = data_train)
  dispersion<- summary(fit)$dispersion

  x<- arm::sim(fit, n_sim)
  sim_models <- as.data.frame(as.matrix(x@coef))
  data_x <- as.data.table(copy(stats::model.frame(formula, data = data_predict)))
  data_x[, n_death:= NULL]

  col_names_sim<-  colnames(sim_models)
  #col_names_rel <- col_names[which(col_names != "(Intercept)")]
  if (!"(Intercept)" %in% colnames(sim_models) ){
    if (offsett == TRUE){
      expected_fix <- cbind(as.matrix( sim_models),1) %*%  rbind(as.matrix(t(data_x)))
    }else{
      expected_fix <- cbind(as.matrix( sim_models)) %*%  rbind(as.matrix(t(data_x)))
    }

  } else{
    if (offsett == TRUE){
      expected_fix <- cbind(as.matrix( sim_models),1) %*%  rbind(1, as.matrix(t(data_x)))
    }else{
      expected_fix <- cbind(as.matrix( sim_models)) %*%  rbind(1, as.matrix(t(data_x)))
    }
  }


  expected <-(stats::rnbinom(length(expected_fix),mu = exp(expected_fix), size = (exp(expected_fix)/(dispersion-1)))) #using a neg bin to draw from a quiasipoison
  #expected <-(rpois(length(expected_fix),exp(expected_fix)))
  #expected <-exp(expected_fix)
  dim(expected)<- dim(expected_fix)
  expected <- as.data.table(expected)


  expected_t <- data.table::transpose(expected)
  expected_t$id_row <- 1:nrow(data_predict)
  data_predict$id_row <- 1:nrow(data_predict)

  new_data <- merge(data_predict, expected_t, by = "id_row", all = TRUE)

  new_data <- data.table::melt(new_data, id.vars = c(col_names, "id_row"))


  setnames(new_data, "variable", "sim_id")
  new_data$sim_id <- as.numeric(as.factor(new_data$sim_id))
  setnames(new_data, "value", "sim_value")


  q025 <- function(x){
    return(stats::quantile(x, 0.025))
  }
  q925 <- function(x){
    return(stats::quantile(x, 0.975))
  }

  col_names_new <- colnames(new_data)
  data.table::setkeyv(new_data,
                      col_names_new[!col_names_new %in% c("sim_value",
                                                  "sim_id"
                                                  )])

  aggregated_sim<- new_data[,unlist(recursive = FALSE,
                                    lapply(.(median = stats::median, q025 = q025, q925 = q925),
                                           function(f) lapply(.SD, f))),
                            by = eval(data.table::key(new_data)),.SDcols = c("sim_value")]

#library(ggplot2)
# q <- ggplot(aggregated_sim,
#             aes(x = week,
#                 y = median.sim_value,
#                 group = as.factor(year),
#                 colour = as.factor(year)))
# q <- q + geom_line()
# q <-q + geom_line(aes(y = n_death))
# q <- q + geom_ribbon(aes(x = week, ymin=q05.sim_value,
#                          ymax=q95.sim_value, fill = year),
#                      alpha=0.5)
#
# q
  retval <- vector(mode = "list")
  retval$simulations <- new_data
  retval$aggregated <- aggregated_sim
  retval$fit <- fit
return(retval )

}
