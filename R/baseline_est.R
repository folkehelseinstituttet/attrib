#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param nowcast_object Object from the function nowcast
#' @param n_week_adjusting Number of weeks to adjust
#' @examples
#'
#'  data_aggregated <- data.table::as.data.table(attrib::data_fake_nowcasting_aggregated)
#'  n_week_training <- 50
#'  n_week_adjusting <- 8
#'  nowcast_object <- nowcast(data_aggregated= data_aggregated,
#'    n_week_training = 50, n_week_adjusting = 8)
#'  nowcast_eval_object <- nowcast_eval(nowcast_object, n_week_adjusting)
#' @return Residualplots for all ncor_i and some evaluationmetrixs for each of them as well as a plot containing credible intervals using the simulation
#'
#'

baseline_est <- function(data_train, data_predict, n_sim = 1000){

  #for developping

  data <- data_fake_nowcasting_aggregated
  data[, year := isoyear_n(cut_doe)]
  data[, week := isoweek(cut_doe)]
  n_sim <- 1000

  pop_data<- as.data.table(fhidata::norway_population_b2020)[ location_code == "norge"]

  pop_tot <-pop_data[, .(
    "pop" = sum(pop)
  ), keyby = .(
    year
  )]

  data[pop_tot, pop := pop, on = "year"]
  col_names <- colnames(data)

  formula <- paste0("n_death", "~sin(2 * pi * (week) / 53) + cos(2 * pi * (week ) / 53) + year + offset(log(pop))")
  fit <- stats::glm(stats::as.formula(formula), family = "quasipoisson", data = data)


  x<- arm::sim(fit, n_sim)
  sim_models <- as.data.frame(x@coef)
  data_x <- as.data.table(copy(stats::model.frame(formula, data = data)))
  data_x[, n_death:= NULL]

  col_names_sim<-  colnames(sim_models)
  col_names_rel <- col_names[which(col_names != "(Intercept)")]
  if (!"(Intercept)" %in% colnames(sim_models) ){
    dim(cbind(as.matrix( sim_models),1))
    dim( rbind(as.matrix(t(data_x))))

    colnames(cbind(cbind(sim_models,1)))
    rownames(rbind(as.matrix(t(data_x))))

    expected_fix <- cbind(as.matrix( sim_models),1) %*%  rbind(as.matrix(t(data_x)))

    expected <- as.data.table(exp(expected_fix))

    expected_t <- data.table::transpose(expected)
    expected_t$id_row <- 1:nrow(data)
    data$id_row <- 1:nrow(data)

    new_data <- merge(data, expected_t, by = "id_row", all = TRUE)

    new_data <- data.table::melt(new_data, id.vars = c(col_names, "id_row"))


    setnames(new_data, "variable", "sim_id")
    new_data$sim_id <- as.numeric(as.factor(new_data$sim_id))
    setnames(new_data, "value", "sim_value")

  } else{
    dim(cbind(as.matrix( sim_models),1))
    dim( rbind(1, as.matrix(t(data_x))))

    colnames(cbind(cbind(sim_models,1)))
    rownames(rbind(1, as.matrix(t(data_x))))

    expected_fix <- cbind(as.matrix( sim_models),1) %*%  rbind(1, as.matrix(t(data_x)))

    expected <- as.data.table(exp(expected_fix))

    expected_t <- data.table::transpose(expected)
    expected_t$id_row <- 1:nrow(data)
    data$id_row <- 1:nrow(data)

    new_data <- merge(data, expected_t, by = "id_row", all = TRUE)

    new_data <- data.table::melt(new_data, id.vars = c(col_names, "id_row"))


    setnames(new_data, "variable", "sim_id")
    new_data$sim_id <- as.numeric(as.factor(new_data$sim_id))
    setnames(new_data, "value", "sim_value")

  }



  q05 <- function(x){
    return(quantile(x, 0.05))
  }
  q95 <- function(x){
    return(quantile(x, 0.95))
  }

  col_names_new <- colnames(new_data)
  data.table::setkeyv(new_data,
                      col_names_new[!col_names_new %in% c("sim_value",
                                                  "sim_id"
                                                  )])

  aggregated_sim<- new_data[,unlist(recursive = FALSE,
                                    lapply(.(median = median, q05 = q05, q95 = q95),
                                           function(f) lapply(.SD, f))),
                            by = eval(data.table::key(new_data)),.SDcols = c("sim_value")]


  # q <- ggplot(aggregated_sim,
  #             aes(x = week,
  #                 y = median.sim_value,
  #                 group = as.factor(year),
  #                 colour = as.factor(year)))
  # q <- q + geom_line()
  # geom_ribbon(aes(x=x,ymin=y3,ymax=y4,fill='blue'),
  #             alpha=0.5)
  # q <- q + geom_ribbon(aes(x = week, ymin=q05.sim_value,
  #                          ymax=q95.sim_value, fill = year),
  #                      alpha=0.5)
  #
  # q
  retval <- vector(mode = "list")
  retval$simulations <- new_data
  retval$aggregated <- aggregated_sim
return(retval )

}
