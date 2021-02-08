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

  col_names<-  colnames(sim_models)
  col_names_rel <- col_names[which(col_names != "(Intercept)")]
  if (!"(Intercept)" %in% colnames(sim_models) ){
    print("ehi")
    dim(cbind(sim_models))
    dim(rbind( as.matrix(t(data_x))))

    colnames(cbind(cbind(sim_models)))
    rownames(rbind(1, as.matrix(t(data_x))))

    expected <- as.matrix(sim_models) %*%  rbind(as.matrix(t(data_x)))
    expected_sim <-data.table(
      sim_id = 1:500,
      sim_value = exp(as.numeric(expected[1:500])),
      cut_doe = cut_doe_cur
    )
    #print(cut_doe_cur)
    expected_sim[, sim_value:= round(as.numeric(sim_value), 2)]
    sim_val_vec[[i +1]]<- expected_sim
  } else{
    dim(cbind(as.matrix( sim_models),1))
    dim( rbind(1, as.matrix(t(data_x))))

    colnames(cbind(cbind(sim_models)))
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



    expected_sim <-data.table(
      sim_id = 1:500,
      sim_value = exp(as.numeric(expected[1:500])),
      cut_doe = cut_doe_cur
    )
    #print(cut_doe_cur)
    expected_sim[, sim_value:= round(as.numeric(sim_value), 2)]
    sim_val_vec[[i +1]]<- expected_sim
  }


}
