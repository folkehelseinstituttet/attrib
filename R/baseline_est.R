#' Baseline estimator.
#'
#'Baseline estimator for mortality. If a random effect is present the negative binomial distribution is used.
#'Otherwise a quisipoisson is fitted to the data.
#'We use simmulations to generate n_sim responses for each row in the dataset to create a baseline.
#'
#' For more details see the help vignette:
#' \code{vignette("nowcast", package="attrib")}
#'
#' @param data_train Data to train on.
#' @param data_predict Data to predict on
#' @param n_sim number of simulations to preform. Default setting is n_sim = 1000
#' @param response Response
#' @param fixef Fixed effekts
#' @param ranef Random effekts, default is NULL
#' @param offset Offset, can be NULL
#' @examples \dontrun{
#' data <- as.data.table(data_fake_nowcasting_county_aggregated)
#' n_sim <- 100
#' fixef <- "sin(2 * pi * (week) / 53) + cos(2 * pi * (week ) / 53) + year"
#' ranef <- "(1|location_code)"
#' response <- "n_death"
#' data_train <- data[cut_doe< "2019-06-30"]
#' data_predict <- data
#' offset <- "log(pop)"
#' baseline_est(data_train, data_predict, n_sim = 1000, fixef, ranef, response, offset)
#' }
#' @examples
#' data <- data.table::as.data.table(data_fake_nowcasting_county_aggregated)
#' data <- data[location_code == "county03"]
#' n_sim <- 100
#' fixef <- "sin(2 * pi * (week) / 53) + cos(2 * pi * (week ) / 53) + year"
#' ranef <- NULL
#' response <- "n_death"
#' data_train <- data[cut_doe< "2019-06-30"]
#' data_predict <- data
#' offset <- "log(pop)"
#' baseline_est(data_train, data_predict, n_sim = 1000, fixef, ranef, response, offset)
#' @return Residualplots for all ncor_i and some evaluationmetrixs for each of them as well as a plot containing credible intervals using the simulation
#' @export
#'
baseline_est <- function(data_train, data_predict, n_sim = 1000, fixef, ranef, response, offset){

  cut_doe <- NULL
  location_code <- NULL
  . <- NULL
  pop <- NULL
  n_death <- NULL
  sim_value <- NULL
  type <- NULL

  #for developping
  #
  # data <- as.data.table(data_fake_nowcasting_aggregated)
  # n_sim <- 1000
  # formula <- paste0("n_death", "~sin(2 * pi * (week) / 53) + cos(2 * pi * (week ) / 53) + year + offset(log(pop))")
  # data_train <- data[cut_doe< "2019-06-30"]
  # data_predict <- data
  # offsett <- "log(pop)"


  # Check if ranef is null to decide to use negative binomial or quasipoisson.
  if(is.null(ranef)){
    # Quasipoisson

    formula <- paste0(response," ~ ", fixef, " + ", offset )
    col_names <- colnames(data_train)
    fit <- stats::glm(stats::as.formula(formula), family = "quasipoisson", data = data_train)
    dispersion<- summary(fit)$dispersion

    ## Simmulations
    x<- arm::sim(fit, n_sim)
    sim_models <- as.data.frame(as.matrix(x@coef))
    data_x <- as.data.table(copy(stats::model.frame(formula, data = data_predict)))
    data_x[, n_death:= NULL]

    col_names_sim<-  colnames(sim_models)
    #col_names_rel <- col_names[which(col_names != "(Intercept)")]
    if (!"(Intercept)" %in% colnames(sim_models) ){
      if (offset == TRUE){
        expected_fix <- cbind(as.matrix( sim_models),1) %*%  rbind(as.matrix(t(data_x)))
      }else{
        expected_fix <- cbind(as.matrix( sim_models)) %*%  rbind(as.matrix(t(data_x)))
      }

    } else{
      if (offset == TRUE){
        expected_fix <- cbind(as.matrix( sim_models),1) %*%  rbind(1, as.matrix(t(data_x)))
      }else{
        expected_fix <- cbind(as.matrix( sim_models)) %*%  rbind(1, as.matrix(t(data_x)))
      }
    }

    ## Draw random sample
    if(dispersion > 1){
      #using a neg bin to draw from a quasipoison, not possible if we have under dispersion
        expected <-(stats::rnbinom(length(expected_fix),mu = exp(expected_fix), size = (exp(expected_fix)/(dispersion-1))))
    } else{
        expected <- stats::rpois(length(expected_fix),lambda  = exp(expected_fix))
    }

    #expected <-(rpois(length(expected_fix),exp(expected_fix)))
    #expected <-exp(expected_fix)
    dim(expected)<- dim(expected_fix)
    expected <- as.data.table(expected)


    expected_t <- data.table::transpose(expected)
    expected_t$id_row <- 1:nrow(data_predict)
    data_predict$id_row <- 1:nrow(data_predict)

    # SImulated data
    new_data <- merge(data_predict, expected_t, by = "id_row", all = TRUE)
    new_data <- data.table::melt(new_data, id.vars = c(col_names, "id_row"))


    setnames(new_data, "variable", "sim_id")
    new_data$sim_id <- as.numeric(as.factor(new_data$sim_id))
    new_data[, type := "quasi_poisson"]
    setnames(new_data, "value", "sim_value")
  } else if(!is.null(ranef)){
    #Negative binomial

    fit <- fit_attrib(data_train, response, fixef, ranef, offset, dist_family = "negbin")

    sim_data<- sim(fit, data_predict[, .(n_death, week, year,  cut_doe, location_code, pop)], n_sim = 1000)

    shape<- lme4::getME(fit, "glmer.nb.theta")
    sim_data[, sim_value := stats::rnbinom(.N, shape, mu = sim_value)]
    sim_data[, type := "neg_bin"]
    new_data <- sim_data

  } else{
    return("Something wrong with model input")
  }

  ## Quantile functions

  q025 <- function(x){
    return(stats::quantile(x, 0.025))
  }
  q925 <- function(x){
    return(stats::quantile(x, 0.975))
  }

  # Aggregate data
  col_names_new <- colnames(new_data)
  data.table::setkeyv(new_data,
                      col_names_new[!col_names_new %in% c("sim_value",
                                                          "sim_id"
                      )])

  aggregated_sim<- new_data[,unlist(recursive = FALSE,
                                    lapply(.(median = stats::median, q025 = q025, q975 = q925),
                                           function(f) lapply(.SD, f))),
                            by = eval(data.table::key(new_data)),.SDcols = c("sim_value")]

  retval <- vector(mode = "list")
  retval$simulations <- new_data
  retval$aggregated <- aggregated_sim
  retval$fit <- fit
  return(retval )

}
