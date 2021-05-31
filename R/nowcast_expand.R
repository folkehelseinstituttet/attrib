

### nowcast_correction_fn_quasipoisson ----

#' Nowcast correction function using the quasipoisson distribution.
#'
#' For more details see the help vignette:
#' \code{vignette("nowcast", package="attrib")}
#'
#' @param data Data generated with nowcast_aggregate containing the part of the dataset that the model should train on.
#' @param n_week_adjusting Number of weeks to correct
#' @param offset Boolian value which is set to true if offset(log(pop)) is a part of the formula
#' @param date_0 Date of aggregation.
#' @return nowcast_correction_object including corrected data for all weeks in n_wwk_adjust and the model fits for all weeks
#' @examples
#' data<- data.table::as.data.table(data_fake_nowcasting_county_aggregated)
#' data <- data[location_code == "county03"]
#' n_week_adjusting <- 5
#' n_week_train <- 52
#' n_week_start <- n_week_adjusting + n_week_train
#' date_0 <- data[nrow(data),]$cut_doe #last date in the dataset, assume the dataset is ordered.
#' data <- data[cut_doe >= (date_0 - n_week_start*7 + 1), ]
#' nowcast_correction_object <- nowcast_correction_fn_quasipoisson(data,
#'      n_week_adjusting, offset = "log(pop)", date_0 )
#' @export
nowcast_correction_fn_quasipoisson <- function(data, n_week_adjusting, offset, date_0){

  temp_variable_n <- NULL
  cut_doe <- NULL
  . <- NULL
  location_code <- NULL

  # for developping
  # data<- data.table::as.data.table(data_fake_nowcasting_county_aggregated)
  # data <- data[location_code == "county03"]
  # n_week_adjusting <- 6
  # offset = "log(pop)"

  # Lagg the data
  for ( i in 0:(n_week_adjusting-1)){

    week_n <- paste0("n0_",(i))
    data[, temp_variable_n := get(week_n)]
    keycol <-  c("year", "week", "location_code")
    setorderv(data, keycol)
    data[, paste0("n0_",(i), "_lag1") := shift(temp_variable_n, 1, fill = 0), by = .(location_code)] #by = (location_code, data)
  }
  data <- subset(data, select= -c(temp_variable_n))


  ########## fit ----
  # For each week in n_week_adjusting model a correction
  data_train <- data[cut_doe <= (date_0 - n_week_adjusting*7) ]
  data_predict <- data[cut_doe > (date_0 - n_week_adjusting*7) ]

  fit_vec <- vector(mode = "list", length = (n_week_adjusting))
  for ( i in 0:(n_week_adjusting-1)){
    # print(i)
    formula <- paste0("n_death", "~sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52)+ year +",
                      glue::glue("n0_{i}_lag1"), "+",  glue::glue("n0_{i}"))
    if(i>=1){
      for (j in 0:(i-1)){
        formula <-  paste0(formula, "+",  glue::glue("n0_{j}"))
      }
    }

    if(!is.null(offset)){
      formula <- paste0(formula,  "+ offset(log(pop))")
    }


    fit <- stats::glm(stats::as.formula(formula), family = "quasipoisson", data = data_train)


    n_cor <- round(stats::predict(fit, newdata = data, type = "response"))
    data[, glue::glue("ncor0_{i}"):= n_cor]

    fit_vec[[i+1]]$fit<- fit
    fit_vec[[i+1]]$formula<- formula

  }

  retval <- vector("list")
  retval$data<- data
  retval$n_week_adjusting <- n_week_adjusting
  retval$fit <- fit_vec

  return(retval)
}



### nowcast_correction_fn_negbin ----

#' Nowcast correction function using the negative binomial distribution.
#'
#' For more details see the help vignette:
#' \code{vignette("nowcast", package="attrib")}
#'
#' @param data Data generated with nowcast_aggregate containing the part of the dataset that the model should train on.
#' @param n_week_adjusting Number of weeks to correct
#' @param offset Boolian value which is set to true if offset(log(pop)) is a part of the formula
#' @param date_0 Date of aggregation.
#' @return nowcast_correction_object including corrected data for all weeks in n_wwk_adjust and the model fits for all weeks
#' @examples \dontrun{
#' data<- data.table::as.data.table(data_fake_nowcasting_county_aggregated)
#' n_week_adjusting <- 5
#' n_week_start <- 52
#' date_0 <- data[nrow(data),]$cut_doe + 1 #first date not in the dataset, assume the dataset is ordered.
#' data <- data[cut_doe >= (date_0 - n_week_start*7 + 1), ]
#' nowcast_correction_object <- nowcast_correction_fn_negbin_mm(data,
#'                            n_week_adjusting, offset = "log(pop)" , date_0)
#' }
#' @export
nowcast_correction_fn_negbin_mm <- function(data, n_week_adjusting, offset, date_0){

  temp_variable_n <- NULL
  cut_doe <- NULL
  . <- NULL
  location_code <- NULL


  ##for developping

  # data<- as.data.table(data_fake_nowcasting_county_aggregated)
  # n_week_adjusting <- 4
  # offset = "log(pop)"
  # date_0 <- data[order(cut_doe)][nrow(data)]$cut_doe +1

  date_0 <- as.Date(cut(date_0, "week"))
  data <- data[cut_doe <= date_0,]

  # Lag the data
  for ( i in 0:(n_week_adjusting-1)){

    week_n <- paste0("n0_",(i))
    data[, temp_variable_n := get(week_n)]
    keycol <-  c("year", "week", "location_code")
    setorderv(data, keycol)
    data[, paste0("n0_",(i), "_lag1") := shift(temp_variable_n, 1, fill = 0), by = .(location_code)] #by = (location_code, data)
  }
  data <- subset(data, select= -c(temp_variable_n))

  data_train <- data[cut_doe <= (date_0 - n_week_adjusting*7) ]
  data_predict <- data[cut_doe > (date_0 - n_week_adjusting*7) ]

  ########## fit ----

  # FIt a model for every correction
  fit_vec <- vector(mode = "list", length = (n_week_adjusting))
  for ( i in 0:(n_week_adjusting-1)){
    # print(i)
    fixef <- paste0("sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52)+ year +",
                        glue::glue("n0_{i}"))

    ranef <- "(1| location_code)"

    response <- "n_death"
    fit <- fit_attrib(data_train, response= response, fixef=fixef, ranef=ranef, offset= offset, dist_family = "negbin")

    n_cor <- round(stats::predict(fit, newdata = data, type = "response"))
    data[, glue::glue("ncor0_{i}"):= n_cor]

    fit_vec[[i+1]]$fit<- fit
    fit_vec[[i+1]]$fixef<- fixef
    fit_vec[[i+1]]$ranef <- ranef
    fit_vec[[i+1]]$response <- response
    fit_vec[[i+1]]$offset <- offset


  }

  retval <- vector("list")
  retval$data<- data
  retval$n_week_adjusting <- n_week_adjusting
  retval$fit <- fit_vec

  return(retval)
}



### nowcast_correction_sim ----

#' Nowcast simmulation function when nowcast_correction_fn_quasipoisson is used for correction.
#'
#' For more details see the help vignette:
#' \code{vignette("nowcast", package="attrib")}
#'
#' @param nowcast_correction_object object returned from function nowcast_correction_fn_expanded
#' @param n_sim Number of simulations
#' @param offset Boolian value which is set to true if offset(log(pop)) is a part of the formula
#' @param date_0 Date of aggregation.
#' @return simulations of the estimate made by the fitted models in nowcast_correction_fn
#' @examples
#' data <- data.table::as.data.table(data_fake_nowcasting_county_aggregated)[location_code == "county03"]
#' n_week_adjusting <- 5
#' n_week_train <- 52
#' n_week_start <- n_week_adjusting + n_week_train
#' date_0 <- data[nrow(data),]$cut_doe #last date in the dataset, assume the dataset is ordered.
#' data <- data[cut_doe >= (date_0 - n_week_start*7 + 1), ]
#' nowcast_correction_object <- nowcast_correction_fn_quasipoisson(data,
#'    n_week_adjusting, offset = TRUE, date_0 )
#' nowcast_sim <- nowcast_correction_sim_quasipoisson(nowcast_correction_object,
#'      offset = "log(pop)", date_0= date_0)
#' @export
nowcast_correction_sim_quasipoisson <- function(nowcast_correction_object, offset, n_sim = 500, date_0){
  # only corrects n_week_Adjusting weeks! not +1
  n_death <- NULL
  sim_value <- NULL
  cut_doe <- NULL


  # for developping
  # data<- as.data.table(data_fake_nowcasting_county_aggregated)[location_code == "county03"]
  # n_week_adjusting <- 4
  # n_sim <- 100
  # nowcast_correction_object<- nowcast_correction_fn_quasipoisson(data, n_week_adjusting, offset = "log(pop)")
  # offset <- "log(pop)"

  fit_vec <- nowcast_correction_object$fit
  data <- nowcast_correction_object$data
  n_week_adjusting <- nowcast_correction_object$n_week_adjusting[[1]]

  ##simulations ----

  data_train <- data[cut_doe < (date_0 - n_week_adjusting*7) ]
  data_predict <- data[cut_doe >= (date_0 - n_week_adjusting*7) ]

  cut_doe_vec <- data_predict$cut_doe
  cut_doe_vec_unique <- unique(cut_doe_vec)

  sim_val_vec <- vector("list", length = (n_week_adjusting))
  for ( i in 0:(n_week_adjusting-1)){


    fit <-fit_vec[[i+1]]$fit
    formula <- fit_vec[[i+1]]$formula
    cut_doe_cur <- cut_doe_vec_unique[n_week_adjusting-i]
    n_rows <- length(which(cut_doe_cur == cut_doe_vec))
    x<- arm::sim(fit, n_sim)
    sim_models <- as.data.frame(x@coef)
    data_x <- as.data.table(copy(stats::model.frame(formula, data = data)))
    data_x <- data_x[(nrow(data_x)-n_rows+1):nrow(data_x),] # Coiuld potentially create trouble but should works because al later rows contain NA and are hence remowed.
    data_x[, n_death:= NULL]


    col_names<-  colnames(sim_models)
    col_names_rel <- col_names[which(col_names != "(Intercept)")]
    if (!"(Intercept)" %in% colnames(sim_models) ){
      if(!is.null(offset)){
        colnames(cbind(sim_models, 1))
        rownames(rbind( as.matrix(t(data_x))))

        expected <- as.matrix(cbind(sim_models, 1)) %*%  rbind(as.matrix(t(data_x)))

      }else{
        colnames(cbind(sim_models))
        rownames(rbind( as.matrix(t(data_x))))

        expected <- as.matrix(cbind(sim_models)) %*%  rbind(as.matrix(t(data_x)))
      }
    } else{
      if(!is.null(offset)){
        colnames(cbind(sim_models, 1))
        rownames(rbind(1, as.matrix(t(data_x))))

        expected <- as.matrix(cbind(sim_models, 1)) %*%  rbind(1,as.matrix(t(data_x)))
      }else{
        colnames(cbind(sim_models))
        rownames(rbind(1, as.matrix(t(data_x))))

        expected <- as.matrix(cbind(sim_models)) %*%  rbind(1,as.matrix(t(data_x)))
      }
    }

    dispersion<- summary(fit)$dispersion
    if(dispersion > 1){
      expected_sim <-data.table(
        sim_id = 1:n_sim,
        sim_value = (stats::rnbinom(length(expected),mu = exp(expected), size = (exp(expected)/(dispersion-1)))),
        cut_doe = cut_doe_cur
      )
    } else{
      expected_sim <-data.table(
        sim_id = 1:n_sim,
        sim_value = stats::rpois(length(expected),lambda  = exp(expected)),
        cut_doe = cut_doe_cur
      )
    }

    sim_val_vec[[i +1]]<- expected_sim


  }

  sim_data <- rbindlist(sim_val_vec)
  retval<- merge(data, sim_data, by = "cut_doe", all = TRUE)
  return(retval)
}

### nowcast_correction_sim ----


#' Nowcast simmulation function when nowcast_correction_fn_negbin is used for correction.
#'
#' For more details see the help vignette.:
#' \code{vignette("nowcast", package="attrib")}
#'
#' @param nowcast_correction_object object returned from function nowcast_correction_fn_expanded
#' @param n_sim Number of simulations
#' @param offset Boolian value which is set to true if offset(log(pop)) is a part of the formula
#' @param date_0 Date of aggregation.
#' @return simulations of the estimate made by the fitted models in nowcast_correction_fn
#' @examples
#' \dontrun{
#' data <- data.table::as.data.table(data_fake_nowcasting_county_aggregated)
#' n_week_adjusting <- 6
#' date_0 <- data[nrow(data),]$cut_doe #last date in the dataset, assume the dataset is ordered.
#' data <- data[cut_doe >= (date_0 - n_week_start*7 + 1), ]
#' nowcast_correction_object <- nowcast_correction_fn_negbin_mm(data, n_week_adjusting,
#'    offset = "log(pop)" , date_0 = date_0)
#' nowcast_sim <- nowcast_correction_sim_neg_bin(nowcast_correction_object,
#'     offset = "log(pop)", date_0 = date_0)
#' }
#' @export
nowcast_correction_sim_neg_bin <- function(nowcast_correction_object, offset, n_sim = 500, date_0){

  n_death <- NULL
  sim_value <- NULL
  . <- NULL
  sim_id <- NULL
  location_code <- NULL
  cut_doe <- NULL


  # for developping
  # data<- as.data.table(data_fake_nowcasting_county_aggregated)
  # n_week_adjusting <- 4
  # n_sim <- 100
  # date_0 <- data[order(cut_doe)][nrow(data)]$cut_doe
  # nowcast_correction_object<- nowcast_correction_fn_negbin_mm(data, n_week_adjusting, offset = "log(pop)", date_0)
  # offset <- "log(pop)"

  fit_vec <- nowcast_correction_object$fit
  data <- nowcast_correction_object$data
  n_week_adjusting <- nowcast_correction_object$n_week_adjusting[[1]]

  ##simulations ----

  data_train <- data[cut_doe <= (date_0 - n_week_adjusting*7) ]
  data_predict <- data[cut_doe >= (date_0 - n_week_adjusting*7) ]

  cut_doe_vec <- data_predict$cut_doe
  cut_doe_vec_unique <- unique(cut_doe_vec)

  sim_val_vec <- vector("list", length = (n_week_adjusting))
  for ( i in 0:(n_week_adjusting-1)){


    fit <-fit_vec[[i+1]]$fit
    formula <- fit_vec[[i+1]]$formula
    cut_doe_cur <- cut_doe_vec_unique[n_week_adjusting-i]
    n_row <- length(which(cut_doe_cur == cut_doe_vec))

    sim_data<- sim(fit, data_predict[cut_doe== cut_doe_cur], n_sim = n_sim)

    shape<- lme4::getME(fit, "glmer.nb.theta")
    sim_data[, sim_value := stats::rnbinom(.N, shape, mu = sim_value)]


    expected_sim <-sim_data[,.(cut_doe, sim_id, sim_value, location_code)]
    sim_val_vec[[i +1]]<- expected_sim


  }


  sim_data <- rbindlist(sim_val_vec)
  retval<- merge(data, sim_data, by = c("cut_doe", "location_code"), all = TRUE)
  return(retval)
}





#' Nowcast, corrigation of data
#'
#' When there is a lag in the reception of data nowcast can be used to correct for this lag,
#'  and predict the true underlying values.
#'
#' For more details see the help vignette:
#' \code{vignette("nowcast", package="attrib")}
#'
#' @param data_aggregated Aggregated dataset from the function npowcast_aggregate
#' @param offset offset
#' @param n_week_adjusting Total number of weeks to correct.
#' @param n_week_training Number of weeks to train on
#' @param date_0 Date of aggregation.
#' @param nowcast_correction_fn Correction function. The deafault is nowcast_correction_fn_expanded. Must return the same as this function.
#' @param nowcast_correction_sim_fn Simmulatoin function. Must return a datatable with the following collumns  "n_death", "sim_value", "cut_doe", "ncor" and simmulations for equally many weeks as n_week_adjust.
#' @examples
#'
#'data_aggregated <- data.table::as.data.table(data_fake_nowcasting_county_aggregated)
#'n_week_training <- 50
#'n_week_adjusting <- 4
#'date_0 <- data_aggregated[order(cut_doe)][nrow(data_aggregated)]$cut_doe + 1
#'offset = "log(pop)"
#'nowcast_object <- nowcast(data_aggregated,offset,n_week_adjusting,n_week_training,date_0)
#'
#' @examples
#'
#'data_aggregated <- data.table::as.data.table(data_fake_nowcasting_county_aggregated)
#'data_aggregated <- data_aggregated[location_code == "county03"]
#'n_week_training <- 50
#'n_week_adjusting <- 4
#'date_0 <- data_aggregated[order(cut_doe)][nrow(data_aggregated)]$cut_doe + 1
#'offset = "log(pop)"
#'nowcast_object <- nowcast(data_aggregated,offset,n_week_adjusting,n_week_training,date_0,
#' nowcast_correction_fn = nowcast_correction_fn_quasipoisson,
#' nowcast_correction_sim_fn = nowcast_correction_sim_quasipoisson)
#' @return Dataset including the corrected values for n_death
#'
#' @export
nowcast <- function(
  data_aggregated,
  offset,
  n_week_adjusting,
  n_week_training,
  date_0,
  nowcast_correction_fn = nowcast_correction_fn_negbin_mm,
  nowcast_correction_sim_fn = nowcast_correction_sim_neg_bin) {

  data_fake_death_clean <- NULL
  ncor <- NULL
  n_death <- NULL
  temp_variable <- NULL
  yrwk <- NULL
  cut_doe <- NULL
  . <- NULL


  ##### for developing
  # data_aggregated <- as.data.table(data_fake_nowcasting_county_aggregated)
  # data_aggregated <- data_aggregated#[location_code == "county03"]
  # n_week_training <- 50
  # n_week_adjusting <- 4
  # date_0 <- data_aggregated[order(cut_doe)][nrow(data_aggregated)]$cut_doe + 1
  # nowcast_correction_fn<- nowcast_correction_fn_negbin_mm
  # nowcast_correction_sim_fn = nowcast_correction_sim_neg_bin
  # # data_aggregated <- data_aggregated[location_code == "county03"]
  # # nowcast_correction_fn<- nowcast_correction_fn_quasipoisson
  # # nowcast_correction_sim_fn = nowcast_correction_sim_quasipoisson
  # offset = "log(pop)"

  data <- as.data.table(data_aggregated)
  n_week_start <- n_week_training + n_week_adjusting
  date_0 <- as.Date(cut(date_0, "week"))

  # Cleaning
  data <- data[cut_doe >= (date_0 - n_week_start*7) ]
  data <- data[cut_doe < date_0]

  #### corrected n_deaths ----
  if (!is.null(offset)){
    nowcast_correction_object <- nowcast_correction_fn(data, n_week_adjusting, offset = offset, date_0)
  } else{
    nowcast_correction_object <- nowcast_correction_fn(data, n_week_adjusting, offset = NULL, date_0)
  }

  data <- nowcast_correction_object$data
  data_sim <- nowcast_correction_sim_fn(nowcast_correction_object, offset = offset, date_0 = date_0)

  #check that all the required variables are there
  # (i.e. that the correction function actually gives reasonable stuff back)

  for ( i in 0:(n_week_adjusting-1)){
    temp <- paste0("ncor0_",i)
    if(! temp %in% colnames(data)){
      stop(glue::glue("nowcast_correction_fn is not returning {temp}"))
    }
  }


  data[, ncor := n_death]

  date_0 <- data[order(cut_doe)][nrow(data)]$cut_doe
  for ( i in 0:(n_week_adjusting-1)){
    date_i <- date_0 - 7*i
    temp <- paste0("ncor0_",i)
    data[, temp_variable := get(temp)]
    data[cut_doe == date_i, ncor:= temp_variable]


  }

  data[,temp_variable:=NULL]


  data[, yrwk:= isoyearweek(cut_doe)]
  data_sim[, yrwk:= isoyearweek(cut_doe)]


  col_order <- c(c("yrwk", "location_code", "n_death", "ncor"),
                 colnames(data)[which(!colnames(data) %in% c("yrwk", "n_death", "ncor", "location_code"))])
  setcolorder(data, col_order)

  date_n_Week_adjusting_start <- date_0 - (n_week_adjusting)*7
  data_sim_clean <- data_sim[cut_doe > date_n_Week_adjusting_start]

  col_order_sim <- c(c("yrwk", "location_code", "n_death", "sim_value"),
                     colnames(data_sim_clean)[which(!colnames(data_sim_clean) %in% c("yrwk", "n_death", "sim_value", "location_code"))])
  setcolorder(data_sim_clean, col_order_sim)

  data_sim_clean <- subset(data_sim_clean, select = c("yrwk", "n_death", "sim_value", "cut_doe", "week", "year", "location_code", "sim_id"))



  q025 <- function(x){
    return(stats::quantile(x, 0.025))
  }
  q975 <- function(x){
    return(stats::quantile(x, 0.975))
  }


  col_names <- colnames(data_sim_clean)
  data.table::setkeyv(data_sim_clean,
                      col_names[!col_names %in% c("sim_value", "sim_id")])

  aggregated_data_sim<- data_sim_clean[, unlist(recursive = FALSE, lapply(.(median = stats::median, q025 = q025, q975 = q975),
                                                                          function(f) lapply(.SD, f))),
                                       by = eval(data.table::key(data_sim_clean)),
                                       .SDcols = c("sim_value")]


  retval <- vector("list")
  retval$data <- data
  retval$data_sim <- data_sim_clean
  retval$data_sim_aggregated <- aggregated_data_sim
  return (retval)
}
