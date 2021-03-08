

### nowcast_correction_fn_simple ----

#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param data Data generated with nowcast_aggregate containing the part of the dataset that the model should train on.
#' @param n_week_adjusting Number of weeks to correct
#' @return nowcast_correction_object including corrected data for all weeks in n_wwk_adjust and the model fits for all weeks
#' @examples
#' data <- attrib::data_fake_nowcasting_aggregated
#' n_week_adjusting <- 8
#' n_week_train <- 52
#' n_week_start <- n_week_adjusting + n_week_train
#' date_0 <- data[nrow(data),]$cut_doe #last date in the dataset, assume the dataset is ordered.
#' data <- data[cut_doe >= (date_0 - n_week_start*7 + 1), ]
#' nowcast_correction_object <- nowcast_correction_fn_simple(data, n_week_adjusting )
#' @export
nowcast_correction_fn_simple <- function(data, n_week_adjusting){
  fit_vec<- vector(mode = "list", length = (n_week_adjusting+1))
  for ( i in 0:n_week_adjusting){

    fit <- stats::glm(stats::as.formula(paste0("n_death", "~",  glue::glue("n0_{i}"))),
                      family = "quasipoisson", data = data[1:(nrow(data)-n_week_adjusting)])
    n_cor <- round(stats::predict(fit, newdata = data, type = "response")) ###SHOULD THIS BE ROUNDED?
    data[, glue::glue("ncor0_{i}"):= n_cor]

    fit_vec[[i+1]]$fit<- fit
    fit_vec[[i+1]]$formula<- stats::as.formula(paste0("n_death", "~",  glue::glue("n0_{i}")))
  }

  retval <- vector("list")
  retval$data<- data
  retval$n_week_adjusting <- n_week_adjusting
  retval$fit <- fit_vec
  return(retval)
}



### nowcast_correction_fn_expanded ----

#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param data Data generated with nowcast_aggregate containing the part of the dataset that the model should train on.
#' @param n_week_adjusting Number of weeks to correct
#' @return nowcast_correction_object including corrected data for all weeks in n_wwk_adjust and the model fits for all weeks
#' @examples
#' data <- attrib::data_fake_nowcasting_aggregated
#' n_week_adjusting <- 8
#' n_week_train <- 52
#' n_week_start <- n_week_adjusting + n_week_train
#' date_0 <- data[nrow(data),]$cut_doe #last date in the dataset, assume the dataset is ordered.
#' data <- data[cut_doe >= (date_0 - n_week_start*7 + 1), ]
#' nowcast_correction_object <- nowcast_correction_fn_expanded(data, n_week_adjusting )
#' @export
nowcast_correction_fn_expanded <- function(data, n_week_adjusting, offset){

  temp_variable_n <- NULL
  cut_doe <- NULL


  # for developping
  # data<- as.data.table(data_fake_nowcasting_aggregated)
  # n_week_adjusting <- 8
  # offset = TRUE
  for ( i in 0:n_week_adjusting){

    week_n <- paste0("n0_",(i))
    data[, temp_variable_n := get(week_n)]
    data[, paste0("n0_",(i), "_lag1") := shift(temp_variable_n, 1, fill = 0)]

  }
  data <- subset(data, select= -c(temp_variable_n))
  # data[, week := isoweek(cut_doe)]
  # data[, year := year(cut_doe)] #er dettte rett?

  ########## fit ----
  cut_doe_vec <- data[(nrow(data)-n_week_adjusting):nrow(data)]$cut_doe

  fit_vec <- vector(mode = "list", length = (n_week_adjusting+1))
  for ( i in 0:n_week_adjusting){
   # print(i)
    formula <- paste0("n_death", "~sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52)+ year +",
                      glue::glue("n0_{i}_lag1"), "+",  glue::glue("n0_{i}"))
    if(i>=1){
      for (j in 0:(i-1)){
        formula <-  paste0(formula, "+",  glue::glue("n0_{j}"))
      }
    }

    if(offset){
      formula <- paste0(formula,  "+ offset(log(pop))")
    }


    fit <- stats::glm(stats::as.formula(formula), family = "quasipoisson", data = data[1:(nrow(data)-n_week_adjusting)])


     n_cor <- round(stats::predict(fit, newdata = data, type = "response")) ###SHOULD THIS BE ROUNDED?
     data[, glue::glue("ncor0_{i}"):= n_cor]

    cut_doe_cur <- cut_doe_vec[n_week_adjusting+1-i]
    fit_vec[[i+1]]$fit<- fit
    fit_vec[[i+1]]$formula<- formula
    #fit_vec[i+1]$i<- i

   }

  retval <- vector("list")
  retval$data<- data
  retval$n_week_adjusting <- n_week_adjusting
  retval$fit <- fit_vec

  return(retval)
  #return(data)
}

#### nowcast_correction_fn_crude ----
nowcast_correction_fn_crude <- function(data, n_week_adjusting){

  temp_variable_n <- NULL
  cut_doe <- NULL


  # for developping
  # data<- as.data.table(data_fake_nowcasting_aggregated)
  # n_week_adjusting <- 8

  for ( i in 0:n_week_adjusting){

    week_n <- paste0("n0_",(i))
    data[, temp_variable_n := get(week_n)]
    data[, paste0("n0_",(i), "_lag1") := shift(temp_variable_n, 1, fill = 0)]

  }
  data <- subset(data, select= -c(temp_variable_n))
  data[, week := isoweek(cut_doe)]
  data[, year := year(cut_doe)] #er dettte rett?

  ########## fit ----
  cut_doe_vec <- data[(nrow(data)-n_week_adjusting):nrow(data)]$cut_doe

  fit_vec <- vector(mode = "list", length = (n_week_adjusting+1))
  for ( i in 0:n_week_adjusting){
    # print(i)

    formula <- paste0("n_death", "~sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52)+ year +", glue::glue("n0_{i}_lag1"), "+",  glue::glue("n0_{i}"), "+ -1" )

    if(i>=1){
      for (j in 0:(i-1)){
        formula <-  paste0(formula, "+",  glue::glue("n0_{j}"))
      }
    }
    fit <- stats::glm(stats::as.formula(formula), family = "quasipoisson", data = data[1:(nrow(data)-n_week_adjusting)])


    n_cor <- round(stats::predict(fit, newdata = data, type = "response")) ###SHOULD THIS BE ROUNDED?
    data[, glue::glue("ncor0_{i}"):= n_cor]

    cut_doe_cur <- cut_doe_vec[n_week_adjusting+1-i]
    fit_vec[[i+1]]$fit<- fit
    fit_vec[[i+1]]$formula<- formula
    #fit_vec[i+1]$i<- i

  }

  retval <- vector("list")
  retval$data<- data
  retval$n_week_adjusting <- n_week_adjusting
  retval$fit <- fit_vec

  return(retval)
  #return(data)
}

### nowcast_correction_sim ----

#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param nowcast_correction_object object returned from function nowcast_correction_fn_expanded
#' @param n_sim Number of simulations
#' @return simulations of the estimate made by the fitted models in nowcast_correction_fn
#' @examples
#' data <- attrib::data_fake_nowcasting_aggregated
#' n_week_adjusting <- 8
#' n_week_train <- 52
#' n_week_start <- n_week_adjusting + n_week_train
#' date_0 <- data[nrow(data),]$cut_doe #last date in the dataset, assume the dataset is ordered.
#' data <- data[cut_doe >= (date_0 - n_week_start*7 + 1), ]
#' nowcast_correction_object <- nowcast_correction_fn_expanded(data, n_week_adjusting )
#' nowcast_sim <- nowcast_correction_sim(nowcast_correction_object)
#' @export
nowcast_correction_sim <- function(nowcast_correction_object, offset, n_sim = 500){

  n_death <- NULL
  sim_value <- NULL



  # for developping
  # data<- as.data.table(data_fake_nowcasting_aggregated)
  # n_week_adjusting <- 8
  # n_sim <- 500
  # nowcast_correction_object<- nowcast_correction_fn_expanded(data, n_week_adjusting, offset = TRUE)
  # offset <- TRUE

   fit_vec <- nowcast_correction_object$fit
   data <- nowcast_correction_object$data
   n_week_adjusting <- nowcast_correction_object$n_week_adjusting[[1]]

  ##########simmuleringer ----
  cut_doe_vec <- data[(nrow(data)-n_week_adjusting):nrow(data)]$cut_doe

  sim_val_vec <- vector("list", length = (n_week_adjusting+1))
  for ( i in 0:n_week_adjusting){


    fit <-fit_vec[[i+1]]$fit
    formula <- fit_vec[[i+1]]$formula
    cut_doe_cur <- cut_doe_vec[n_week_adjusting+1-i]

    x<- arm::sim(fit, n_sim)
    sim_models <- as.data.frame(x@coef)
    data_x <- as.data.table(copy(stats::model.frame(formula, data = data)))
    data_x <- data_x[nrow(data_x)]
    data_x[, n_death:= NULL]


    col_names<-  colnames(sim_models)
    col_names_rel <- col_names[which(col_names != "(Intercept)")]
    if (!"(Intercept)" %in% colnames(sim_models) ){
      if(offset){
        colnames(cbind(sim_models, 1))
        rownames(rbind( as.matrix(t(data_x))))

        expected <- as.matrix(cbind(sim_models, 1)) %*%  rbind(as.matrix(t(data_x)))

      }else{
        colnames(cbind(sim_models))
        rownames(rbind( as.matrix(t(data_x))))

        expected <- as.matrix(cbind(sim_models)) %*%  rbind(as.matrix(t(data_x)))
      }
    } else{
        if(offset){
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
    print(dispersion)
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


    #print(cut_doe_cur)
    #expected_sim[, sim_value:= round(as.numeric(sim_value), 2)]
    sim_val_vec[[i +1]]<- expected_sim


  }


  sim_data <- rbindlist(sim_val_vec)
  retval<- merge(data, sim_data, by = "cut_doe", all = TRUE)
  return(retval)
}




#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param data_aggregated Aggregated dataset from the function npowcast_aggregate
#' @param n_week_adjusting Number of weeks to correct
#' @param n_week_training Number of weeks to train on
#' @param pop_data, Dataset containing population data on a weekly basis, must contain year so it can be merged with the aggregated data. If the dataset contains week this will also be uusCan be NULL.
#' @param nowcast_correction_fn Correction function. Must return a table with columnames ncor0_i for i in 0:n_week and cut_doe. The default uses "n_death ~ n0_i" for all i in 0:n_week.
#' @param nowcast_correction_sim_fn Simmulatoin function. Must return a datatable with the following collumns  "n_death", "sim_value", "cut_doe", "ncor" and simmulations for equally many weeks as n_week_adjust.
#' @examples
#'
#' data <- attrib::data_fake_nowcasting_aggregated
#' n_week_adjusting <- 8
#' n_week_training <- 12
#' nowcast_object <- nowcast(data, n_week_adjusting,n_week_training )
#' @return Dataset including the corrected values for n_death
#'
#' @export
nowcast <- function(
  data_aggregated,
  offset,
  n_week_adjusting,
  n_week_training,
  nowcast_correction_fn = nowcast_correction_fn_expanded,
  nowcast_correction_sim_fn = nowcast_correction_sim) {

  data_fake_death_clean <- NULL
  ncor <- NULL
  n_death <- NULL
  temp_variable <- NULL
  yrwk <- NULL
  cut_doe <- NULL


  ##### for developing
  # data_aggregated <- as.data.table(data_fake_nowcasting_aggregated)
  # n_week_training <- 50
  # n_week_adjusting <- 8
  # nowcast_correction_fn<- nowcast_correction_fn_expanded
  # #nowcast_correction_fn<- nowcast_correction_fn_simple
  # #nowcast_correction_fn <- nowcast_correction_fn_crude
  # nowcast_correction_sim_fn = nowcast_correction_sim


  data <- as.data.table(data_aggregated)
  n_week_start <- n_week_training + n_week_adjusting

  date_0 <- data[nrow(data)]$cut_doe

  data <- data[cut_doe >= (date_0 - n_week_start*7 + 1) ]

  #### corrected n_deaths ----
  if (offset){
    nowcast_correction_object <- nowcast_correction_fn(data, n_week_adjusting, offset = TRUE)
  } else{
    nowcast_correction_object <- nowcast_correction_fn(data, n_week_adjusting, offset = FALSE)
  }

  data <- nowcast_correction_object$data
  data_sim <- nowcast_correction_sim_fn(nowcast_correction_object, offset = TRUE)

  #check that all the required variables are there
  # (i.e. that the correction function actually gives reasonable stuff back)

  for ( i in 0:n_week_adjusting){
    temp <- paste0("ncor0_",i)
    if(! temp %in% colnames(data)){
      stop(glue::glue("nowcast_correction_fn is not returning {temp}"))
    }
  }


  data[, ncor := n_death]

  date_0 <- data[nrow(data)]$cut_doe
  for ( i in 0:n_week_adjusting){
    date_i <- date_0 - 7*i
    temp <- paste0("ncor0_",i)
    data[, temp_variable := get(temp)]
    data[cut_doe == date_i, ncor:= temp_variable]


  }

  data[,temp_variable:=NULL]


  data[, yrwk:= isoyearweek(cut_doe)]
  data_sim[, yrwk:= isoyearweek(cut_doe)]


  col_order <- c(c("yrwk", "n_death", "ncor"), colnames(data)[which(!colnames(data) %in% c("yrwk", "n_death", "ncor"))])
  setcolorder(data, col_order)

  date_n_Week_adjusting_start <- date_0 - (n_week_adjusting)*7
  data_sim_clean <- data_sim[cut_doe >= date_n_Week_adjusting_start]

  col_order_sim <- c(c("yrwk", "n_death", "sim_value"), colnames(data_sim_clean)[which(!colnames(data_sim_clean) %in% c("yrwk", "n_death", "sim_value"))])
  setcolorder(data_sim_clean, col_order_sim)

  data_sim_clean <- subset(data_sim_clean, select = c("yrwk", "n_death", "sim_value", "cut_doe", "week", "year"))

  retval <- vector("list")
  retval$data <- data
  retval$data_sim <- data_sim_clean
  return (retval)
}
