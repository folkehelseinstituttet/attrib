#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param data cleaned data to perform correction formula on
#' @param n_week_adjusting Number of weeks to correct
#'


nowcast_correction_fn_simple <- function(data, n_week_adjusting){
  for ( i in 0:n_week_adjusting){
    
    fit <- stats::glm(stats::as.formula(paste0("n_death", "~",  glue::glue("n0_{i}"))), family = "quasipoisson", data = data[1:(nrow(data)-n_week_adjusting)])
    n_cor <- round(stats::predict(fit, newdata = data, type = "response")) ###SHOULD THIS BE ROUNDED?
    data[, glue::glue("ncor0_{i}"):= n_cor]
    
  }
  return(data)
}

#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param data cleaned data to perform correction formula on
#' @param n_week_adjusting Number of weeks to correct
#'

nowcast_correction_fn_expanded <- function(data, n_week_adjusting){
  
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
  
  ##########simmuleringer ---- 
  cut_doe_vec <- data[(nrow(data)-n_week_adjusting):nrow(data)]$cut_doe

  sim_val_vec <- vector("list", length = (n_week_adjusting+1))
  for ( i in 0:n_week_adjusting){
    print(i)

    formula <- paste0("n_death", "~sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52)+ year +", glue::glue("n0_{i}_lag1"), "+",  glue::glue("n0_{i}"))

    if(i>=1){
      for (j in 0:(i-1)){
        formula <-  paste0(formula, "+",  glue::glue("n0_{j}"))
      }
    }
    fit <- stats::glm(stats::as.formula(formula), family = "quasipoisson", data = data[1:(nrow(data)-n_week_adjusting)])

  
     n_cor <- round(stats::predict(fit, newdata = data, type = "response")) ###SHOULD THIS BE ROUNDED?
     data[, glue::glue("ncor0_{i}"):= n_cor]

    cut_doe_cur <- cut_doe_vec[n_week_adjusting+1-i]

    n_sim = 500
    x<- arm::sim(fit, n_sim)
    sim_models <- as.data.frame(x@coef)
    data_x <- as.data.table(copy(stats::model.frame(formula, data = data)))
    data_x <- data_x[nrow(data_x)]
    data_x[, n_death:= NULL]

    col_names<-  colnames(sim_models)
    col_names_rel <- col_names[which(col_names != "Intercept")]

    dim(cbind(sim_models))
    dim(rbind(1, as.matrix(t(data_x))))

    colnames(cbind(cbind(sim_models)))
    rownames(rbind(1, as.matrix(t(data_x))))

    expected <- as.matrix(sim_models) %*%  rbind(1, as.matrix(t(data_x)))
    expected_sim <-data.table(
      sim_id = 1:500,
      sim_value = exp(as.numeric(expected[1:500])),
      cut_doe = cut_doe_cur
    )
    print(cut_doe_cur)
    expected_sim[, sim_value:= round(as.numeric(sim_value), 2)]
    sim_val_vec[[i +1]]<- expected_sim
    
   }


  sim_data <- rbindlist(sim_val_vec)
  retval<- merge(data, sim_data, by = "cut_doe", all = TRUE)
  return(retval)
  #return(data)
}

#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param data_aggregated Aggregated dataset from the function npowcast_aggregate
#' @param n_week_adjusting Number of weeks to correct
#' @param n_week_training Number of weeks to train on
#' @param nowcast_correction_fn Correction function. Must return a table with columnames ncor0_i for i in 0:n_week and cut_doe. The default uses "n_death ~ n0_i" for all i in 0:n_week. 
#' @examples
#' \dontrun{
#'
#' data <- attrib::data_fake_nowcasting_aggregated
#' n_week_adjusting <- 8
#' n_week_training <- 12
#' data_correct <- nowcast(data, n_week_adjusting,n_week_training )
#' }
#' @return Dataset including the corrected values for n_death
#'
#' @export
nowcast <- function(
  data_aggregated,
  n_week_adjusting,
  n_week_training,
  nowcast_correction_fn = nowcast_correction_fn_expanded) {

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

  
  data <- as.data.table(data_aggregated)
  n_week_start <- n_week_training + n_week_adjusting
  
  date_0 <- data[nrow(data)]$cut_doe
  
  data <- data[cut_doe >= (date_0 - n_week_start*7 + 1) ]
  
  #### corrected n_deaths ----
  data <- nowcast_correction_fn(data, n_week_adjusting)
  
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

  
  col_order <- c(c("yrwk", "n_death", "ncor"), colnames(data)[which(!colnames(data) %in% c("yrwk", "n_death", "ncor"))])
  setcolorder(data, col_order)
  ##### multiple weeks in formula ---- 
  
  # data <- as.data.table( data_fake_death_clean)
  # n_week <- 4
  
  # for ( i in 1:n_week){
  #   
  #   formula <- paste0("n_death", "~",  glue::glue("n0_{i}"))
  #   if(i>1){
  #     for (j in 1:(i-1)){
  #      formula <-  paste0(formula, "+",  glue::glue("n0_{j}"))
  #     }
  #   }
  #   
  #   fit <- glm(stats::as.formula(formula), family = "poisson", data = data[1:(nrow(data)-n_week)])
  #   n_cor <- round(predict(fit, newdata = data, type = "response")) ###SHOULD THIS BE ROUNDED?
  #   data[, glue::glue("ncor0_{i}"):= n_cor]
  #   
  # }
  
  
  
  
  #### testing ----
  
  # data_aggregated
  # fit <- stats::glm(stats::as.formula(paste0("n_death", "~", "n0_2 + n0_3 + n0_4")), family = "poisson", data = data_aggregated[600:(nrow(data_aggregated)-n_week_adjusting),])
  # summary(fit)
  # n_cor <- round(stats::predict(fit, newdata = data_aggregated, type = "response")) ###SHOULD THIS BE ROUNDED?
  # n_cor
  # 
  # 
  # data_aggregated[, n_cor :=n_cor]
  # 
  # data_aggregated[, diff := n_death - n_cor]
  
   # data[, n_death_lag_2 := shift(n_death, 1L)]
  # fit <- glm(n_death ~ n0_2 + n0_3 + n0_4, family = "poisson", data = data[1:(nrow(data)-3)])
  # summary(fit)
  # n_cor <- predict(fit, newdata = data, type = "response")
  # 
  # mean(abs(data$n_death[1:99]-n_cor[1:99]))
  # Metrics::mse(data$n_death[1:99], n_cor[1:99])
  # 
  # data[, n_death_lag_2 := shift(n_death, 1L)]
  # fit <- glm(n_death ~ n0_4 + n0_3, family = "poisson", data = data[1:(nrow(data)-3)])
  # summary(fit)
  # n_cor <- predict(fit, newdata = data, type = "response")
  # 
  # mean(abs(data$n_death[1:99]-n_cor[1:99]))
  # Metrics::mse(data$n_death[1:99], n_cor[1:99])
  # 
  # fit <- glm(n_death ~  n0_4, family = "poisson", data = data[1:(nrow(data)-3)])
  # summary(fit)
  # n_cor <- predict(fit, newdata = data, type = "response")
  # 
  # mean(abs(data$n_death[1:99]-n_cor[1:99]))
  # 
  # 
  # 
  # Metrics::mse(data$n_death[1:99], n_cor[1:99])
  
  retval <- data
  
  return (retval)
}
