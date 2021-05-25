
#' Evaluation of the nowcast performance.
#'
#' Evaluates the performance of the nowcast function on historic data.
#'
#' For more details see the help vignette:
#' \code{vignette("nowcast", package="attrib")}
#'
#' @param nowcast_object Object from the function nowcast
#' @param n_week_adjusting Number of weeks to adjust
#' @examples
#'
#'  data_aggregated <-  data.table::as.data.table(data_fake_nowcasting_county_aggregated)[location_code == "county03",]
#'  n_week_training <- 50
#'  n_week_adjusting <- 5
#'  date_0 <- data_aggregated[nrow(data_aggregated),]$cut_doe
#'  nowcast_object <- nowcast(data_aggregated,offset = "log(pop)",
#'   n_week_adjusting,n_week_training,date_0,
#'   nowcast_correction_fn = nowcast_correction_fn_quasipoisson,
#'   nowcast_correction_sim_fn = nowcast_correction_sim_quasipoisson)
#'  nowcast_eval_object <- nowcast_eval(nowcast_object, n_week_adjusting)
#' @return Residualplots for all ncor_i and some evaluationmetrixs for each of them as well as a plot containing credible intervals using the simulations
#' @export
#'
nowcast_eval <- function(nowcast_object, n_week_adjusting){


  temp_variable <- NULL
  residual <- NULL
  n_death <- NULL
  std_residual <- NULL
  diff_n_death_mean <- NULL
  na.omit <- NULL
  data_fake_nowcasting_aggregated <- NULL
  diff_n_death_mean <- NULL
  q05 <- NULL
  q95 <- NULL
  . <- NULL
  median <- NULL
  yrwk <- NULL
  median.sim_value <- NULL
  q025.sim_value <- NULL
  q975.sim_value <- NULL


  ### for developing

  # data_aggregated <- as.data.table(data_fake_nowcasting_county_aggregated)
  # n_week_training <- 50
  # n_week_adjusting <- 4
  # nowcast_object <- nowcast_exp(data_aggregated= data_aggregated,
  #                               n_week_training = n_week_training,
  #                               n_week_adjusting = n_week_adjusting,
  #                               offset = "log(pop)")



  data <- nowcast_object$data
  data_sim <- nowcast_object$data_sim
  data_aggregated <- nowcast_object$data_sim_aggregated

  retval <- vector("list" , length = (n_week_adjusting))

  # Compute R2, mse, residualplot for every correction
  for (i in 0:(n_week_adjusting-1) ){
    temp <- paste0("ncor0_", i)
    data[, temp_variable := get(temp)]
    data[, residual:= temp_variable -n_death]
    std <- (sum(data$residual[1:50]**2))**0.5
    data[, std_residual:= (temp_variable -n_death)/std]

    mean <- sum(data$n_death)/nrow(data)
    data[, diff_n_death_mean := n_death - mean]

    R2 <- 1- (sum(na.omit(data)$residual**2))/(sum(na.omit(data)$diff_n_death_mean**2))
    MSE <- sum(na.omit(data)$residual**2)/nrow(na.omit(data))
    q <- ggplot2::ggplot(data, ggplot2::aes(x = temp_variable, y = std_residual))
    q <- q + ggplot2::geom_point()
    q <- q + ggplot2::geom_hline(yintercept = 0, colour = "red")

    q <- q + ggplot2::scale_y_continuous("Standard residuals")
    q <- q + ggplot2::scale_x_continuous("Number of deaths")

    q <- q + ggplot2::labs(caption = glue::glue(" {lubridate::today()}"))
    q <- q + ggplot2::ggtitle(paste( "Stdandard residuals for", temp))
    q
    temp_retval <- list()
    temp_retval$ncor <- i
    temp_retval$std_residualplot <- copy(q)

    # q <- ggplot2::ggplot(data, ggplot2::aes(x = temp_variable, y = residual))
    # q <- q + ggplot2::geom_point()
    # q <- q + ggplot2::geom_hline(yintercept = 0, colour = "red")
    # q <- q + ggplot2::ggtitle(temp)
    # temp_retval$residualplot <- copy(q)

    abs_error <- sum(abs(na.omit(data$residual)))/nrow(na.omit(data))
    temp_retval$abs_error <- abs_error
    temp_retval$R_squared <- R2
    temp_retval$MSE <- MSE
    temp_retval$RMSE <- MSE**0.5

    retval[[i +1]] <- temp_retval
  }

   return (retval)
}

