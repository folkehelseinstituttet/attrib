#' For more details see the help vignette:
#' \code{vignette("intro", package="attrib")}
#'
#' @param nowcast_object Object from the function nowcast
#' @param n_week_adjusting Number of weeks to adjust
#'
#' @examples
#' \dontrun{
#'
#' data <- attrib::data_fake_nowcasting_aggregated
#' n_week_adjusting <- 8
#' data_correct <- nowcast_eval(data, n_week_adjusting )
#' }
#' @return Residualplots for all ncor_i and some evaluationmetrixs for each of them
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




  # data <- read.table("C:/Users/AUHO/Desktop/FHIDOD2_20201229.txt", sep = ";", header = TRUE)
  # data <- as.data.table(data)
  #
  # data[, doe := as.Date(as.character(DODS_DATO), format = "%Y%m%d")]
  # data[, dor := as.Date(as.character(ENDR_DATO), format = "%Y%m%d")]
  #
  # data<- na.omit(data)
  #
  # data_aggregated <- nowcast_aggregate(data, lubridate::today(), n_week = 13)

  #data_cast<-nowcast(data_aggregated, n_week_adjusting = 8, n_week_training = 40)


  # for developint
  # data_aggregated <- as.data.table(data_fake_nowcasting_aggregated)
  # n_week_training <- 50
  # n_week_adjusting <- 8
  # nowcast_object <- nowcast(data_aggregated= data_aggregated, n_week_training = 50, n_week_adjusting = 8)
  #


  data <- nowcast_object$data
  data_sim <- nowcast_object$data_sim

  retval <- vector("list" , length = (n_week_adjusting+1))


  for (i in 0:n_week_adjusting ){
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

  q05 <- function(x){
    return(quantile(x, 0.05))
  }
  q95 <- function(x){
    return(quantile(x, 0.95))
  }


  col_names <- colnames(data_sim)
  data.table::setkeyv(data_sim,
                      col_names[!col_names %in% c("sim_value")])

  aggregated_data_sim<- data_sim[, unlist(recursive = FALSE, lapply(.(median = median, q05 = q05, q95 = q95),
                                                                    function(f) lapply(.SD, f))),
                                 by = eval(data.table::key(data_sim)),
                                 .SDcols = c("sim_value")]

  q <- ggplot2::ggplot(aggregated_data_sim,
                       ggplot2::aes(x = yrwk, y = median.sim_value))
  q <- q + ggplot2::geom_errorbar(ggplot2::aes(ymin=q05.sim_value, ymax=q95.sim_value), colour="blue", width=.1)
  q <- q + ggplot2::geom_point( size=3)
  q <- q + ggplot2::ggtitle("Estimated mortality with 90 percent credible intervals")
  q <- q +  ggplot2::scale_y_continuous("N corrected")
  q <- q + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),axis.title.x=ggplot2::element_blank())
  #q <- q +  labs(caption = glue::glue(""))
  q


  retval$CI_plot <- q
   return (retval)
}

