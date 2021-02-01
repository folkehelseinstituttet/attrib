# create_basis <- function(x, type, knots = NULL, boundary_knots = NULL) {
#   lg <- 30 # 30 days maximum lag (fixed)
#
#   if (is.null(knots)) knots <- stats::quantile(x, c(0.2, 0.8))
#   if (is.null(boundary_knots)) boundary_knots <- range(x)
#
#   if (type == "cubic") {
#     retval <- dlnm::crossbasis(x,
#       lag = lg,
#       argvar = list(fun = "ns", knots = knots, Boundary.knots = boundary_knots),
#       arglag = list(fun = "ns", knots = dlnm::logknots(lg, 3))
#     )
#   } else if (type == "linear") {
#     retval <- dlnm::crossbasis(x,
#       lag = lg,
#       argvar = list(fun = "poly", degree = 1),
#       arglag = list(fun = "ns", knots = dlnm::logknots(lg, 3))
#     )
#   }
#   return(retval)
# }
#
# gen_basis_name <- function(tag) {
#   return(glue::glue("x_basis_{tag}"))
# }
#
# # fit_attrib
# # @param dates x
# # @param outcome x
# # @param exposure_values x
# # @param exposure_types a
# # @param exposure_knots a
# # @param exposure_boundary_knots a
# # @export
# fit_attrib <- function(
#                        dates = FluMoDL::greece$daily$date,
#                        outcome = FluMoDL::greece$daily$deaths,
#                        exposure_values = list(
#                          "tg" = FluMoDL::greece$daily$temp
#                        ),
#                        exposure_types = list(
#                          "tg" = "cubic"
#                        ),
#                        exposure_knots = list(
#                          "tg" = c(-10, 20)
#                        ),
#                        exposure_boundary_knots = list(
#                          "tx" = c(-25, 35)
#                        )) {
#   calc_year <- fhi::isoyear_n(dates) - 2000
#   calc_week <- fhi::isoweek_n(dates)
#
#   basis_names <- c()
#   basis <- list()
#   for (i in seq_along(exposure_values)) {
#     name <- names(exposure_values)[[i]]
#     new_name <- gen_basis_name(tag = name)
#     basis[[name]] <- temp <- create_basis(
#       x = exposure_values[[name]],
#       type = exposure_types[[name]],
#       knots = exposure_knots[[name]],
#       boundary_knots = exposure_boundary_knots[[name]]
#     )
#     txt <- glue::glue("{new_name} <- temp")
#     eval(parse(text = txt))
#     basis_names <- c(basis_names, new_name)
#   }
#
#   exposures <- glue::glue_collapse(basis_names, sep = " + ")
#   formula <- glue::glue("outcome ~ {exposures} + calc_year + sin(2 * pi * (calc_week - 1) / 52) + cos(2 * pi * (calc_week - 1) / 52)")
#
#   fit <- stats::glm(stats::as.formula(formula), family = "quasipoisson")
#
#   x <- fit_preds(
#     basis = basis,
#     exposure_values = exposure_values,
#     fit = fit
#   )
#   MMPs <- x$mmps
#   pred <- x$pred
#
#   attrib_small <- list(
#     "outcome" = outcome,
#     "exposure_values" = exposure_values,
#     "fit" = fit,
#     "basis" = basis,
#     "pred" = pred,
#     "mmps" = MMPs,
#     "can_be_used" = TRUE
#   )
#   class(attrib_small) <- "attrib_small"
#
#   attrib_blup <- list("can_be_used" = FALSE)
#   class(attrib_blup) <- "attrib_small"
#
#   attrib <- list(
#     attrib_fixed = attrib_small,
#     attrib_blup = attrib_blup
#   )
#   class(attrib) <- "attrib"
#
#   return(attrib)
# }
#
# fit_preds <- function(basis, exposure_values, fit = NULL, coef = NULL, vcov = NULL) {
#   basis_names <- glue::glue("x_basis_{names(basis)}")
#   for (i in seq_along(basis_names)) {
#     new_name <- basis_names[i]
#     txt <- glue::glue("{new_name} <- basis[[i]]")
#     eval(parse(text = txt))
#
#     txt <- glue::glue("colnames({new_name}) <- paste0('{new_name}',colnames({new_name}))")
#     eval(parse(text = txt))
#   }
#
#   MMPs <- pred <- list()
#   for (i in seq_along(exposure_values)) {
#     name <- names(exposure_values)[[i]]
#     vals <- exposure_values[[name]]
#
#     if (is.null(fit) & !is.null(coef) & !is.null(vcov)) {
#       index <- names(coef) %in% colnames(get(basis_names[i]))
#       coefx <- coef[index]
#       vcovx <- vcov[index, index]
#       model.link <- "log"
#     } else {
#       model.link <- coefx <- vcovx <- NULL
#     }
#
#     txt <- glue::glue(
#       "dlnm::crosspred({basis_names[i]}, model=fit, coef=coefx, vcov=vcovx,model.link=model.link,",
#       "at = seq(ceiling(min(vals)), floor(max(vals)), 1),",
#       "bylag=0.2, cen=round(median(vals)), cumul=TRUE)"
#     )
#     pred[[name]] <- eval(parse(text = txt))
#
#     MMP <- as.integer(names(which(pred[[name]]$allfit == min(pred[[name]]$allfit))))
#     MMPs[[name]] <- MMP
#     # Refit prediction for temperature, centered at the MMP
#     txt <- glue::glue(
#       "dlnm::crosspred({basis_names[i]}, model=fit, coef=coefx, vcov=vcovx,model.link=model.link,",
#       "at = seq(ceiling(min(vals)), floor(max(vals)), 1),",
#       "bylag=0.2, cen={MMP}, cumul=TRUE)"
#     )
#     pred[[name]] <- eval(parse(text = txt))
#     if (!is.null(coef) & !is.null(vcov)) {
#       pred[[name]]$coefficients <- coefx
#       pred[[name]]$vcov <- vcovx
#     }
#   }
#
#   return(list(
#     "pred" = pred,
#     "mmps" = MMPs
#   ))
# }
#
# get_attrib_int <- function(list_of_attrib_small, tag, range, sub = NULL, coef = NULL, vcov = NULL) {
#   if (is.null(sub)) {
#     sub <- 1:length(list_of_attrib_small[[1]]$outcome)
#   }
#
#   retval <- matrix(NA, nrow = 5000, ncol = length(list_of_attrib_small) * length(tag))
#   for (j in 1:length(tag)) {
#     tag_x <- tag[j]
#     start_column <- (j - 1) * length(list_of_attrib_small)
#
#     for (i in 1:length(list_of_attrib_small)) {
#       attrib_small <- list_of_attrib_small[[i]]
#       if (!is.null(coef) & !is.null(vcov)) {
#         index <- which(names(coef) %in% attrib_small$pred[[tag_x]]$coefficients)
#         coefx <- coef[index]
#         vcovx <- vcov[index, index]
#       } else {
#         coefx <- attrib_small$pred[[tag_x]]$coefficients
#         vcovx <- attrib_small$pred[[tag_x]]$vcov
#       }
#
#       retvalx <- attrdl(
#         x = attrib_small$exposure_values[[tag_x]],
#         basis = attrib_small$basis[[tag_x]],
#         cases = attrib_small$outcome,
#         coef = coefx,
#         vcov = vcovx,
#         type = "an",
#         cen = attrib_small$mmp[[tag_x]],
#         range = range,
#         sim = T,
#         nsim = 5000,
#         sub = sub
#       )
#       retval[, start_column + i] <- retvalx
#     }
#   }
#   retval <- apply(retval, 1, sum)
#
#   retval <- as.data.frame(t(stats::quantile(c(retval), probs = c(0.025, 0.5, 0.975))))
#   data.table::setDT(retval)
#   data.table::setnames(retval, c("attr_low", "attr_est", "attr_high"))
#
#   return(retval)
# }
#
# attrib_use <- function(attrib, use_blup = FALSE) {
#   stopifnot("attrib" %in% class(attrib))
#
#   if ("attrib" %in% class(attrib) & use_blup == FALSE) {
#     tag <- "attrib_fixed"
#   } else if ("attrib" %in% class(attrib) & use_blup == TRUE) {
#     tag <- "attrib_blup"
#   }
#   if (!attrib[[tag]]$can_be_used) {
#     stop("can_be_used flag set to false")
#   }
#
#   return(tag)
# }
#
# # get_attrib
# # @param attrib x
# # @param use_blup x
# # @param tag x
# # @param range a
# # @param sub a
# # @import data.table
# # @export
# get_attrib <- function(attrib, use_blup = FALSE, tag, range, sub = NULL) {
#   if ("attrib" %in% class(attrib)) attrib <- list(attrib)
#
#   a_use <- attrib_use(attrib = attrib[[1]], use_blup = use_blup)
#
#   if (is.null(sub)) {
#     sub <- list(
#       1:length(attrib[[1]][[a_use]]$outcome)
#     )
#   } else if (is.list(sub) & length(sub) == 0) {
#     # this is an error and we can't compute
#     return(NULL)
#   } else if (!is.list(sub)) {
#     sub <- list(
#       sub
#     )
#   }
#
#   list_of_attrib_small <- lapply(attrib, function(x) x[[a_use]])
#
#   retval <- vector("list", length = length(sub))
#   pb <- fhi::txt_progress_bar(max = length(retval))
#   for (i in seq_along(retval)) {
#     utils::setTxtProgressBar(pb, i)
#
#     retval[[i]] <- get_attrib_int(
#       list_of_attrib_small = list_of_attrib_small,
#       tag = tag,
#       range = range,
#       sub = sub[[i]]
#     )
#   }
#   retval <- rbindlist(retval)
#   return(retval)
# }
