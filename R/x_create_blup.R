# # create_blup
# # @param summaries attrib
# # @export
# create_blup <- function(summaries) {
#   TE <- lapply(summaries, function(s) stats::coef(s$attrib_fixed$fit))
#   seTE <- lapply(summaries, function(s) stats::vcov(s$attrib_fixed$fit))
#   TE <- do.call("rbind", TE)
#
#   fit_meta <- mvmeta::mvmeta(TE, S = seTE, method = "reml")
#   res <- mvmeta::blup(fit_meta, vcov = T)
#
#   retval <- vector("list", length = length(summaries))
#   for (i in seq_along(summaries)) {
#     retval[[i]] <- summaries[[i]]
#
#     retval[[i]]$attrib_blup$outcome <- retval[[i]]$attrib_fixed$outcome
#     retval[[i]]$attrib_blup$exposure_values <- retval[[i]]$attrib_fixed$exposure_values
#     retval[[i]]$attrib_blup$fit <- NULL
#     retval[[i]]$attrib_blup$basis <- retval[[i]]$attrib_fixed$basis
#
#     x <- fit_preds(
#       basis = retval[[i]]$attrib_blup$basis,
#       exposure_values = retval[[i]]$attrib_blup$exposure_values,
#       coef = res[[i]]$blup,
#       vcov = res[[i]]$vcov
#     )
#
#     retval[[i]]$attrib_blup$pred <- x$pred
#     retval[[i]]$attrib_blup$mmps <- x$mmps
#     retval[[i]]$attrib_blup$can_be_used <- TRUE
#   }
#   return(retval)
# }
