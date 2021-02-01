weather <- fd::get_weather()
weather <- weather[location_code=="county03"]

weather[,ratio:=mean(tg/tx,na.rm=T)]
weather[is.na(tg),tg:=tx*ratio]
weather[,ratio:=NULL]

weather[,ratio:=mean(tx/tg,na.rm=T)]
weather[is.na(tx),tx:=tg*ratio]
weather[,ratio:=NULL]

weather[,ratio:=mean(tn/tg,na.rm=T)]
weather[is.na(tn),tn:=tg*ratio]
weather[,ratio:=NULL]

mem <- fd::tbl("spuls_mem_results") %>%
  dplyr::filter(tag == "influensa") %>%
  dplyr::filter(location_code == "county03") %>%
  dplyr::collect() %>%
  fd::latin1_to_utf8()

ils <- data.table(date=seq.Date(min(mem$date),max(mem$date),1))
ils[mem,on="date",ils:=rate]
ils[,ils:=zoo::na.locf(ils, fromLast=T)]

d <- fd::tbl("normomo_daily_results") %>%
  dplyr::filter(location_code == "norway") %>%
  dplyr::filter(age == "Total") %>%
  dplyr::collect() %>%
  fd::latin1_to_utf8()

dates <- intersect(weather$date, d$date)
dates <- intersect(dates, ils$date)

weather <- weather[date %in% dates]
d <- d[date %in% dates]
ils <- ils[date %in% dates]
dates <- sort(dates)
dates <- as.Date(dates, origin = "1970-01-01")

outcome <- d$nbc
temp <- weather$tx
ils <- ils$ils

fit3 <- fit_attrib(
  dates = dates,
  outcome = outcome,
  exposure_values = list(
    "tx" = temp,
    "ils" = ils
  ),
  exposure_types = list(
    "tx" = "cubic",
    "ils" = "linear"
  ))

plot(names(fit$pred$tx$allRRfit),fit$pred$tx$allRRfit)

get_attrib(fit3, use_blup=F, tag="tx", range=20:200)

a <- fit_preds(
  basis = fit$basis,
  exposure_values = fit$exposure_values,
  fit = fit$fit
)

b <- fit_preds(
  basis = fit$basis,
  exposure_values = fit$exposure_values,
  coef = coef(fit$fit),
  vcov= vcov(fit$fit)
)

names(fit)
fit$pred

x <- create_blup(fit1, fit2, fit3)
fit1 <- x[[1]]
fit2 <- x[[2]]
fit3 <- x[[3]]

get_attrib(fit1, use_blup=F, tag="tx", range=20:200)
get_attrib(fit1, use_blup=T, tag="tx", range=20:200)


fit1$attrib_fixed$pred$tx$allRRfit
fit1$attrib_blup$pred$tx$allRRfit

metaFluMoDL <- function(summaries, par=c("tx","ils")) {
  if (length(summaries)<2 || length(unique(sapply(summaries, class)))!=1 ||
      unique(sapply(summaries, class))!="summary.FluMoDL")
    stop("Argument `summaries` must be a list of objects of class `summary.FluMoDL`.")
  nm <- names(summaries)
  par <- paste0("proxy", par)
  # Get the number of summaries with data for each parameter
  Nsum <- rowSums(sapply(summaries, function(s) par %in% names(s$coef)))
  # If a parameter has <2 summaries, drop it (without a warning; maybe add a warning in the future)
  par <- par[Nsum>=2]
  # Loop through the parameters and create the meta-analysis
  M <- lapply(par, function(p) {
    TE <- lapply(summaries, function(s) s$coef[[p]])
    seTE <- lapply(summaries, function(s) s$vcov[[p]])

    nmm <- nm[!sapply(seTE, is.null)]
    TE <- TE[!sapply(TE, is.null)]
    TE <- do.call("rbind", TE)
    seTE <- seTE[!sapply(seTE, is.null)]

    rownames(TE) <- nmm
    names(seTE) <- nmm

    mvmeta(TE, S=seTE)
  })
  names(M) <- par
  class(M) <- "metaFluMoDL"
  M
}
