  #For de-clustered non-thunderstorm: Gumbel Fittings, GPD-Poisson-GPD extremeStat results and statistics (years, weeks, months, gaps)
  # 1) Write (raw_data_station_id_fitted.xlsx, variable fnfitted) "Gumbel fittings and GPD-Poisson-GPD extremeStat results"
  #    to sheets names:
  #  - t_evd-fgev_fGumbel: fitting Gumbel using evd::fgev
  #  - t_bbmle-mle2: fitting Gumbel using bbmle::mle2
  #  - t_nll-optim: fitting Gumbel using negative likelihood and stats::optim
  #  - t_fitdistrplus-fitdist: fitting Gumbel using fitdistrplus::fitdist
  #  - t_extRemes: calculation of return levels POT-Poisson-GPD, using extRemes::fevd
  #  - t_distLquantile_quant: calculation of return levels and RMSE (POT-Poisson-GPD and EVDs), using extremeStat::distLquantile
  #  - t_distLquantile_parameters: calculation of fitting parameters POT-Poisson-GPD and EVD, using extremeStat::distLquantile
  #  - t_distLextreme_returnlev: calculation of return levels POT-Poisson-GPD and EVD, using extremeStat::distLextreme
  #  - t_distLextreme_parameter: calculation of fitting parameters POT-Poisson-GPD and EVD, using extremeStat::distLextreme
  # 2) Send to PDF (FittedModel_ID.pdf) graphics for declustered non-thunderstorm (Gumbel fittings and GPD-Poisson-GPD extremeStat results)
  #  - Data Histogram and Fitted Gumbel Probability Density Curve - Log-Likelihood(Gumbel) - Optim (nll-optim)
  #  - Declustered - Non-Thunderstorm - fitdistrplus-fitdist(gumbel)
  # 3) In file raw_data_station_ID_statistics.xlsx (variable 'statsfile') create the statistics sheets for thunderstorm:
  #  - declu_nt_years
  #  - declu_nt_weeks
  #  - declu_nt_months
  #  - declu_nt_gaps30days
  # 4) Send to PDF (FittedModel_ID.pdf) graphics for declustered non-thunderstorm:
  #  - Declustered Non-Thunderstorm ('t') Time Series
if (length(imp.vals$nt.series.dt) > 0) {
  #Print raw.data histogram
  title= paste("Frequency Histogram of Decluster and Thresholding Non-Thunderstorm\n", 
               "Station: ", number, sep="")
  print(hist(imp.vals$nt.series, probability = FALSE, col="cadetblue3", main=title))
  mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
  numberofplots = numberofplots + 1
  #_________________________________________________
  #Checking parameters using others methods!!!
  #Use evd to estimate parameters using "pp"
  library(evd)
  library(lubridate)
  potdata = data.frame(time=lubridate::decimal_date(imp.vals$nt.series.dt[1:length(imp.vals$nt.series)]), obs= imp.vals$nt.series)
  
  #save declustered nonthunderstorm time series
  mydnt=data.frame(dnt=imp.vals$nt.series, time =imp.vals$nt.series.dt)
  #write mydnt declustered nonthunderstorm data
  write.table(mydnt,file=paste0(outputpath, "dnt_", number, ".csv"),sep=";", row.names=FALSE)
  
  #obsperyears = length(imp.vals$nt.series)/(imp.vals$total.time/365.25)
  obsperyears = imp.vals$n.nthunders.per.year
  M2 <- evd::fpot(potdata$obs, threshold = nt.thresh, cmax=FALSE, npp= obsperyears, model="pp", shape = 0, std.err = FALSE)
  myfit = cbind("loc" = M2$estimate[1], "scale" = M2$estimate[2])
  #If have time please review this pp some day!!
  #write.xlsx(myfit, file=fnfitted, sheetName="nt_pp-evd", append=TRUE, row.names=FALSE, col.names=TRUE)
  #M2Ploc <- profile(M2, conf=0.975)
  #plot(M2Ploc)
  #M2Pscale <- profile(M2, which = "scale", conf=0.975, mesh=c(0.001))
  #plot(M2Pscale)
  #If have time please review this pp some day!!
  #par(mfrow = c(2,2))
  #plot(M2)
  #mtext(side = 1, text = paste0("Page ", numberofplots, " - Declustered - Non-Thunderstorm - Package EVD - Fitting a PP. Location: ",
  #        round(M2$estimate[1], digits=2), ". Scale: ", round(M2$estimate[2], digits = 2)),
  #      outer = TRUE)
  #numberofplots = numberofplots + 1


  #Use function fGumbel to estimate parameters
  require(evd)
  library
  cat(number)
  fit.evd <- evd::fgev(x=imp.vals$nt.series, shape = 0.0)
  fit.new <- fGumbel(imp.vals$nt.series)
  myfit = cbind("evd::fgev" = c(fit.evd$estimate, "deviance" = fit.evd$deviance),
                "new" = c(fit.new$estimate, fit.new$deviance))
  locbyfgev = fit.evd$estimate[1]
  scalebyfgev = fit.evd$estimate[2]

  #write.xlsx(myfit, file=fnfitted, sheetName="nt_evd-fgev_fGumbel", append=TRUE, row.names=TRUE)
  addWorksheet(fnfitted_OUT, "nt_evd-fgev_fGumbel")
  writeData(fnfitted_OUT, sheet = "nt_evd-fgev_fGumbel", x = myfit)

  #

  #Use function logLH
  #Estimators of moments method
  mu = mean(imp.vals$nt.series) + (0.45006 * sd(imp.vals$nt.series))
  sigma = (sd(imp.vals$nt.series)*sqrt(6))/pi
  library(bbmle)
  est <- bbmle::mle2(logLH, start = list(mu = mu, sigma = sigma), data = list(x = imp.vals$nt.series))
  intervals = confint(est, level = 0.95)
  myfit = list(mu2.5 = intervals["mu",1], mu=est@coef[1], mu97.5 = intervals["mu",2],
               sigma2.5 = intervals["sigma",1], sigma=est@coef[2], sigma97.5 = intervals["sigma",2])
  #write.xlsx(myfit, file=fnfitted, sheetName="nt_bbmle-mle2", append=TRUE, row.names=TRUE)
  addWorksheet(fnfitted_OUT, "nt_bbmle-mle2")
  writeData(fnfitted_OUT, sheet = "nt_bbmle-mle2", x = myfit)

  #Use the minus log-likelihood (function mllGumbel) and optim
  mllToBeOptimized <- function(par)
    mllGumbel(par[1], par[2], imp.vals$nt.series)
  mle <- optim(c(mu, sigma), mllToBeOptimized)$par

  #dgumbel <- function(x,mu,sigma){ # PDF
  #  exp((mu - x)/sigma - exp((mu - x)/sigma))/sigma
  #}
  par(mfrow = c(1,1))
  hist (imp.vals$nt.series, probability = TRUE, col='cadetblue3',
        xlab="Declustered - Non-Thunderstorm Series", main="Data Histogram and Fitted Gumbel Probability Density Curve")
  curve(dgumbel(x, mle[1], mle[2]), col = "red", add = TRUE)
  myfit = list(mu=mle[1], sigma=mle[2])
  #write.xlsx(myfit, file=fnfitted, sheetName="nt_nll-optim", append=TRUE, row.names=TRUE)
  addWorksheet(fnfitted_OUT, "nt_nll-optim")
  writeData(fnfitted_OUT, sheet = "nt_nll-optim", x =myfit)

  mtext(side = 1, text = paste0("Page ", numberofplots, " - Log-Likelihood(Gumbel) - Optim (nll-optim). Location: ",
                                round(mle[1], digits=2), ". Scale: ", round(mle[2], digits=2)), outer = TRUE)
  #legend("topright", c("Data", "Fitted"), fill=c("cadetblue3", "red"))
  #legend("topright", c("Data", "Fitted"), col=c("cadetblue3", "red"), lwd=c(15,1))
  legend("topright", c("Data-Empirical", "Fitted-Theoretical"),
         bty = "n",
         col = c("cadetblue3", "red"),
         lty = c(0, 1), lwd = c(0, 1),
         pch = c(22, NA),
         pt.bg = c("cadetblue3", NA),
         pt.cex = 2)
  #assign(paste0("myprint", numberofplots), recordPlot())
  #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))
  numberofplots = numberofplots + 1

  #Use package library(fitdistrplus)
  library(fitdistrplus)
  gumbel.fit <- fitdistrplus::fitdist(imp.vals$nt.series, "gumbel", start=list(mu=mu, sigma=sigma), method="mle")
  gofstat(gumbel.fit, discrete=FALSE) # goodness-of-fit statistics
  par(cex=1.2, bg="white")
  plot(gumbel.fit, lwd=2, col="cadetblue3")
  mtext(side = 1, text = paste0("Page ", numberofplots, " - Declustered - Non-Thunderstorm - fitdistrplus-fitdist(gumbel). Location: ",
                                round(gumbel.fit$estimate["mu"], digits = 2), ". Scale: ", round(gumbel.fit$estimate["sigma"], digits = 2)),
        outer = TRUE)
  #assign(paste0("myprint", numberofplots), recordPlot())
  #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))
  numberofplots = numberofplots + 1
  myfit = list(mu=gumbel.fit$estimate["mu"], sigma=gumbel.fit$estimate["sigma"])
  #write.xlsx(myfit, file=fnfitted, sheetName="nt_fitdistrplus-fitdist", append=TRUE, row.names=TRUE)
  addWorksheet(fnfitted_OUT, "nt_fitdistrplus-fitdist")
  writeData(fnfitted_OUT, sheet = "nt_fitdistrplus-fitdist", x = myfit)

  #Use package extremeStat to fit all the modes includes POT-GPD from different packages
  #imp.vals$t.series
  #imp.vals$nt.series
  #imp.vals$t.series.dt
  #imp.vals$nt.series.dt
  #imp.vals$t.length.time
  #imp.vals$nt.length.time
  #imp.vals$total.time
  #imp.vals$n.thunders.per.year
  #imp.vals$n.nthunders.per.year

  #extRemes Alexys
  #library(extRemes)
  tipicalReturnPeriods = c(10,20,50,100,250,500,700,1000,1700,3000,7000)
  npy=imp.vals$n.nthunders.per.year
  myextrRemes = alexys_extRemes(imp.vals$nt.series, threshold=nt.thresh,
                                RPs=tipicalReturnPeriods, npy=npy)
  #write.xlsx(myextrRemes, file=fnfitted, sheetName="nt_extRemes", append=TRUE, row.names=TRUE)
  addWorksheet(fnfitted_OUT, "nt_extRemes")
  writeData(fnfitted_OUT, sheet = "nt_extRemes", x = myextrRemes)


  #extRemes berry
  library(extremeStat)
  npy=imp.vals$n.nthunders.per.year   #Number of observations per year
  #w = length(imp.vals$nt.series)/npy  #Fitting period: Total observations divided in npy
  #Observations over threshold
  overthresh = imp.vals$nt.series > nt.thresh
  #overthreshold = imp.vals$t.series >= z2
  #lambda = length(imp.vals$nt.series[overthresh])/w
  tipicalReturnPeriods = c(10,20,50,100,250,500,700,1000,1700,3000,7000)
  p = (1 - (1/(npy*tipicalReturnPeriods)))

  truncate = 1 - (sum(overthresh)/length(imp.vals$nt.series))
  d <- distLquantile(imp.vals$nt.series, truncate=truncate, probs=p, quiet=TRUE, list=TRUE)

  #plotLquantile(d, breaks=50, xlab="Declustered - Non-Thunderstorm - plotLquantile {extremeStat}")
  #mtext(side = 1, text = paste0("Page ",
  #      numberofplots, " - Station: ", number), outer = TRUE)
  #numberofplots = numberofplots + 1

  #write.xlsx(d$quant, file=fnfitted, sheetName="nt_distLquantile_quant", append=TRUE, row.names=TRUE)
  addWorksheet(fnfitted_OUT, "nt_distLquantile_quant")
  writeData(fnfitted_OUT, sheet = "nt_distLquantile_quant", x = d$quant)

  #write.xlsx(capture.output(d$parameter), file=fnfitted, sheetName="nt_distLquantile_parameters", append=TRUE, row.names=TRUE)
  addWorksheet(fnfitted_OUT, "nt_distLquantile_parameters")
  writeData(fnfitted_OUT, sheet = "nt_distLquantile_parameters", x = capture.output(d$parameter))

  dlf <- distLextreme(imp.vals$nt.series, quiet=TRUE, RPs=tipicalReturnPeriods, npy=npy, truncate=truncate)

  #plotLextreme(dlf, log=TRUE, legargs=list(cex=0.6, bg="transparent"), xlab="Return Period - RP", ylab="Velocidades [Km/h]", xlim=c(10,7000), ylim=c(20,250))
  #mtext(side = 1, text = paste0("Page ",
  #                              numberofplots, " - Declustered - Non-Thunderstorm - plotLextreme {extremeStat} - Station: ", number), outer = TRUE)
  #numberofplots = numberofplots + 1
  #write.xlsx(dlf$returnlev, file=fnfitted, sheetName="nt_distLextreme_returnlev", append=TRUE, row.names=TRUE)
  addWorksheet(fnfitted_OUT, "nt_distLextreme_returnlev")
  writeData(fnfitted_OUT, sheet = "nt_distLextreme_returnlev", x = dlf$returnlev)
  
  #write.xlsx(capture.output(dlf$parameter), file=fnfitted, sheetName="nt_distLextreme_parameter", append=TRUE, row.names=TRUE)
  addWorksheet(fnfitted_OUT, "nt_distLextreme_parameter")
  writeData(fnfitted_OUT, sheet = "nt_distLextreme_parameter", x = capture.output(dlf$parameter))

  #_________________________________________________

  #Statistics work
  ntds = data.frame(imp.vals$nt.series.dt, imp.vals$nt.series)
  names(ntds) = c("nt.series.dt", "nt.series")
  ntds = as_tibble(ntds)
  years = generate_stats_time_serie(ntds, "nt.series", ntds$nt.series.dt, "years")
  months = generate_stats_time_serie(ntds, "nt.series", ntds$nt.series.dt, "months")
  weeks = generate_stats_time_serie(ntds, "nt.series", ntds$nt.series.dt, "weeks")

  #write.xlsx(years, file=statsfile, sheetName="declu_nt_years", append=TRUE, row.names=TRUE)
  addWorksheet(statsfile_OUT, "declu_nt_years")
  writeData(statsfile_OUT, sheet = "declu_nt_years", x = years)

  #write.xlsx(months, file=statsfile, sheetName="declu_nt_months", append=TRUE, row.names=TRUE)
  addWorksheet(statsfile_OUT, "declu_nt_months")
  writeData(statsfile_OUT, sheet = "declu_nt_months", x = months)

  #write.xlsx(weeks, file=statsfile, sheetName="declu_nt_weeks", append=TRUE, row.names=TRUE)
  addWorksheet(statsfile_OUT, "declu_nt_weeks")
  writeData(statsfile_OUT, sheet = "declu_nt_weeks", x = weeks)

  #Search time differences in days between consecutive samples greather than threshold in days (last parameter next function)
  thresholdindays = 30
  holesindays = locate_holes_time_serie(ntds, "nt.series", ntds$nt.series.dt, thresholdindays)

  #write.xlsx(holesindays, file=statsfile, sheetName=paste0("declu_nt_gaps",thresholdindays,"days"), append=TRUE, row.names=FALSE)
  addWorksheet(statsfile_OUT, paste0("declu_nt_gaps",thresholdindays,"days"))
  writeData(statsfile_OUT, sheet = paste0("declu_nt_gaps",thresholdindays,"days"), x = holesindays)

  #Plot time serie
  print(plotxts(data=ntds, variable="nt.series", time=ntds$nt.series.dt, cex.main=0.2, major.ticks="years",
                xlab=paste0("Page ",numberofplots, " - Declustered Non-Thunderstorm ('nt') Time Series - Station: ", number),
                main = paste0("Station ID: ",  number, "\nWind Velocity [Km/h]")))
  #assign(paste0("myprint", numberofplots), recordPlot())
  #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))				
  numberofplots = numberofplots + 1
}
