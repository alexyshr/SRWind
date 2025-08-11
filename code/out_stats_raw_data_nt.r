#If there are records in the raw data non-thunderstorm time series, 
# 1) Generate statistics by years, weeks, months, gaps, and save the results  
#      to the Excel file raw_data_station_ID_statistics.xlsx (variable 'statsfile'), 
#      sheets nt_years, nt_weeks, nt_months, nt_gaps respectively.
# 2) Generate the time series plot for non-thunderstorm raw data
# 3) Calculate return levels using yearly maxima with function alexys_extRemes_yearlymaxima
if (length(raw.data.nt$date.time) > 0) {
  #Print raw.data histogram
  title= paste("Frequency Histogram of Raw Data Non-Thunderstorm\n", 
               "Station: ", number, sep="")
  print(hist(raw.data.nt$speed.kph, probability = FALSE, col="cadetblue3", main=title))
  mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
  numberofplots = numberofplots + 1
  
  years = generate_stats_time_serie(raw.data.nt, "speed.kph", raw.data.nt$date.time, "years")
  months = generate_stats_time_serie(raw.data.nt, "speed.kph", raw.data.nt$date.time, "months")
  weeks = generate_stats_time_serie(raw.data.nt, "speed.kph", raw.data.nt$date.time, "weeks")

  #write.xlsx(years, file=statsfile, sheetName="nt_years", append=TRUE, row.names=TRUE)
  addWorksheet(statsfile_OUT, "nt_years")
  writeData(statsfile_OUT, sheet = "nt_years", x = years)

  #write.xlsx(months, file=statsfile, sheetName="nt_months", append=TRUE, row.names=TRUE)
  addWorksheet(statsfile_OUT, "nt_months")
  writeData(statsfile_OUT, sheet = "nt_months", x = months)

  #write.xlsx(weeks, file=statsfile, sheetName="nt_weeks", append=TRUE, row.names=TRUE)
  addWorksheet(statsfile_OUT, "nt_weeks")
  writeData(statsfile_OUT, sheet = "nt_weeks", x = weeks)

  #Search time differences in days between consecutive samples greather than threshold in days (last parameter next function)
  thresholdindays = 30
  holesindays = locate_holes_time_serie(raw.data.nt, "speed.kph", raw.data.nt$date.time, thresholdindays)
  if (length(holesindays) == 0){
    holesindays = "No holes!"
  }
  #write.xlsx(holesindays, file=statsfile, sheetName=paste0("nt_gaps",thresholdindays,"days"), append=TRUE, row.names=FALSE)
  addWorksheet(statsfile_OUT, paste0("nt_gaps",thresholdindays,"days"))
  writeData(statsfile_OUT, sheet = paste0("nt_gaps",thresholdindays,"days"), x = holesindays)

  #Plot time serie
  print(plotxts(data=raw.data.nt, variable="speed.kph", time=raw.data.nt$date.time, cex.main=0.2, major.ticks="years",
                xlab=paste0("Page ",numberofplots, " - Time Series Plot for Non-Thunderstorm ('nt') - Station: ", number),
                main = paste0("Station ID: ",  number, "\nWind Velocity [Km/h]")))
  #assign(paste0("myprint", numberofplots), recordPlot())
  #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))
  numberofplots = numberofplots + 1
  
  #Apply yearly maxima method and print its graphics
  tipicalReturnPeriods = c(10,20,50,100,250,500,700,1000,1700,3000,7000)
  myextrRemesyearlymaxima = alexys_extRemes_yearlymaxima(x=raw.data.nt, RPs=tipicalReturnPeriods,
                                                         variable.description=paste0("Non-thunderstorm raw data for Station ID:", number))
  #write.xlsx(myextrRemesyearlymaxima$a, file=fnfitted, sheetName="nt_rawdata_RL_yearlymaxima_GEV", append=TRUE, row.names=TRUE)
  addWorksheet(fnfitted_OUT, "nt_rawdata_RL_yearlymaxima_GEV")
  writeData(fnfitted_OUT, sheet = "nt_rawdata_RL_yearlymaxima_GEV", x = myextrRemesyearlymaxima$a)
  
  #sheet nt_rawdata_RL_yearlymaxima_GEV (row 2)(row 1 are names)
  #function: alexys_extRemes_yearlymaxima
  #package: extRemes::fevd(ams, method="MLE", type="GEV") >>> extRemes::return.level
  #method: mle
  ymmlegev = myextrRemesyearlymaxima$a[1,1:11]
  nt_ym_mle_gev[i,10:20]=ymmlegev
  nt_ym_mle_gev[i,3]=myextrRemesyearlymaxima$a[1,13] # location
  nt_ym_mle_gev[i,4]=myextrRemesyearlymaxima$a[1,14] # scale
  nt_ym_mle_gev[i,5]=myextrRemesyearlymaxima$a[1,15] # #shape
  
  #sheet nt_rawdata_RL_yearlymaxima_GEV(row 3)(row 1 are names)
  #function: alexys_extRemes_yearlymaxima
  #package: fit_lmom <- extRemes::fevd(ams, method = "Lmoments", type="GEV") >>> extRemes::return.level
  #method: lmoments
  ymlmomgev = myextrRemesyearlymaxima$a[2,1:11]
  nt_ym_lmom_gev[i,10:20]=ymlmomgev
  nt_ym_lmom_gev[i,3]=myextrRemesyearlymaxima$a[2,13] # location
  nt_ym_lmom_gev[i,4]=myextrRemesyearlymaxima$a[2,14] # scale
  nt_ym_lmom_gev[i,5]=myextrRemesyearlymaxima$a[2,15] # #shape
  
  numberofplots = myextrRemesyearlymaxima$b
}
