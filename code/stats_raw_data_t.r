#If there are records in the raw data thunderstorm time series, 
# 1) Generate statistics by years, weeks, months, gaps, and save the results  
#      to the Excel file raw_data_station_ID_statistics.xlsx (variable 'statsfile'), 
#      sheets t_years, t_weeks, t_months, t_gaps respectively.
# 2) Generate the time series plot for thunderstorm raw data
# 3) Calculare return levels using yearly maxima with function alexys_extRemes_yearlymaxima
if (length(raw.data.t$date.time) > 0) {
  #Print raw.data histogram
  title= paste("Frequency Histogram of Raw Data Thunderstorm\n", 
               "Station: ", number, sep="")
  print(hist(raw.data.t$speed.kph, probability = FALSE, col="cadetblue3", main=title))
  mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
  numberofplots = numberofplots + 1
  
  years = generate_stats_time_serie(raw.data.t, "speed.kph", raw.data.t$date.time, "years")
  months = generate_stats_time_serie(raw.data.t, "speed.kph", raw.data.t$date.time, "months")
  weeks = generate_stats_time_serie(raw.data.t, "speed.kph", raw.data.t$date.time, "weeks")

  #write.xlsx(years, file=statsfile, sheetName="t_years", append=TRUE, row.names=TRUE)
  addWorksheet(statsfile_OUT, "t_years")
  writeData(statsfile_OUT, sheet = "t_years", x = years)

  #write.xlsx(months, file=statsfile, sheetName="t_months", append=TRUE, row.names=TRUE)
  addWorksheet(statsfile_OUT, "t_months")
  writeData(statsfile_OUT, sheet = "t_months", x = months)

  #write.xlsx(weeks, file=statsfile, sheetName="t_weeks", append=TRUE, row.names=TRUE)
  addWorksheet(statsfile_OUT, "t_weeks")
  writeData(statsfile_OUT, sheet = "t_weeks", x = weeks)

  #Search time differences in days between consecutive samples greather than threshold in days (last parameter next function)
  thresholdindays = 30
  holesindays = locate_holes_time_serie(raw.data.t, "speed.kph", raw.data.t$date.time, thresholdindays)
  if (length(holesindays) == 0){
    holesindays = "No holes!"
  }
  #write.xlsx(holesindays, file=statsfile, sheetName=paste0("t_gaps",thresholdindays,"days"), append=TRUE, row.names=FALSE)
  addWorksheet(statsfile_OUT, paste0("t_gaps",thresholdindays,"days"))
  writeData(statsfile_OUT, sheet = paste0("t_gaps",thresholdindays,"days"), x = holesindays)

  #Plot time serie
  print(plotxts(data=raw.data.t, variable="speed.kph", time=raw.data.t$date.time, cex.main=0.2, major.ticks="years",
                xlab=paste0("Page ",numberofplots," - Time Series Plot for Thunderstorm ('t') - Station: ", number),
                main = paste0("Station ID: ",  number, "\nWind Velocity [Km/h]")))
  #assign(paste0("myprint", numberofplots), recordPlot())
  #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))				
  numberofpots = numberofplots + 1
  
  #Apply yearly maxima method
  tipicalReturnPeriods = c(10,20,50,100,250,500,700,1000,1700,3000,7000)
  myextrRemesyearlymaxima = alexys_extRemes_yearlymaxima(x=raw.data.t, RPs=tipicalReturnPeriods,
                                                         variable.description="Thunderstorm raw data")
  #write.xlsx(myextrRemesyearlymaxima$a, file=fnfitted, sheetName="t_rawdata_RL_yearlymaxima", append=TRUE, row.names=TRUE)
  addWorksheet(fnfitted_OUT, "t_rawdata_RL_yearlymaxima")
  writeData(fnfitted_OUT, sheet = "t_rawdata_RL_yearlymaxima", x = myextrRemesyearlymaxima$a)

  numberofplots = myextrRemesyearlymaxima$b
}
