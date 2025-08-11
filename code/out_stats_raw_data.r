#If there are records in the raw data time series, 
# 1) Generate statistics by years, weeks, months, gaps, and save the results  
#      to the Excel file raw_data_station_ID_statistics.xlsx (variable 'statsfile'), 
#      sheets all_years, all_weeks, all_months, all_gaps respectively.
# 2) Generate the time series plot for raw data (thunderstorm and non-thunderstorm)
if (length(raw.data.tibble$date.time) > 0) {
  #Print raw.data histogram
  title= paste("Frequency Histogram of Raw Data\n", 
               "Station: ", number, sep="")
  print(hist(raw.data.tibble$speed.kph, probability = FALSE, col="cadetblue3", main=title))
  mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
  numberofplots = numberofplots + 1
  
  #Use the function 'generate_stats_time_serie' to create the statistics based on four parameters:
  # data: tibble data frame with time series
  # variable: column name of time series (for instance: "speed.kph")
  # time: column name with date time (POSIXct) column  (for instance: raw.data.tibble$date.time)
  # index: period for the statistics (for instance: "years", "months", "weeks")
  years = generate_stats_time_serie(raw.data.tibble, "speed.kph", raw.data.tibble$date.time, "years")
  months = generate_stats_time_serie(raw.data.tibble, "speed.kph", raw.data.tibble$date.time, "months")
  weeks = generate_stats_time_serie(raw.data.tibble, "speed.kph", raw.data.tibble$date.time, "weeks")

  #myxts = na.omit(xts(x=select(raw.data.tibble, "speed.kph"), order.by = raw.data.tibble$date.time))
  #Split dataset by year
  #xts5_yearly <- split(myxts,f="years") #Convert to [[]]
  #sc = lapply(xts5_yearly, cumsum) #Calcular suma cumulativa but still [[]]
  #do.call(rbind,  sc) #Unstack [[]] and leave all in one index []

  #statistics <- list()
  #statistics[[1]] = years
  #statistics[[2]] = months
  #statistics[[3]] = weeks
  #lapply(statistics, write, statsfile, append=TRUE)

  #library(xlsx)
  #Write to a different sheet name: all_years, all_months, all_weeks
  #write.xlsx(years, file=statsfile, sheetName="all_years", row.names=TRUE)
  addWorksheet(statsfile_OUT, "all_years")
  writeData(statsfile_OUT, sheet = "all_years", x = years)

  #write.xlsx(months, file=statsfile, sheetName="all_months", append=TRUE, row.names=TRUE)
  addWorksheet(statsfile_OUT, "all_months")
  writeData(statsfile_OUT, sheet = "all_months", x = months)

  #write.xlsx(weeks, file=statsfile, sheetName="all_weeks", append=TRUE, row.names=TRUE)
  addWorksheet(statsfile_OUT, "all_weeks")
  writeData(statsfile_OUT, sheet = "all_weeks", x = weeks)

  #Write gaps statistics 
  # search time differences in days between consecutive samples 
  # greather than threshold in days (last parameter next function)
  thresholdindays = 30
  holesindays = locate_holes_time_serie(raw.data.tibble, "speed.kph", raw.data.tibble$date.time, thresholdindays)
  if (length(holesindays) == 0){
    holesindays = "No holes!"
  }
  #write.xlsx(holesindays, file=statsfile, sheetName=paste0("all_gaps",thresholdindays,"days"), append=TRUE, row.names=FALSE)  
  addWorksheet(statsfile_OUT, paste0("all_gaps",thresholdindays,"days"))
  writeData(statsfile_OUT, sheet = paste0("all_gaps",thresholdindays,"days"), x = holesindays)

  #Plot time series
  #library(xts)
  #myxts = na.omit(xts(x=select(raw.data.tibble, "speed.kph"), order.by = raw.data.tibble$date.time))
  #par(oma = c(2,0,0,0))
  #main=paste0("Time Series Plot for Raw.Data\nStation: ", number, " - Wind Velocity [Km/h]")
  #print(plot.xts(myxts, main=main, major.ticks="year", format.labels = "%b-%d\n%Y",
  #         col="green", legend.loc = "top", cex.main=0.2))
  #mtext(side = 1, text = paste0("Page ",numberofplots, " - Time Series Plot for Raw.Data - Station: ", number), outer = TRUE)
  print(plotxts(data=raw.data.tibble, variable="speed.kph", time=raw.data.tibble$date.time,
                cex.main=0.2, major.ticks="years",
                xlab=paste0("Page ",numberofplots, " - Time Series Plot for Raw.Data - Station: ", number),
                main = paste0("Station ID: ",  number, "\nWind Velocity [Km/h]")))
  #assign(paste0("myprint", numberofplots), recordPlot())
  #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))
  
  #Increase the number of plots after printing in the PDF
  numberofplots = numberofplots + 1
  

}
