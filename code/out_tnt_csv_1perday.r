#Write "nt" to csv, but changing to one data per day (the maximun)
if (length(imp.vals$t.series.dt) > 0) {
  #Write "t" to csv, but changing to one data per day (the maximun)
  t.data = data.frame(date=imp.vals$t.series.dt, t.series=imp.vals$t.series)
  t.data = as_tibble(t.data)
  library(xts)
  library(dplyr)
  select <- dplyr::select
  myxts = na.omit(xts(x=select(t.data, "t.series"), order.by = t.data$date))
  endp = endpoints(myxts,on="days")
  period = period.apply(myxts,INDEX=endp,FUN=max)
  #indexFormat(period) <- "%Y-%m-%d"
  period2 = data.frame(date=format(index(period),"%Y-%m-%d"), t.series=period$t.series, stringsAsFactors =FALSE)
  rownames(period2) = NULL
  #period2 = as.xts(period2$speed.kph, order.by=as.Date(period2$date,"%Y-%m-%d"))
  #colnames(period2) = c("speed.kph")
  #write.zoo(period2,sep=";",file=paste0(number, ".csv"))
#write.table(period2,file=paste0("t", number, ".csv"),sep=";", row.names=FALSE)
}

#Write "t" to csv, but changing to one data per day (the maximun)
if (length(imp.vals$nt.series.dt) > 0) {
  #Write "nt" to csv, but changing to one data per day (the maximun)
  nt.data = data.frame(date=imp.vals$nt.series.dt, nt.series=imp.vals$nt.series)
  nt.data = as_tibble(nt.data)
  library(xts)
  library(dplyr)
  select <- dplyr::select
  myxts = na.omit(xts(x=select(nt.data, "nt.series"), order.by = nt.data$date))
  endp = endpoints(myxts,on="days")
  period = period.apply(myxts,INDEX=endp,FUN=max)
  #indexFormat(period) <- "%Y-%m-%d"
  period2 = data.frame(date=format(index(period),"%Y-%m-%d"), nt.series=period$nt.series, stringsAsFactors =FALSE)
  rownames(period2) = NULL
  #period2 = as.xts(period2$speed.kph, order.by=as.Date(period2$date,"%Y-%m-%d"))
  #colnames(period2) = c("speed.kph")
  #write.zoo(period2,sep=";",file=paste0(number, ".csv"))
  #write.table(period2,file=paste0("nt", number, ".csv"),sep=";", row.names=FALSE)
}
