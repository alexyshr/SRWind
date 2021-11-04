#Ojo que esto está con ncdf4, pero pot_pp_ERA5_isla.r si está con stars!
#Ya le cambié el orden al for para que se fuera primero por filas (izquierda a derecha y bajar)
Sys.setenv(TZ='UTC')
source('./code/function_lib.R')

## script parameters
## ##################################
t.run <- 6/24         #Time in days to assume different thunderstorms
nt.run <- 4           #Time in days to assume different non thunderstorms
t.length <- 1/24      #Length of time in days of a single thunderstorm
min.n.per.year <- 4   #Minimun observations per year when looking for threshold
max.n.per.year <- 15  #Maximun observation per year when looking for threshold
remove.gap <- 180     #Removing time gaps in days (6 months)

#Read ERA5 
library(ncdf4)
(ncname <- "outfile_nc4c_zip9")
outputpathnetcdf = "../../data_srwind/"

(era5filename <- paste(outputpathnetcdf, ncname, ".nc", sep = ""))
ncin <- nc_open(era5filename)
print (ncin)
lon <- ncvar_get(ncin, "longitude")
lon
nlon = dim(lon)
nlon
lat <- ncvar_get(ncin, "latitude")
lat
nlat = dim(lat)
nlat
ntime <-  dim(ncvar_get(ncin, "time"))
variablename <- "fg10"
variablenumber <- 1 #the position of the variable inside netcdf file 
#(usefull for stars proxy which does not have the names in the object!)

fg10.units <- ncatt_get(ncin, variablename, "units")
fg10.units
lonlat.unstack <- expand.grid(lon=as.numeric(lon), lat=as.numeric(lat))
# Get datetime object from netCdf file 
#
t.units <- ncatt_get(ncin, "time", "units")
t.units
time.array <-  ncvar_get(ncin, "time")
nt = dim(time.array)
nt
head(time.array, 1)
tail(time.array, 1)
tustr <- strsplit(t.units$value, " ")
anio_mes_dia = unlist(tustr)[3]
anio_mes_dia
library(lubridate)
timestamp = as_datetime(c(time.array*60*60),origin=anio_mes_dia)
timestamp_string <- as.character(timestamp)


#estaciones <- read.delim("./alexys/01 estaciones.txt", header = FALSE, sep = "\t")
selected_era5_stations = read.delim("./selected_era5_stations.txt", header = FALSE, sep = "\t")
estaciones = data.frame(seq(1:length(lonlat.unstack$lon)))

outputpath="./ERA5/"
#outputpath="D:/jd/01 C?digos de programaci?n/02 Ajuste con R/alexys/"

#zzz=matrix(data=NA,length(estaciones[,1]),86)
zzz=matrix(data=NA,length(selected_era5_stations[,1]),86)


colnames(zzz) <- c("id", "t_thresh", "t_mu_location", "t_psi_scale",
                         "nt_thresh", "nt_mu_location", "nt_psi_scale", "distance_w", "station",
                   "t_10_poissonprocessintfunc", "t_20_poissonprocessintfunc", "t_50_poissonprocessintfunc", "t_100_poissonprocessintfunc", "t_250_poissonprocessintfunc", "t_500_poissonprocessintfunc", "t_700_poissonprocessintfunc", "t_1000_poissonprocessintfunc", "t_1700_poissonprocessintfunc", "t_3000_poissonprocessintfunc", "t_7000_poissonprocessintfunc",
                   "t_10_gumbeltailintfunc", "t_20_gumbeltailintfunc", "t_50_gumbeltailintfunc", "t_100_gumbeltailintfunc", "t_250_gumbeltailintfunc", "t_500_gumbeltailintfunc", "t_700_gumbeltailintfunc", "t_1000_gumbeltailintfunc", "t_1700_gumbeltailintfunc", "t_3000_gumbeltailintfunc", "t_7000_gumbeltailintfunc",
                   "t_10_gumbelquantilefunc", "t_20_gumbelquantilefunc", "t_50_gumbelquantilefunc", "t_100_gumbelquantilefunc", "t_250_gumbelquantilefunc", "t_500_gumbelquantilefunc", "t_700_gumbelquantilefunc", "t_1000_gumbelquantilefunc", "t_1700_gumbelquantilefunc", "t_3000_gumbelquantilefunc", "t_7000_gumbelquantilefunc",
                   "nt_10_poissonprocessintfunc", "nt_20_poissonprocessintfunc", "nt_50_poissonprocessintfunc", "nt_100_poissonprocessintfunc", "nt_250_poissonprocessintfunc", "nt_500_poissonprocessintfunc", "nt_700_poissonprocessintfunc", "nt_1000_poissonprocessintfunc", "nt_1700_poissonprocessintfunc", "nt_3000_poissonprocessintfunc", "nt_7000_poissonprocessintfunc",
                   "nt_10_gumbeltailintfunc", "nt_20_gumbeltailintfunc", "nt_50_gumbeltailintfunc", "nt_100_gumbeltailintfunc", "nt_250_gumbeltailintfunc", "nt_500_gumbeltailintfunc", "nt_700_gumbeltailintfunc", "nt_1000_gumbeltailintfunc", "nt_1700_gumbeltailintfunc", "nt_3000_gumbeltailintfunc", "nt_7000_gumbeltailintfunc",
                   "nt_10_gumbelquantilefunc", "nt_20_gumbelquantilefunc", "nt_50_gumbelquantilefunc", "nt_100_gumbelquantilefunc", "nt_250_gumbelquantilefunc", "nt_500_gumbelquantilefunc", "nt_700_gumbelquantilefunc", "nt_1000_gumbelquantilefunc", "nt_1700_gumbelquantilefunc", "nt_3000_gumbelquantilefunc", "nt_7000_gumbelquantilefunc",
                   "tnt_10_poissonprocessintfunc", "tnt_20_poissonprocessintfunc", "tnt_50_poissonprocessintfunc", "tnt_100_poissonprocessintfunc", "tnt_250_poissonprocessintfunc", "500_poissonprocessintfunc", "tnt_700_poissonprocessintfunc", "tnt_1000_poissonprocessintfunc", "tnt_1700_poissonprocessintfunc", "tnt_3000_poissonprocessintfunc", "tnt_7000_poissonprocessintfunc")

fn <- paste0(outputpath, "fitted_model_result_PoissonProcessGumbelIntFunc.xlsx")
if (file.exists(fn)) 
  #Delete file if it exists
  file.remove(fn)

#for (zz in 1:length(estaciones[,1])) {
#for (zz in 1:length(estaciones[,1])) {
zz = 0
for (latindex in 1:nlat){
  for (lonindex in 1:nlon) {
    zz = zz + 1
    #number <- estaciones[zz,1]
    number = zz
    
    if (any(selected_era5_stations[,1] == number))
      print(number)
      #Check its existence
      fnfitted <- paste0(outputpath, "raw_data_station_", number, "_fitted", ".xlsx")
      if (file.exists(fnfitted)) 
        #Delete file if it exists
        file.remove(fnfitted)
      
      statsfile = paste(outputpath, "raw_data_station_", number, "_statistics", ".xlsx", sep="")
      if (file.exists(statsfile)) 
        #Delete file if it exists
        file.remove(statsfile)
      
      #number=800360
      ## ##################################
      
      #raw.data <- ReadWindFile(station.number=number, path = outputpath)
      raw.data <- ReadWindERA5Station(ncin = ncin, lonindex = lonindex,
                                     latindex = latindex, ntime = ntime, timestamp = timestamp)
      
      #raw.data <- ReadWindERA5ProxyStation(starsOrStarsProxy = ncin, attribute = variablenumber, 
      #                                     lonindex = lonindex, latindex = latindex, ntime = ntime)
      
      
      library(dplyr)
      raw.data.tibble = as_tibble(raw.data)
      
      #Important THIS IS UNIQUE FOR ERA5,
      #as ERA5 has hourly values the declustering will leave a point every year
      #So to avoit it we will filter ONE value a week, then proceed
      #Write raw.data to csv but only one data per day (the maximun)
      library(xts)
      #library(dplyr)
      select <- dplyr::select
      myxts = na.omit(xts(x=select(raw.data.tibble, "speed.kph"), order.by = raw.data.tibble$date.time))
      endweeks = endpoints(myxts,on="weeks")
      myxtsweeks = xts::period.apply(myxts, INDEX=endweeks, FUN=max)
      #period = period.apply(myxts,INDEX=endp,FUN=max)
      #indexFormat(period) <- "%Y-%m-%d"
      #period2 = data.frame(date=format(index(period),"%Y-%m-%d"), speed.kph=period$speed.kph, stringsAsFactors =FALSE)
      #myxtsweeks$thunder_flag = "nt"
      thunder_flag = rep("nt", times=length(myxtsweeks))
      mydataf = as.data.frame(myxtsweeks$speed.kph)
      rownames(mydataf) = NULL
      speed.kph = mydataf$speed.kph
      raw.data = list(date.time=index(myxtsweeks), speed.kph=speed.kph)
      raw.data$t.nt.flag = thunder_flag
      rownames(raw.data) = NULL
      raw.data.tibble = as_tibble(raw.data)
      #period2 = as.xts(period2$speed.kph, order.by=as.Date(period2$date,"%Y-%m-%d"))
      #colnames(period2) = c("speed.kph")
      #write.zoo(period2,sep=";",file=paste0(number, ".csv"))
      #write.table(period2,file=paste0(number, ".csv"),sep=";", row.names=FALSE)
      #head(period)
      #head(format(index(period),"%Y-%m-%d"))
      #names(period) = "max"
      
      #Save all graphics to PDF
      pdf(file= paste0(outputpath, paste("FittedModel",number,sep="_"),".pdf"),  paper="a4r", width = 0, height = 0)
      numberofplots = 1
      par(oma = c(2,0,0,0))
      
      #Raw Data (whole dataset) Statistics and Send to CSV 
      source('./code/stats_raw_data.r')
      
      
      library(dplyr)
      require(dplyr)
      select <- dplyr::select
      raw.data.tibble  %>%
        select(date.time, speed.kph, t.nt.flag) %>%
        filter(t.nt.flag == "nt") -> raw.data.nt
      
      #Non Thunderstorm - Create Raw Data Statistics and Send to CSV 
      source('./code/stats_raw_data_nt.r')
      
      
      require(dplyr)
      select <- dplyr::select
      raw.data.tibble  %>%
        select(date.time, speed.kph, t.nt.flag) %>%
        filter(t.nt.flag == "t") -> raw.data.t
      
      #Thunderstorm - Create Raw Data Statistics and Send to CSV
      source('./code/stats_raw_data_t.r')
      
      
      dt <- raw.data$date.time
      
      ws <- raw.data$speed.kph
      ws[raw.data$t.nt.flag == "t"] <- ws[raw.data$t.nt.flag == "t"]*(-1)
      
      imp.vals <- PrepareData(ws=ws, dt=dt,
                              t.thresh=0, nt.thresh=0,
                              remove.gap=remove.gap,
                              t.run=t.run, nt.run=nt.run,
                              t.length=t.length)
      total.time <- imp.vals$total.time
      
      t.nt.grid <- GenThresholds(ws=ws, dt=dt,
                                 total.time=total.time,
                                 t.run=t.run, nt.run=nt.run,
                                 t.length=t.length,
                                 min.n.per.year=min.n.per.year,
                                 max.n.per.year=max.n.per.year,
                                 remove.gap=remove.gap)
      
      n.thresholds <- dim(t.nt.grid)[1]
      
      ## calculate a summary statistics of model
      ## appropriateness for each threshold pair
      ## in the grid
      stats <- NULL
      for (j in 1:n.thresholds) {
      
        tmp.stat <- CompareStatGumbel(ws=ws, dt=dt,
                                      t.thresh=t.nt.grid[j, 1],
                                      nt.thresh=t.nt.grid[j, 2],
                                      remove.gap=remove.gap,
                                      t.run=t.run, nt.run=nt.run,
                                      t.length=t.length)
      
        stats <- c(stats, tmp.stat)
      }
      
      ## ################################################
      ## the best threshold pair is the one with the
      ## smallest summary statistic.
      min.stats <- min(stats)
      tmp <- stats == min.stats
      if (sum(tmp) > 1) {
      
        tmp <- t.nt.grid[tmp, ]
        value <- c(number, tmp[1, ], min.stats)
      } else {
      
        value <- c(number, t.nt.grid[tmp, ], min.stats)
      }
      ## #################################################
      
      t.thresh <- value[2]
      nt.thresh <- value[3]
      
      imp.vals <- PrepareData(ws=ws, dt=dt,
                              t.thresh=t.thresh, nt.thresh=nt.thresh,
                              remove.gap=remove.gap,
                              t.run=t.run, nt.run=nt.run,
                              t.length=t.length)
      
      my_str <- capture.output(str(imp.vals))
      write.xlsx(my_str, file=statsfile, sheetName="IMP.VALS", append=TRUE, row.names=TRUE)
      
      #Write "t" to csv, but changing to one data per day (the maximun)
      #Write "nt" to csv, but changing to one data per day (the maximun)
      #source('./code/write_t_nt_csv_one_data_per_day.r')
      source('./code/tnt_csv_1perday.r')
      
      
      
      t.pp.fit <- FindStartVals(N=length(imp.vals$t.series),
                                T=imp.vals$t.length.time,
                                thresh=t.thresh,
                                sum.y=sum(imp.vals$t.series))
      
      nt.pp.fit <- FindStartVals(N=length(imp.vals$nt.series),
                                 T=imp.vals$nt.length.time,
                                 thresh=nt.thresh,
                                 sum.y=sum(imp.vals$nt.series))
      t.theta <- c(t.pp.fit$mu, t.pp.fit$psi, 0)
      nt.theta <- c(nt.pp.fit$mu, nt.pp.fit$psi, 0)
      
      
      #Statistics and graphics for declustered non-thunderstorm
      #source('./code/statistics_and_graphics_declustered_nt.r')
      source('./code/stats_graphs_dnt.r')
      
      #Statistics and graphics for declustered thunderstorm
      #source('./code/statistics_and_graphics_declustered_t.r')
      source('./code/stats_graphs_dt.r')
      
      #bmp(filename = paste(paste("Wplot",number,sep=" "),".bmp"), width = 480, height = 480)
      z8=WPlot(t.series=imp.vals$t.series,
            nt.series=imp.vals$nt.series,
            t.thresh=t.thresh,
            nt.thresh=nt.thresh,
            t.theta=t.theta,
            nt.theta=nt.theta,
            t.n=length(imp.vals$t.series),
            nt.n=length(imp.vals$nt.series),
            tf.plot=FALSE,
            BW=FALSE,
            details=FALSE)
      #dev.off()
      z2=t.thresh
      z3=t.theta[1]
      z4=t.theta[2]
      z5=nt.thresh
      z6=nt.theta[1]
      z7=nt.theta[2]
      
      zzz[zz,1]=zz      #consecutive
      zzz[zz,2]=z2      #t_thresh
      zzz[zz,3]=z3      #t_mu = location
      zzz[zz,4]=z4      #t_psi = scale
      zzz[zz,5]=z5      #nt_thresh
      zzz[zz,6]=z6      #nt_mu = location
      zzz[zz,7]=z7      #nt_psi = scale
      zzz[zz,8]=z8      #max W for optimal thresholds
      zzz[zz,9]=number  #station number
      
      tipicalReturnPeriods = c(10,20,50,100,250,500,700,1000,1700,3000,7000)
      typicalExcedenceProbabilities = 1 /tipicalReturnPeriods
      yvels = 1:600 #Velocities from 1 to 600
      
      
      #Plot: Page 1: W-Statistics Plot for best threshold pair
      WPlot(t.series=imp.vals$t.series,
            nt.series=imp.vals$nt.series,
            t.thresh=t.thresh,
            nt.thresh=nt.thresh,
            t.theta=t.theta,
            nt.theta=nt.theta,
            t.n=length(imp.vals$t.series),
            nt.n=length(imp.vals$nt.series),
            tf.plot=TRUE,
            BW=FALSE,
            details=FALSE)
      mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
      numberofplots = numberofplots + 1
      
      #Plots for thunderstorm
      source('./code/plot_t.r')
      
      #Plots for non-thunderstorm
      source('./code/plot_nt.r')
      
      #Plots for non-thunderstorm and thunderstorm
      source('./code/plot_t_nt.r')
      dev.off()
  }
}
write.xlsx(zzz, file=fn, sheetName="pp_pintar", append=TRUE, row.names=FALSE, col.names=TRUE)
#write.table(zzz, file="fitted_model_result_PoissonProcessGumbelIntFunc.csv", sep=",", row.names=FALSE, col.names=TRUE)
#

#

#GPD is not comparable here?
#Density
#.x <- seq(-150, 200, length.out=1000)
#library(POT)
#df = POT::dgpd(.x, loc=z3, scale=z4, shape=0)
#plotDistr(.x, df, cdf=TRUE, xlab="Wind Velocity - Km/h", ylab="P(X=y)",
#          main=paste("GPD - Density Function \n", "Location=", round(z3,2), " Scale=", round(z4,2), " Shape=0"))

#Distribution
#.x <- seq(-200, 200, length.out=1000)
#Df = POT::pgpd(.x, loc=z3, scale=z4, shape=0)#, lower.tail=T)

#plotDistr(.x, Df, cdf=TRUE, xlab="Wind Velocity - Km/h", ylab="P(X<x)",
#          main=paste("GPD:  Distribution Function \n", "Location=", round(z3,2), " Scale=", round(z4,2), " Shape=0"))

#plot(x= paP, y=yvels, xlab="Annual Excedence Probabilities - Poisson", ylab="Velocities Km/h", main=paste("Hazard Curve - Station:", number, sep=" "), type="l", col="cadetblue3")
#plot(x= paG, y=yvels, xlab="Annual Excedence Probabilities - Gumbel", ylab="Velocities Km/h", main=paste("Hazard Curve - Station:", number, sep=" "), type="l", col="cadetblue3")
#

