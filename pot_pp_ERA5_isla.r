Sys.setenv(TZ='UTC')
Sys.setenv(GDAL_MAX_BAND_COUNT=1000000)

# Load libraries
library(xts)
require(dplyr)
#library(xlsx)
library(openxlsx)#It is important to change all to openxlsx (avoid using xlsx)
library(stars)
library(lubridate)
#Load the functions library file
source('./code/function_lib.R')

## script parameters
## ##################################
t.run <- 6/24             #Time in days to assume different thunderstorms
nt.run <- 4               #Time in days to assume different non thunderstorms
t.length <- 1/24          #Length of time in days of a single thunderstorm
min.n.per.year <- 4       #Minimun observations per year when looking for threshold
max.n.per.year <- 15      #Maximun observation per year when looking for threshold
remove.gap <- 180         #Removing time gaps in days (6 months)
#inputpath="./raw_data/"   #Folder with time series text files
#inputpath= "D:/Rthesis/data/standardized/selectedisd/"
#Modify Alexys (NETCDF folder)
inputpath= "../../data_srwind/"
ncfiles = list.files(path=inputpath, pattern = "\\.nc$")
ncfiles = paste0(inputpath, ncfiles)
#mync1 = read_ncdf("../data/sanandres/isla_fg10_1979-1992.nc")
#mync2 = read_ncdf("../data/sanandres/isla_fg10_1993-2006.nc")
#mync3 = read_ncdf("../data/sanandres/isla_fg10_2007-2019.nc")

mync = read_stars(ncfiles, along = "time")#, proxy=TRUE)#, sub = "fg10")
variablename <- "fg10"
variablenumber <- 1 #the position of the variable inside netcdf file 
                    #(usefull for stars proxy which does not have the names in the object!)
names(mync) = variablename
#ncname <- "outfile_nc4c_zip9"

#Read NETCDF file
#ncfile = paste0(inputpath, ncname, ".nc", sep = "")
#lon <- read_ncdf(ncfile, var = c("longitude"))
lon <- seq(from=attr(mync, "dimensions")$x$offset, 
           to=attr(mync, "dimensions")$x$offset + (attr(mync, "dimensions")$x$delta * (attr(mync, "dimensions")$x$to-1)),
           by = attr(mync, "dimensions")$x$delta) #Note that in to is minus 1
#nlon = dim(lon)
nlon = dim(mync)[1]
#lat <- read_ncdf(ncfile, var = c("latitude"))
lat <- seq(from=attr(mync, "dimensions")$y$offset, 
           to=attr(mync, "dimensions")$y$offset + (attr(mync, "dimensions")$y$delta * (attr(mync, "dimensions")$y$to-1)),
           by = attr(mync, "dimensions")$y$delta) #Note that in to is minus 1

#nlat = dim(lat)
nlat = dim(mync)[2]
#ntime <-  dim(read_ncdf(ncfile, var = c("time")))
ntime = dim(mync)[3]

#lonlat.unstack <- expand.grid(lon=as.numeric(lon$longitude), lat=as.numeric(lat$latitude))
lonlat.unstack <- expand.grid(lon=lon, lat=lat)

#Get timestamp string from NetCDF file,
#and cell index, and center latitude and longitude from ERA5 cells
#time.array = read_ncdf(ncfile, var = c("time"))$time
#t.units <- as.character(units(time.array)$numerator)
#time.array = as.matrix(time.array)
#nt = dim(time.array)
#tustr <- strsplit(t.units, " ")
#Our data is in Gregorian Calendar: "hours"      "since"      "1900-01-01" "00:00:00.0"
#anio_mes_dia = unlist(tustr)[3]
#Note that the dates are converted first to seconds!
#timestamp = as_datetime(c(time.array*60*60),origin=anio_mes_dia, tz="UTC")

#This is not working for file outfile_nc4c_zip9.nc
if (is.null(attr(mync, "dimensions")$time$values))
  start_time = strptime(attr(mync, "dimensions")$time$offset, "%Y-%m-%d %H:%M:%S")
  timestamp = seq(start_time, by="hour", length.out=ntime)
  attr(mync, "dimensions")$time$values = timestamp
  
timestamp = attr(mync, "dimensions")$time$values
timestamp_string <- as.character(timestamp)

#Read the text file with list of stations to process
#stations <- read.delim(paste0(inputpath, "stations.txt"), header = FALSE, sep = "\t")
#estaciones = data.frame(seq(1:length(lonlat.unstack$lon)))
stations = data.frame(1:(nlon*nlat))

#Folder to store output files (PDF and XLSX)
#outputpath = "./isd2020-3/"
#outputpath = "./ideam2020/"
outputpath="./ERA5/"

#zzz = matrix to store intensity function parameters and return values using different methods
#Records: Each record stores the result for one station. The number of records depends on the number of stations in variable 'stations'
#Columns:

# 1: id. D:\Rthesis\data\standardized\selectedisdRow consecutive identifier
# 2: t_thresh. Thunderstorm threshold
# 3: t_mu_location
# 4: t_psi_scale
# 5: t_average_events_per_year (look inside imp.vals)
# 6: t_average_time_per_year(At). For thunderstorm, parameter At for calculation of return values. Same As of equation between equation 3 and 4 (page 16) NIST report 500-301
# 7: t_gamma_Pot-Poisson-GPD. For thunderstorm, main parameter (sometimes called lamda in thesis called gamma) of Poisson 1D for time dimension calculated using Pot-Poisson-GPD. 
#                             The meaning is equal to column 5 t_average_events_per_year
#                             Not used XXX as is equal to column 5
# 8: nt_thresh
# 9: nt_mu_location
# 10: nt_psi_scale
# 11: nt_average_events_per_year
# 12: nt_average_time_per_year(Ant). For non-thunderstorm, parameter Ant for calculation of return values. Same Ans of equation between equation 3 and 4 (page 16) NIST report 500-301
# 13: nt_gamma_Pot-Poisson-GPD. For non-thunderstorm, main parameter (sometimes called lamda in thesis called gamma) of Poisson 1D for time dimension calculated using Pot-Poisson-GPD. 
#                             The meaning is equal to column 11 nt_average_events_per_year
# 14: distance_w
# 15: station ID
#
# 16-59 ------> Considering the dataset only composed by THUNDERSTORM
#  16-26: t_MRI_poissonprocessintfunc. Thunderstorm return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), using Poisson Process Intensity Function (equation page 28, NIST Report 500-301), and 
#  27-37: t_MRI_gumbeltailintfunc. Thunderstorm return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), using PP Gumbel Tail Intensity Function (equation 10 page 32, NIST Report 500-301)
#  38-48: t_MRI_gumbelquantilefunc. qgumbel((1-(1/mri)). Thunderstorm return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), using PP Gumbel Quantile Function
#  49-59: t_MRI_POT-Poisson-GPD. Thunderstorm return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), 
#                                         using POT-Poisson-GPD. This calculation is pending XXX
#
# 60-103 ------> Considering the dataset only composed by NON-THUNDERSTORM
#  60-70: nt_MRI_poissonprocessintfunc. Non-thunderstorm return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), using Poisson Process Intensity Function (equation page 28, NIST Report 500-301)
#  71-81: nt_MRI_gumbeltailintfunc. Non-thunderstorm return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), using PP Gumbel Tail Intensity Function (equation 10 page 32, NIST Report 500-301)
#  82-92: nt_MRI_gumbelquantilefunc. Non-thunderstorm return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), using PP Gumbel Quantile Function
#  93-103: nt_MRI_POT-Poisson-GPD. Non-thunderstorm return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), 
#                                         using POT-Poisson-GPD. This calculation is pending XXX
# 104-114: ------> Considering the dataset composed simultaneously by THUNDERSTORM and NON-THUNDERSTORM
# 104-114: tnt_MRI_poissonprocessintfunc. Combined (t and nt) return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), using Poisson Process Intensity Function
#                                               Solution to equation between equation 3 and 4 (page 16) NIST report 500-301, where
#                                               As equal to column 6 (t_average_time_per_year(At))
#                                               Ans equal to column 12 (nt_average_time_per_year(Ant))

selected_era5_stations = read.delim("./selected_era5_stations.txt", header = FALSE, sep = "\t")

#zzz=matrix(data=NA,length(stations[,1]),136)#114)
zzz=matrix(data=NA,length(selected_era5_stations[,1]),136)


#Column names of zzz will be the column names of the output Excel file with return values: fitted_model_result.xlsx
colnames(zzz) <- c("id", "t_thresh", "t_mu_location", "t_psi_scale",
                         "t_average_events_per_year", "t_average_time_per_year(At)", "t_at",
                         "nt_thresh", "nt_mu_location", "nt_psi_scale", 
                         "nt_average_events_per_year", "nt_average_time_per_year(Ant)", "nt_ant",
                         "distance_w", "station",
                   "t_10_poissonprocessintfunc", "t_20_poissonprocessintfunc", "t_50_poissonprocessintfunc", "t_100_poissonprocessintfunc", "t_250_poissonprocessintfunc", "t_500_poissonprocessintfunc", "t_700_poissonprocessintfunc", "t_1000_poissonprocessintfunc", "t_1700_poissonprocessintfunc", "t_3000_poissonprocessintfunc", "t_7000_poissonprocessintfunc",
                   "t_10_gumbeltailintfunc", "t_20_gumbeltailintfunc", "t_50_gumbeltailintfunc", "t_100_gumbeltailintfunc", "t_250_gumbeltailintfunc", "t_500_gumbeltailintfunc", "t_700_gumbeltailintfunc", "t_1000_gumbeltailintfunc", "t_1700_gumbeltailintfunc", "t_3000_gumbeltailintfunc", "t_7000_gumbeltailintfunc",
                   "t_10_gumbelquantilefunc", "t_20_gumbelquantilefunc", "t_50_gumbelquantilefunc", "t_100_gumbelquantilefunc", "t_250_gumbelquantilefunc", "t_500_gumbelquantilefunc", "t_700_gumbelquantilefunc", "t_1000_gumbelquantilefunc", "t_1700_gumbelquantilefunc", "t_3000_gumbelquantilefunc", "t_7000_gumbelquantilefunc",
                   "t_10_POT-Poisson-GPD", "t_20_POT-Poisson-GPD", "t_50_POT-Poisson-GPD", "t_100_POT-Poisson-GPD", "t_250_POT-Poisson-GPD", "t_500_POT-Poisson-GPD", "t_700_POT-Poisson-GPD", "t_1000_POT-Poisson-GPD", "t_1700_POT-Poisson-GPD", "t_3000_POT-Poisson-GPD", "t_7000_POT-Poisson-GPD",
                   "nt_10_poissonprocessintfunc", "nt_20_poissonprocessintfunc", "nt_50_poissonprocessintfunc", "nt_100_poissonprocessintfunc", "nt_250_poissonprocessintfunc", "nt_500_poissonprocessintfunc", "nt_700_poissonprocessintfunc", "nt_1000_poissonprocessintfunc", "nt_1700_poissonprocessintfunc", "nt_3000_poissonprocessintfunc", "nt_7000_poissonprocessintfunc",
                   "nt_10_gumbeltailintfunc", "nt_20_gumbeltailintfunc", "nt_50_gumbeltailintfunc", "nt_100_gumbeltailintfunc", "nt_250_gumbeltailintfunc", "nt_500_gumbeltailintfunc", "nt_700_gumbeltailintfunc", "nt_1000_gumbeltailintfunc", "nt_1700_gumbeltailintfunc", "nt_3000_gumbeltailintfunc", "nt_7000_gumbeltailintfunc",
                   "nt_10_gumbelquantilefunc", "nt_20_gumbelquantilefunc", "nt_50_gumbelquantilefunc", "nt_100_gumbelquantilefunc", "nt_250_gumbelquantilefunc", "nt_500_gumbelquantilefunc", "nt_700_gumbelquantilefunc", "nt_1000_gumbelquantilefunc", "nt_1700_gumbelquantilefunc", "nt_3000_gumbelquantilefunc", "nt_7000_gumbelquantilefunc",
                   "nt_10_POT-Poisson-GPD", "nt_20_POT-Poisson-GPD", "nt_50_POT-Poisson-GPD", "nt_100_POT-Poisson-GPD", "nt_250_POT-Poisson-GPD", "nt_500_POT-Poisson-GPD", "nt_700_POT-Poisson-GPD", "nt_1000_POT-Poisson-GPD", "nt_1700_POT-Poisson-GPD", "nt_3000_POT-Poisson-GPD", "nt_7000_POT-Poisson-GPD",
                   "tnt_10_poissonprocessintfunc", "tnt_20_poissonprocessintfunc", "tnt_50_poissonprocessintfunc", "tnt_100_poissonprocessintfunc", "tnt_250_poissonprocessintfunc", "500_poissonprocessintfunc", "tnt_700_poissonprocessintfunc", "tnt_1000_poissonprocessintfunc", "tnt_1700_poissonprocessintfunc", "tnt_3000_poissonprocessintfunc", "tnt_7000_poissonprocessintfunc",
                   "t_perc_10", "t_perc_20", "t_perc_50", "t_perc_100", "t_perc_250", "t_perc_500", "t_perc_700", "t_perc_1000", "t_perc_1700", "t_perc_3000", "t_perc_7000",
                   "nt_perc_10", "nt_perc_20", "nt_perc_50", "nt_perc_100", "nt_perc_250", "nt_perc_500", "nt_perc_700", "nt_perc_1000", "nt_perc_1700", "nt_perc_3000", "nt_perc_7000")

#Output excel file with return values for all stations
#This file will have the final content of matrix zzz
fn <- paste0(outputpath, "fitted_model_result.xlsx")

#Delete file if it exists
if (file.exists(fn)) 
  file.remove(fn)

fn_OUT <- createWorkbook()#Create a blank Workbook!
#addWorksheet(fn_OUT, "IMP.VALS")
#writeData(fn_OUT, sheet = "IMP.VALS", x = my_str)
#saveWorkbook(statsfile_OUT, statsfile)

#Next loop will run the POT-PP for all the stations in variable 'stations'
#in each loop cycle a station
zz=0
#latindex = 8
#lonindex = 3
for (latindex in 1:nlat){
  for (lonindex in 1:nlon) {
#for (latindex in 8:nlat){
#for (latindex in 8:8){
#for (lonindex in 1:nlon) {
#for (lonindex in 3:3) {
    #browser()    
    zz = zz + 1
    #Read the station ID from 'stations' variable
    #number <- stations[zz,1]
    number = zz
    
    if (any(selected_era5_stations[,1] == number))
      print(number)
      #browser()
      #Check the existence of the POT-PP (+ others) output parameters Excel file for the current station
      fnfitted <- paste0(outputpath, "raw_data_station_", number, "_fitted", ".xlsx")
      if (file.exists(fnfitted)) 
        #Delete file if it exists
        file.remove(fnfitted)
    
      #Create the workbook
      fnfitted_OUT <- createWorkbook()#Create a blank Workbook!
      
      #Function ReadWindFile reads the time series text file raw_data_station_ID.txt (date_time kph thunder_flag)
      # into variable raw.data
      #raw.data <- ReadWindFile(station.number=number, path = inputpath)
      #raw.data <- ReadWindISDStation(ncin = ncin, lonindex = lonindex,
      #                               latindex = latindex, ntime = ntime, timestamp = timestamp)
      
      raw.data <- ReadWindERA5ProxyStation(starsOrStarsProxy = mync, attribute = variablenumber, 
                                           lonindex = lonindex, latindex = latindex, ntime = ntime)
      
      #filter velocities
      #Alexys
      #raw.data = lapply(raw.data, function(x) x[raw.data$speed.kph<100])
      #browser()
      raw.data.tibble = as_tibble(raw.data)
      
      #raw.data.tibble = filter(raw.data.tibble, speed.kph <130)
      
      
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
      
      
      #Save all graphics to PDF FittedModel_ID.pdf
      # one PDF file for each station                                        
      pdf(file= paste0(outputpath, paste("FittedModel",number,sep="_"),".pdf"),  paper="a4r", width = 0, height = 0)
    
      #numberofplots will count the number of plots inside the PDF file
      numberofplots = 1
      par(oma = c(2,0,0,0))
    
      #The file raw_data_station_ID_statistics.xlsx (variable 'statsfile')
      # will store statistics (years, months, weeks, gaps) for:
      # - raw data (thunderstorm and non-thunderstorm): sheets all_years, all_weeks, all_months, all_gaps
      # - non-thunderstorm raw data: sheets nt_years, nt_weeks, nt_months, nt_gaps
      # - thunderstorm raw data: t_years, t_weeks, t_months, t_gaps
      statsfile = paste(outputpath, "raw_data_station_", number, "_statistics", ".xlsx", sep="")
      if (file.exists(statsfile)) 
        #Delete file if it exists
        file.remove(statsfile)
      
      statsfile_OUT <- createWorkbook()#Create a blank Workbook!
      
      #In file 'statsfile' create the stat sheets for raw data (thunderstorm and non-thunderstorm): 
      # all_years, all_weeks, all_months, all_gaps  
      # and create the raw data time series graphic
      source('./code/stats_raw_data.r')
    
      #create the raw.data.nt tibble data frame
      select <- dplyr::select
      raw.data.tibble  %>%
        select(date.time, speed.kph, t.nt.flag) %>%
        filter(t.nt.flag == "nt") -> raw.data.nt
    
      #In file 'statsfile' create the stat sheets for non-thunderstorm raw data:
      # nt_years, nt_weeks, nt_months, nt_gaps  
      # and create the non-thunderstorm raw data time series graphic
      # and calculate yearly maxima return levels
      source('./code/stats_raw_data_nt.r')
    
      #create the raw.data.t tibble data frame
      require(dplyr)
      select <- dplyr::select
      raw.data.tibble  %>%
        select(date.time, speed.kph, t.nt.flag) %>%
        filter(t.nt.flag == "t") -> raw.data.t
    
      #In file 'statsfile' create the stat sheets for thunderstorm raw data:
      # t_years, t_weeks, t_months, t_gaps  
      # and create the thunderstorm raw data time series graphic
      # and calculate yearly maxima return levels
      source('./code/stats_raw_data_t.r')
    
          #Pintar R Code (Declustering & Thresholding)
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
      
      #In file 'statsfile' create the IMP.VALS sheet
      # which is an array with nine variables - List of 9
      #  $ t.series: thunderstorm time series
      #  $ nt.series: non-thunderstorm time series
      #  $ t.series.dt: date-time object of t.series
      #  $ nt.series.dt: date-time object of nt.series
      #  $ t.length.time: total number of days of t.series
      #  $ nt.length.time: total number of days of nt.series
      #  $ total.time: total number of days of raw data (t + nt)
      #  $ n.thunders.per.year: average number of thunderstorm per year
      #  $ n.nthunders.per.year: average number of non-thunderstorm per year
      my_str <- capture.output(str(imp.vals))
      
      #write.xlsx(my_str, file=statsfile, sheetName="IMP.VALS", append=TRUE, row.names=TRUE)
      #Using openxlsx
      addWorksheet(statsfile_OUT, "IMP.VALS")
      writeData(statsfile_OUT, sheet = "IMP.VALS", x = my_str)
      
      #Write "nt" to csv, but changing to one data per day (the maximun)
      #Write "t" to csv, but changing to one data per day (the maximun)
      source('./code/tnt_csv_1perday.r')
    
          #Pintar R Code (Declustering & Thresholding)
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
    
    
      #For de-clustered non-thunderstorm: Gumbel Fittings, GPD-Poisson-GPD extremeStat results and statistics (years, weeks, months, gaps)
      # 1) Write (raw_data_station_id_fitted.xlsx, variable fnfitted) "Gumbel fittings and GPD-Poisson-GPD extremeStat results"
      #    to sheets names:
      #  - t_evd-fgev_fGumbel: fitting Gumbel using evd::fgev
      #  - t_bbmle-mle2: fitting Gumbel using bbmle::mle2
      #  - t_nll-optim: fitting Gumbel using negative likelihood and stats::optim
      #  - t_fitdistrplus-fitdist: fitting Gumbel using fitdistrplus::fitdist
      #  - t_extRemes: calculation of return values POT-Poisson-GPD, using extRemes::fevd
      #  - t_distLquantile_quant: calculation of return values and RMSE (POT-Poisson-GPD and EVDs), using extremeStat::distLquantile
      #  - t_distLquantile_parameters: calculation of fitting parameters POT-Poisson-GPD and EVD, using extremeStat::distLquantile
      #  - t_distLextreme_returnlev: calculation of return values POT-Poisson-GPD and EVD, using extremeStat::distLextreme
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
      source('./code/stats_graphs_dnt.r')
    
      #For de-clustered thunderstorm: Gumbel Fittings, GPD-Poisson-GPD extremeStat results and statistics (years, weeks, months, gaps)
      # 1) Write (raw_data_station_id_fitted.xlsx, variable fnfitted) "Gumbel fittings and GPD-Poisson-GPD extremeStat results"
      #    to sheets names:
      #  - t_evd-fgev_fGumbel: fitting Gumbel using evd::fgev
      #  - t_bbmle-mle2: fitting Gumbel using bbmle::mle2
      #  - t_nll-optim: fitting Gumbel using negative likelihood and stats::optim
      #  - t_fitdistrplus-fitdist: fitting Gumbel using fitdistrplus::fitdist
      #  - t_extRemes: calculation of return values POT-Poisson-GPD, using extRemes::fevd
      #  - t_distLquantile_quant: calculation of return values and RMSE (POT-Poisson-GPD and EVDs), using extremeStat::distLquantile
      #  - t_distLquantile_parameters: calculation of fitting parameters POT-Poisson-GPD and EVD, using extremeStat::distLquantile
      #  - t_distLextreme_returnlev: calculation of return values POT-Poisson-GPD and EVD, using extremeStat::distLextreme
      #  - t_distLextreme_parameter: calculation of fitting parameters POT-Poisson-GPD and EVD, using extremeStat::distLextreme
      # 2) Send to PDF (FittedModel_ID.pdf) graphics for declustered thunderstorm (Gumbel fittings and GPD-Poisson-GPD extremeStat results)
      #  - Data Histogram and Fitted Gumbel Probability Density Curve - Log-Likelihood(Gumbel) - Optim (nll-optim)
      #  - Declustered - Thunderstorm - fitdistrplus-fitdist(gumbel)
      # 3) In file raw_data_station_ID_statistics.xlsx (variable 'statsfile') create the statistics sheets for thunderstorm:
      #  - declu_t_years
      #  - declu_t_weeks
      #  - declu_t_months
      #  - declu_t_gaps30days
      # 4) Send to PDF (FittedModel_ID.pdf) graphics for declustered thunderstorm:
      #  - Declustered Thunderstorm ('t') Time Series
      source('./code/stats_graphs_dt.r')
    
      #Get model parameters from Pintar Code
      #Save in z8: max W for optimal thresholds
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
    
      #Get model parameters from Pintar Code
      z2=t.thresh     #Thunderstorm threshold
      z3=t.theta[1]   #Thunderstorm location (t_mu)
      z4=t.theta[2]   #Thunderstorm scale (t_psi)
      z5=nt.thresh    #Non-thunderstorm threshold
      z6=nt.theta[1]  #Non-thunderstorm location (nt_mu)
      z7=nt.theta[2]  #Non-thunderstorm scale (nt_psi)
    
      zzz[zz,1]=zz       #consecutive
      zzz[zz,2]=z2       #t_thresh
      zzz[zz,3]=z3       #t_mu = location
      zzz[zz,4]=z4       #t_psi = scale
      zzz[zz,5]=imp.vals$n.thunders.per.year               #t_average_events_per_year
      #Alexys: change: this valune need to come from rl_plot_t.r or rl_plot_t_nt.r 
      zzz[zz,6]=imp.vals$n.thunders.per.year*(1/24)*(1/365)        #t_average_time_per_year(At): zzz[zz,5] * 1 hour (result in days) (Ok) >>>Same At!
      zzz[zz,7]=NA                                         #t_gamma_Pot-Poisson-GPD. The average number of events per year calculated using POT-Poisson-GPD. 
                                                           #Main parameter of Poisson 1D (lambda or gamma). Need to be calculated (this part is pending)
      zzz[zz,8]=z5       #nt_thresh
      zzz[zz,9]=z6       #nt_mu = location
      zzz[zz,10]=z7      #nt_psi = scale
      #Alexys: change: this valune need to come from rl_plot_t.r or rl_plot_t_nt.r 
      zzz[zz,11]=imp.vals$n.nthunders.per.year           #nt_average_events_per_year
      zzz[zz,12]=1-(zzz[zz,6])                         #nt_average_time_per_year(Ant) = 365-At
      zzz[zz,13]=NA       #nt_gamma_Pot-Poisson-GPD. The average number of events per year calculated using POT-Poisson-GPD. 
                          #Main parameter of Poisson 1D (lambda or gamma).Need to be calculated (this part is pending)
      zzz[zz,14]=z8      #max W for optimal thresholds
      zzz[zz,15]=number  #station number
    
      #List with typical return periods
      tipicalReturnPeriods = c(10,20,50,100,250,500,700,1000,1700,3000,7000)
      #List with exceedance probabilities for typical return periods
      typicalExcedenceProbabilities = 1 /tipicalReturnPeriods
      #Velocities from 1 to 600 km/h
      yvels = c(0.0000000001, seq(from=0.0001, to=0.0009, by=0.0001), seq(from=0.001, to=0.009, by=0.001), seq(from=0.01, to=0.09, by=0.01), seq(from=0.1, to=0.9, by=0.1), 1:600) 
    
          #Pintar R Code (Declustering & Thresholding)
          #bmp(filename = paste(paste("Wplot",number,sep=" "),".bmp"), width = 480, height = 480)
    
      
      #Send to PDF (FittedModel_ID.pdf) graphics the graphic: W-Statistics Plot for best threshold pair
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
      #assign(paste0("myprint", numberofplots), recordPlot())
      #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), #paste0(outputpath, "myprint", numberofplots, ".rds"))
      numberofplots = numberofplots + 1
    
      #Return levels and plots considering the whole dataset as:
      # ONLY thunderstorm
      source('./code/rl_plot_t.r')
    
      #Return levels and plots considering the whole dataset as:
      # ONLY non-thunderstorm
      source('./code/rl_plot_nt.r')
    
      #Return levels and plots considering the whole dataset as simultaneously made up of:
      # non-thunderstorm and thunderstorm
      source('./code/rl_plot_t_nt.r')
      
      #Save workbooks to disk: statsfile and fnfitted
      saveWorkbook(statsfile_OUT, statsfile)
      saveWorkbook(fnfitted_OUT, fnfitted)
      dev.off()
  }
}

#Save output excel file (fitted_model_result.xlsx - variable fn) with return values for all stations
#write.xlsx(zzz, file=fn, sheetName="pp_pintar", append=TRUE, row.names=FALSE, col.names=TRUE)
addWorksheet(fn_OUT, "pp_pintar")
writeData(fn_OUT, sheet = "pp_pintar", x = zzz)

# 16-59 ------> Considering the dataset only composed by THUNDERSTORM
#zzz[, c(1, 15, 16:26)] -> t_MRI_poissonprocessintfunc
addWorksheet(fn_OUT, "t_MRI_poissonprocessintfunc")
writeData(fn_OUT, sheet = "t_MRI_poissonprocessintfunc", x = zzz[, c(1, 15, 16:26)])

#zzz[, c(1, 15, 27:37)] -> t_MRI_gumbeltailintfunc
addWorksheet(fn_OUT, "t_MRI_gumbeltailintfunc")
writeData(fn_OUT, sheet = "t_MRI_gumbeltailintfunc", x = zzz[, c(1, 15, 27:37)])

#zzz[, c(1, 15, 38:48)] -> t_MRI_gumbelquantilefunc
addWorksheet(fn_OUT, "t_MRI_gumbelquantilefunc")
writeData(fn_OUT, sheet = "t_MRI_gumbelquantilefunc", x = zzz[, c(1, 15, 38:48)])

#zzz[, c(1, 15, 49:59)] -> t_MRI_POT-Poisson-GPD
addWorksheet(fn_OUT, "t_MRI_POT-Poisson-GPD")
writeData(fn_OUT, sheet = "t_MRI_POT-Poisson-GPD", x = zzz[, c(1, 15, 49:59)])

#60-103 ------> Considering the dataset only composed by NON-THUNDERSTORM
#zzz[, c(1, 15, 60:70)] -> nt_MRI_poissonprocessintfunc
addWorksheet(fn_OUT, "nt_MRI_poissonprocessintfunc")
writeData(fn_OUT, sheet = "nt_MRI_poissonprocessintfunc", x = zzz[, c(1, 15, 60:70)])

#zzz[, c(1, 15, 71:81)] -> nt_MRI_gumbeltailintfunc
addWorksheet(fn_OUT, "nt_MRI_gumbeltailintfunc")
writeData(fn_OUT, sheet = "nt_MRI_gumbeltailintfunc", x = zzz[, c(1, 15, 71:81)])

#zzz[, c(1, 15, 82:92)] -> nt_MRI_gumbelquantilefunc
addWorksheet(fn_OUT, "nt_MRI_gumbelquantilefunc")
writeData(fn_OUT, sheet = "nt_MRI_gumbelquantilefunc", x = zzz[, c(1, 15, 82:92)])

#zzz[, c(1, 15, 93:103)] -> nt_MRI_POT-Poisson-GPD
addWorksheet(fn_OUT, "nt_MRI_POT-Poisson-GPD")
writeData(fn_OUT, sheet = "nt_MRI_POT-Poisson-GPD", x = zzz[, c(1, 15, 93:103)])

# 104-114: ------> Considering the dataset composed simultaneously by THUNDERSTORM and NON-THUNDERSTORM
#zzz[, c(1, 15, 104:114)] -> tnt_MRI_poissonprocessintfunc
addWorksheet(fn_OUT, "tnt_MRI_poissonprocessintfunc")
writeData(fn_OUT, sheet = "tnt_MRI_poissonprocessintfunc", x = zzz[, c(1, 15, 104:114)])

# 115-136: ------> Test of Return Level Weights (Percentage) in the Combination of Hurricane and Non-Hurricane in POT-PP(equation 3, page 16, NIST.SP.500-301.pdf) : what percentage contributes hurricanes? and what percentage contributes non-hurricanes?
#zzz[, c(1, 15, 115:125)] -> t_percentage_pot_pp
addWorksheet(fn_OUT, "t_percentage_pot_pp")
writeData(fn_OUT, sheet = "t_percentage_pot_pp", x = zzz[, c(1, 15, 115:125)])

#zzz[, c(1, 15, 126:136)] -> nt_percentage_pot_pp
addWorksheet(fn_OUT, "nt_percentage_pot_pp")
writeData(fn_OUT, sheet = "nt_percentage_pot_pp", x = zzz[, c(1, 15, 126:136)])

#Save workbook to disk
saveWorkbook(fn_OUT, fn)



#write.table(zzz, file="fitted_model_result.csv", sep=",", row.names=FALSE, col.names=TRUE)
print(paste("Total number of samples: ", (length(raw.data$date.time))))
print(paste0("Maximum velocity in dataset: ", (max(raw.data$speed.kph))))
