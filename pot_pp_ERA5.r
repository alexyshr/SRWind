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
#ncfiles = list.files(path=inputpath, pattern = "\\.nc$")
#ncfiles = paste0(inputpath, ncfiles)
ncfile = "outfile_nc4c_zip9"
ncfile = paste0(inputpath, ncfile, ".nc")
#mync1 = read_ncdf("../data/sanandres/isla_fg10_1979-1992.nc")
#mync2 = read_ncdf("../data/sanandres/isla_fg10_1993-2006.nc")
#mync3 = read_ncdf("../data/sanandres/isla_fg10_2007-2019.nc")

#mync = read_stars(ncfiles, along = "time")#, proxy=TRUE)#, sub = "fg10")
mync = read_stars(ncfile, along = "time")#, proxy=TRUE)#, sub = "fg10")
#mync2 = read_ncdf(ncfile, along = "time")
#mysf2 = sf::st_as_sf(mync2[, , , 1])
#xy1 = sf::st_coordinates(sf::st_centroid(sf::st_geometry(mysf2)))
#colnames(xy1) = c("x", "y")
#head(xy1)

variablename <- "fg10"
variablenumber <- 1 #the position of the variable inside netcdf file 
                    #(usefull for stars proxy which does not have the names in the object!)
names(mync) = variablename

#Read NETCDF file with ncdf4
library(ncdf4)
ncname <- "outfile_nc4c_zip9"
ncfile = paste0(inputpath, ncname, ".nc", sep = "")
ncin <- nc_open(ncfile)
# lon <- ncvar_get(ncin, "longitude")
# lon
# nlon = dim(lon)
# nlon
# lat <- ncvar_get(ncin, "latitude")
# lat
# nlat = dim(lat)
# nlat
# ntime <-  dim(ncvar_get(ncin, "time"))
# fg10.units <- ncatt_get(ncin, variablename, "units")
# fg10.units
# lonlat.unstack <- expand.grid(lon=as.numeric(lon), lat=as.numeric(lat))
# # Get datetime object from netCdf file 
# #
# t.units <- ncatt_get(ncin, "time", "units")
# t.units
# time.array <-  ncvar_get(ncin, "time")
# nt = dim(time.array)
# nt
# head(time.array, 1)
# tail(time.array, 1)
# tustr <- strsplit(t.units$value, " ")
# anio_mes_dia = unlist(tustr)[3]
# anio_mes_dia
# library(lubridate)
# timestamp = as_datetime(c(time.array*60*60),origin=anio_mes_dia)
# timestamp_string <- as.character(timestamp)

# Stars Approach!
#lon <- read_ncdf(ncfile, var = c("longitude"))
lon <- seq(from=attr(mync, "dimensions")$x$offset + (attr(mync, "dimensions")$x$delta/2), 
           to=attr(mync, "dimensions")$x$offset + (attr(mync, "dimensions")$x$delta/2) + (attr(mync, "dimensions")$x$delta * (attr(mync, "dimensions")$x$to-1)),
           by = attr(mync, "dimensions")$x$delta) #Note that in to is minus 1
#nlon = dim(lon)
nlon = dim(mync)[1]
#lat <- read_ncdf(ncfile, var = c("latitude"))
lat <- seq(from=attr(mync, "dimensions")$y$offset + (attr(mync, "dimensions")$y$delta/2), 
           to=attr(mync, "dimensions")$y$offset + (attr(mync, "dimensions")$y$delta/2) + (attr(mync, "dimensions")$y$delta * (attr(mync, "dimensions")$y$to-1)),
           by = attr(mync, "dimensions")$y$delta) #Note that in to is minus 1

#nlat = dim(lat)
nlat = dim(mync)[2]
#ntime <-  dim(read_ncdf(ncfile, var = c("time")))
ntime = dim(mync)[3]

#lonlat.unstack <- expand.grid(lon=as.numeric(lon$longitude), lat=as.numeric(lat$latitude))
xy2 <- expand.grid(x=lon, y=lat)
xy2$cellindexlr = 1:(nlon*nlat)
head(xy2)
lonindex = 1:nlon
latindex = 1:nlat
indexes = expand.grid(lonindex=lonindex, latindex=latindex)
indexes$lonlatindex = paste0(indexes$lonindex, ".", indexes$latindex)
xy = cbind(xy2, indexes)

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
if (is.null(attr(mync, "dimensions")$time$values)){
  start_time = strptime(attr(mync, "dimensions")$time$offset, "%Y-%m-%d %H:%M:%S")
  timestamp = seq(start_time, by="hour", length.out=ntime)
  attr(mync, "dimensions")$time$values = timestamp
}

timestamp = attr(mync, "dimensions")$time$values
timestamp_string <- as.character(timestamp)

#Read the text file with list of stations to process
#stations <- read.delim(paste0(inputpath, "stations.txt"), header = FALSE, sep = "\t")
#estaciones = data.frame(seq(1:length(lonlat.unstack$lon)))
stations = data.frame(1:(nlon*nlat))

#Folder to store output files (PDF and XLSX)
#outputpath = "./isd2020-3/"
#outputpath = "./ideam2020/"
outputpath="../../ERA5/"

#Read Hurricane Uniandes Rasters (USED FOR THESIS & COLOMBIAN WIND MAPS)
h = read_hurricane_rasters(path= paste0(input_path, "/h_rasters/"),
            pattern = "*.*.tif$")
h_u_sf = h$h_sf #Uniandes (12 maps), low values (10, 25, 50, 100, 250, 500, 700, 1000, 1700, 2500, 5000, 10000)
h_u_st = h$h_st #Uniandes (12 maps), low values (10, 25, 50, 100, 250, 500, 700, 1000, 1700, 2500, 5000, 10000)
rm(h)

#Read Hurricane Ingeniar Rasters
h = read_hurricane_rasters(path= paste0(input_path, "/h_rasters/"),
                           pattern = "*.*.tif$")
h_i_sf = h$h_sf #Ingeniar (11 maps), high values (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000)
h_i_st = h$h_st #Ingeniar (11 maps), high values
rm(h)

#zzz = matrix to store intensity function parameters and return values using different methods
#Records: Each record stores the result for one station. The number of records depends on the number of stations in variable 'stations'
#Columns:
# 1:    "x", 
# 2:    "y",
# 3:    "cellindexlr", 
# 4:    "lonindex", 
# 5:    "latindex", 
# 6:    "lonlatindex", 
# 7(13):  nt_gamma_Pot-Poisson-GPD.  For non-thunderstorm, main parameter (sometimes called lamda in thesis called gamma) of Poisson 1D for time dimension calculated using Pot-Poisson-GPD. 
#                             The meaning is equal to column 9(11) nt_average_events_per_year
# 8(1):   "id", 
# 9(11):  "nt_average_events_per_year", #(look inside imp.vals)
# 10(12): "nt_average_time_per_year", For non-thunderstorm, parameter Ant for calculation of return values. 
#                                     Same Ans of equation between equation 3 and 4 (page 16) NIST report 500-301
# 11(14): "distance_w", 
# 12(15): "station",
# 13(8):  "nt_threshold", 
# 14(9):  "nt_mu_location", 
# 15(10): "nt_psi_scale", 
# 16:     "nt_shape"
# 17(7):  t_gamma_Pot-Poisson-GPD. For thunderstorm, main parameter (sometimes called lamda in thesis called gamma) of Poisson 1D for time dimension calculated using Pot-Poisson-GPD. 
#                             The meaning is equal to column 18(5) t_average_events_per_year
#                             Not used XXX as is equal to column 18(5)
# 18(5):  "t_average_events_per_year", #(look inside imp.vals)
# 19(6):  "t_average_time_per_year", #For thunderstorm, parameter At for calculation of return values. 
#                                 Same As of equation between equation 3 and 4 (page 16) NIST report 500-301
# 20(2):  "t_threshold", 
# 21(3):  "t_mu_location", 
# 22(4):  "t_psi_scale", 
# 23:     "t_shape"
#
# 24-67 (16-59) ------> Considering the dataset only composed by THUNDERSTORM
#  24-34(16-26): t_MRI_poissonprocessintfunc. Thunderstorm return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), using Poisson Process Intensity Function (equation page 28, NIST Report 500-301), and 
#  35-45(27-37): t_MRI_gumbeltailintfunc. Thunderstorm return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), using PP Gumbel Tail Intensity Function (equation 10 page 32, NIST Report 500-301)
#  46-56(38-48): t_MRI_gumbelquantilefunc. qgumbel((1-(1/mri)). Thunderstorm return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), using PP Gumbel Quantile Function
#  57-67(49-59): t_MRI_POT-Poisson-GPD. Thunderstorm return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), 
#                                         using POT-Poisson-GPD. This calculation is pending XXX
# 68-111(60-103) ------> Considering the dataset only composed by NON-THUNDERSTORM
#  68-78(60-70): nt_MRI_poissonprocessintfunc. Non-thunderstorm return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), using Poisson Process Intensity Function (equation page 28, NIST Report 500-301)
#  79-89(71-81): nt_MRI_gumbeltailintfunc. Non-thunderstorm return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), using PP Gumbel Tail Intensity Function (equation 10 page 32, NIST Report 500-301)
#  90-100(82-92): nt_MRI_gumbelquantilefunc. Non-thunderstorm return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), using PP Gumbel Quantile Function
#  101-111(93-103): nt_MRI_POT-Poisson-GPD. Non-thunderstorm return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), 
#                                         using POT-Poisson-GPD. This calculation is pending XXX
#
# 112-122(104-114): ------> Considering the dataset composed simultaneously by THUNDERSTORM and NON-THUNDERSTORM
# 112-122(104-114): tnt_MRI_poissonprocessintfunc. Combined (t and nt) return values for MRIs (10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), using Poisson Process Intensity Function
#                                               Solution to equation between equation 3 and 4 (page 16) NIST report 500-301, where
#                                               As equal to column 6 (t_average_time_per_year(At))
#                                               Ans equal to column 12 (nt_average_time_per_year(Ant))
# 123-150 ---> 28(12 hurricane uniandes, 11 hurricane ingeniear, 2 nsr-old, 3 msr-new)
#  - 123-134(1:12) uniandes #paste0("hu_", returnPeriodsUniandes)
#  - 135-145(13:23) ingeniear #paste0("hi_", tipicalReturnPeriods)
#  - 146-147(24:25) NSR-OLD # "MRIO_700", "MRIO_1700"
#  - 148-150(26:28) NSR-NEW # "MRIN_700", "MRIN_1700", "MRIN_3000"
# 151-172 ---> 22(combined Hurricane: 
#  151-161 ---> 11 uniandes paste0("cu_", tipicalReturnPeriods)
#  162-172 ---> 11 ingeniar paste0("ci_", tipicalReturnPeriods)

tipicalReturnPeriods = c(10,20,50,100,250,500,700,1000,1700,3000,7000) #Same as Ingeniar Hurricane (thesis and NSR based on Uniandes Hurricane)
returnPeriodsUniandes = c(10,25,50,100,250,500,700,1000,1700,2500,5000,10000) 

zzz=matrix(data=NA,length(selected_era5_stations[,1]),172)


#Column names of zzz will be the column names of the output Excel file with return values: fitted_model_result.xlsx

colnames(zzz) <- c("x", "y", "cellindexlr", "lonindex", "latindex", "lonlatindex", "nt_gamma_Pot-Poisson-GPD",
                   "id", "nt_average_events_per_year", "nt_average_time_per_year", "distance_w", "station",
                   "nt_threshold", "nt_mu_location", "nt_psi_scale", "nt_shape",
                   "t_gamma_Pot-Poisson-GPD", "t_average_events_per_year", "t_average_time_per_year",
                   "t_threshold", "t_mu_location", "t_psi_scale", "t_shape",
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
                   "nt_perc_10", "nt_perc_20", "nt_perc_50", "nt_perc_100", "nt_perc_250", "nt_perc_500", "nt_perc_700", "nt_perc_1000", "nt_perc_1700", "nt_perc_3000", "nt_perc_7000",
                   paste0("hu_", returnPeriodsUniandes),
                   paste0("hi_", tipicalReturnPeriods),
                   "MRIO_700", "MRIO_1700", "MRIN_700", "MRIN_1700", "MRIN_3000",
                   paste0("cu_", tipicalReturnPeriods),
                   paste0("ci_", tipicalReturnPeriods))


selected_era5_stations = read.delim("./selected_era5_stations.txt", header = FALSE, sep = "\t")

#zzz=matrix(data=NA,length(stations[,1]),136)#114)

#Structural Reliability (Down)
#1 & 2)Non-Thunderstorm & Thunderstorm
#Next results comes from:
#Sheets: nt_distLextreme_returnlev, and nt_distLextreme_parameter
#Package: extremeStat::distLquantile, extremeStat::distLextreme which is based on package lmomco
#Method: lmoments from lmomco
#
prefix_nt="nt"
prefix_t="t"
numberofcolumns=77 #
# - 7 (x, y, cellindexlr, lonindex, latindex, lonlatindex, empty)
# - 20 (current) #"id", "average_events_per_year", "average_time_per_year", "distance_w", "station",
#                 "threshold", "xi_location", "alpha_scale", "none_shape", paste0("nh_", tipicalReturnPeriods)
# - 28 (12 hurricane uniandes, 11 hurricane ingeniear, 2 nsr-old, 3 msr-new)
  #  - (1:12) uniandes #paste0("hu_", returnPeriodsUniandes)
  #  - (13:23) ingeniear #paste0("hi_", tipicalReturnPeriods)
  #  - (24:25) NSR-OLD # "MRIO_700", "MRIO_1700"
  #  - (26:28) NSR-NEW # "MRIN_700", "MRIN_1700", "MRIN_3000"
# - 22 (combined Hurricane: 
  #  - 11 uniandes paste0("cu_", tipicalReturnPeriods)
  #  - 11 ingeniar paste0("ci_", tipicalReturnPeriods)



nt_pot_lmon_gum = matrix(data=NA,length(selected_era5_stations[,1]),numberofcolumns)
the_names = c("x", "y", "cellindexlr", "lonindex", "latindex", "lonlatindex", "empty",
              "id", "average_events_per_year", "average_time_per_year", "distance_w", "station",
              "threshold", "xi_location", "alpha_scale", "none_shape", paste0("nh_", tipicalReturnPeriods),
              paste0("hu_", returnPeriodsUniandes), paste0("hi_", tipicalReturnPeriods),
              "MRIO_700", "MRIO_1700", "MRIN_700", "MRIN_1700", "MRIN_3000",
              paste0("cu_", tipicalReturnPeriods), paste0("ci_", tipicalReturnPeriods))
colnames(nt_pot_lmon_gum) = the_names
t_pot_lmon_gum = nt_pot_lmon_gum



nt_pot_lmon_wei = matrix(data=NA,length(selected_era5_stations[,1]),numberofcolumns)
the_names[14:16] = c("zeta_location", "beta_scale", "delta_shape")
colnames(nt_pot_lmon_wei) = the_names
t_pot_lmon_wei = nt_pot_lmon_wei


nt_pot_lmom_ln3 = matrix(data=NA,length(selected_era5_stations[,1]),numberofcolumns) 
the_names[14:16] = c("mulog_location", "sigmalog_scale", "zeta_lowerbounds")
colnames(nt_pot_lmom_ln3) = the_names
t_pot_lmom_ln3 = nt_pot_lmom_ln3


nt_pot_lmom_gpa = matrix(data=NA,length(selected_era5_stations[,1]),numberofcolumns)
the_names[14:16] = c("xi_location", "alpha_scale", "kappa_shape")
colnames(nt_pot_lmom_gpa) = the_names
t_pot_lmom_gpa = nt_pot_lmom_gpa


nt_pot_lmom_exp = matrix(data=NA,length(selected_era5_stations[,1]),numberofcolumns)
the_names[14:16] = c("xi_location", "alpha_scale", "none_shape")
colnames(nt_pot_lmom_exp) = the_names
t_pot_lmom_exp =  nt_pot_lmom_exp 


nt_pot_lmom_gam = matrix(data=NA,length(selected_era5_stations[,1]),numberofcolumns)
the_names[14:16] = c("none_location", "beta_scale", "alpha_shape")
colnames(nt_pot_lmom_gam) = the_names
t_pot_lmom_gam = nt_pot_lmom_gam

#DIFFERENT - this is different IN COLUMN 7: "h_shape2"
nt_pot_lmom_kap = matrix(data=NA,length(selected_era5_stations[,1]),numberofcolumns+1)
the_names[c(7,14:16)] = c("h_shape2", "xi_location", "alpha_scale", "kappa_shape1")
colnames(nt_pot_lmom_kap) = the_names
t_pot_lmom_kap = nt_pot_lmom_kap  

nt_pot_lmom_revgum = matrix(data=NA,length(selected_era5_stations[,1]),numberofcolumns)
the_names[14:16] = c("xi_location", "alpha_scale", "none_shape")
colnames(nt_pot_lmom_revgum) = the_names
t_pot_lmom_revgum = nt_pot_lmom_revgum


nt_pot_lmom_gev = matrix(data=NA,length(selected_era5_stations[,1]),numberofcolumns)
the_names[14:16] = c("xi_location", "alpha_scale", "kappa_shape")
colnames(nt_pot_lmom_gev) = the_names
t_pot_lmom_gev = nt_pot_lmom_gev


#Until here lmomco (extremeStat::distLextreme and extremeStat::distLquantile)

#
#fitdistrplus
numberofcolumns_fdp = 339 #289+50
#fitdistrplus gum 
nt_pot_fdp_gum = matrix(data=NA,length(selected_era5_stations[,1]),numberofcolumns_fdp)
the_names = c("x", "y", "cellindexlr", "lonindex", "latindex", "lonlatindex", "empty",
              "id", "average_events_per_year", "average_time_per_year", "distance_w", "station",
              "threshold",
              "location_mle", "scale_mle", "shape_mle", 
              "location_mle_median_bootstrap", "scale_mle_median_bootstrap", "shape_mle_median_bootstrap",
              "location_mle_2.5_bootstrap", "scale_mle_2.5_bootstrap", "shape_mle_2.5_bootstrap",
              "location_mle_97.5_bootstrap", "scale_mle_97.5_bootstrap", "shape_mle_97.5_bootstrap",
              "location_mle_stderror_bootstrap", "scale_mle_stderror_bootstrap", "shape_mle_stderror_bootstrap",
              "loglikelihood_mle", "aic_mle", "bic_mle", "chisq_mle", "chisq_pvalue_mle", "cramer_von_mises_stat_mle",
              "anderson_darling_stat_mle", "kolmogorov_smirnov_stat_mle", "empty2", "empty3",
              paste0("nh_", "mle_", tipicalReturnPeriods),
              paste0("nh_", "mle_", "median_", tipicalReturnPeriods),
              paste0("nh_", "mle_", "2.5_", tipicalReturnPeriods),
              paste0("nh_", "mle_", "97.5_", tipicalReturnPeriods),
              "location_mgeadr", "scale_mgeadr", "shape_mgeadr", 
              "location_mgeadr_median_bootstrap", "scale_mgeadr_median_bootstrap", "shape_mgeadr_median_bootstrap",
              "location_mgeadr_2.5_bootstrap", "scale_mgeadr_2.5_bootstrap", "shape_mgeadr_2.5_bootstrap",
              "location_mgeadr_97.5_bootstrap", "scale_mgeadr_97.5_bootstrap", "shape_mgeadr_97.5_bootstrap",
              "location_mgeadr_stderror_bootstrap", "scale_mgeadr_stderror_bootstrap", "shape_mgeadr_stderror_bootstrap",
              "loglikelihood_mgeadr", "aic_mgeadr", "bic_mgeadr", "chisq_mgeadr", "chisq_pvalue_mgeadr", "cramer_von_mises_stat_mgeadr", 
              "anderson_darling_stat_mgeadr", "kolmogorov_smirnov_stat_mgeadr", "empty4", "empty5",
              paste0("nh_", "mgeadr_", tipicalReturnPeriods),
              paste0("nh_", "mgeadr_", "median_", tipicalReturnPeriods),
              paste0("nh_", "mgeadr_", "2.5_", tipicalReturnPeriods),
              paste0("nh_", "mgeadr_", "97.5_", tipicalReturnPeriods),
              "location_mgead2r", "scale_mgead2r", "shape_mgead2r", 
              "location_mgead2r_median_bootstrap", "scale_mgead2r_median_bootstrap", "shape_mgead2r_median_bootstrap",
              "location_mgead2r_2.5_bootstrap", "scale_mgead2r_2.5_bootstrap", "shape_mgead2r_2.5_bootstrap",
              "location_mgead2r_97.5_bootstrap", "scale_mgead2r_97.5_bootstrap", "shape_mgead2r_97.5_bootstrap",
              "location_mgead2r_stderror_bootstrap", "scale_mgead2r_stderror_bootstrap", "shape_mgead2r_stderror_bootstrap",
              "loglikelihood_mgead2r", "aic_mgead2r", "bic_mgead2r", "chisq_mgead2r", "chisq_pvalue_mgead2r", "cramer_von_mises_stat_mgead2r", 
              "anderson_darling_stat_mgead2r", "kolmogorov_smirnov_stat_mgead2r", "empty6", "empty7",
              paste0("nh_", "mgead2r_", tipicalReturnPeriods),
              paste0("nh_", "mgead2r_", "median_", tipicalReturnPeriods),
              paste0("nh_", "mgead2r_", "2.5_", tipicalReturnPeriods),
              paste0("nh_", "mgead2r_", "97.5_", tipicalReturnPeriods),
              "location_mme", "scale_mme", "shape_mme", 
              "location_mme_median_bootstrap", "scale_mme_median_bootstrap", "shape_mme_median_bootstrap",
              "location_mme_2.5_bootstrap", "scale_mme_2.5_bootstrap", "shape_mme_2.5_bootstrap",
              "location_mme_97.5_bootstrap", "scale_mme_97.5_bootstrap", "shape_mme_97.5_bootstrap",
              "location_mme_stderror_bootstrap", "scale_mme_stderror_bootstrap", "shape_mme_stderror_bootstrap",
              "loglikelihood_mme", "aic_mme", "bic_mme", "chisq_mme", "chisq_pvalue_mme", "cramer_von_mises_stat_mme", 
              "anderson_darling_stat_mme", "kolmogorov_smirnov_stat_mme", "empty8", "empty9",
              paste0("nh_", "mme_", tipicalReturnPeriods),
              paste0("nh_", "mme_", "median_", tipicalReturnPeriods),
              paste0("nh_", "mme_", "2.5_", tipicalReturnPeriods),
              paste0("nh_", "mme_", "97.5_", tipicalReturnPeriods),
              paste0("hu_", returnPeriodsUniandes), paste0("hi_", tipicalReturnPeriods),
              "MRIO_700", "MRIO_1700", "MRIN_700", "MRIN_1700", "MRIN_3000",
              paste0("cu_", tipicalReturnPeriods), paste0("ci_", tipicalReturnPeriods))
colnames(nt_pot_fdp_gum) = the_names
t_pot_fdp_gum = nt_pot_fdp_gum

numberofcolumns_fdp_gof = 48

#fitdistrplus gum goodness-of-fit
nt_pot_fdp_gum_gof = matrix(data=NA,length(selected_era5_stations[,1]),numberofcolumns_fdp_gof)
the_names = c("id", "station",
              "fitted_function", "fitting_package", 
              "fitting_method_mle", "fitting_gof_method_mle", "fitting_optim_method_mle",
              "aic_mle", "bic_mle", 
              "cramer_von_mises_stat_mle", "cramer_von_mises_test_mle", 
              "anderson_darling_stat_mle", "anderson_darling_test_mle", 
              "kolmogorov_smirnov_stat_mle", "kolmogorov_smirnov_test_mle",
      
              "fitting_method_mgeadr", "fitting_gof_method_mgeadr", "fitting_optim_method_mgeadr",
              
              "aic_mgeadr", "bic_mgeadr", 
              "cramer_von_mises_stat_mgeadr", "cramer_von_mises_test_mgeadr", 
              "anderson_darling_stat_mgeadr", "anderson_darling_test_mgeadr", 
              "kolmogorov_smirnov_stat_mgeadr", "kolmogorov_smirnov_test_mgeadr",
              
              "fitting_method_mgead2r", "fitting_gof_method_mgead2r", "fitting_optim_method_mgead2r",
              
              "aic_mgead2r", "bic_mgead2r", 
              "cramer_von_mises_stat_mgead2r", "cramer_von_mises_test_mgead2r", 
              "anderson_darling_stat_mgead2r", "anderson_darling_test_mgead2r", 
              "kolmogorov_smirnov_stat_mgead2r", "kolmogorov_smirnov_test_mgead2r",
              
              "fitting_method_mme", "fitting_gof_method_mme", "fitting_optim_method_mme",
              
              "aic_mme", "bic_mme", 
              "cramer_von_mises_stat_mme", "cramer_von_mises_test_mme", 
              "anderson_darling_stat_mme", "anderson_darling_test_mme", 
              "kolmogorov_smirnov_stat_mme", "kolmogorov_smirnov_test_mme")
colnames(nt_pot_fdp_gum_gof) = the_names
t_pot_fdp_gum_gof = nt_pot_fdp_gum_gof

numberofcolumns_fdp_gof_short = 22
#fitdistrplus gum goodness-of-fit
nt_pot_fdp_gof_short = matrix(data=NA,length(selected_era5_stations[,1]),numberofcolumns_fdp_gof_short)
the_names = c("id", "station",
              "gum_aic_mle", "gum_bic_mle", 
              "gum_cramer_von_mises_stat_mle", 
              "gum_anderson_darling_stat_mle", 
              "gum_kolmogorov_smirnov_stat_mle", 
            
              "gum_aic_mgeadr", "gum_bic_mgeadr", 
              "gum_cramer_von_mises_stat_mgeadr", 
              "gum_anderson_darling_stat_mgeadr",  
              "gum_kolmogorov_smirnov_stat_mgeadr", 
              
              "gum_aic_mgead2r", "gum_bic_mgead2r", 
              "gum_cramer_von_mises_stat_mgead2r",  
              "gum_anderson_darling_stat_mgead2r",  
              "gum_kolmogorov_smirnov_stat_mgead2r",
              
              "gum_aic_mme", "gum_bic_mme", 
              "gum_cramer_von_mises_stat_mme", 
              "gum_anderson_darling_stat_mme", 
              "gum_kolmogorov_smirnov_stat_mme")
colnames(nt_pot_fdp_gof_short) = the_names
t_pot_fdp_gof_short = nt_pot_fdp_gof_short


#
#
#function alexys_extRemes (POT - MLE - GP - extRemes::fevd)
#  extRemes::fevd(x, method="MLE", type="GP"..)
#sheet nt_extRemes_GP
#method: mle
nt_pot_mle_gp = matrix(data=NA,length(selected_era5_stations[,1]),numberofcolumns)
t_pot_mle_gp = matrix(data=NA,length(selected_era5_stations[,1]),numberofcolumns)
the_names = c("_thresh", "_threshold_location", "_scale", "_shape", "_average_events_per_year(lambda)", "_average_time_per_year(Ant)")
colnames(nt_pot_mle_gp) = c("id", paste0(prefix_nt, the_names), "distance_w", "station", paste0(prefix_nt, "_nh_", tipicalReturnPeriods))
colnames(t_pot_mle_gp) = c("id", paste0(prefix_t, the_names), "distance_w", "station", paste0(prefix_t, "_nh_", tipicalReturnPeriods))
  

#sheet nt_rawdata_RL_yearlymaxima_GEV (row 2)(row 1 are names)
#function: alexys_extRemes_yearlymaxima
#package: extRemes::fevd(ams, method="MLE", type="GEV") >>> extRemes::return.level
#method: mle
nt_ym_mle_gev = matrix(data=NA,length(selected_era5_stations[,1]),numberofcolumns)
t_ym_mle_gev = matrix(data=NA,length(selected_era5_stations[,1]),numberofcolumns)
the_names = c("_thresh", "_location", "_scale", "_shape", "_average_events_per_year(lambda)", "_average_time_per_year(Ant)")
colnames(nt_ym_mle_gev) = c("id", paste0(prefix_nt, the_names), "distance_w", "station", paste0(prefix_nt, "_nh_", tipicalReturnPeriods))
colnames(t_ym_mle_gev) = c("id", paste0(prefix_t, the_names), "distance_w", "station", paste0(prefix_t, "_nh_", tipicalReturnPeriods))
  

#sheet nt_rawdata_RL_yearlymaxima_GEV(row 3)(row 1 are names)
#function: alexys_extRemes_yearlymaxima
#package: fit_lmom <- extRemes::fevd(ams, method = "Lmoments", type="GEV") >>> extRemes::return.level
#method: lmoments
nt_ym_lmom_gev = matrix(data=NA,length(selected_era5_stations[,1]),20)
t_ym_lmom_gev = matrix(data=NA,length(selected_era5_stations[,1]),20)
the_names =  c("_thresh", "_location", "_scale", "_shape", "_average_events_per_year(lambda)", "_average_time_per_year(Ant)")
colnames(nt_ym_lmom_gev) = c("id", paste0(prefix_nt, the_names), "distance_w", "station", paste0(prefix_nt, "_nh_", tipicalReturnPeriods))
colnames(t_ym_lmom_gev) = c("id", paste0(prefix_t, the_names), "distance_w", "station", paste0(prefix_t, "_nh_", tipicalReturnPeriods))
#Structural Reliability (Up)

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
i=0
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
    zz = zz + 1  # R indexes stars in one (1)
    #Read the station ID from 'stations' variable
    #number <- stations[zz,1]
    number = zz
    if (any(selected_era5_stations[,1] == number)){
      # x, y, cellindexlr, lonindex, latindex, lonlatindex
      currentxy = xy[number, ] #Dataframe
      currentxy_sf = st_as_sf(currentxy, coords=c(1,2)) # sf
      
      # Filter hurricane sf with current station
      h_u_sf_filter <- h_u_sf %>% filter(station == number)
      h_i_sf_filter <- h_i_sf %>% filter(station == number)
      
      # Join Hurricane Uniandes & Integrar in one Dataframe
      h = cbind(st_drop_geometry(h_u_sf_filter), st_drop_geometry(h_i_sf_filter))
      # h: (23 columns)
      #  - (1:12) uniandes
      #  - (13:23) ingeniear
      ncol_h_u = ncol(st_drop_geometry(h_u_sf_filter)) # 12 maps
      ncol_h_i = ncol(st_drop_geometry(h_i_sf_filter)) # 11 maps
      
      # Add NRS-10 maps & information (old / new) information to hurricane data frame
      #Load NEW maps from Colombian Design Norm NSR_10
      nsr700 = st_read(paste0(input_path, "/nsr10windmap_700.shp"))
      nsr1700 = st_read(paste0(input_path, "/nsr10windmap_1700.shp"))
      nsr3000 = st_read(paste0(input_path, "/nsr10windmap_3000.shp"))
      #
      idx = st_nearest_feature(currentxy_sf, nsr700["MRI_700"], check_crs = FALSE)
      MRIN_700 = st_drop_geometry(nsr700[idx,"MRI_700"])
      colnames(MRIN_700) = "MRIN_700"
      #
      idx = st_nearest_feature(currentxy_sf, nsr1700["MRI_1700"], check_crs = FALSE)
      MRIN_1700 = st_drop_geometry(nsr1700[idx,"MRI_1700"])
      colnames(MRIN_1700) = "MRIN_1700"
      #
      idx = st_nearest_feature(currentxy_sf, nsr3000["MRI_3000"], check_crs = FALSE)
      MRIN_3000 = st_drop_geometry(nsr3000[idx,"MRI_3000"])
      colnames(MRIN_3000) = "MRIN_3000"
      
      #Load OLD maps from Colombian Design Norm NSR_10
      nsrold = st_read(paste0(input_path, "/mapa_viento_nsr_10.shp"))
      #
      idx = st_nearest_feature(currentxy_sf, nsrold["b23"], check_crs = FALSE)
      MRIO_700 = st_drop_geometry(nsrold[idx,"b23"])
      colnames(MRIO_700) = "MRIO_700"
      #
      idx = st_nearest_feature(currentxy_sf, nsrold["b24"], check_crs = FALSE)
      MRIO_1700 = st_drop_geometry(nsr1700[idx,"b24"])
      colnames(MRIO_1700) = "MRIO_1700"
      
      # Join Hurricane to hurricane data frame
      h_nsr = cbind(h, MRIO_700, MRIO_1700, MRIN_700, MRIN_1700, MRIN_3000)
      #columns h_nsr: 1 to ncol_h_u >> Hurricane Uniandes
      #         (ncol_h_u) + 1 to (ncol_h_u + 1) + ncol_h_i >>> Hurricane Ingeniar
      #         (ncol_h_u + 1 + ncol_h_i) + 1 to (ncol_h_u + 1 + ncol_h_i + 1) + 2 >> NSR-OLD (2 colums)
      #         (ncol_h_u + 1 + ncol_h_i + 1 + 2) + 1 to (ncol_h_u + 1 + ncol_h_i + 1 + 2 + 1) + 3 >> NSR-NEW (3 colums)
      # columns h_nsr: (12 + 11 +2 +3) = 28 columns
      #  - (1:12) uniandes
      #  - (13:23) ingeniear
      #  - (24:25) NSR-OLD
      #  - (26:28) NSR-NEW
      i = i + 1
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
      raw.data <- ReadWindERA5Station(ncin = ncin, lonindex = lonindex,
                                     latindex = latindex, ntime = ntime, timestamp = timestamp)
      
      #raw.data <- ReadWindERA5ProxyStation(starsOrStarsProxy = mync, attribute = variablenumber, 
      #                                     lonindex = lonindex, latindex = latindex, ntime = ntime)
      
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
      #source('./code/stats_raw_data.r')
      if (length(raw.data.tibble$date.time) > 0) {
        numberofplots = stats_data(raw.data.tibble, data_label="Raw Data", prefix="all", numberofplots=numberofplots, exel_file=statsfile_OUT)
      }
      #create the raw.data.nt tibble data frame
      select <- dplyr::select
      raw.data.tibble  %>%
        select(date.time, speed.kph, t.nt.flag) %>%
        filter(t.nt.flag == "nt") -> raw.data.nt
    
      #In file 'statsfile' create the stat sheets for non-thunderstorm raw data:
      # nt_years, nt_weeks, nt_months, nt_gaps  
      # and create the non-thunderstorm raw data time series graphic
      # and calculate yearly maxima return levels
      #source('./code/stats_raw_data_nt.r')
      if (length(raw.data.nt$date.time) > 0) {
        numberofplots = stats_data(raw.data.nt, data_label="Raw Data Non-Thunderstorm", prefix=prefix_nt, numberofplots=numberofplots, exel_file=statsfile_OUT)
        #Yearly Maxima calculation for non-thunderstorm
        ymg = yearly_maxima_and_graphs(data=raw.data.nt, 
                                               data_label=paste0("Non-thunderstorm raw data for Station ID:", number), 
                                               prefix=prefix_nt, 
                                               numberofplots=numberofplots,
                                               tipicalReturnPeriods = tipicalReturnPeriods,
                                               exel_file=fnfitted_OUT,
                                               ym_mle_gev=nt_ym_mle_gev,
                                               ym_lmom_gev=nt_ym_lmom_gev)
        numberofplots = ymg$numberofplots
        nt_ym_mle_gev = ymg$ym_mle_gev
        nt_ym_lmom_gev = ymg$ym_lmom_gev
      }
      
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
      #source('./code/stats_raw_data_t.r')
      if (length(raw.data.t$date.time) > 0) {
        numberofplots = stats_data(raw.data.t, data_label="Raw Data Thunderstorm", prefix=prefix_t, numberofplots=numberofplots, exel_file=statsfile_OUT)
        #Yearly Maxima calculation for non-thunderstorm
        ymg = yearly_maxima_and_graphs(data=raw.data.t, 
                                               data_label=paste0("Thunderstorm raw data for Station ID:", number), 
                                               prefix=prefix_t, 
                                               numberofplots=numberofplots,
                                               tipicalReturnPeriods = tipicalReturnPeriods,
                                               exel_file=fnfitted_OUT,
                                               ym_mle_gev=t_ym_mle_gev,
                                               ym_lmom_gev=t_ym_lmom_gev)
        numberofplots = ymg$numberofplots
        t_ym_mle_gev = ymg$ym_mle_gev
        t_ym_lmom_gev = ymg$ym_lmom_gev
      }
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
      
      #source('./code/tnt_csv_1perday.r')
      #Write "nt" to csv, but changing to one data per day (the maximun)
      #Write "nt" to csv, but changing to one data per day (the maximun)
      if (length(imp.vals$t.series.dt) > 0) {
        tnt_csv_1perday(date=imp.vals$t.series.dt, series=imp.vals$t.series, check=length(imp.vals$t.series.dt) > 0, type="t")
      }
      #Write "t" to csv, but changing to one data per day (the maximun)
      #Write "t" to csv, but changing to one data per day (the maximun)
      if (length(imp.vals$nt.series.dt) > 0) {
        tnt_csv_1perday(date=imp.vals$nt.series.dt, series=imp.vals$nt.series, check=length(imp.vals$nt.series.dt) > 0, type="nt")
      }
      
    
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
    
      #Alexys
          
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
          
      #For de-clustered non-thunderstorm: Gumbel Fittings, GPD-Poisson-GPD extremeStat results and statistics (years, weeks, months, gaps)
      #source('./code/stats_graphs_dnt.r')
      if (length(imp.vals$nt.series.dt) > 0) {
        # 1) Plot histogram
        # Send to PDF (FittedModel_ID.pdf) graphics for declustered non-thunderstorm
        numberofplots = plot_frequency_histogram(series=imp.vals$nt.series, data_label="Non-Thunderstorm", numberofplots=numberofplots)#, check=length(series) > 0)
        # 2) Save time series
        save_time_series(date=imp.vals$nt.series.dt, series=imp.vals$nt.series, prefix=paste0("d", prefix_nt, "_"), outputpath=outputpath, check=length(imp.vals$nt.series.dt) > 0)
        # 3) Gumbel fittings different options. Write (raw_data_station_id_fitted.xlsx, variable fnfitted) "Gumbel fittings"
        #    to sheets names:
        #  - nt_evd-fgev_fGumbel_Gumbel: fitting Gumbel using two different options: a)evd::fgev, b) function fGumbel
        #  - nt_bbmle-mle2_Gumbel: fitting Gumbel using bbmle::mle2
        #  - nt_nll-optim_Gumbel: fitting Gumbel using negative likelihood and stats::optim
        #  - nt__fitdistrplus-fitdist_Gumbel: fitting Gumbel using fitdistrplus::fitdist
        ## Send to PDF (FittedModel_ID.pdf) graphics for declustered non-thunderstorm (Gumbel fittings)
        #  - Fitted Gumbel Probability Density Curve - Log-Likelihood(Gumbel) - Optim (nll-optim)
        #  - Declustered - Non-Thunderstorm - fitdistrplus-fitdist(gumbel)
        numberofplots = fit_gumbel_different_options(date=imp.vals$nt.series.dt, series=imp.vals$nt.series, obsperyears = imp.vals$n.nthunders.per.year,
                                                 threshold=nt.thresh, shape=0, excelfile=fnfitted_OUT, prefix=prefix_nt, data_label="Non-Thunderstorm",
                                                 numberofplots=numberofplots)#, check=length(series) > 0)
        # 4) GPD-Poisson-GPD extremeStat results
        # Excel sheets names
        #  - nt_extRemes_gp: calculation of return values POT-Poisson-GPD, using extRemes::fevd
        #  - nt_pot_mle_gpa (same calculations in another excel file: fitted_models_results.xlsx) 
        nt_pot_mle_gp = pot_gp(series=imp.vals$nt.series, threshold=nt.thresh, tipicalReturnPeriods= tipicalReturnPeriods,
                           npy=imp.vals$n.nthunders.per.year, excelfile=fnfitted_OUT, prefix=prefix_nt, pot_mle_gp=nt_pot_mle_gp)#, check=length(series) > 0)
        # 5) POT using extremeStat package (return many POT individual fittings)
        #  - nt_distLquantile_quant: calculation of return values and RMSE (POT-Poisson-GPD and EVDs), using extremeStat::distLquantile
        #  - nt_distLquantile_parameters: calculation of fitting parameters POT-Poisson-GPD and EVD, using extremeStat::distLquantile
        #  - nt_distLextreme_returnlev: calculation of return values POT-Poisson-GPD and EVD, using extremeStat::distLextreme
        #  - nt_distLextreme_parameter: calculation of fitting parameters POT-Poisson-GPD and EVD, using extremeStat::distLextreme
        #  - nt_pot_*: (same calculations in another excel file, but extracting individual pdf values per sheet: fitted_models_results.xlsx)
        pes = pot_extremeStat(series=imp.vals$nt.series, threshold=nt.thresh, tipicalReturnPeriods= tipicalReturnPeriods,
                                                npy=imp.vals$n.nthunders.per.year, excelfile=fnfitted_OUT, prefix=prefix_nt,  
                                                pot_lmon_gum=nt_pot_lmon_gum,
                                                pot_lmon_wei=nt_pot_lmon_wei,
                                                pot_lmom_ln3=nt_pot_lmom_ln3,
                                                pot_lmom_gpa=nt_pot_lmom_gpa,
                                                pot_lmom_exp=nt_pot_lmom_exp,
                                                pot_lmom_gam=nt_pot_lmom_gam,
                                                pot_lmom_kap=nt_pot_lmom_kap,
                                                pot_lmom_revgum=nt_pot_lmom_revgum,
                                                pot_lmom_gev=nt_pot_lmom_gev)#, check=length(series) > 0)
        nt_pot_lmon_gum = pes$pot_lmon_gum
        nt_pot_lmon_wei = pes$pot_lmon_wei
        nt_pot_lmom_ln3 = pes$pot_lmom_ln3
        nt_pot_lmom_gpa = pes$pot_lmom_gpa
        nt_pot_lmom_exp = pes$pot_lmom_exp
        nt_pot_lmom_gam = pes$pot_lmom_gam
        nt_pot_lmom_kap = pes$pot_lmom_kap
        nt_pot_lmom_revgum = pes$pot_lmom_revgum 
        nt_pot_lmom_gev = pes$pot_lmom_gev
        
        # 6) In file raw_data_station_ID_statistics.xlsx (variable 'statsfile') create the statistics sheets for Non-Thunderstorm:
        #  - declu_nt_years
        #  - declu_nt_weeks
        #  - declu_nt_months
        #  - declu_nt_gaps30days
        data_statistics(date=imp.vals$nt.series.dt, series=imp.vals$nt.series, excelfile=statsfile_OUT, prefix=prefix_nt)#, check=length(series) > 0)
        # 7) Plot time series
        #  Send to PDF (FittedModel_ID.pdf): Declustered Non-Thunderstorm ('nt') Time Series
        numberofplots = plot_timeseries(series=imp.vals$nt.series, date=imp.vals$nt.series.dt, data_label="Non-Thunderstorm ('nt')", 
                                        numberofplots=numberofplots)#, check=length(series > 0)
        
        # 8) Fits and plots with fitdistplus using bootstrap and different fitting methods
        fdp = fit_multiple_pdf_fitdistplus(date=imp.vals$nt.series.dt, series=imp.vals$nt.series, obsperyears=imp.vals$n.nthunders.per.year,
                 threshold=nt.thresh, prefix=prefix_nt, data_label="Non-Thunderstorm",
                 numberofplots=numberofplots, number=number, variable_description= "Wind Velocity [Km/h]",
                 fdp_gum=nt_pot_fdp_gum,
                 #fdp_wei=nt_pot_fdp_wei,
                 #fdp_ln3=nt_pot_fdp_ln3,
                 #fdp_gpa=nt_pot_fdp_gpa,
                 #fdp_exp=nt_pot_fdp_exp,
                 #fdp_gam=nt_pot_fdp_gam,
                 #fdp_kap=nt_pot_fdp_kap,
                 #fdp_revgum=nt_pot_fdp_revgum,
                 #fdp_gev=nt_pot_fdp_gev, 
                 tipicalReturnPeriods = tipicalReturnPeriods,
                 tipicalProbabilities = 1-1/(imp.vals$n.nthunders.per.year*tipicalReturnPeriods), 
                 i = i, #consecutive
                 x = currentxy$x,
                 y = currentxy$y,
                 cellindexlr = currentxy$cellindexlr,
                 lonindex = currentxy$lonindex,
                 latindex = currentxy$latindex,
                 lonlatindex = currentxy$lonlatindex,
                 average_time_per_year = 1-(imp.vals$n.thunders.per.year*(1/24)*(1/365)),
                 maxW = z8,
                 niter_boost = 1001)
        nt_pot_fdp_gum = fdp$fdp_gum
        nt_pot_fdp_gum_gof = fdp$fdp_gum_gof
        nt_pot_fdp_gof_short = fdp$fdp_gof_short
        numberofplots = fdp$numberofplots
      
      }
      
      #For de-clustered thunderstorm: Gumbel Fittings, GPD-Poisson-GPD extremeStat results and statistics (years, weeks, months, gaps)
      #source('./code/stats_graphs_dt.r')
      if (length(imp.vals$t.series.dt) > 0) {
        # 1) Plot histogram
        # Send to PDF (FittedModel_ID.pdf) graphics for declustered thunderstorm
        numberofplots = plot_frequency_histogram(series=imp.vals$t.series, data_label="Thunderstorm", numberofplots=numberofplots)#, check=length(series) > 0)
        # 2) Save time series
        save_time_series(date=imp.vals$t.series.dt, series=imp.vals$t.series, prefix=paste0("d", prefix_t, "_"), outputpath=outputpath, check=length(imp.vals$nt.series.dt) > 0)
        # 3) Gumbel fittings different options. Write (raw_data_station_id_fitted.xlsx, variable fnfitted) "Gumbel fittings"
        #    to sheets names:
        #  - t_evd-fgev_fGumbel_Gumbel: fitting Gumbel using two different options: a)evd::fgev, b) function fGumbel
        #  - t_bbmle-mle2_Gumbel: fitting Gumbel using bbmle::mle2
        #  - t_nll-optim_Gumbel: fitting Gumbel using negative likelihood and stats::optim
        #  - t__fitdistrplus-fitdist_Gumbel: fitting Gumbel using fitdistrplus::fitdist
        ## Send to PDF (FittedModel_ID.pdf) graphics for declustered non-thunderstorm (Gumbel fittings)
        #  - Fitted Gumbel Probability Density Curve - Log-Likelihood(Gumbel) - Optim (nll-optim)
        #  - Declustered - Thunderstorm - fitdistrplus-fitdist(gumbel)
        numberofplots = fit_gumbel_different_options(date=imp.vals$t.series.dt, series=imp.vals$t.series, obsperyears = imp.vals$n.thunders.per.year,
                                                     threshold=t.thresh, shape=0, excelfile=fnfitted_OUT, prefix=prefix_t, data_label="Thunderstorm",
                                                     numberofplots=numberofplots)#, check=length(series) > 0)
        # 4) GPD-Poisson-GPD extremeStat results
        # Excel sheets names
        #  - t_extRemes_gp: calculation of return values POT-Poisson-GPD, using extRemes::fevd
        #  - t_pot_mle_gpa (same calculations in another excel file: fitted_models_results.xlsx) 
        t_pot_mle_gp = pot_gp(series=imp.vals$t.series, threshold=t.thresh, tipicalReturnPeriods= tipicalReturnPeriods,
                               npy=imp.vals$n.thunders.per.year, excelfile=fnfitted_OUT, prefix=prefix_t, pot_mle_gp=t_pot_mle_gp)#, check=length(series) > 0)
        # 5) POT using extremeStat package (return many POT individual fittings)
        #  - t_distLquantile_quant: calculation of return values and RMSE (POT-Poisson-GPD and EVDs), using extremeStat::distLquantile
        #  - t_distLquantile_parameters: calculation of fitting parameters POT-Poisson-GPD and EVD, using extremeStat::distLquantile
        #  - t_distLextreme_returnlev: calculation of return values POT-Poisson-GPD and EVD, using extremeStat::distLextreme
        #  - t_distLextreme_parameter: calculation of fitting parameters POT-Poisson-GPD and EVD, using extremeStat::distLextreme
        #  - t_pot_*: (same calculations in another excel file, but extracting individual pdf values per sheet: fitted_models_results.xlsx)
        pes = pot_extremeStat(series=imp.vals$t.series, threshold=t.thresh, tipicalReturnPeriods= tipicalReturnPeriods,
                              npy=imp.vals$n.thunders.per.year, excelfile=fnfitted_OUT, prefix=prefix_t,  
                              pot_lmon_gum=t_pot_lmon_gum,
                              pot_lmon_wei=t_pot_lmon_wei,
                              pot_lmom_ln3=t_pot_lmom_ln3,
                              pot_lmom_gpa=t_pot_lmom_gpa,
                              pot_lmom_exp=t_pot_lmom_exp,
                              pot_lmom_gam=t_pot_lmom_gam,
                              pot_lmom_kap=t_pot_lmom_kap,
                              pot_lmom_revgum=t_pot_lmom_revgum,
                              pot_lmom_gev=t_pot_lmom_gev)#, check=length(series) > 0)
        t_pot_lmon_gum = pes$pot_lmon_gum
        t_pot_lmon_wei = pes$pot_lmon_wei
        t_pot_lmom_ln3 = pes$pot_lmom_ln3
        t_pot_lmom_gpa = pes$pot_lmom_gpa
        t_pot_lmom_exp = pes$pot_lmom_exp
        t_pot_lmom_gam = pes$pot_lmom_gam
        t_pot_lmom_kap = pes$pot_lmom_kap
        t_pot_lmom_revgum = pes$pot_lmom_revgum 
        t_pot_lmom_gev = pes$pot_lmom_gev
        # 6) In file raw_data_station_ID_statistics.xlsx (variable 'statsfile') create the statistics sheets for thunderstorm:
        #  - declu_t_years
        #  - declu_t_weeks
        #  - declu_t_months
        #  - declu_t_gaps30days
        data_statistics(date=imp.vals$t.series.dt, series=imp.vals$t.series, excelfile=statsfile_OUT, prefix=prefix_t)#, check=length(series) > 0)
        # 7) Plot time series
        #  Send to PDF (FittedModel_ID.pdf): Declustered Thunderstorm ('t') Time Series
        numberofplots = plot_timeseries(series=imp.vals$t.series, date=imp.vals$t.series.dt, data_label="Thunderstorm ('t')", 
                                        numberofplots=numberofplots)#, check=length(series > 0)
        # 8) Fits and plots with fitdistplus using bootstrap and different fitting methods
        fdp = fit_multiple_pdf_fitdistplus(date=imp.vals$t.series.dt, series=imp.vals$t.series, obsperyears=imp.vals$n.thunders.per.year,
                                           threshold=t.thresh, prefix=prefix_t, data_label="Thunderstorm",
                                           numberofplots=numberofplots, number=number, variable_description= "Wind Velocity [Km/h]",
                                           fdp_gum=t_pot_fdp_gum,
                                           #fdp_wei=t_pot_fdp_wei,
                                           #fdp_ln3=t_pot_fdp_ln3,
                                           #fdp_gpa=t_pot_fdp_gpa,
                                           #fdp_exp=t_pot_fdp_exp,
                                           #fdp_gam=t_pot_fdp_gam,
                                           #fdp_kap=t_pot_fdp_kap,
                                           #fdp_revgum=t_pot_fdp_revgum,
                                           #fdp_gev=t_pot_fdp_gev, 
                                           tipicalReturnPeriods = tipicalReturnPeriods,
                                           tipicalProbabilities = 1-1/(imp.vals$n.thunders.per.year*tipicalReturnPeriods), 
                                           i = i, #consecutive
                                           x = currentxy$x,
                                           y = currentxy$y,
                                           cellindexlr = currentxy$cellindexlr,
                                           lonindex = currentxy$lonindex,
                                           latindex = currentxy$latindex,
                                           lonlatindex = currentxy$lonlatindex,
                                           average_time_per_year = imp.vals$n.thunders.per.year*(1/24)*(1/365),
                                           maxW = z8,
                                           niter_boost = 1001)
        t_pot_fdp_gum = fdp$fdp_gum
        t_pot_fdp_gum_gof = fdp$fdp_gum_gof
        t_pot_fdp_gof_short = fdp$fdp_gof_short
        numberofplots = fdp$numberofplots
      }
    
      zzz[i,1]=i       #consecutive
      nt_pot_lmon_gum[i,1]=i
      nt_pot_lmon_wei[i,1]=i
      nt_pot_lmom_ln3[i,1]=i
      nt_pot_lmom_gpa[i,1]=i
      nt_pot_lmom_exp[i,1]=i
      nt_pot_lmom_gam[i,1]=i
      nt_pot_lmom_kap[i,1]=i
      nt_pot_lmom_revgum[i,1]=i
      nt_pot_lmom_gev[i,1]=i
      nt_ym_mle_gev[i,1]=i
      nt_ym_lmom_gev[i,1]=i
      nt_pot_mle_gp[i,1]=i
      #
      t_pot_lmon_gum[i,1]=i
      t_pot_lmon_wei[i,1]=i
      t_pot_lmom_ln3[i,1]=i
      t_pot_lmom_gpa[i,1]=i
      t_pot_lmom_exp[i,1]=i
      t_pot_lmom_gam[i,1]=i
      t_pot_lmom_kap[i,1]=i
      t_pot_lmom_revgum[i,1]=i
      t_pot_lmom_gev[i,1]=i
      t_ym_mle_gev[i,1]=i
      t_ym_lmom_gev[i,1]=i
      t_pot_mle_gp[i,1]=i
      
      zzz[i,2]=z2       #t_thresh PP
      zzz[i,3]=z3       #PP t_mu = location
      zzz[i,4]=z4       #PP t_psi = scale
      zzz[i,5]=imp.vals$n.thunders.per.year               #t_average_events_per_year
      #Alexys: change: this valune need to come from rl_plot_t.r or rl_plot_t_nt.r 
      zzz[i,6]=imp.vals$n.thunders.per.year*(1/24)*(1/365)        #t_average_time_per_year(At): zzz[i,5] * 1 hour (result in days) (Ok) >>>Same At!
      zzz[i,7]=NA                                         #t_gamma_Pot-Poisson-GPD. The average number of events per year calculated using POT-Poisson-GPD. 
                                                           #Main parameter of Poisson 1D (lambda or gamma). Need to be calculated (this part is pending)
      zzz[i,8]=z5       #nt_thresh
      nt_pot_lmon_gum[i,2]=z5
      nt_pot_lmon_wei[i,2]=z5
      nt_pot_lmom_ln3[i,2]=z5
      nt_pot_lmom_gpa[i,2]=z5
      nt_pot_lmom_exp[i,2]=z5
      nt_pot_lmom_gam[i,2]=z5
      nt_pot_lmom_kap[i,2]=z5
      nt_pot_lmom_revgum[i,2]=z5
      nt_pot_lmom_gev[i,2]=z5
      nt_ym_mle_gev[i,2]=NA
      nt_ym_lmom_gev[i,2]=NA
      nt_pot_mle_gp[i,2]=z5
      #
      t_pot_lmon_gum[i,2]=z5
      t_pot_lmon_wei[i,2]=z5
      t_pot_lmom_ln3[i,2]=z5
      t_pot_lmom_gpa[i,2]=z5
      t_pot_lmom_exp[i,2]=z5
      t_pot_lmom_gam[i,2]=z5
      t_pot_lmom_kap[i,2]=z5
      t_pot_lmom_revgum[i,2]=z5
      t_pot_lmom_gev[i,2]=z5
      t_ym_mle_gev[i,2]=NA
      t_ym_lmom_gev[i,2]=NA
      t_pot_mle_gp[i,2]=z5
      
      zzz[i,9]=z6       #nt_mu = location
      
      zzz[i,10]=z7      #nt_psi = scale
      
      #Alexys: change: this valune need to come from rl_plot_t.r or rl_plot_t_nt.r 
      zzz[i,11]=imp.vals$n.nthunders.per.year           #nt_average_events_per_year
      nt_pot_lmon_gum[i,6]=imp.vals$n.nthunders.per.year
      nt_pot_lmon_wei[i,6]=imp.vals$n.nthunders.per.year
      nt_pot_lmom_ln3[i,6]=imp.vals$n.nthunders.per.year
      nt_pot_lmom_gpa[i,6]=imp.vals$n.nthunders.per.year
      nt_pot_lmom_exp[i,6]=imp.vals$n.nthunders.per.year
      nt_pot_lmom_gam[i,6]=imp.vals$n.nthunders.per.year
      nt_pot_lmom_kap[i,6]=imp.vals$n.nthunders.per.year
      nt_pot_lmom_revgum[i,6]=imp.vals$n.nthunders.per.year
      nt_pot_lmom_gev[i,6]=imp.vals$n.nthunders.per.year
      nt_ym_mle_gev[i,6]=1
      nt_ym_lmom_gev[i,6]=1
      nt_pot_mle_gp[i,6]=imp.vals$n.nthunders.per.year
      #
      t_pot_lmon_gum[i,6]=imp.vals$n.nthunders.per.year
      t_pot_lmon_wei[i,6]=imp.vals$n.nthunders.per.year
      t_pot_lmom_ln3[i,6]=imp.vals$n.nthunders.per.year
      t_pot_lmom_gpa[i,6]=imp.vals$n.nthunders.per.year
      t_pot_lmom_exp[i,6]=imp.vals$n.nthunders.per.year
      t_pot_lmom_gam[i,6]=imp.vals$n.nthunders.per.year
      t_pot_lmom_kap[i,6]=imp.vals$n.nthunders.per.year
      t_pot_lmom_revgum[i,6]=imp.vals$n.nthunders.per.year
      t_pot_lmom_gev[i,6]=imp.vals$n.nthunders.per.year
      t_ym_mle_gev[i,6]=1
      t_ym_lmom_gev[i,6]=1
      t_pot_mle_gp[i,6]=imp.vals$n.nthunders.per.year
      
      zzz[i,12]=1-(zzz[i,6])                         #nt_average_time_per_year(Ant) = 365-At
      nt_pot_lmon_gum[i,7]=1-(zzz[i,6])
      nt_pot_lmon_wei[i,7]=1-(zzz[i,6])
      nt_pot_lmom_ln3[i,7]=1-(zzz[i,6])
      nt_pot_lmom_gpa[i,7]=1-(zzz[i,6])
      nt_pot_lmom_exp[i,7]=1-(zzz[i,6])
      nt_pot_lmom_gam[i,7]=1-(zzz[i,6])
      nt_pot_lmom_kap[i,7]=1-(zzz[i,6])
      nt_pot_lmom_revgum[i,7]=1-(zzz[i,6])
      nt_pot_lmom_gev[i,7]=1-(zzz[i,6])
      nt_ym_mle_gev[i,7]=NA
      nt_ym_lmom_gev[i,7]=NA
      nt_pot_mle_gp[i,7]=1-(zzz[i,6])
      #
      t_pot_lmon_gum[i,7]=1-(zzz[i,6])
      t_pot_lmon_wei[i,7]=1-(zzz[i,6])
      t_pot_lmom_ln3[i,7]=1-(zzz[i,6])
      t_pot_lmom_gpa[i,7]=1-(zzz[i,6])
      t_pot_lmom_exp[i,7]=1-(zzz[i,6])
      t_pot_lmom_gam[i,7]=1-(zzz[i,6])
      t_pot_lmom_kap[i,7]=1-(zzz[i,6])
      t_pot_lmom_revgum[i,7]=1-(zzz[i,6])
      t_pot_lmom_gev[i,7]=1-(zzz[i,6])
      t_ym_mle_gev[i,7]=NA
      t_ym_lmom_gev[i,7]=NA
      t_pot_mle_gp[i,7]=1-(zzz[i,6])
        
      zzz[i,13]=NA       #nt_gamma_Pot-Poisson-GPD. The average number of events per year calculated using POT-Poisson-GPD. 
                          #Main parameter of Poisson 1D (lambda or gamma).Need to be calculated (this part is pending)
      zzz[i,14]=z8      #max W for optimal thresholds
      nt_pot_lmon_gum[i,8]=z8
      nt_pot_lmon_wei[i,8]=z8
      nt_pot_lmom_ln3[i,8]=z8
      nt_pot_lmom_gpa[i,8]=z8
      nt_pot_lmom_exp[i,8]=z8
      nt_pot_lmom_gam[i,8]=z8
      nt_pot_lmom_kap[i,8]=z8
      nt_pot_lmom_revgum[i,8]=z8
      nt_pot_lmom_gev[i,8]=z8
      nt_ym_mle_gev[i,8]=NA
      nt_ym_lmom_gev[i,8]=NA
      nt_pot_mle_gp[i,8]=z8
      #
      t_pot_lmon_gum[i,8]=z8
      t_pot_lmon_wei[i,8]=z8
      t_pot_lmom_ln3[i,8]=z8
      t_pot_lmom_gpa[i,8]=z8
      t_pot_lmom_exp[i,8]=z8
      t_pot_lmom_gam[i,8]=z8
      t_pot_lmom_kap[i,8]=z8
      t_pot_lmom_revgum[i,8]=z8
      t_pot_lmom_gev[i,8]=z8
      t_ym_mle_gev[i,8]=NA
      t_ym_lmom_gev[i,8]=NA
      t_pot_mle_gp[i,8]=z8
        
      zzz[i,15]=number  #station number
      nt_pot_lmon_gum[i,9]= number
      nt_pot_lmon_wei[i,9]= number
      nt_pot_lmom_ln3[i,9]= number
      nt_pot_lmom_gpa[i,9]= number
      nt_pot_lmom_exp[i,9]= number
      nt_pot_lmom_gam[i,9]= number
      nt_pot_lmom_kap[i,9]= number
      nt_pot_lmom_revgum[i,9]= number
      nt_pot_lmom_gev[i,9]= number
      nt_ym_mle_gev[i,9]= number
      nt_ym_lmom_gev[i,9]= number
      nt_pot_mle_gp[i,9]= number
      #
      t_pot_lmon_gum[i,9]= number
      t_pot_lmon_wei[i,9]= number
      t_pot_lmom_ln3[i,9]= number
      t_pot_lmom_gpa[i,9]= number
      t_pot_lmom_exp[i,9]= number
      t_pot_lmom_gam[i,9]= number
      t_pot_lmom_kap[i,9]= number
      t_pot_lmom_revgum[i,9]= number
      t_pot_lmom_gev[i,9]= number
      t_ym_mle_gev[i,9]= number
      t_ym_lmom_gev[i,9]= number
      t_pot_mle_gp[i,9]= number
      
      #List with typical return periods
      #tipicalReturnPeriods = c(10,20,50,100,250,500,700,1000,1700,3000,7000)
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
}

#Save output excel file (fitted_model_result.xlsx - variable fn) with return values for all stations
#write.xlsx(zzz, file=fn, sheetName="pp_pintar", append=TRUE, row.names=FALSE, col.names=TRUE)
addWorksheet(fn_OUT, "pp_pintar")
writeData(fn_OUT, sheet = "pp_pintar", x = zzz)

# 16-59 ------> Considering the dataset only composed by THUNDERSTORM
#zzz[, c(1, 15, 16:26)] -> t_MRI_poissonprocessintfunc
addWorksheet(fn_OUT, "t_MRI_poissonprocessintfunc")
writeData(fn_OUT, sheet = "t_MRI_poissonprocessintfunc", x = zzz[, c(1:15, 16:26)])

#zzz[, c(1, 15, 27:37)] -> t_MRI_gumbeltailintfunc
addWorksheet(fn_OUT, "t_MRI_gumbeltailintfunc")
writeData(fn_OUT, sheet = "t_MRI_gumbeltailintfunc", x = zzz[, c(1:15, 27:37)])

#zzz[, c(1, 15, 38:48)] -> t_MRI_gumbelquantilefunc
addWorksheet(fn_OUT, "t_MRI_gumbelquantilefunc")
writeData(fn_OUT, sheet = "t_MRI_gumbelquantilefunc", x = zzz[, c(1:15, 38:48)])

#zzz[, c(1, 15, 49:59)] -> t_MRI_POT-Poisson-GPD
addWorksheet(fn_OUT, "t_MRI_POT-Poisson-GPD")
writeData(fn_OUT, sheet = "t_MRI_POT-Poisson-GPD", x = zzz[, c(1:15, 49:59)])

#60-103 ------> Considering the dataset only composed by NON-THUNDERSTORM
#zzz[, c(1, 15, 60:70)] -> nt_MRI_poissonprocessintfunc
addWorksheet(fn_OUT, "nt_MRI_poissonprocessintfunc")
writeData(fn_OUT, sheet = "nt_MRI_poissonprocessintfunc", x = zzz[, c(1:15, 60:70)])

#zzz[, c(1, 15, 71:81)] -> nt_MRI_gumbeltailintfunc
addWorksheet(fn_OUT, "nt_MRI_gumbeltailintfunc")
writeData(fn_OUT, sheet = "nt_MRI_gumbeltailintfunc", x = zzz[, c(1:15, 71:81)])

#zzz[, c(1, 15, 82:92)] -> nt_MRI_gumbelquantilefunc
addWorksheet(fn_OUT, "nt_MRI_gumbelquantilefunc")
writeData(fn_OUT, sheet = "nt_MRI_gumbelquantilefunc", x = zzz[, c(1:15, 82:92)])

#zzz[, c(1, 15, 93:103)] -> nt_MRI_POT-Poisson-GPD
addWorksheet(fn_OUT, "nt_MRI_POT-Poisson-GPD")
writeData(fn_OUT, sheet = "nt_MRI_POT-Poisson-GPD", x = zzz[, c(1:15, 93:103)])


#Structural Reliability
#nt_pot_lmon_gum -> nt_pot_lmon_gum
addWorksheet(fn_OUT, "nt_pot_lmon_gum")
writeData(fn_OUT, sheet="nt_pot_lmon_gum", x=nt_pot_lmon_gum)

#nt_pot_lmon_wei
addWorksheet(fn_OUT, "nt_pot_lmon_wei")
writeData(fn_OUT, sheet="nt_pot_lmon_wei", x=nt_pot_lmon_wei)

#nt_pot_lmom_ln3
addWorksheet(fn_OUT, "nt_pot_lmom_ln3")
writeData(fn_OUT, sheet="nt_pot_lmom_ln3", x=nt_pot_lmom_ln3)

#nt_pot_lmom_gpa
addWorksheet(fn_OUT, "nt_pot_lmom_gpa")
writeData(fn_OUT, sheet="nt_pot_lmom_gpa", x=nt_pot_lmom_gpa)


#nt_pot_lmom_exp
addWorksheet(fn_OUT, "nt_pot_lmom_exp")
writeData(fn_OUT, sheet="nt_pot_lmom_exp", x=nt_pot_lmom_exp)

#nt_pot_lmom_gam
addWorksheet(fn_OUT, "nt_pot_lmom_gam")
writeData(fn_OUT, sheet="nt_pot_lmom_gam", x=nt_pot_lmom_gam)

#nt_pot_lmom_kap
addWorksheet(fn_OUT, "nt_pot_lmom_kap")
writeData(fn_OUT, sheet="nt_pot_lmom_kap", x=nt_pot_lmom_kap)

#nt_pot_lmom_gev
addWorksheet(fn_OUT, "nt_pot_lmom_gev")
writeData(fn_OUT, sheet="nt_pot_lmom_gev", x=nt_pot_lmom_gev)

#nt_pot_mle_gp
addWorksheet(fn_OUT, "nt_pot_mle_gp")
writeData(fn_OUT, sheet="nt_pot_mle_gp", x=nt_pot_mle_gp)

#nt_ym_mle_gev
addWorksheet(fn_OUT, "nt_ym_mle_gev")
writeData(fn_OUT, sheet="nt_ym_mle_gev", x=nt_ym_mle_gev)

#nt_ym_lmom_gev
addWorksheet(fn_OUT, "nt_ym_lmom_gev")
writeData(fn_OUT, sheet="nt_ym_lmom_gev", x=nt_ym_lmom_gev)
#
#Thunderstorm
#t_pot_lmon_gum -> t_pot_lmon_gum
addWorksheet(fn_OUT, "t_pot_lmon_gum")
writeData(fn_OUT, sheet="t_pot_lmon_gum", x=t_pot_lmon_gum)

#t_pot_lmon_wei
addWorksheet(fn_OUT, "t_pot_lmon_wei")
writeData(fn_OUT, sheet="t_pot_lmon_wei", x=t_pot_lmon_wei)

#t_pot_lmom_ln3
addWorksheet(fn_OUT, "t_pot_lmom_ln3")
writeData(fn_OUT, sheet="t_pot_lmom_ln3", x=t_pot_lmom_ln3)

#t_pot_lmom_gpa
addWorksheet(fn_OUT, "t_pot_lmom_gpa")
writeData(fn_OUT, sheet="t_pot_lmom_gpa", x=t_pot_lmom_gpa)


#t_pot_lmom_exp
addWorksheet(fn_OUT, "t_pot_lmom_exp")
writeData(fn_OUT, sheet="t_pot_lmom_exp", x=t_pot_lmom_exp)

#t_pot_lmom_gam
addWorksheet(fn_OUT, "t_pot_lmom_gam")
writeData(fn_OUT, sheet="t_pot_lmom_gam", x=t_pot_lmom_gam)

#t_pot_lmom_kap
addWorksheet(fn_OUT, "t_pot_lmom_kap")
writeData(fn_OUT, sheet="t_pot_lmom_kap", x=t_pot_lmom_kap)

#t_pot_lmom_gev
addWorksheet(fn_OUT, "t_pot_lmom_gev")
writeData(fn_OUT, sheet="t_pot_lmom_gev", x=t_pot_lmom_gev)

#t_pot_mle_gp
addWorksheet(fn_OUT, "t_pot_mle_gp")
writeData(fn_OUT, sheet="t_pot_mle_gp", x=t_pot_mle_gp)

#t_ym_mle_gev
addWorksheet(fn_OUT, "t_ym_mle_gev")
writeData(fn_OUT, sheet="t_ym_mle_gev", x=t_ym_mle_gev)

#t_ym_lmom_gev
addWorksheet(fn_OUT, "t_ym_lmom_gev")
writeData(fn_OUT, sheet="t_ym_lmom_gev", x=t_ym_lmom_gev)
#Structural Reliability
#Structural Reliability Fitdistplus
#
#nt_pot_fdp_gum -> nt_pot_fdp_gum
addWorksheet(fn_OUT, "nt_pot_fdp_gum")
writeData(fn_OUT, sheet="nt_pot_fdp_gum", x=nt_pot_fdp_gum)

#t_pot_fdp_gum -> t_pot_fdp_gum
addWorksheet(fn_OUT, "t_pot_fdp_gum")
writeData(fn_OUT, sheet="t_pot_fdp_gum", x=t_pot_fdp_gum)
#
#nt_pot_fdp_gum_gof -> nt_pot_fdp_gum_gof
addWorksheet(fn_OUT, "nt_pot_fdp_gum_gof")
writeData(fn_OUT, sheet="nt_pot_fdp_gum_gof", x=nt_pot_fdp_gum_gof)

#t_pot_fdp_gum_gof -> t_pot_fdp_gum_gof
addWorksheet(fn_OUT, "t_pot_fdp_gum_gof")
writeData(fn_OUT, sheet="t_pot_fdp_gum_gof", x=t_pot_fdp_gum_gof)
#
#nt_pot_fdp_gof_short -> nt_pot_fdp_gof_short
addWorksheet(fn_OUT, "nt_pot_fdp_gof_short")
writeData(fn_OUT, sheet="nt_pot_fdp_gof_short", x=nt_pot_fdp_gof_short)


the_names = nt_pot_fdp_gof_short[, "station"]
the_names = paste0("station_", the_names)
nt_pot_fdp_gof_short_t = t(nt_pot_fdp_gof_short)
#nt_pot_fdp_gof_short_t = as.data.frame(nt_pot_fdp_gof_short_t)
#nt_pot_fdp_gof_short_t <- sapply(nt_pot_fdp_gof_short_t, as.numeric)
#nt_pot_fdp_gof_short_t = cbind(colnames(nt_pot_fdp_gof_short), nt_pot_fdp_gof_short_t)
colnames(nt_pot_fdp_gof_short_t) = the_names
#nt_pot_fdp_gof_short_t -> nt_pot_fdp_gof_short_t
addWorksheet(fn_OUT, "nt_pot_fdp_gof_short_t")
writeData(fn_OUT, sheet="nt_pot_fdp_gof_short_t", x=nt_pot_fdp_gof_short_t, rowNames = T, colNames = T)

#t_pot_fdp_gof_short -> t_pot_fdp_gof_short
addWorksheet(fn_OUT, "t_pot_fdp_gof_short")
writeData(fn_OUT, sheet="t_pot_fdp_gof_short", x=t_pot_fdp_gof_short)

the_names = t_pot_fdp_gof_short[, "station"]
the_names = paste0("station_", the_names)
t_pot_fdp_gof_short_t = t(t_pot_fdp_gof_short)
#t_pot_fdp_gof_short_t = as.data.frame(t_pot_fdp_gof_short_t)
#t_pot_fdp_gof_short_t[] <- lapply(t_pot_fdp_gof_short_t, function(x) as.numeric(x))
#t_pot_fdp_gof_short_t = cbind(colnames(t_pot_fdp_gof_short), t_pot_fdp_gof_short_t)
colnames(t_pot_fdp_gof_short_t) = the_names
rownames(t_pot_fdp_gof_short_t) = paste0()
#t_pot_fdp_gof_short_t -> t_pot_fdp_gof_short_t
addWorksheet(fn_OUT, "t_pot_fdp_gof_short_t")
writeData(fn_OUT, sheet="t_pot_fdp_gof_short_t", x=t_pot_fdp_gof_short_t, rowNames = T, colNames = T)

# 104-114: ------> Considering the dataset composed simultaneously by THUNDERSTORM and NON-THUNDERSTORM
#zzz[, c(1, 15, 104:114)] -> tnt_MRI_poissonprocessintfunc
addWorksheet(fn_OUT, "tnt_MRI_poissonprocessintfunc")
writeData(fn_OUT, sheet = "tnt_MRI_poissonprocessintfunc", x = zzz[, c(1:15, 104:114)])

# 115-136: ------> Test of Return Level Weights (Percentage) in the Combination of Hurricane and Non-Hurricane in POT-PP(equation 3, page 16, NIST.SP.500-301.pdf) : what percentage contributes hurricanes? and what percentage contributes non-hurricanes?
#zzz[, c(1, 15, 115:125)] -> t_percentage_pot_pp
addWorksheet(fn_OUT, "t_percentage_pot_pp")
writeData(fn_OUT, sheet = "t_percentage_pot_pp", x = zzz[, c(1:15, 115:125)])

#zzz[, c(1, 15, 126:136)] -> nt_percentage_pot_pp
addWorksheet(fn_OUT, "nt_percentage_pot_pp")
writeData(fn_OUT, sheet = "nt_percentage_pot_pp", x = zzz[, c(1:15, 126:136)])

#Save workbook to disk
saveWorkbook(fn_OUT, fn)

#write.table(zzz, file="fitted_model_result.csv", sep=",", row.names=FALSE, col.names=TRUE)
#print(paste("Total number of samples: ", (length(raw.data$date.time))))
#print(paste0("Maximum velocity in dataset: ", (max(raw.data$speed.kph))))
