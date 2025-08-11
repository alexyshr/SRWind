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
  }
}