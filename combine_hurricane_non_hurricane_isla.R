library(stars)
library(sf)
library(lubridate)
library(xts)
library(ncdf4)
library(lubridate)
library(tidyverse)
library(xlsx)
library(stringr)
library(ggplot2)
library(viridis)
library(ggthemes)
library(tibble)
library(openxlsx)

input_path= "../../data_srwind/isla/"

# ncname <- "10fg_jan1_2017"
ncname <- "isla_fg10"

output_path <- "../../data_srwind/isla/out/"

#Read NETCDF file and get dataframe with indexes and coordinates of all cells
# ncfiles = list.files(path=input_path, pattern = "\\.nc$")
# ncfiles = paste0(input_path, ncfiles)
# mync = read_stars(ncfiles, along = "time", proxy=TRUE)


ncfile = paste0(input_path, ncname, ".nc")
mync = stars:::read_mdim(ncfile)
#mync = read_ncdf(ncfile)

variablename <- "fg10"
variablenumber <- 1 #the position of the variable inside netcdf file 
#(usefull for stars proxy which does not have the names in the object!)
names(mync) = variablename

variablename <- "fg10"
names(mync) = variablename
lon <- seq(from=attr(mync, "dimensions")$longitude$offset, 
           to=attr(mync, "dimensions")$longitude$offset + (attr(mync, "dimensions")$longitude$delta * (attr(mync, "dimensions")$longitude$to-1)),
           by = attr(mync, "dimensions")$longitude$delta) #Note that in to is minus 1
nlon = dim(mync)[1]
lat <- seq(from=attr(mync, "dimensions")$latitude$offset, 
           to=attr(mync, "dimensions")$latitude$offset + (attr(mync, "dimensions")$latitude$delta * (attr(mync, "dimensions")$latitude$to-1)),
           by = attr(mync, "dimensions")$latitude$delta) #Note that in to is minus 1

nlat = dim(mync)[2]
ntime = dim(mync)[3]
lonlat.unstacklr <- expand.grid(x=lon, y=lat)
lonlat.unstacklr$cellindexlr = as.integer(rownames(lonlat.unstacklr))
#get lon_index and lat_index
lonlat.unstacklr$lonindex = as.integer(lonlat.unstacklr$cellindexlr)%%nlon
lonlat.unstacklr$lonindex[lonlat.unstacklr$lonindex == 0] <- nlon
lonlat.unstacklr$latindex = ceiling(as.integer(lonlat.unstacklr$cellindex)/nlon)
lonlat.unstacklr$lonlatindex = paste0(lonlat.unstacklr$lonindex, "-",lonlat.unstacklr$latindex)
lonlat.unstacklr

#Poisson Process Intensity Function
#getSheetNames(system.file("extdata", "readTest.xlsx", package = "openxlsx"))

#Read non-hurricane return levels from Excel File (Output of pot_pp_ERA5_isla.r)
nhrl_file = paste0("../../ERA5_isla/", "fitted_model_result.xlsx")
nhrl = openxlsx::read.xlsx(xlsxFile=nhrl_file, sheet="nt_MRI_poissonprocessintfunc")
nhrl$station = as.integer(nhrl$station)
nhrl
colnames(nhrl)
#Leave only the return levels
# nhrl  = nhrl[, c(1:13)]
# colnames(nhrl)
#Change column names
theFieldNames = names(nhrl)
theFieldNames = c(theFieldNames[1:15], paste0("nh_", c(10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000)))
names(nhrl) = theFieldNames
colnames(nhrl)


#
#Filter rows of lonlat.unstacklr to have only cells with nhrl$station (filter stations to study)
lonlat.unstacklr.filter <- lonlat.unstacklr %>% filter(cellindexlr %in% nhrl$station)

#Join both dataframes (by columns: add more columns, preserve rows)
#
lonlat.unstacklr.filter = cbind(lonlat.unstacklr.filter, nhrl)

#Create simple features points at the center of analyzed cells
era5colpoints = st_as_sf(lonlat.unstacklr.filter, coords=1:2, crs=st_crs(4326))

#
#units::units_options(allow_mixed = TRUE)
tiff_file = paste0(input_path, "h_rasters/max_h_10.tif")
back = read_stars(tiff_file)
#Check results
plot(back[1], reset=FALSE, main="Structural Relliability")
#plot(st_as_sf(back), reset=FALSE, col=NA, border="gray", main="Structural Relliability")
plot(st_as_sf(back), add=TRUE, col=NA, border="blue")
plot(era5colpoints[1], add=TRUE, pch=20, col="red")
text(lonlat.unstacklr.filter$x, lonlat.unstacklr.filter$y, 
     lonlat.unstacklr.filter$station, cex=0.6, pos = 2)


#Read non-hurricane rasters 4326 (represent max value inside cells of ERA5)
rasterfilesera5 = list.files(path=paste0(input_path, "/h_rasters/"), pattern = "*.*.tif$")
rasterfilesera5 = paste0(input_path, "/h_rasters/", rasterfilesera5)
rasterfilesera5 = str_sort(rasterfilesera5, numeric = TRUE)
h_st = read_stars(rasterfilesera5, quiet = TRUE)
st_crs(h_st) = 4326
mynames = names(h_st)
start = str_locate(mynames, "_")
stop = str_locate(mynames, "\\.")
mynames = str_sub(mynames, start=start[,1]+1, end=stop[,1]-1)
h_st = setNames(h_st, mynames)
nlon = attr(h_st, "dimensions")$x$to
nlat = attr(h_st, "dimensions")$y$to
h_st$station = 1:(nlon*nlat)
h_st_long= st_xy2sfc(h_st, as_points = FALSE) #From stars x, y dimensions to geometry dimension!
h_sf = st_as_sf(h_st_long) #from stars geometry dimension  to sf

names(h_sf)

#Check results
plot(h_sf["h_10"], reset=FALSE, main="Non Hurricanes")
plot(era5colpoints[1], add=TRUE, pch=20, col="red")
text(lonlat.unstacklr.filter$x, lonlat.unstacklr.filter$y, 
     lonlat.unstacklr.filter$station, cex=0.6, pos = 2)


#Filter 
h_sf_filter <- h_sf %>% filter(station %in% nhrl$station)


#Join Hurricane and Non Hurricane in one Dataframe
h_nh = cbind(lonlat.unstacklr.filter, st_drop_geometry(h_sf_filter))






mycuenta = 0
# Load the function combined_columns_df_apply to combine hurricane & non_hurricane data frame
# This function need to be applied to st_apply
source("./code/function_lib.r")

mri_c = c(10,20,50,100,250,500,700,1000,1700,3000,7000)

combined_rl = apply(h_nh, 1, combined_columns_df_apply, 
                       bands_rl_h = c(33:44), mri_h = c(10,25,50,100,250,500,700,1000,1700,2500,5000,10000),
                       #bands_rl_h = c(13:23), mri_h = c(10,25,50,100,250,500,700,1000,1700,2500,5000),
                       bands_rl_nh = c(22:32), mri_nh = c(10,20,50,100,250,500,700,1000,1700,3000,7000), 
                       mri_c = mri_c) #1 means the rows of input dataframe

# df_combined <- t(combined_rl) %>%
#   as_tibble() %>%
#   setNames(paste0("c_", mri_c))

df_combined <- t(combined_rl) %>% as.data.frame()
colnames(df_combined) = paste0("c_", mri_c)


h_nh_c = cbind(h_nh, df_combined)

#Load final maps from Colombian Design Norm NSR_10
nsr700 = st_read(paste0("../../data_srwind/", "nsr10windmap_700.shp"))
nsr1700 = st_read(paste0("../../data_srwind/", "/nsr10windmap_1700.shp"))
nsr3000 = st_read(paste0("../../data_srwind/", "/nsr10windmap_3000.shp"))
#Join Velocity values of MRIs 700, 1700, 3000
era5colpoints = st_as_sf(h_nh_c, coords=1:2, crs=st_crs(4326), remove=FALSE)
#pt = st_join(era5colpoints, nsr700["MRI_700"], left=TRUE)
#pt = st_join(pt, nsr1700["MRI_1700"], left=TRUE)
#pt = st_join(pt, nsr3000["MRI_3000"], left=TRUE)

idx = st_nearest_feature(era5colpoints, nsr700["MRI_700"], check_crs = FALSE)
MRI_700 = st_drop_geometry(nsr700[idx,"MRI_700"])

idx = st_nearest_feature(era5colpoints, nsr1700["MRI_1700"], check_crs = FALSE)
MRI_1700 = st_drop_geometry(nsr1700[idx,"MRI_1700"])

idx = st_nearest_feature(era5colpoints, nsr3000["MRI_3000"], check_crs = FALSE)
MRI_3000 = st_drop_geometry(nsr3000[idx,"MRI_3000"])

h_nh_c = cbind(st_drop_geometry(era5colpoints), MRI_700, MRI_1700, MRI_3000)


#Delete file if it exists
exel_file <- paste0(output_path, "combined_h_nh_isla_pot_pp.xlsx")
if (file.exists(exel_file)) 
  file.remove(exel_file)

fn_out1 <- openxlsx::createWorkbook()
openxlsx::addWorksheet(fn_out1, "combined_h_nh")
openxlsx::writeData(fn_out1, sheet = "combined_h_nh", x = h_nh_c)
openxlsx::saveWorkbook(fn_out1, exel_file, overwrite = TRUE)
rm(fn_out1)
