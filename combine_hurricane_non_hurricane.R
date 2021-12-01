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

input_path <- "../../data_srwind/"
# ncname <- "10fg_jan1_2017"
ncname <- "outfile_nc4c_zip9"

output_path <- "../../data_srwind/out/"

#Read NETCDF file and get dataframe with indexes and coordinates of all cells
ncfile = paste0(input_path, ncname, ".nc", sep = "")
mync = read_stars(ncfile, proxy=TRUE)
variablename <- "fg10"
names(mync) = variablename
lon <- seq(from=attr(mync, "dimensions")$x$offset, 
           to=attr(mync, "dimensions")$x$offset + (attr(mync, "dimensions")$x$delta * (attr(mync, "dimensions")$x$to-1)),
           by = attr(mync, "dimensions")$x$delta)
nlon = dim(mync)[1]
lat <- seq(from=attr(mync, "dimensions")$y$offset, 
           to=attr(mync, "dimensions")$y$offset + (attr(mync, "dimensions")$y$delta * (attr(mync, "dimensions")$y$to-1)),
           by = attr(mync, "dimensions")$y$delta) #Note that in to is minus 1

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
#Read non-hurricane return levels from Excel File (Output of pot_pp_ERA5_isla.r)
nhrl_file = paste0(input_path, "/fitted_model_result.xlsx")
nhrl = read.xlsx(xlsxFile=nhrl_file, sheet = "nt_MRI_poissonprocessintfunc")
nhrl$station = as.integer(nhrl$station)
nhrl
colnames(nhrl)
#Leave only the return levels
# nhrl  = nhrl[, c(1:13)]
# colnames(nhrl)
#Change column names
theFieldNames = names(nhrl)
theFieldNames = c(theFieldNames[1:2], paste0("nh_", c(10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000)))
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

#Check results
plot(st_as_sf(mync[1]), reset=FALSE, col=NA, border="gray", main="Structural Relliability")
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
plot(h_sf["h_10"], reset=FALSE, , main="Non Hurricanes")
plot(era5colpoints[1], add=TRUE, pch=20, col="red")
text(lonlat.unstacklr.filter$x, lonlat.unstacklr.filter$y, 
     lonlat.unstacklr.filter$station, cex=0.6, pos = 2)


#Filter 
h_sf_filter <- h_sf %>% filter(station %in% nhrl$station)

#Join Hurricane and Non Hurricane in one Dataframe
h_nh = cbind(lonlat.unstacklr.filter, st_drop_geometry(h_sf_filter))




#Create function to combine hurricane & non_hurricane dataframe
#This function need to be applied to st_apply
mycuenta = 0
combined_columns_st_apply <- function(st=h_nh, 
                                      bands_rl_h = c(20:31), mri_h = c(10,25,50,100,250,500,700,1000,1700,2500,5000,10000),
                                      bands_rl_nh = c(9:19), mri_nh = c(10,20,50,100,250,500,700,1000,1700,3000,7000),
                                      mri_c = c(10,20,50,100,250,500,700,1000,1700,3000,7000)) {
  mycuenta <<- mycuenta + 1
  contar = mycuenta
  assign("mycuenta", contar, envir = parent.frame() )
  print(contar)
  print(mycuenta)
  #if (mycuenta == 34) browser()
  
  rl600 = seq(from=1, to=600, by=1)
  
  extrapola <- function(x, y, xout){
    y=Hmisc::approxExtrap(x=x, y=y, xout=xout)$y
  }
  
  #Hurricanes
  #Para cada celda armo el data frame con los return levels de hurricanes en X y las probabilidades en y
  #data frame with rl and probabilities for hurricanes (for each cell)
  rl_h = NULL
  for (band in bands_rl_h) {
    rl_h = c(rl_h, as.double(st[band]))  #st[]: the value of each cell in st for specific band (previous attribute with same order)
    #why? because this function will be used in st_apply (be aware of previous merge: attributes to band dimension)
  }
  
  #Check data with problems in hurricane dataset
  if (any(!is.na(rl_h))){
    if (sum(rl_h) == 0 | sum(rl_h) < 0) {
      rl_h = rep(NA, length(bands_rl_h))
    }
  }    
  
  h = data.frame(rl=rl_h, prob = 1/mri_h)
  
  #create hurricane hazard curve (detailed >>> 1:600) !Here the error
  #if (is.na(h$rl[1])){ # check if hurricane data is NA
  if (any(is.na(h$rl)) | any(is.na(h$prob))){ # check if dataframe hurricane columns have NA values
    #only one NA value >>>> avoid extrapola!
    hc_h_prob = rep(NA, length(rl600))
  } else{
    hc_h_prob = extrapola(x=h$rl, y=h$prob, xout=rl600)  
  }
  
  hc_h = data.frame(rl=rl600, prob=hc_h_prob)
  #Do not forget to check extreme values in probs
  hc_h$prob[hc_h$prob < 0] = 0
  hc_h$prob[hc_h$prob > 1] = 1
  
  
  #Non-Hurricanes  
  #data frame with rl and probabilities for non-hurricanes (for each cell)
  rl_nh = NULL
  for (band in bands_rl_nh) {
    rl_nh = c(rl_nh, as.double(st[band]))  #st[]: the value of each cell in st for specific band (previous attribute with same order)
    #why? because this function will be used in st_apply (be aware of previous merge: attributes to band dimension)
  }
  
  #Check data with problems in non-hurricane dataset
  if (any(!is.na(rl_nh))){
    if (sum(rl_nh) == 0 | sum(rl_nh) < 0) {
      rl_nh = rep(NA, length(bands_rl_nh))
    }
  }
  
  h = data.frame(rl=rl_h, prob = 1/mri_h)
  
  nh = data.frame(rl=rl_nh, prob = 1/mri_nh)
  
  #create non-hurricane hazard curve (detailed >>> 1:600)
  if (any(is.na(nh$rl)) | any(is.na(nh$prob))){ # check if dataframe hurricane columns have NA values
    #only one NA value >>>> avoid extrapola!
    hc_nh_prob = rep(NA, length(rl600))
  } else{
    hc_nh_prob = extrapola(x=nh$rl, y=nh$prob, xout=rl600) #Not check for NA values because non-hurricane has to have rl value in all cells
  }
  
  hc_nh = data.frame(rl=rl600, prob=hc_nh_prob)
  #Do not forget to check extreme values in probs
  hc_nh$prob[hc_nh$prob < 0] = 0
  hc_nh$prob[hc_nh$prob > 1] = 1
  
  
  #Combined (detailed curve)
  #Previous to combine: 
  #1) nh can not be null in any cell
  #2) hurricane can be null
  
  #create combined hazard curve (detailed >>> 1:600)
  if (any(is.na(hc_h$prob)) | any(is.na(hc_h$rl))){
    #if (is.na(hc_h$prob[1])){ #Solo chequeo la primera probabilidad en el dataframe (corresponding to velocity 1)
    #check the hurricanes data.frame >> Be Aware hc_h
    hc_c_prob = hc_nh$prob
  } else {
    #pec = (1 - ((1-peh)*(1-penh)))
    hc_c_prob = (1 - ((1-hc_h$prob)*(1-hc_nh$prob)))     
  }
  hc_c = data.frame(rl=rl600, prob=hc_c_prob)
  #Do not forget to check extreme lower values in probs
  hc_c$prob[hc_c$prob<0] = 0
  
  #Interpolate rl in combined hazard curve based on 1/mri_output
  prob_c = 1/mri_c
  if (any(is.na(hc_c$prob)) | any(is.na(hc_h$rl))){
    rl_c_output = rep(NA, length(prob_c))
  } else {
    rl_c_output = extrapola(x=hc_c$prob, y=hc_c$rl, xout=prob_c)  
  }
  # myCount = 0
  # for (band in bands_rl_c) {
  #   myCount = myCount + 1
  #   st[band] = rl_c_output[myCount]
  # }
  return (rl_c_output)  # Note that returns return level by columns (not by rows)
                        # Then you need to transpose
}

mri_c = c(10,20,50,100,250,500,700,1000,1700,3000,7000)

combined_rl = apply(h_nh, 1, combined_columns_st_apply, 
                       bands_rl_h = c(20:31), mri_h = c(10,25,50,100,250,500,700,1000,1700,2500,5000,10000),
                       #bands_rl_h = c(13:23), mri_h = c(10,25,50,100,250,500,700,1000,1700,2500,5000),
                       bands_rl_nh = c(9:19), mri_nh = c(10,20,50,100,250,500,700,1000,1700,3000,7000), 
                       mri_c = mri_c) #1 means the rows of input dataframe

df_combined <- t(combined_rl) %>%
  as_tibble() %>%
  setNames(paste0("c_", mri_c))


h_nh_c = cbind(h_nh, df_combined)

#Delete file if it exists
exel_file <- paste0(output_path, "combined_h_nh.xlsx")
if (file.exists(exel_file)) 
  file.remove(exel_file)

fn_out1 <- createWorkbook()
addWorksheet(fn_out1, "combined_h_nh")
writeData(fn_out1, sheet = "combined_h_nh", x = h_nh_c)
saveWorkbook(fn_out1, exel_file, overwrite = TRUE)

