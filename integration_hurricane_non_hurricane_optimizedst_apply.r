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
#(Sys.setenv(GDAL_MAX_BAND_COUNT=400000)
path <- "../data/"
ncname <- "10fg_jan1_2017" #To avoid loading outfile_nc4c_zip9.nc (too big)


#1) Create start object from netcdf file
ncfile = paste0(path, ncname, ".nc", sep = "")

#inputpath= paste0(path, "sanandres/")
#ncfiles = list.files(path=inputpath, pattern = "\\.nc$")
#ncfiles = paste0(inputpath, ncfiles)
#mync = read_stars(ncfiles, along = "time")#, proxy=TRUE)#, sub = "fg10")
mync = read_stars(ncfile)
variablename <- "fg10"
variablenumber <- 1 #the position of the variable inside netcdf file 
#(usefull for stars proxy which does not have the names in the object!)
names(mync) = variablename

#lon <- read_ncdf(ncfile, var = c("longitude"))
#nlon = dim(lon)
#lat <- read_ncdf(ncfile, var = c("latitude"))
#nlat = dim(lat)
#ntime <-  dim(read_ncdf(ncfile, var = c("time")))
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

#Id cell values from left to right (lf)
#lonlat.unstacklr <- expand.grid(lon=as.numeric(lon$longitude), lat=as.numeric(lat$latitude))
lonlat.unstacklr <- expand.grid(x=lon, y=lat)

lonlat.unstacklr$cellindexlr = as.integer(rownames(lonlat.unstacklr))
#get lon_index and lat_index
lonlat.unstacklr$lonindex = as.integer(lonlat.unstacklr$cellindexlr)%%nlon
lonlat.unstacklr$lonindex[lonlat.unstacklr$lonindex == 0] <- nlon
lonlat.unstacklr$latindex = ceiling(as.integer(lonlat.unstacklr$cellindex)/nlon)
lonlat.unstacklr$lonlatindex = paste0(lonlat.unstacklr$lonindex, "-",lonlat.unstacklr$latindex)

#Id cell values from top to bottom (tb)
#lonlat.unstacktb <- expand.grid(lat=as.numeric(lat$latitude), lon=as.numeric(lon$longitude))
lonlat.unstacktb <- expand.grid(y=lat, x=lon)
lonlat.unstacktb$cellindextb = as.integer(rownames(lonlat.unstacktb))
#get lon_index and lat_index
lonlat.unstacktb$latindex = as.integer(lonlat.unstacktb$cellindextb)%%nlat
lonlat.unstacktb$latindex[lonlat.unstacktb$latindex == 0] <- nlat
lonlat.unstacktb$lonindex = ceiling(as.integer(lonlat.unstacktb$cellindextb)/nlat)
lonlat.unstacktb$lonlatindex = paste0(lonlat.unstacktb$lonindex, "-",lonlat.unstacktb$latindex)

#Join indexes from tb in lf
matchIndexes = match(lonlat.unstacklr$lonlatindex, lonlat.unstacktb$lonlatindex)
lonlat.unstacklr$cellindextb = lonlat.unstacktb$cellindextb[matchIndexes]

lonlat.unstacklr = lonlat.unstacklr[ ,c(1:5,7)]
#Read POT-PP non hurricane return levels and add to era5colpoints
nhrl = read.xlsx(file="./ERA5/fitted_model_result.xlsx", sheetName = "nt_MRI_poissonprocessintfunc")
nhrl$station = as.integer(nhrl$station)

#The ERA5 isla process had the id from left to right 
#(it is not necessary to change order to join as previous ERA5 (based on top-bottom ids) )
#change the order in nhrl to match lonlat.unstacklr$cellindextb
#nhrl = nhrl[lonlat.unstacklr$cellindextb, ]
#So next line is redundant as it is already in that order (note using LEFT-RIGHT!)
#OJO que este si se hizo top-bottom, so change cellindexlr to cellindextb
nhrl = nhrl[lonlat.unstacklr$cellindextb, ]

nhrl  = nhrl[, c(1,6,7:17)]

theFieldNames = names(nhrl)
theFieldNames = c(theFieldNames[1:2], paste0("nh_", c(10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000)))
names(nhrl) = theFieldNames

#Pero lonlat.unstacklr no lo trabajé como top-bottom, así que toca ordenarlo
#lonlat.unstacklr = lonlat.unstacklr[lonlat.unstacklr$cellindextb, ]

#Once same order, then join both nhrl and lonlat.unstacklr 
lonlat.unstacklr = cbind(lonlat.unstacklr, nhrl)


#Get the indexes of the first column, 
#but using TOP-BOTTON (indexex) >> Open lonlat.unstacklr and check with cellindextb field
firstcolumindexes = nlat*(1:nlon)-(nlat-1)

#Get the indixes of the last column,
#but using TOP-BOTTON (indexex) >> Open lonlat.unstacklr and check with cellindextb field
lastcolumindexes = nlat*(1:nlon)

#Create point sf from unstacked array of ERA5 coordinates (WGS84)
era5colpoints = st_as_sf(lonlat.unstacklr, coords=1:2, crs=st_crs(4326))

#Transform to 3116 (Magna Colombia Bogotá)
#era5colpoints3116 = st_transform(era5colpoints, crs=3116)



#Convert ERA5 sf points to stars object, but:
#- each point at center of the cell
#- as cell value an unique index (cell index), corresponding to the row index of lonlat.unstacklr
#Cell shape for this netCDF file is a square: 0.25? x 0.25?

pointsbbox = st_bbox(era5colpoints)
cellsize = lonlat.unstacklr$x[2]- lonlat.unstacklr$x[1]
#cellsize = lonlat.unstacklr$lat[1]- lonlat.unstacklr$lat[2]
mybbox = st_bbox(c(pointsbbox$xmin - (cellsize/2), pointsbbox$xmax + (cellsize/2), pointsbbox$ymax + (cellsize/2), pointsbbox$ymin - (cellsize/2)), crs = st_crs(4326))
(isdcolraster.st = st_rasterize(era5colpoints, st_as_stars(mybbox, nx = nlon, ny = nlat, values = era5colpoints$cellindexlr)))
#isdcolraster.st3116 = st_transform(isdcolraster.st, crs=3116)
#isdcolraster.st3116 = st_warp(isdcolraster.st, crs=st_crs(3116))
#Avoid NA column and row because st_wrap function
#isdcolraster.st3116 = isdcolraster.st3116[,1:49,1:69]
#st_write(era5colpoints, "./rastersera5/era5grid_up_down.shp")
#write_stars(era5colraster.st, "./rastersera5/era5grid_up_down.tif")


#Convert stars to polygons geometry collection (not sf yet)
#era5_nh_pol.sf3116 = st_as_sfc(isdcolraster.st3116, as_points=FALSE, na.rm=TRUE, long=TRUE)
#class(era5_nh_pol.sf3116)
#st_make_valid(era5_nh_pol.sf3116)
#class(era5_nh_pol.sf3116)

era5_nh_pol.sfc = st_as_sfc(isdcolraster.st, as_points=FALSE, na.rm=TRUE, long=TRUE)
st_make_valid(era5_nh_pol.sfc)

#Find centroids coordinates from geometry collection
#pts <- do.call(rbind, st_centroid(st_geometry(era5_nh_pol.sf3116)))
#Calculate centroid coordinates in 'x' and 'y' variables
#x = pts[,1]
#y = pts[,2]

#Convert geometry collections to sf, adding more columns attributes (.., x, y)
#era5_nh_pol.sf3116 = st_sf(value=1:(nlon*nlat), x=x, y=y, geom = era5_nh_pol.sf3116)
#era5_nh_pol.sf3116 = st_sf(cellindexlr=era5colpoints$cellindexlr, cellindextb=era5colpoints$cellindextb, x=x, y=y, geom = era5_nh_pol.sf3116)
#era5_nh_pol.sf3116 = st_sf(lonlat.unstacklr, x=x, y=y, geometry = era5_nh_pol.sf3116)
era5_nh_pol.sf = st_sf(lonlat.unstacklr, geometry = era5_nh_pol.sfc)

#theFieldNames = names(era5_nh_pol.sf)
#theFieldNames = c(c("x", "y"), theFieldNames[3:9], paste0("nt_", c(10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000)), theFieldNames[21])
#names(era5_nh_pol.sf) = theFieldNames
#class(era5_nh_pol.sf3116)
#Once you have the sf object, you can add attributes with $
#era5_nh_pol.sf3116$index = 1:(nlon*nlat)
#era5_nh_pol.sf3116


#Convert sf non-hurricanes to stars non-hurricanes with x and y dimmensions (using column lon and lat), then write to disk!
era5_nh_stars_xy = st_as_stars(era5_nh_pol.sf[,c(1:19), drop=TRUE])


#Using stars capabilities (easy and short way)
mync$idlr = 1:(nlon*nlat)
era5_nh_pol.sf2 = st_as_sf(adrop(mync[,,,1]))
era5_nh_pol.sf2 = st_sf(lonlat.unstacklr, era5_nh_pol.sf2[, 1:2, drop=TRUE], geometry=era5_nh_pol.sf2$geometry)
era5_nh_stars.xy2 = st_as_stars(era5_nh_pol.sf2) #Geometry as only dimension
era5_nh_stars.xy2_1 = st_as_stars(era5_nh_pol.sf2[,1:19, drop=TRUE]) #x AND y as dimmensions
era5_nh_stars.xy2_3 = st_xy2sfc(era5_nh_stars.xy2_1, as_points = TRUE) #From x,y to geometry (points)
#era5_nh_stars.xy2_4 = st_sfc2xy(era5_nh_stars.xy2_3) #Cambiar from geometry (points) to X and Y
#era5_nh_stars_xy2_1 = st_sfc2xy(era5_nh_stars.xy2_3) #Cambiar from geometry (points) to X and Y

#Check with plot
era5_nh_stars.xy2_1.bands = merge(era5_nh_stars.xy2_1, name = "band")
ggplot()+
  geom_stars(data=era5_nh_stars.xy2_1.bands[1, , ,7:17]) + #, downsample = c(10, 10, 1)) + 
  facet_wrap("band") + 
  scale_fill_viridis() +
  coord_equal() +
  theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))

plot(era5_nh_stars.xy2_1["nh_10"])

#Save TIF of non-hurricanes to the disk!
conse = 0
for (attname in names(era5_nh_stars.xy2_1)){#[c(1:3, 5, 8:18)]){#Already removed the string attribute lonlatindex
  conse = conse + 1
  #maxof_stars_hurric
  #st = st_rasterize(sf, st_as_stars(st_bbox(rl_nh_sf_10$geometry), nx = nlon, ny = nlat))
  st = era5_nh_stars.xy2_1[attname]
  #write_stars(st, "./rastersera5_2020/nh_10.tif")
  write_stars(st, paste0("./ERA5/nh_rasters/", "st", conse, "_nh__", attname, ".tif"))
}


#era5_nh_stars_xy.st3116 = st_as_stars(era5_nh_pol.sf3116[,c(25:26,1:24), drop=TRUE])#, dimmensions=c("x", "y"))

#Up to here with DATA PROFESSOR LOMBARDO. That USA area does not INTERSECT HURRICANE DATA
#No Finish here (THIS WAS DONE BY MISTAKE THINKING DATA WAS FROM ISLA)

#Read non-hurricane rasters 4326
rasterfilesera5 = list.files(path="./ERA5/nh_rasters/", pattern = "*.*.tif$")
rasterfilesera5 = paste0("./ERA5/nh_rasters/", rasterfilesera5)
rasterfilesera5 = str_sort(rasterfilesera5, numeric = TRUE)
nh_st = read_stars(rasterfilesera5, quiet = TRUE)
st_crs(nh_st) = 4326
mynames = names(nh_st)
start = str_locate(mynames, "__")
stop = str_locate(mynames, "\\.")
mynames = str_sub(mynames, start=start[,2]+1, end=stop[,1]-1)
nh_st = setNames(nh_st, mynames)
nlon = attr(nh_st, "dimensions")$x$to
nlat = attr(nh_st, "dimensions")$y$to
nh_st$id = 1:(nlon*nlat)
nh_st_long= st_xy2sfc(nh_st, as_points = FALSE) #From stars x, y dimensions to geometry dimension!
nh_sf = st_as_sf(nh_st_long) #from stars geometry dimension  to sf



#Read hurrican from disk!
#Read hurrican rasters ingenier (with grd there is a problem in return level 500 file COL_CV-WIND_TR500_INT1.grd)
#Read hurrican rasters uniandes

#rasterfiles = list.files(path="../data/uniandes/", pattern = "TR.*.grd$")
#rasterfiles = paste0("../data/ingeniar/",rasterfiles)
#If you want to craeate the TIF version of GRD
#Fix coordinate systems and save to disk
# lapply(rasterfiles, function(x){
#   namenoextension = substr(x, start=1, stop=nchar(x)-4)
#   stars_hurric = read_stars(x, quiet = TRUE)
#   st_crs(stars_hurric) = 4326
#   write_stars(stars_hurric, paste0(namenoextension,".tif"))
# })

rasterfiles = list.files(path="../data/uniandes/", pattern = "TR.*.grd$")
rasterfiles = paste0("../data/uniandes/",rasterfiles)
rasterfiles = rasterfiles[c(1,6,9,2,7,10,12,3,5,8,11,4)]
stars_hurric = read_stars(rasterfiles, quiet = TRUE)
stars_hurric = setNames(stars_hurric, c("h_10", "h_25", "h_50", "h_100", "h_250",
                                        "h_500", "h_700", "h_1000", "h_1700", "h_2500", "h_5000", "h_10000"))
st_crs(stars_hurric) = 4326
#stars_hurric_3116 = st_warp(stars_hurric, crs=st_crs(3116))
#st_crs(stars_hurric_3116) = 3116
#stars_hurric.st3116 = st_warp(stars_hurric, crs=st_crs(3116))
#stars_hurric.st3116 = stars_hurric.st3116[,1:1548,1:1106]

#st_crs(stars_hurric) = 4326
#write_stars(stars_hurric, "D:/Rthesis/data/ingeniar/COL_CV-WIND_TR10_INT1_1.tif")

# rasterfiles = list.files(path="D:/Rthesis/data/ingeniar/", pattern = "COL.*.tif$")
# rasterfiles = paste0("D:/Rthesis/data/ingeniar/",rasterfiles)
# stars_hurric = read_stars(rasterfiles, quiet = TRUE)
# #
# #
# stars_hurric = read_stars(rasterfiles[8], quiet = TRUE)
# stars_hurric = read_stars(rasterfiles[c(1:8,10:11)], quiet = TRUE)
# stars_hurric = read_stars(rasterfiles[c(8:9)], quiet = TRUE)



#Calculation of zonal statistics (MAXIMUN) in hurricane maps using polygons of ERA5
#maxof_stars_hurric.st3116 = aggregate(stars_hurric.st3116, era5_nh_pol.sf3116$geometry, max) #exact=TRUE
maxof_stars_hurric = aggregate(stars_hurric, nh_sf$geometry, max) #exact=TRUE

#From stars to sf
maxof_sf_hurric = st_as_sf(maxof_stars_hurric, as_points=FALSE, merge =FALSE)

maxof_sf_hurric$id = 1:(nlon*nlat)
#Plot to see ID order
#ggplot() +
#  geom_sf(data = maxof_sf_hurric, colour = "red", fill = NA) +
#  geom_sf_label(data = maxof_sf_hurric, aes(label = id))
#plot(maxof_sf_hurric["id"])
#plot(maxof_stars_hurric[1,])

#Create again stars but with x and y dimmensions
maxof_df_hurric = cbind(lonlat.unstacklr[,1:2], maxof_sf_hurric[,1:12,drop=TRUE])
maxof_stars_hurric2 = st_as_stars(maxof_df_hurric)
st_crs(maxof_stars_hurric2) = 4326

#Check if before to write disk
maxof_stars_hurric2_bands = merge(maxof_stars_hurric2, name="band")
ggplot()+
  geom_stars(data=maxof_stars_hurric2_bands)+
  facet_wrap("band")
plot(maxof_stars_hurric[2,])#, extent = x)

#Write MAX (based on cells of ERA5-non hurricanes) of hurricanes to disk
for (attname in names(maxof_stars_hurric2)){
  #maxof_stars_hurric
  #st = st_rasterize(sf, st_as_stars(st_bbox(rl_nh_sf_10$geometry), nx = nlon, ny = nlat))
  st = maxof_stars_hurric2[attname]
  #write_stars(st, "./ERA5/h_rasters/nh_10.tif")
  write_stars(st, paste0("./ERA5/h_rasters/", "max_", attname, ".tif"))
}



#maxof_sf_hurric.sf3116 = st_as_sf(maxof_stars_hurric.st3116)
#maxof_sf_hurric = st_as_sf(maxof_stars_hurric)

#max_h = st_as_stars(maxof_sf_hurric.sf3116)

#Hurrican and Non-Hurricane Together

#h_nh.st3116 = c(era5_nh_stars.st3116, maxof_stars_hurric.st3116, along=1)
#h_nh.st3116_2 = c(max_h, era5_nh_stars.st3116, along=1)

#era5_nh_df = era5_nh_pol.sf3116 %>% as.data.frame %>% select(1:26)
era5_nh_df = era5_nh_pol.sf %>% as.data.frame %>% dplyr::select(1:19)

#era5_nh_df = era5_nh_pol.sf3116[, 1:26, drop=TRUE]

#integrateddf = cbind(era5_nh_df, maxof_sf_hurric.sf3116[, 1:11, drop=TRUE])
nh_h_df = cbind(era5_nh_df, maxof_sf_hurric[, c(1:12,14), drop=TRUE])

#Leave only relevant information
nh_h_df = nh_h_df[, c(1:3,9:31)]
#Creat stars object with combined (hurricane and non-hurricane), using x and y dimensions
nh_h_st = st_as_stars(nh_h_df)
st_crs(nh_h_st) = 4326


# nh_h_st$c_10 = NA
# nh_h_st$c_20 = NA
# nh_h_st$c_50 = NA
# nh_h_st$c_100 = NA
# nh_h_st$c_250 = NA
# nh_h_st$c_500 = NA
# nh_h_st$c_700 = NA
# nh_h_st$c_1000 = NA
# nh_h_st$c_1700 = NA
# nh_h_st$c_3000 = NA
# nh_h_st$c_7000 = NA

#Change attributes to dimensions "band"
nh_h_st_bands = merge(nh_h_st, name="band")
names(nh_h_st_bands) = "wind"

ggplot()+
  geom_stars(data= nh_h_st_bands[1, , ,2:24]) + 
  facet_wrap("band") +
  scale_fill_viridis() +
  coord_equal() +
  theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))

ggplot()+
  geom_stars(data= nh_h_st_bands[1, , ,2])

#h_nh_c.sf3116 = st_sf(integrateddf, geometry=era5_nh_pol.sf3116$geometry)
nh_h_sf = st_sf(nh_h_df, geometry=era5_nh_pol.sf$geometry)

#summary(h_nh_c.sf3116$h_10)

#names(nh_h_st)
#[1] "cellindexlr" "nh_10"       "nh_20"       "nh_50"       "nh_100"      "nh_250"      "nh_500"      "nh_700"     
#[9] "nh_1000"     "nh_1700"     "nh_3000"     "nh_7000"     "h_10"        "h_25"        "h_50"        "h_100"      
#[17] "h_250"       "h_500"       "h_700"       "h_1000"      "h_1700"      "h_2500"      "h_5000"      "h_10000"

#Function to combine hurricane (h_) and non-hurricane (nh_)

#New feature based on stars object
#This function need to be applied to st_apply
mycuenta = 0
combined_columns_st_apply <- function(st=nh_h_st_bands, 
                             bands_rl_h = c(13:24), mri_h = c(10,25,50,100,250,500,700,1000,1700,2500,5000,10000),
                             bands_rl_nh = c(2:12), mri_nh = c(10,20,50,100,250,500,700,1000,1700,3000,7000),
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
    #Para cada celda armo el data frame con los return levels de huracanes en X y las probabilidades en y
    #data frame with rl and probabilities for hurricanes (for each cell)
    rl_h = NULL
    for (band in bands_rl_h) {
      rl_h = c(rl_h, st[band])  #st[]: the value of each cell in st for specific band (previous attribute with same order)
                                      #why? because this function will be used in st_apply (be aware of previous merge: attributes to band dimension)
    }

    #Check data with problems in non-hurricane dataset
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
      rl_nh = c(rl_nh, st[band])  #st[]: the value of each cell in st for specific band (previous attribute with same order)
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
  return (rl_c_output)
}

mri_c = c(10,20,50,100,250,500,700,1000,1700,3000,7000)

combined_st = st_apply(nh_h_st_bands, c("x", "y"), combined_columns_st_apply, 
                                bands_rl_h = c(13:24), mri_h = c(10,25,50,100,250,500,700,1000,1700,2500,5000,10000),
                                #bands_rl_h = c(13:23), mri_h = c(10,25,50,100,250,500,700,1000,1700,2500,5000),
                                bands_rl_nh = c(2:12), mri_nh = c(10,20,50,100,250,500,700,1000,1700,3000,7000), 
                                mri_c = mri_c)

ggplot()+
  geom_stars(data=combined_st) + 
  facet_wrap("combined_columns_st_apply")
  
#Return from dimension combined_columns_st_apply to attributes c_*
combined_st = split(combined_st, "combined_columns_st_apply")
names(combined_st) = paste0("c_", mri_c)

#Write combined to disk!
count = 0
for (attname in names(combined_st)){
  if (attname != "lonlatindex"){
    count = count + 1
    #maxof_stars_hurric
    #st = st_rasterize(sf, st_as_stars(st_bbox(rl_nh_sf_10$geometry), nx = nlon, ny = nlat))
    st = combined_st[attname]
    #write_stars(st, "./ERA5_isla/c_rasters/nh_10.tif")
    write_stars(st, paste0("./ERA5/c_rasters/", "st", as.character(count), "__", attname, ".tif"))
  }
}  

#It is possible to integrate all together
integrated_st = c(nh_h_st, combined_st)


#Write to disk all the result
count = 0
for (attname in names(integrated_st)){
  if (attname != "lonlatindex"){
    count = count + 1
    #maxof_stars_hurric
    #st = st_rasterize(sf, st_as_stars(st_bbox(rl_nh_sf_10$geometry), nx = nlon, ny = nlat))
    st = integrated_st[attname]
    #write_stars(st, "./ERA5_isla/all_rasters/nh_10.tif")
    write_stars(st, paste0("./ERA5/all_rasters/", "st", as.character(count), "__", attname, ".tif"))
  }
}

#Convert to start whitout x and y dimensions, only geometry dimension
#h_nh_c.stnoxy = st_xy2sfc(h_nh_c.st, as_points=FALSE, na.rm = TRUE)


#h_nh_c.st3116["nh_10"]
#nh10sf = st_as_sf(h_nh_c.st3116["nh_10"])
#nh10st = st_as_stars(nh10sf)
##nh10st2 = st_sfc2xy(nh10st)
#nh10st2 = st_xy2sfc(nh10st, as_points=FALSE, na.rm = TRUE)


#Another way to export using sf data (select, rasterize and write_stars)
#rl_nh_sf_10 = h_nh_c.sf3116[, "nh_10"]
#rl_nh_st_10 = st_rasterize(rl_nh_sf_10, st_as_stars(st_bbox(rl_nh_sf_10$geometry), nx = nlon, ny = nlat))
#rl_nh_st_10 = st_xy2sfc(rl_nh_st_10, as_points=FALSE, na.rm = TRUE)
#write_stars(rl_nh_st_10, "./rastersera5_2020/nh_10.tif")

#Write to disk all non-hurricanes
#attributesnames = c("nh_10", "nh_20", "nh_50", "nh_100", "nh_250",
#                    "nh_500", "nh_700", "nh_1000", "nh_1700", "nh_3000", "nh_7000")
# for (attname in attributesnames){
#   sf = h_nh_c.sf3116[, attname]
#   st = st_rasterize(sf, st_as_stars(st_bbox(rl_nh_sf_10$geometry), nx = nlon, ny = nlat))
#   #write_stars(st, "./rastersera5_2020/nh_10.tif")
#   write_stars(st, paste0("./rastersera5_2020/", attname, ".tif"))
# }

#attributesnames = c("nh_700", "nh_1700", "nh_3000")
#plot(h_nh_c.stnoxy[attributesnames,], border=NA)


#attributesnames = c("c_700", "c_1700", "c_3000")
#plot(h_nh_c.stnoxy[attributesnames,], border=NA)
#(rl_nonhurricanes_4326_10_st = st_rasterize(rl_nonhurricanes_4326_10, st_as_stars(mybbox, nx = nlon, ny = nlat)))
#write_stars(rl_nonhurricanes_4326_10_st, "./rastersera5/rl_nonhurricanes_4326_10_st.tif")
