library(reprex)


reprex({
  library(stars)
  Sys.setenv(GDAL_MAX_BAND_COUNT=1000000)
  #get NETCDF file
  myurl = "http://geocorp.co/wind/isla_fg10_1979-2019.zip"
  filename = "isla_fg10_1979-2019.zip"
  download.file(myurl, filename, mode = "wb")
  unzip(filename)
  path <- "./"
  #ncname <- "usafl_fg10_2007-2019"
  ncnames = list.files(path, pattern= "*.nc")
  ncfiles = paste0(path, ncnames, sep = "")
  
  #read_stars
  st_proxy_rs = read_stars(ncfiles[1], along = "time", proxy=TRUE)
  st_proxy_rs
  attr(st_proxy_rs, "dimensions")
  #Filter to have only one date
  st_proxy_rs = st_proxy_rs[ , , ,1]
  st_proxy_rs
  class(st_proxy_rs) #stars_proxy object
  st_proxy_rs %>% slice(time, 1) -> st_proxy_rs
  st_proxy_rs %>% filter(time = 1) -> st_proxy_rs
  
  #convert to stars no proxy
  st_proxy_rs = st_as_stars(st_proxy_rs)
  #Convert to sf (points and polygons)
  st_proxy_rs_poi = st_as_sf(st_proxy_rs, as_points = TRUE) # Not working
  st_proxy_rs_pol = st_as_sf(st_proxy_rs, as_points = FALSE) # Not working
  
  #read_ncdf
  st_proxy_rn = read_ncdf(ncfiles[1], along = "time", proxy=TRUE)
  st_proxy_rn
  attr(st_proxy_rn, "dimensions")
  #Filter to have only one date
  st_proxy_rn = st_proxy_rn[ , , ,1]
  st_proxy_rn
  class(st_proxy_rn) #No stars_proxy object
  #Convert to sf (points and polygons)
  st_proxy_rn_poi = st_as_sf(st_proxy_rn, as_points = TRUE)
  st_proxy_rn_pol = st_as_sf(st_proxy_rn, as_points = FALSE)
  
  #read_mdim (experimental)
  st_rm = stars:::read_mdim(ncfiles[1]) 
  #Filter to have only one date
  units::units_options(allow_mixed = TRUE)
  #attr(st_rm, "dimensions")
  #st_rm = st_rm[ , , ,1] #Not working
  #library(dplyr)
  #st_rm %>% slice(time, 1) -> st_rm # Not working
  #longitude
  from = attr(st_rm, "dimensions")$longitude$from
  to = attr(st_rm, "dimensions")$longitude$to
  offset = attr(st_rm, "dimensions")$longitude$offset
  delta = attr(st_rm, "dimensions")$longitude$delta
  lon = seq(from=offset, length.out=to, by=delta)
  lon
  attr(st_rm, "dimensions")$longitude$values = lon
  
  #latitude
  from = attr(st_rm, "dimensions")$latitude$from
  to = attr(st_rm, "dimensions")$latitude$to
  offset = attr(st_rm, "dimensions")$latitude$offset
  delta = attr(st_rm, "dimensions")$latitude$delta
  lat = seq(from=offset, length.out=to, by=delta)
  lat
  attr(st_rm, "dimensions")$latitude$values = lat
  st_rm = st_rm[ , , ,1]
  st_rm %>% slice(time, 1) -> st_rm
  st_rm %>% filter(time = 1) -> st_rm
  
},  wd = "./issues2/", session_info=TRUE, style=TRUE)


# clean up
file.remove(
  list.files(path="./issues/", full.names = TRUE)
)


reprex({
  library(stars)
  Sys.setenv(GDAL_MAX_BAND_COUNT=1000000)
  #get NETCDF file
  myurl = "http://geocorp.co/wind/usafl_fg10_2007-2019.zip"
  filename = "usafl_fg10_2007-2019.zip"
  download.file(myurl, filename, mode = "wb")
  unzip(filename)
  path <- "./"
  ncname <- "usafl_fg10_2007-2019"
  ncfile = paste0(path, ncname, ".nc", sep = "")
  my_mdim = stars:::read_mdim(ncfile, proxy=TRUE) #proxy is not implemented!
  my_mdim
  class(my_mdim)
  my_ndim2_selection = my_mdim[1, 1, 1, 1:105144]
  my_ndim2_selection
},  wd = "./issues2/", session_info=TRUE, style=TRUE)


#
library(stars)
Sys.setenv(GDAL_MAX_BAND_COUNT=1000000)
path <- "./issues2/"
ncname <- "usafl_fg10_2007-2019"
ncfile = paste0(path, ncname, ".nc", sep = "")
my_mdim = stars:::read_mdim(ncfile, proxy=TRUE) #proxy is not implemented!
my_mdim
class(my_mdim)

my_ndim2_selection = my_mdim[1, 1, 1, 1:105144]
my_ndim2_selection


my_mdim = stars:::read_mdim(ncfile, proxy=TRUE)
class(my_mdim)

path2 <- "../../data_srwind/"
ncname2 <- "outfile_nc4c_zip9"
ncfile2 = paste0(path2, ncname2, ".nc", sep = "")
my_mdim2 = stars:::read_mdim(ncfile2, proxy=TRUE)
my_ndim2_selection = my_mdim2[1, 1, 1, 1:105144]

my_mdim2
class(my_mdim2)


