library(reprex)


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
  mync_proxy = read_stars(ncfile, along = "time", proxy=TRUE)
  mync_proxy
  mync_proxy_selection = mync_proxy[1, 1, 1, 1:105144]
  mync_proxy_selection
  mync_noproxy = st_as_stars(mync_proxy_selection)
  mync_noproxy
},  wd = "./issues2/", session_info=TRUE, style=TRUE)

reprex({
  library(stars)
  #Sys.setenv(GDAL_MAX_BAND_COUNT=1000000)
  #get NETCDF file
  myurl = "http://geocorp.co/wind/usafl_fg10_2007-2019.zip"
  filename = "usafl_fg10_2007-2019.zip"
  download.file(myurl, filename, mode = "wb")
  unzip(filename)
  path <- "./"
  ncname <- "usafl_fg10_2007-2019"
  ncfile = paste0(path, ncname, ".nc", sep = "")
  mync_proxy = read_ncdf(ncfile, along = "time", proxy=TRUE)
  mync_proxy
  class(mync_proxy) #proxy=TRUE not working?
  mync_proxy_selection = mync_proxy[1, 1, 1, 1:105144]
  mync_proxy_selection
  mync_noproxy = st_as_stars(mync_proxy_selection)
  mync_noproxy
},  wd = "./issues2/", session_info=TRUE, style=TRUE)


# clean up
file.remove(
  list.files(path="./issues2/", full.names = TRUE)
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


