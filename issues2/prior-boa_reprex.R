#' ---
#' output:
#'   reprex::reprex_document:
#'     session_info: TRUE
#'     style: TRUE
#' ---

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
