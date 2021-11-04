library(reprex)


reprex({
  library(stars)
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
  #mync_noproxy
},  wd = "./issues/", session_info=TRUE)


# clean up
file.remove(
  list.files(path="./issues/", full.names = TRUE)
)
