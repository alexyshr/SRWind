``` r
library(stars)
#> Loading required package: abind
#> Loading required package: sf
#> Linking to GEOS 3.9.1, GDAL 3.2.1, PROJ 7.2.1
Sys.setenv(GDAL_MAX_BAND_COUNT = 1000000)
# get NETCDF file
myurl <- "http://geocorp.co/wind/usafl_fg10_2007-2019.zip"
filename <- "usafl_fg10_2007-2019.zip"
download.file(myurl, filename, mode = "wb")
unzip(filename)
path <- "./"
ncname <- "usafl_fg10_2007-2019"
ncfile <- paste0(path, ncname, ".nc", sep = "")
my_mdim <- stars:::read_mdim(ncfile, proxy = TRUE) # proxy is not implemented!
#> Warning in read_mdim(file, array_name, options): GDAL Message 1: Recode from
#> UTF-8 to CP_ACP failed with the error: "Invalid argument".
my_mdim
#> stars object with 3 dimensions and 1 attribute
#> attribute(s), summary of first 1e+05 cells:
#>            Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#> fg10  0.7361377 5.559857 7.224236 7.613486 9.177769 24.06575
#> dimension(s):
#>           from     to     offset     delta  refsys point
#> longitude    1      9 -102.6 [°]  0.25 [°]      NA    NA
#> latitude     1      9   40.5 [°] -0.25 [°]      NA    NA
#> time         1 105144         NA        NA POSIXct    NA
#>                                       values x/y
#> longitude                               NULL [x]
#> latitude                                NULL [y]
#> time      2007-01-01,...,2019-12-31 23:00:00
class(my_mdim)
#> [1] "stars"
my_ndim2_selection <- my_mdim[1, 1, 1, 1:105144]
#> Error in c.units(xd$offset, xd$delta, r$affine[1], yd$offset, r$affine[2], : units are not convertible, and cannot be mixed; try setting units_options(allow_mixed = TRUE)?
my_ndim2_selection
#> Error in eval(expr, envir, enclos): object 'my_ndim2_selection' not found
```

<sup>Created on 2021-11-05 by the [reprex package](https://reprex.tidyverse.org) (v2.0.1)</sup>

<details style="margin-bottom:10px;">
<summary>
Session info
</summary>

``` r
sessioninfo::session_info()
#> - Session info ---------------------------------------------------------------
#>  setting  value                       
#>  version  R version 4.1.1 (2021-08-10)
#>  os       Windows 10 x64              
#>  system   x86_64, mingw32             
#>  ui       RTerm                       
#>  language (EN)                        
#>  collate  English_United States.1252  
#>  ctype    English_United States.1252  
#>  tz       America/New_York            
#>  date     2021-11-05                  
#> 
#> - Packages -------------------------------------------------------------------
#>  package     * version date       lib source                          
#>  abind       * 1.4-5   2016-07-21 [1] CRAN (R 4.1.1)                  
#>  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.1.1)                  
#>  backports     1.2.1   2020-12-09 [1] CRAN (R 4.1.1)                  
#>  class         7.3-19  2021-05-03 [2] CRAN (R 4.1.1)                  
#>  classInt      0.4-3   2020-04-07 [1] CRAN (R 4.1.1)                  
#>  cli           3.0.1   2021-07-17 [1] CRAN (R 4.1.1)                  
#>  crayon        1.4.1   2021-02-08 [1] CRAN (R 4.1.1)                  
#>  DBI           1.1.1   2021-01-15 [1] CRAN (R 4.1.1)                  
#>  digest        0.6.28  2021-09-23 [1] CRAN (R 4.1.1)                  
#>  dplyr         1.0.7   2021-06-18 [1] CRAN (R 4.1.1)                  
#>  e1071         1.7-9   2021-09-16 [1] CRAN (R 4.1.1)                  
#>  ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.1.1)                  
#>  evaluate      0.14    2019-05-28 [1] CRAN (R 4.1.1)                  
#>  fansi         0.5.0   2021-05-25 [1] CRAN (R 4.1.1)                  
#>  fastmap       1.1.0   2021-01-25 [1] CRAN (R 4.1.1)                  
#>  fs            1.5.0   2020-07-31 [1] CRAN (R 4.1.1)                  
#>  generics      0.1.0   2020-10-31 [1] CRAN (R 4.1.1)                  
#>  glue          1.4.2   2020-08-27 [1] CRAN (R 4.1.1)                  
#>  highr         0.9     2021-04-16 [1] CRAN (R 4.1.1)                  
#>  htmltools     0.5.2   2021-08-25 [1] CRAN (R 4.1.1)                  
#>  KernSmooth    2.23-20 2021-05-03 [2] CRAN (R 4.1.1)                  
#>  knitr         1.36    2021-09-29 [1] CRAN (R 4.1.1)                  
#>  lifecycle     1.0.1   2021-09-24 [1] CRAN (R 4.1.1)                  
#>  lwgeom        0.2-8   2021-10-06 [1] CRAN (R 4.1.1)                  
#>  magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.1.1)                  
#>  pillar        1.6.3   2021-09-26 [1] CRAN (R 4.1.1)                  
#>  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.1.1)                  
#>  proxy         0.4-26  2021-06-07 [1] CRAN (R 4.1.1)                  
#>  purrr         0.3.4   2020-04-17 [1] CRAN (R 4.1.1)                  
#>  R.cache       0.15.0  2021-04-30 [1] CRAN (R 4.1.1)                  
#>  R.methodsS3   1.8.1   2020-08-26 [1] CRAN (R 4.1.1)                  
#>  R.oo          1.24.0  2020-08-26 [1] CRAN (R 4.1.1)                  
#>  R.utils       2.11.0  2021-09-26 [1] CRAN (R 4.1.1)                  
#>  R6            2.5.1   2021-08-19 [1] CRAN (R 4.1.1)                  
#>  Rcpp          1.0.7   2021-07-07 [1] CRAN (R 4.1.1)                  
#>  rematch2      2.1.2   2020-05-01 [1] CRAN (R 4.1.1)                  
#>  reprex        2.0.1   2021-08-05 [1] CRAN (R 4.1.1)                  
#>  rlang         0.4.11  2021-04-30 [1] CRAN (R 4.1.1)                  
#>  rmarkdown     2.11    2021-09-14 [1] CRAN (R 4.1.1)                  
#>  rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.1.1)                  
#>  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 4.1.1)                  
#>  sf          * 1.0-3   2021-10-07 [1] CRAN (R 4.1.1)                  
#>  stars       * 0.5-4   2021-11-04 [1] Github (r-spatial/stars@b57b259)
#>  stringi       1.7.5   2021-10-04 [1] CRAN (R 4.1.1)                  
#>  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.1.1)                  
#>  styler        1.6.2   2021-09-23 [1] CRAN (R 4.1.1)                  
#>  tibble        3.1.5   2021-09-30 [1] CRAN (R 4.1.1)                  
#>  tidyselect    1.1.1   2021-04-30 [1] CRAN (R 4.1.1)                  
#>  units         0.7-2   2021-06-08 [1] CRAN (R 4.1.1)                  
#>  utf8          1.2.2   2021-07-24 [1] CRAN (R 4.1.1)                  
#>  vctrs         0.3.8   2021-04-29 [1] CRAN (R 4.1.1)                  
#>  withr         2.4.2   2021-04-18 [1] CRAN (R 4.1.1)                  
#>  xfun          0.26    2021-09-14 [1] CRAN (R 4.1.1)                  
#>  yaml          2.2.1   2020-02-01 [1] CRAN (R 4.1.1)                  
#> 
#> [1] C:/Users/500596972/Documents/R/win-library/4.1
#> [2] C:/Program Files/R/R-4.1.1/library
```

</details>
