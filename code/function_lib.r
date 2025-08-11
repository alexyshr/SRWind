#This functions are not density functions
#This functions are not comparable to mass functions where integral between -inf and inf is one
#You can not compare this functions for instance with evd::dgpd evd::pgpd (those are mass functions)(integral of evd::dgpd is one)
#See gpd_test.r (where it is possible to see that ecuations from extraDist::GPD and hysj_40_02_0165.pdf are equivalent)
  #dmygpd
  # taken extraDist::GPD
  dmygpd <- function(x ,shape, location, scale, dummy){
    (1/scale)*((1+((shape*(x-location))/scale))^(-(1/shape)-1))*(dummy/dummy)
  } 

  #dmygpd
  # taken from hysj_40_02_0165.pdf, or rjef1_10_8.pdf, or os-2016-47.pdf 
  #dmygpd <- function(x ,shape, location, scale, dummy){
  #  (1/scale)*((1-((shape*(x-location))/scale))^((1/shape)-1))*(dummy/dummy)
  #}} 

  #pmygpd
  # taken extraDist::GPD
  #pmygpd <- function(q ,shape, location, scale, dummy){
  #  1-((1+((shape*(q-location))/scale))^(-1/shape))*(dummy/dummy)
  #}

  #pmygpd
  # taken from hysj_40_02_0165.pdf, or rjef1_10_8.pdf, or os-2016-47.pdf
  pmygpd <- function(q ,shape, location, scale, dummy){
    1-((1-((shape*(q-location))/scale))^(1/shape))*(dummy/dummy)
  }

  #dmygpd
  # taken extraDist::GPD
  qmygpd <- function(p ,shape, location, scale, dummy){
    location + scale * ((1-p)^(-shape) - 1)/shape*(dummy/dummy)
  } 

#As this function is based on evd::dgpd all results are based on density (mass) 
#(different to pmygpd which is related to frequencies (values not between 0 and 1))
dgpd_pp <- function (x, location, shape, scale, threshold) { 
  sigma = (-shape)*((-scale/shape)+location-threshold)
  gamma = (-scale/(shape*((-scale/shape)+location-threshold)))^(1/shape)
  evd::dgpd(x, loc=threshold, scale=sigma, shape=0.00000001, log = FALSE)
}

#As this function is based on evd::pgpd all results are based on density (mass) (different to dmygpd which is related to frequencies)
pgpd_pp <- function (x, location, shape, scale, threshold) { 
  sigma = (-shape)*((-scale/shape)+location-threshold)
  gamma = (-scale/(shape*((-scale/shape)+location-threshold)))^(1/shape)
  evd::pgpd(x, loc=threshold, scale=sigma, shape=0.00000001)
}

#Equation 18 page 39 (NIST.SP.500-301.pdf)
returnlevelPP <- function (x, location, shape, scale){#x is MRI
  rl = (scale/shape)*(-log((x-1)/x))^(-shape)-(scale/shape)+location
}

#Equation 15 page 39 (NIST.SP.500-301.pdf)
returnlevelGPD <- function (x, location, shape, scale, threshold) { #x is MRI #From NIST.SP.500-301.pdf
  sigma = (-shape)*((-scale/shape)+location-threshold)
  gamma = (-scale/(shape*((-scale/shape)+location-threshold)))^(1/shape)
  rl = ((((sigma*(gamma^shape)))/shape)*((-log((x-1)/x))^(-shape)))-(sigma/shape)+threshold
}

#Not used: x is MRI #from AAAARLGPD-JCOMM-TR-057.pdf
returnlevelGPD2 <- function (x, location, shape, scale, threshold) { #
  sigma = (-shape)*((-scale/shape)+location-threshold)
  gamma = (-scale/(shape*((-scale/shape)+location-threshold)))^(1/shape)
  if (shape == 0){  #Never will go inside this, because it is not possible to pass shape = 0
                    #as gamma and sigma has shape dividing (division by zero)
    rl = threshold + sigma*(log(gamma*x))
  }
  else
  {
    rl = threshold + (sigma/shape)*(((gamma*x)^shape)-1)
  }
  return(rl)
}

alexys_extRemes <- function(x, threshold, RPs=c(10,20,50), npy)
{
  unit <- paste0(npy,"/year")
  z <- extRemes::fevd(x, method="MLE", type="GP", threshold=threshold, time.units=unit)
  excesses <- x > threshold
  w2 <- length(x)/npy # Duration of the fit period (6 years)
  lambda2 <- sum(excesses)/w2  #Average number of point per year, but above the threshold
  probs <- 1 - 1/(lambda2*RPs)
  probs[probs<0] <- NA
  ### probs     0.9375000         0.9750000         0.9875000         0.9937500         0.9975000 
  ###probs <- c(0.93803727875591, 0.97521491150236, 0.98760745575118, 0.99380372787559, 0.99752149115024)
  scale <- z$results$par[1]
  shape <- z$results$par[2]
  vel1 <- extRemes::qevd(probs, loc=z$threshold, scale=scale, shape=shape, threshold=z$threshold, type="GP")
  out <- extRemes::return.level(z, return.period=RPs)
  n <- names(out)
  out <- as.numeric(out)
  names(out) <- n
  out = rbind(out)
  myPar=c(thr=z$threshold, scale=scale, shape=shape, probs=probs, lambda2=lambda2)
  myPar = rbind(myPar)
  out = cbind(out,myPar)
  #out <- list(RL=out, PAR=c(thr=z$threshold, scale=scale, shape=shape, probs=probs, lambda2=lambda2))
  out  
}


#Function to calculate Yearly Maxima Method
#Receive the complete serie and it calculates the AMS (Anual Maxima Series)
alexys_extRemes_yearlymaxima <- function(x, RPs=c(10,20,50,100,200,500,700,1000,1700,3000,7000), variable.description="")
#Parameters
#-x: tibble data frame object with columns "speed.kph", "date.time", "t.nt.flag"
#    with the complete series of data (no declustering, no thresholding,
#    no AMS (Anual Maxima Series)
#- RPs: list with return periods!
#- variable.description: descriptive text of the variable
#Notes:
#- Uses methods MLE and Lmoments and compare
#- The type is allways "GEV" because is Yearly Maxima
{
  select <- dplyr::select
  myxts = na.omit(xts::xts(x=select(x, "speed.kph"), order.by = x$date.time))
  
  # derive AMS (Anual Maxima Series) for X
  ams <- apply.yearly(myxts, max)
  
  #AMS time series plot
  par(oma = c(2,0,0,0))
  title = paste0(variable.description, "\nWind Velocity [Km/h]")
  xlab = paste0("Page ",numberofplots, " AMS (Anual Maxima Series)- Time Series Plot - Station: ", number)
  print(plot.xts(ams, main = title, major.ticks="years", format.labels = "%b-%d\n%Y",
                 col="green", legend.loc = "top", cex.main=0.2, add=FALSE))
  mtext(side = 1, text = xlab, outer = TRUE)
  #assign(paste0("myprint", numberofplots), recordPlot())
  #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))
  numberofplots = numberofplots + 1
  
  #convert as.vector and remove NA
  ams <- as.vector(na.omit(ams))
  
  #MLE
    ## maximum-likelihood fitting of the GEV distribution
    fit_mle <- extRemes::fevd(ams, method="MLE", type="GEV")
    
    #diagnostic plots
    #par(oma = c(4,0,4,0))
    print(plot(fit_mle))
    xlab = paste0("Page ",numberofplots, " Yearly Maxima(GEV) MLE Diagnostics Plot - Station: ", number)
    mtext(side = 1, line = 1, text = xlab, cex=0.8, outer = TRUE)
    #assign(paste0("myprint", numberofplots), recordPlot())
    #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))
    numberofplots = numberofplots + 1
    
    #return levels:
    rl_mle <- extRemes::return.level(fit_mle, conf = 0.05, return.period=RPs)

    # return level plots (printing the vertical and horizontal line of return period = 100)
    #par(mfcol=c(1,2))
    # return level plot w/ MLE
    #plot(fit_mle, type="rl",
    #     main= paste0("Return Level Plot for ", variable.description, " w/ MLE"),
    #     ylim=c(0,200), pch=16)
    #loc <- as.numeric(return.level(fit_mle, conf = 0.05,return.period=100))
    #segments(100, 0, 100, loc, col= 'midnightblue',lty=6)
    #segments(0.01,loc,100, loc, col='midnightblue', lty=6)
    
  #L-moments
  # fitting of GEV distribution based on L-moments estimation
  fit_lmom <- extRemes::fevd(ams, method = "Lmoments", type="GEV")
  # diagnostic plots
  #plot(fit_lmom)
  
  # return levels:
  rl_lmom <- extRemes::return.level(fit_lmom, conf = 0.05, return.period=RPs)
  
  # return level plot w/ LMOM (printing the vertical and horizontal line of return period = 100)
  #plot(fit_lmom, type="rl",
  #     main=paste0("Return Level Plot for ",  variable.description, " w/ L-Moments"),
  #     ylim=c(0,200))
  #loc <- as.numeric(return.level(fit_lmom, conf = 0.05,return.period=100))
  #segments(100, 0, 100, loc, col= 'midnightblue',lty=6)
  #segments(0.01,loc,100, loc, col='midnightblue', lty=6)
  
  # comparison of return levels
  results <- t(data.frame(mle=as.numeric(rl_mle),
                          lmom=as.numeric(rl_lmom)))
  colnames(results) <- RPs
  round(results,1)  
  results
  
  
  #the number of events per year is one (lambda=1), so in the next variable probs 
  #it is not important to use lambda. probs = 1-1/(lambda*RPs)
  lambda=1
  probs = 1-1/(lambda*RPs)
  probs[probs<0] <- NA
  
  #compare all the results including parameters (not only return levels)
  #MLE
  location <- fit_mle$results$par[1]
  #One of the next two commands to remove names of variable
  location = as.numeric(location)
  attr(location, "names") <- NULL
  scale <- fit_mle$results$par[2]
  attr(scale, "names") <- NULL
  shape <- fit_mle$results$par[3]
  attr(shape, "names") <- NULL
  #Optional way to get return levels!
  #we are not using 'rl' variable, just for testing concepts!
  rl <- extRemes::qevd(probs, loc=location, scale=scale, shape=shape, type="GEV")
  n <- names(rl_mle)
  mle <- as.numeric(rl_mle)
  names(mle) <- n
  mle = rbind(mle)
  myPar=c(threshold=NA, location=location, scale=scale, shape=shape, probs=probs, lambda=lambda)
  myPar = rbind(myPar)
  mle = cbind(mle,myPar)
  
  #LMOMENTS
  location <- fit_lmom$results[1]
  location = as.numeric(location)
  attr(location, "names") <- NULL
  scale <- fit_lmom$results[2]
  attr(scale, "names") <- NULL
  shape <- fit_lmom$results[3]
  attr(shape, "names") <- NULL
  #Optional way to get return levels!
  #we are not using 'rl' variable, just for testing concepts!
  rl <- extRemes::qevd(probs, loc=location, scale=scale, shape=shape, type="GEV")
  n <- names(rl_lmom)
  lmom <- as.numeric(rl_lmom)
  names(lmom) <- n
  lmom = rbind(lmom)
  myPar=c(threshold=NA, location=location, scale=scale, shape=shape, probs=probs, lambda=lambda)
  myPar = rbind(myPar)
  lmom = cbind(lmom,myPar)
  out = list("a"=rbind(mle, lmom), "b"=numberofplots)
}


#The minus-log-likelihood divided by n (mean replaces sum).
mllGumbel <- function(mu, sigma, x)
  log(sigma) + (mean(x) - mu) / sigma + mean(exp(- (x - mu) / sigma))

#=================================================================================
# Define the PDF, CDF and quantile function for the Gumbel distribution
#=================================================================================

dgumbel1 <- function(x,mu,sigma){ # PDF
  exp((mu - x)/sigma - exp((mu - x)/sigma))/sigma
}

pgumbel1 <- function(q,mu,sigma){ # CDF
  exp(-exp(-((q - mu)/sigma)))
}

qgumbel1 <- function(p, mu, sigma){ # quantile function
  mu-sigma*log(-log(p))
}

logLH <- function(x, mu, sigma){
  Z <- -( (x - mu)/sigma)
  t1 <- log(sigma)
  t3 <- sum(log(exp(Z - exp(Z))))
  l <- -t1 * length(x) + t3
  return(-l)
}

fGumbel <- function(x) {
  
  n <- length(x)
  ## choose the location of the exponential distribution of the
  ## marks 'Y', i.e. the threshold for the POT model. The block
  ## duration 'w' is implicitly set to 1.
  muY <- min(x) 
  
  negLogLikc <- function(sigmaY) {
    lambdaHat <-  n / sum(exp(-(x - muY) / sigmaY))
    nll <- -n * log(lambdaHat) + n * log(sigmaY) + sum(x - muY) / sigmaY
    attr(nll, "lambda") <- lambdaHat
    nll
  }
  
  ## the search interval for the minimum could be improved...
  opt <- optimize(f = negLogLikc, interval = c(1e-6, 1e3))
  
  lambdaHat <- attr(opt$objective, "lambda")
  sigmaMHat <- sigmaYHat <- opt$minimum
  muMHat <- muY+ log(lambdaHat) * sigmaYHat
  deviance.check <- -2 * sum(evd::dgumbel(x, loc = muMHat, scale = sigmaMHat,
                                     log = TRUE))
  
  list(estimate = c("loc" = muMHat, "scale" = sigmaMHat),
       lambda = lambdaHat,
       deviance = deviance.check)
}


plotxts <- function(data, variable, time, xlab, cex.main, main, major.ticks){
  library(xts)
  library(dplyr)
  select <- dplyr::select
  myxts = na.omit(xts(x=select(data, variable), order.by = time))
  #plot.new()
  #par(new=FALSE)
  par(oma = c(2,0,0,0))
  print(plot.xts(myxts, main=main, major.ticks=major.ticks, format.labels = "%b-%d\n%Y", 
           col="green", legend.loc = "top", cex.main=cex.main, add=FALSE))
  mtext(side = 1, text = xlab, outer = TRUE)
  #par(oma = c(0,0,0,0))
}

#Generate statistics of time series based on four parameters:
# data: tibble data frame with time series
# variable: column name of time series (for instance: "speed.kph")
# time: column name with date time (POSIXct) column  (for instance: raw.data.tibble$date.time)
# index: period for the statistics (for instance: "years", "months", "weeks")
generate_stats_time_serie <- function (data, variable, time, index) {
  library(xts)
  library(dplyr)
  select <- dplyr::select
  myxts = na.omit(xts(x=select(data, variable), order.by = time))
  endp = endpoints(myxts,on=index)
  period = period.apply(myxts,INDEX=endp,FUN=function(x) length(x))
  names(period) = "count"
  period$mean = period.apply(myxts,INDEX=endp,FUN=mean)
  period$min = period.apply(myxts,INDEX=endp,FUN=min)
  period$max = period.apply(myxts,INDEX=endp,FUN=max)  
  return(period)
}

#Locate and extract dates and times differences for gaps in data greather than parameter thresholdindays (in days)
locate_holes_time_serie <- function (data, variable, time, thresholdindays) {
  library(xts)
  select <- dplyr::select
  myxts = na.omit(xts(x=select(data, variable), order.by = time))
  #Search time differences in days between consecutive samples
  timediff = diff(time(myxts))
  units(timediff) <- "days"
  #Another way to get time differences
  #timediff = difftime(time(myxts), lag(time(myxts)), units=c("days"))
  #select differences bettween samples grather than 30 days
  indexes = timediff > thresholdindays
  #Show time differences that satisfy the condition
  timediff[indexes]
  #Show the samples whose next sample has previous time differences
  myxts[indexes]
  myxts[which(indexes)]
  #Show next sample 
  myxts[which(indexes)+1]
  #Calculate time differences to test
  time(myxts[which(indexes)]) - time(myxts[which(indexes)+1])
  result = cbind(initialdate=as.character(time(myxts[which(indexes)])), nextdate=as.character(time(myxts[which(indexes)+1])), timeindays=timediff[indexes])
  return(result)
}

#Read the time series text file raw_data_station_ID.txt (date_time kph thunder_flag)
ReadWindFile <- function (station.number, path) {

  filename <- paste(path, "raw_data_station_", station.number, ".txt", sep="")
  raw.data <- read.table(file=filename,
                         header=TRUE,
                         colClasses=c("character", "numeric", "character"), sep="\t")
  date.time <- as.POSIXct(raw.data[, "date_time"], tz="GMT", usetz=TRUE)
  speed.kph <- raw.data[, "kph"] 
  t.nt.flag <- raw.data[, "thunder_flag"]

  value <- list(date.time=date.time,
                speed.kph=speed.kph,
                t.nt.flag=t.nt.flag)
  return(value)
}

#This name is BAD (instead of ISD is ERA5), this function is the old version using nedf4. 
#The new version ReadWindERA5ProxyStation is using stars and proxy object
#Read the matrix in m/s and transform to km/h
#UNITS
#ERA5: m/s
#This function transform the units to Kph, so output units are Kph
ReadWindERA5Station <- function (ncin, lonindex, latindex, ntime, timestamp) {
  
  #statera5_xts = na.omit(xts(x= ncvar_get(ncin, 'fg10', start=c(lonindex, latindex,1), count=c(1,1,ntime)), order.by = timestamp))
  statera5 = data.frame(ncvar_get(ncin, 'fg10', start=c(lonindex, latindex,1), count=c(1,1,ntime)))
  
  colnames(statera5) = "kph"
  statera5$kph  = statera5$kph * 3.6  #from mts/seg to km/hour
  statera5$thunder_flag = "nt"
    
  #filename <- paste(path, "raw_data_station_", station.number, ".txt", sep="")
  #raw.data <- read.table(file=filename,
  #                       header=TRUE,
  #                       colClasses=c("character", "numeric", "character"))
  #date.time <- as.POSIXct(raw.data[, "date_time"], tz="GMT", usetz=TRUE)
  date.time <- timestamp
  speed.kph <- statera5$kph
  t.nt.flag <- statera5$thunder_flag
  
  value <- list(date.time=date.time,
                speed.kph=speed.kph,
                t.nt.flag=t.nt.flag)
  return(value)
}

clean_units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  x
}

#Read the matrix in m/s and transform to km/h
#Be sure to use attribute = attribute number inside the stars object
#   why?: because inside stars proxy object it is not possible to use attribute names (it has as names the nc file paths)
#UNITS
#ERA5: m/s
#This function transform the units to Kph, so output units are Kph
ReadWindERA5ProxyStation <- function (starsOrStarsProxy, attribute, lonindex, latindex, ntime) {
  #statera5_xts = na.omit(xts(x= ncvar_get(ncin, 'fg10', start=c(lonindex, latindex,1), count=c(1,1,ntime)), order.by = timestamp))
  #statera5 = data.frame(ncvar_get(ncin, 'fg10', start=c(lonindex, latindex,1), count=c(1,1,ntime)))
  #st = starsProxy[attribute, lonindex, latindex, 1:ntime][[attribute]]
  #stdf = data.frame(s)
  
  #statera5 = mync[attribute, 1, 1, 1:ntime]
  #statera5 = st_as_stars(statera5) #Convert from starsproxy to stars (if it is not starsproxy: NO PROBLEM)
  # timestamp = attr(mync, "dimensions")$time$values
  # statera5 = adrop(statera5) #Erase X and Y dimmensions
  # statera5 = statera5[[attribute]] #It could be class units
  # statera5 = clean_units(statera5) #so, remove units, then class array
  # statera5 = data.frame(statera5) #convert to data.frame
  #attribute = ensym(attribute)
  statera5 = starsOrStarsProxy[attribute, lonindex, latindex, 1:ntime]
  statera5 = st_as_stars(statera5) #If stars proxy, then convert to stars (Fetching the data)
  timestamp = attr(starsOrStarsProxy, "dimensions")$time$values
  statera5 = adrop(statera5) #Erase X and Y dimmensions
  statera5 = statera5[[attribute]] #It could be class units
  statera5 = clean_units(statera5) #so, remove units, then class array
  statera5 = data.frame(statera5) #convert to data.frame
  
  colnames(statera5) = "kph" #It is in m/s (from ERA5), but next is the transformation
  statera5$kph  = statera5$kph * 3.6  #from mts/seg to km/hour
  statera5$thunder_flag = "nt"
  
  #filename <- paste(path, "raw_data_station_", station.number, ".txt", sep="")
  #raw.data <- read.table(file=filename,
  #                       header=TRUE,
  #                       colClasses=c("character", "numeric", "character"))
  #date.time <- as.POSIXct(raw.data[, "date_time"], tz="GMT", usetz=TRUE)
  date.time <- timestamp
  speed.kph <- statera5$kph
  t.nt.flag <- statera5$thunder_flag
  
  value <- list(date.time=date.time,
                speed.kph=speed.kph,
                t.nt.flag=t.nt.flag)
  return(value)
  
}






## ####################################################
## AltDecluster is my version of the decluster
## function in the evir package.  I do not like
## the fact that the decluster function in
## evir package requires more than two clusters
## ###################################################
## series - the series to decluster
## date.time - the dates and times of the
##             observations that comprise the
##             series in POSIXct format
## run - the length of time in days used as a
##       seperator of clusters
## n - the length of series
AltDecluster <- function (series, date.time, run, n) {

  ## if the series is of length 0 or 1
  ## return the original series
  if (n <= 1) {

    value <- list(maxes=series, dt.maxes=date.time)
    return(value)
  }

  ## if the series is longer than 1
  ## we loop through it to find the
  ## cluster maxes
  maxes <- NULL
  cluster <- series[1]
  cluster.dt <- date.time[1]
  for (i in 1:(n - 1)) {

    gap <- difftime(time1=date.time[(i + 1)],
                    time2=date.time[i], units="days")
    gap <- as.numeric(gap)

    ## if the gap is larger than the specified
    ## run, a cluster has been identified,
    ## so the cluster maximum is found, and
    ## a new cluster is started
    if (gap > run) {

      c.max <- max(cluster)
      if (is.null(maxes)) {

          # if there are duplicate speeds, there may be more than one
          # date and time that corresponds to the max, so just take
          # the first one
        dt.maxes <- (cluster.dt[cluster == c.max])[1]
      } else {

          # if there are duplicate speeds, there may be more than one
          # date and time that corresponds to the max, so just take
          # the first one
        dt.maxes <- c(dt.maxes, (cluster.dt[cluster == c.max])[1])
      }
      maxes <- c(maxes, c.max)
      cluster <- series[(i + 1)]
      cluster.dt <- date.time[(i + 1)]
    } else {

      ## if the gap is not larger than the
      ## specified run, the next observation
      ## is added to the cluster

      cluster <- c(cluster, series[(i + 1)])
      cluster.dt <- c(cluster.dt, date.time[(i + 1)])
    }
  }

  ## find the maximum of the last cluster
  c.max <- max(cluster)
  if (is.null(maxes)) {
      # if there are duplicate speeds, there may be more than one
      # date and time that corresponds to the max, so just take
      # the first one
      dt.maxes <- cluster.dt[cluster == c.max][1]
  } else {
      # if there are duplicate speeds, there may be more than one
      # date and time that corresponds to the max, so just take
      # the first one
      dt.maxes <- c(dt.maxes, (cluster.dt[cluster == c.max])[1])
  }
  maxes <- c(maxes, c.max)

  value <- list(maxes=maxes, dt.maxes=dt.maxes)

  return(value)
}

## ###########################################################
## The function PrepareData takes in the raw station data
## and prepares the arguments for TntPpMle
## ###########################################################
## ws - all observed windspeeds over some threshold (the 
##      threshold changes with time) #Alexys: Data is raw.data not data over threshold!
## dt - the date and time of all observations in ws
## t.thresh - the threshold to use for the
##            thunderstorm observations
## nt.thresh - the threshold to use for the
##             non-thunderstorm observations
## remove.gap - lengh in days of time gaps to remove
## t.run - the run length (in days) to use in the
##         declustering algorithm for the thunderstorm
##         observations
## nt.run - the run length (in days) to use in the
##          declustering algorithm for the non-thunderstorm
##          observations
## t.length - the amount of time between thunderstorm
##            observations such that we'll assume
##            thunderstorm observations separated by
##            this much time are assumed to be
##            separate thunderstorms
PrepareData <- function (ws, dt, t.thresh, nt.thresh,
                         remove.gap, t.run, nt.run,
                         t.length) {

  ## separate the thunderstorm winds from the
  ## non-thunderstorm winds
  t.indices <- ws < 0
  t.ws <- abs(ws[t.indices])
  t.dt <- dt[t.indices]
  nt.ws <- ws[!t.indices]
  nt.dt <- dt[!t.indices]

  ## threshold both series
  t.dt <- t.dt[t.ws > t.thresh]
  t.ws <- t.ws[t.ws > t.thresh]
  nt.dt <- nt.dt[nt.ws > nt.thresh]
  nt.ws <- nt.ws[nt.ws > nt.thresh]

  ## decluster the thunderstorm series
  if (length(t.ws) > 1) {

    tmp.t.ws <- t.ws
    tmp.d.t <- AltDecluster(series=tmp.t.ws,
                            date.time=t.dt,
                            run=t.run, n=length(tmp.t.ws))
    d.t.ws <- tmp.d.t$maxes
    d.t.dt <- tmp.d.t$dt.maxes
  } else {

    d.t.ws <- t.ws
    d.t.dt <- t.dt
  }

  ## decluster the non-thunderstorm series
  if (length(nt.ws) > 1) {

    tmp.nt.ws <- nt.ws
    tmp.d.nt <- AltDecluster(series=tmp.nt.ws,
                             date.time=nt.dt,
                             run=nt.run, n=length(tmp.nt.ws))
    d.nt.ws <- tmp.d.nt$maxes
    d.nt.dt <- tmp.d.nt$dt.maxes
  } else {

    d.nt.ws <- nt.ws
    d.nt.dt <- nt.dt
  }

  ## calculate the amount of time that thunderstorms
  ## account for in the observational period
  if (sum(t.indices) > 1) {

    orig.t.dt <- dt[t.indices]
    n.t.obs <- length(orig.t.dt)
    time.lags <- difftime(orig.t.dt[2:n.t.obs],
                          orig.t.dt[1:(n.t.obs - 1)],
                          units="days")
    n.thunders <- time.lags > t.run
    n.thunders <- sum(n.thunders)
    ## add one for the last thunderstorm
    n.thunders <- n.thunders + 1
    t.time <- n.thunders*t.length
  } else if (sum(t.indices) == 1) {

    t.time <- t.length
    n.thunders <- 1
  } else {

    t.time <- 0
    n.thunders <- 0
  }

  ## Alexys
  ## calculate the amount of time that non-thunderstorms
  ## account for in the observational period
  if (sum(!t.indices) > 1) {
    
    orig.nt.dt <- dt[!t.indices]
    n.nt.obs <- length(orig.nt.dt)
    time.lags <- difftime(orig.nt.dt[2:n.nt.obs],
                          orig.nt.dt[1:(n.nt.obs - 1)],
                          units="days")
    n.nthunders <- time.lags > nt.run
    n.nthunders <- sum(n.nthunders)
    ## add one for the last non-thunderstorm
    n.nthunders <- n.nthunders + 1
    #As I do not have nt.lengh I can not do this!
    #nt.time <- n.nthunders*nt.length
  } else if (sum(!t.indices) == 1) {
    
    #nt.time <- nt.length
    n.nthunders <- 1
  } else {
    
    #nt.time <- 0
    n.nthunders <- 0
  }
  
  ## calculate the total time of the observation
  ## period removing gaps of length \code{remove.gap}
  total.time <- difftime(dt[length(dt)],
                         dt[1],
                         units="days")
  total.time <- as.numeric(total.time)

  big.gaps <- difftime(dt[2:length(dt)],
                       dt[1:(length(dt) - 1)],
                       units="days")
  big.gaps <- as.numeric(big.gaps)
  big.gaps <- big.gaps[big.gaps > remove.gap]

  total.time <- total.time - sum(big.gaps)

  ## the amount of non-thunderstorm time is the
  ## total time minus the amount of thunderstorm
  ## time
  nt.time <- total.time - t.time

  ## calculate the average number of thunderstorms
  ## per year
  n.thunders.per.year <- n.thunders/(total.time/365)
  ## Alexys
  n.nthunders.per.year <- n.nthunders/(total.time/365)

  value <- list(t.series=d.t.ws, nt.series=d.nt.ws,
                t.series.dt=d.t.dt, nt.series.dt=d.nt.dt,
                t.length.time=t.time, nt.length.time=nt.time,
                total.time=total.time,
                n.thunders.per.year=n.thunders.per.year,
                n.nthunders.per.year=n.nthunders.per.year)
  return(value)
}

## ######################################################
## GenThresholds generates a two-dimensional grid
## (one dimension each for thunderstorm and
## non-thunderstorm) of thresholds overwhich
## the Poisson process model will be evaluated
## ######################################################
## ws - the observed series of wind speeds
## dt - the dates and times of the observed wind
## total.time - the total time over which wind speeds
##              are observed in days
## t.run - if two thunderstorm observations are
##         separated by at least this amount of
##         time in days they are assumed to be
##         from different thunderstorms
## nt.run - if two non-thunderstorm observations are
##          separated by at least this amount of
##          time in days they are assumed to be
##          from independent phenomena
## t.length - the length of time of a single
##            thunderstorm in days
GenThresholds <- function (ws, dt, total.time, t.run, nt.run,
                           t.length, min.n.per.year,
                           max.n.per.year, remove.gap) {

  ## calculate the number of years under
  ## consideration
  n.years <- total.time/365
  n.years <- round(n.years)

  ## break out the thunderstorm and
  ## non-thunderstorm observations
  t.indices <- ws < 0
  t.ws <- abs(ws[t.indices])
  nt.ws <- ws[!t.indices]

  ## ##########################################################
  ## calculate the minimum and maximum number of allowable
  ## observations.  We can not just multiply
  ## min.n.per.year by n.years because some stations
  ## may not be able to contribute that many observations;
  ## however, it's a starting point
  ## ##########################################################

  ## first calculate min.n.obs and max.n.obs
  ## in the natural way
  min.n.obs.t <- min.n.per.year*n.years
  max.n.obs.t <- max.n.per.year*n.years
  min.n.obs.nt <- min.n.per.year*n.years
  max.n.obs.nt <- max.n.per.year*n.years

  ## now check that max.n.obs is appropriate
  t.min <- floor(suppressWarnings(min(t.ws)))
  nt.min <- floor(min(nt.ws))
  imp.vals <- PrepareData(ws=ws, dt=dt,
                          t.thresh=t.min,
                          nt.thresh=nt.min,
                          remove.gap=remove.gap,
                          t.run=t.run,
                          nt.run=nt.run,
                          t.length=t.length)
  t.n <- length(imp.vals$t.series)
  nt.n <- length(imp.vals$nt.series)

  ## if it's not possible to get
  ## max.n.obs, reset max.n.obs
  if (t.n <= max.n.obs.t) {

    max.n.obs.t <- t.n - 1
  }

  if (nt.n <= max.n.obs.nt) {

    max.n.obs.nt <- nt.n - 1
  }

  ## if the maximum number of observations possible
  ## is less than the minimum we want, reset the
  ## minimum
  if (t.n <= min.n.obs.t) {

    min.n.obs.t <- t.n - 1
  }

  if (nt.n <= min.n.obs.nt) {

    min.n.obs.nt <- nt.n - 1
  }

  ## set initial thresholds and initialize vectors
  ## that will contain the results
  t.thresh <- floor(suppressWarnings(max(t.ws)))
  nt.thresh <- floor(max(nt.ws))
  t.thresholds <- NULL
  nt.thresholds <- NULL
  repeat {

    ## threshold and decluster the thunderstorm
    ## and non-thunderstorm observations
    imp.vals <- PrepareData (ws=ws, dt=dt,
                             t.thresh=t.thresh,
                             nt.thresh=nt.thresh,
                             remove.gap=remove.gap,
                             t.run=t.run,
                             nt.run=nt.run,
                             t.length=t.length)

    t.n <- length(imp.vals$t.series)
    nt.n <- length(imp.vals$nt.series)

    ## if the number of remaining thunderstorm observations
    ## is more than the minimum number to include and less
    ## than the maximum number to include, add the threshold
    ## to the list
    if ((t.n >= min.n.obs.t) && (t.n <= max.n.obs.t)) {

      t.thresholds <- c(t.thresholds, t.thresh)
    }

    ## if the number of remaining non-thunderstorm observations
    ## is more than the minimum number to include and less
    ## than the maximum number to include, add the threshold
    ## to the list
    if ((nt.n >= min.n.obs.nt) && (nt.n <= max.n.obs.nt)) {

      nt.thresholds <- c(nt.thresholds, nt.thresh)
    }

    ## if the remaining observations have not reached
    ## the maximum allowable observations,
    ## for either thunderstorms,
    ## or non-thunderstorms, update current thresholds
    ## appropriately and continue the process
    if ((t.n <= max.n.obs.t) && (nt.n <= max.n.obs.nt)) {

      t.thresh <- t.thresh - 1
      nt.thresh <- nt.thresh - 1
    }
    if ((t.n <= max.n.obs.t) && (nt.n > max.n.obs.nt)) {

      t.thresh <- t.thresh - 1
    }
    if ((t.n > max.n.obs.t) && (nt.n <= max.n.obs.nt)) {

      nt.thresh <- nt.thresh - 1
    }

    ## if the thresholds are such that the number of
    ## included observations are larger than
    ## than the maximum number of allowable observations
    ## exit the loop
    if ((t.n > max.n.obs.t) && (nt.n > max.n.obs.nt)) {

      break
    }
  }

  ## if we escape the loop without proposing
  ## a single threshold, propose the one that
  ## leads to the largest number possible of
  ## observations
  if (is.null(t.thresholds)) {

    t.thresholds <- floor(suppressWarnings(min(t.ws)))
  }
  if (is.null(nt.thresholds)) {

    nt.thresholds <- floor(min(nt.ws))
  }

  ## some of the thresholds may not be unique
  t.thresholds <- unique(x=t.thresholds)
  nt.thresholds <- unique(x=nt.thresholds)

  ## create the grid
  t.nt.grid <- expand.grid(t.thresholds, nt.thresholds)
  t.nt.grid <- as.matrix(t.nt.grid)

  return(t.nt.grid)
}

#It receives the complete dataset in ws
#If there are points applying to the threshold
#1-Do the calculation of theta(mu,psi, 0) for "t" and "nt",
#  calling FindStartVals -> findpsi -> eqforpsi
#2-Do the W-plot and return all the parameters of the model with WPlot
#Note: This function is call in an Iterative 
# loop of the "Threshold Matrix" (previously created with GenThresholds)
CompareStatGumbel <- function (ws, dt, t.thresh, nt.thresh,
                               remove.gap, t.run, nt.run,
                               t.length) {

  imp.vals <- PrepareData(ws=ws, dt=dt, t.thresh=t.thresh,
                          nt.thresh=nt.thresh,
                          remove.gap=remove.gap,
                          t.run=t.run, nt.run=nt.run,
                          t.length=t.length)

  if (length(imp.vals$t.series) > 0) {

    t.pp.fit <- FindStartVals(N=length(imp.vals$t.series),
                              T=imp.vals$t.length.time,
                              thresh=t.thresh,
                              sum.y=sum(imp.vals$t.series))

  } else {

    t.pp.fit <- NULL
  }

  if (length(imp.vals$nt.series) > 0) {

    nt.pp.fit <- FindStartVals(N=length(imp.vals$nt.series),
                               T=imp.vals$nt.length.time,
                               thresh=nt.thresh,
                               sum.y=sum(imp.vals$nt.series))
  } else {

    stop("There must be some non-thunderstorm observations")
  }

  compare.stat <- WPlot(t.series=imp.vals$t.series,
                        nt.series=imp.vals$nt.series,
                        t.thresh=t.thresh, nt.thresh=nt.thresh,
                        t.theta=c(t.pp.fit$mu, t.pp.fit$psi, 0.0),
                        nt.theta=c(nt.pp.fit$mu, nt.pp.fit$psi, 0.0),
                        t.n=length(imp.vals$t.series),
                        nt.n=length(imp.vals$nt.series),
                        tf.plot=FALSE, BW=FALSE, details=FALSE)

  return(compare.stat)
}

## ##################################################
## FindStartVals solves the score equations
## for the limit of the Poission process likelihood
## as the shape parameter goes to zero
## ##################################################
## N - the number of observations over the
##     threshold
## T - the length of time in days over which
##     observations are recorded
## thresh - the threshold
## sum.y - the sum of the observations that
##         are over the threshold
FindStartVals <- function (N, T, thresh, sum.y) {
  #Change made by Alexys to work time in years
  T = T/365
  #If you change this value, the value T will be in years
  # in FindPsi and EqForPsi whitout changing those functions!
  if (N != 0 ) {
    psi <- FindPsi(T=T, N=N, thresh=thresh, sum.y=sum.y)
    mu <- psi*log((N/T)) + thresh
    value <- list(mu=mu, psi=psi)    
  } else {
    value <- list(mu=NULL, psi=NULL)
  }
  return(value)
}

## ##################################################
## FindPsi solves the second of the score equations
## for the limit of the Poisson process likelihood
## where the value of mu from the first score
## equation, as a function of psi, is plugged in.
## Poisson process likelihood is actually the
## limit as the shape parameter goes to zero.
## ##################################################
## N - the number of observations over the
##     threshold
## T - the length of time in days over which
##     observations are recorded
## thresh - the threshold
## sum.y - the sum of the observations that
##         are over the threshold
FindPsi <- function (N, T, thresh, sum.y) {

  a <- 0.1

  b <- 1

  eq.a <- EqForPsi(psi=a, T=T, N=N,
                   thresh=thresh, sum.y=sum.y)

  eq.b <- EqForPsi(psi=b, T=T, N=N,
                   thresh=thresh, sum.y=sum.y)

  repeat {

    if(sign(eq.a) != sign(eq.b)) {

      break
    }

    a <- a/2

    eq.a <- EqForPsi(psi=a, T=T, N=N,
                     thresh=thresh, sum.y=sum.y)

    if(sign(eq.a) != sign(eq.b)) {

      break
    }

    b <- b + 1

    eq.b <- EqForPsi(psi=b, T=T, N=N,
                     thresh=thresh, sum.y=sum.y)

    if(sign(eq.a) != sign(eq.b)) {

      break
    }

    if (b > 100) {

      stop("A reasonable value for psi does not exist")
    }
  }

  psi <- uniroot(f=EqForPsi, interval=c(a, b),
                 T=T, N=N, thresh=thresh, sum.y=sum.y)
  psi <- psi$root

  return(psi)
}

## ###########################################
## EqForPsi is the function that FindPsi
## calculates the root of.
## ###########################################
## psi - the value of psi at which the
##       function is evaluated
## N - The length of the data vector
## T - The length of time over which
##     observations were taken
## thresh - the threshold over which all
##          observations fall
## sum.y - the sum of the observations
EqForPsi <- function (psi, N, T, thresh, sum.y) {

  mu <- psi*log((N/T)) + thresh

  term1 <- -psi*N

  term2 <- sum.y

  term3 <- -N*mu

  term4 <- -T*(thresh - mu)
  term4 <- term4*exp((-(thresh - mu))/psi)

  value <- term1 + term2 + term3 + term4

  return(value)
}


WPlot <- function (t.series, nt.series,
                   t.thresh, nt.thresh,
                   t.theta, nt.theta,
                   t.n, nt.n, tf.plot,
                   BW, details) {

  ## unpack the parameters and
  ## vectorize everything so that loops
  ## are unneccesary
  if (round(t.n, 10) > 0) {

    t.mu <- rep(x=t.theta[1], times=t.n)
    t.psi <- rep(x=t.theta[2], times=t.n)
    t.k <- rep(x=t.theta[3], times=t.n)
  } else {

    t.mu <- NULL
    t.psi <- NULL
    t.k <- NULL
  }

  if (round(nt.n, 10) == 0) {

    stop("There must be some non-thunderstorm observations")
  } else {

    nt.mu <- rep(x=nt.theta[1], times=nt.n)
    nt.psi <- rep(x=nt.theta[2], times=nt.n)
    nt.k <- rep(x=nt.theta[3], times=nt.n)
  }


  ## mu <- c(t.mu, nt.mu)
  ## psi <- c(t.psi, nt.psi)
  ## k <- c(t.k, nt.k)
  ## thresh <- c(rep(x=t.thresh, times=t.n),
  ##             rep(x=nt.thresh, times=nt.n))
  ## series <- c(t.series, nt.series)

  ## the formula requires the excesses
  ## instead of the actual values
  if (round(t.n, 10) > 0) {

    t.Y <- t.series - t.thresh
  } else {

    t.Y <- NULL
  }
  nt.Y <- nt.series - nt.thresh

  ## calculate the W statistics
  if (round(t.n, 10) > 0) {

    if (round(t.theta[3], 10) != 0) {

      t.W1 <- (t.k*t.Y)/(t.psi + t.k*(t.thresh - t.mu))
      t.W2 <- 1 + t.W1
      ## in case any part of W is less than 0 at this point,
      ## we raise it to 0
      t.W3 <- t.W2
      t.W3[t.W3 < 0] <- 0
      t.W4 <- log(t.W3)
      t.W <- (1/t.k)*t.W4
    } else {

      t.W1 <- rep(x=NA, times=t.n)
      t.W2 <- rep(x=NA, times=t.n)
      t.W3 <- rep(x=NA, times=t.n)
      t.W4 <- rep(x=NA, times=t.n)
      t.W <- t.Y/t.psi
    }
  } else {

    t.W1 <- NULL
    t.W2 <- NULL
    t.W3 <- NULL
    t.W4 <- NULL
    t.W <- NULL
  }

  if (round(nt.theta[3], 10) != 0) {

    nt.W1 <- (nt.k*nt.Y)/(nt.psi + nt.k*(nt.thresh - nt.mu))
    nt.W2 <- 1 + nt.W1
    ## in case any part of W is less than 0 at this point,
    ## we raise it to 0
    nt.W3 <- nt.W2
    nt.W3[nt.W3 < 0] <- 0
    nt.W4 <- log(nt.W3)
    nt.W <- (1/nt.k)*nt.W4
  } else {

    nt.W1 <- rep(x=NA, times=nt.n)
    nt.W2 <- rep(x=NA, times=nt.n)
    nt.W3 <- rep(x=NA, times=nt.n)
    nt.W4 <- rep(x=NA, times=nt.n)
    nt.W <- nt.Y/nt.psi
  }

  series <- c(t.series, nt.series)
  Y <- c(t.Y, nt.Y)
  W1 <- c(t.W1, nt.W1)
  W2 <- c(t.W2, nt.W2)
  W3 <- c(t.W3, nt.W3)
  W4 <- c(t.W4, nt.W4)
  W <- c(t.W, nt.W)

  ## get the values ready to return if details
  ## are asked for
  if (details){


    series <- series[order(W)]
    t.nt.flag <- c(rep(x="t", times=t.n),
                   rep(x="nt", times=nt.n))
    t.nt.flag <- t.nt.flag[order(W)]
    Y <- Y[order(W)]
    W1 <- W1[order(W)]
    W2 <- W2[order(W)]
    W3 <- W3[order(W)]
    W4 <- W4[order(W)]
  }
  W <- sort(x=W)

  ## calculate the appropriate exp(1)
  ## quantiles
  quantiles <- ((1:(t.n + nt.n)) - 0.375)/(t.n + nt.n + 0.25)
  exp1.quantiles <- qexp(p=quantiles)

  if (tf.plot) {

    plot(x=exp1.quantiles, y=W,
         xlab="Exponential with mean 1 and pdf U(0,1) - quantiles",
         ylab="Ordered W-Statistics",
         main=bquote(paste("W-Statistic Plot for best pair of thresholds (", b[t], "=", .(t.thresh), ", ", b[nt], "=", .(nt.thresh), ")")))
    if (BW) {

      abline(a=0, b=1, col="black")
    } else {

      abline(a=0, b=1, col="red")
    }
  }

  if (details) {

    value <- abs((W - exp1.quantiles))
    return(data.frame(series, t.nt.flag, Y, W1, W2, W3, W4, W, value))
  } else {

    value <- max(abs((W - exp1.quantiles)))
    return(value)
  }
}

#Combine Hurricane and Non-Hurricane Information (tested with data frame as input) (please test with stars as input)
# This function takes a data frame with hurricane and non-hurricane columns
# and combine them using ASCE7-16 formula ASCE7-16 equation C26.5-2 for combination of statistically independent events
# Pe( y > YMRI) = 1 – PNH(y < YMRI)PH(y < YMRI)
combined_columns_df_apply <- function(st=h_nh, 
                                      bands_rl_h = c(33:44), mri_h = c(10,25,50,100,250,500,700,1000,1700,2500,5000,10000),
                                      bands_rl_nh = c(22:32), mri_nh = c(10,20,50,100,250,500,700,1000,1700,3000,7000),
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
  #rownames(hc_h) <- rl600
  
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
  #rownames(hc_nh) <- rl600
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
  #rownames(hc_c) <- rl600
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

#Function to extrapolate
#It returns Y values for xout values given x and Y pairs
extrapola <- function(x, y, xout){
  y=Hmisc::approxExtrap(x=x, y=y, xout=xout)$y
}

# Monte Carlo Simulations + LHS (latin hypercube sampling) for
#
#    WIND PRESURES (low rise enclosed building)
#
# NOTE: Units of this function in SI (N, m, s), but velocity inputs as KPH!!!! and R in KN/m2 !!!!
# NOTE: The fitting process of probability distributions with ERA5 took velocity in m/s as input,
#       and the output was Kph, so Velocities inputs here and statistical parameters (thresholds, location, scale, shape) are inputs in Kph.
#       So, as the velocity inputs and parameters are in Kph, it is necessary to transform to m/s to calculate wind pressure.
# NOTE: The order of input RVS need to be: c("Kh", "Kd", "GCp", "GCpi", "R")

mcs_wind <- function(st = h_nh,       #Units of this function in SI (N, m, s), but capacity is in kN/m2 (conversion inside the code, i.e *1000)
                     wind_band_threshold = 14,
                     wind_band_location = 15,
                     wind_band_scale = 16,
                     wind_band_shape = 17,
                     wind_band_events_per_year = 18, # Only for POT
                     wind_band_station = 21,
                     number_of_samples = 5, 
                     number_of_rvs = 7,
                     names_of_rvs = c("Kh", "Kd", "GCp", "GCpi", "R", "VNH", "VH"), # (1)exposure, (2)wind direction, (3)external pressure, 
                     # (4)internal pressure, (5)capacity (in KN/m2) - Conversion to N/m2 inside the function!!, and 
                     # (6) Velocity non-hurricane (NH), and (7) velocity hurricane (H)
                     # NOTE: Seven (7) values in names_of_rvs and type_pdf_rvs, 
                     #       but only five (5) in rvs_location, rvs_cov
                     #       Why?: because (1) "VNH" (velocity non hurricanes) fitting parameters comes from the input Excel file,
                     #                            (each row of the excel file in as "st" (data frame) input, in this specific columns): 
                     #                            - wind_band_threshold
                     #                            - wind_band_location
                     #                            - wind_band_scale
                     #                            - wind_band_shape
                     #                            - wind_band_events_per_year (*** not used because probabilities are random)
                     #             because (2) "VH" (velocity non hurricanes) comes from the interpolation of hazard curve from the input Excel file,
                     #                            and the hazard curve is defined in  mri_h, bands_h
                     # NOTE: Velocity "VNH" as R.V. (random variable) is only for non-hurricane, hurricane part is missing, so this isweakness of this simulation,
                     #       the probability of failure with V as R.V is not complete or valid
                     type_pdf_rvs = c("stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qnorm", "stats::qlnorm", "evd::qgpd", "interpolation_hurricane"),
                     rvs_location = c(0.71, 1.01, -0.855, 0.150, 6.2), # 
                     rvs_cov = c(0.19, 0.093, 0.12, 0.33, 0.12),
                     names_of_deterministics = c("Kzt", "Ke"), #topographic, altitude (air density)
                     values_of_deterministics = c(1, 1),
                     bands_c = c(46:56), #### Combined Return Levels for fixed velocity simulations
                     mri_c = c(10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000), # for bands_c
                                         #### Note: the hazard curve of combined hazard (h + nh) is defined by:
                                         ####       x: mri_c, bands_c. This hazard curve is not used for interpolations,
                                         ####       Why: because velocities here are used as fixed values (not random variables)
                     bands_h = c(33:44), ### Hurricane hazard to work wind hurricane as random variable and
                                         ### perform interpolations using random probabilities
                                         ### Note: the hurricane hazard curve is composed by: x: mri_h; y: bands_h
                     mri_h = c(10, 25, 50, 100, 250, 500, 700, 1000, 1700, 2500, 5000, 10000),
                     mri_fixed_vel = c(10, 20, 50, 100, 250, 500, 700, 1000, 1700, 3000, 7000, "C700", "C1700", "C3000"), #Static Velocities Simulations
                     bands_fixed_vel = c(46:59), #Static Velocities Simulations
                     plot= c(1,2,3) #Id of the stations to plot!
                     #mycuenta = mycuenta
){
  library(lhs)
  # print(wind_band_location)
  # print(wind_band_scale)
  # print(number_of_samples)
  # print(number_of_rvs)
  # print(names_of_rvs)
  # print(type_pdf_rvs)
  # print(rvs_location)
  # print(rvs_cov)
  # print(names_of_deterministics)
  # print(values_of_deterministics)
  # print(bands_c)
  # print(seed)
  
  #Velocities from 1 to 600 to construct detailed hazard curve
  rl600 = seq(from=1, to=600, by=1)
  
  mycuenta <<- mycuenta + 1
  contar = mycuenta
  assign("mycuenta", contar, envir = parent.frame() )
  #print(contar)
  print(paste0("row number: ", mycuenta))
  #if (mycuenta == 34) browser()
  
  #set.seed(seed)
  #U <- optimumLHS(number_of_samples, number_of_rvs, maxSweeps = 4, eps = 0.01)
  #Latin Hypercube Sampling (Output are uniform random variables)
  U <- randomLHS(number_of_samples, number_of_rvs)
  RVS <- matrix(nrow = nrow(U), ncol = ncol(U))
  colnames(RVS) = names_of_rvs
  #eval(parse(text=paste0("myprint", numberofplots)))
  #
  #From random vector of probabilities (output of LHS),
  #get random velocities. Note input parameters are related to velocities in Kph
  for (i in 1:length(names_of_rvs)) {
    if (type_pdf_rvs[i] == "stats::qlnorm"){ # LogNormal (Other R.V.S, but not any Velocity)
      m = rvs_location[i]
      s = (abs(rvs_location[i])*rvs_cov[i])
      location <- log(m^2 / sqrt(s^2 + m^2))
      scale <- sqrt(log(1 + (s^2 / m^2)))
      #
      # print(i)
      # print(names_of_rvs[i])
      # print(type_pdf_rvs[i])
      # print(rvs_location[i])
      # print(abs(rvs_cov[i])*rvs_location[i])
      package_function = strsplit(type_pdf_rvs[i], "::")
      #f <- get(type_pdf_rvs[i])
      f <- getFromNamespace(package_function[[1]][2], package_function[[1]][1])
      
      RVS[,i] <- f(U[,i], location, scale) # NOT ANY RANDOM VELOCITIES. HERE TO PROCESS ALL THE OTHER RANDOM VARIABLES
                                         # Input here are rvs_location, and rvs_cov
    }
    else if (type_pdf_rvs[i] == "stats::qnorm") { # Normal (Other R.V.S, but not any Velocity)
      m = rvs_location[i]
      s = (abs(rvs_location[i])*rvs_cov[i])
      location <- m
      scale <- s
      #
      # print(i)
      # print(names_of_rvs[i])
      # print(type_pdf_rvs[i])
      # print(rvs_location[i])
      # print(abs(rvs_cov[i])*rvs_location[i])
      #f <- get(type_pdf_rvs[i])
      package_function = strsplit(type_pdf_rvs[i], "::")
      #f <- get(type_pdf_rvs[i])
      f <- getFromNamespace(package_function[[1]][2], package_function[[1]][1])      
      RVS[,i] <- f(U[,i], location, scale) # NOT ANY RANDOM VELOCITIES. HERE TO PROCESS ALL THE OTHER RANDOM VARIABLES
                                         # Input here are rvs_location, and rvs_cov
    }
    else if (type_pdf_rvs[i] == "evd::qgpd") { # GPD replacing Poisson Process (Only for Non Hurricane Random Velocities)
      shape = 0.00001
      threshold = as.double(st[wind_band_threshold])
      m = as.double(st[wind_band_location])
      s = as.double(st[wind_band_scale])
      #print(s)
      #
      scale =  (-1*shape)*(((-1*s)/shape)+m-threshold)
      location <- threshold
      #shape <- s
      #
      # print(i)
      # print(names_of_rvs[i])
      # print(type_pdf_rvs[i])
      # print(rvs_location[location])
      # print(scale)
      #f <- get(type_pdf_rvs[i])
      package_function = strsplit(type_pdf_rvs[i], "::")
      #f <- get(type_pdf_rvs[i])
      f <- getFromNamespace(package_function[[1]][2], package_function[[1]][1])    
      RVS[,i] <- f(U[,i], location, scale, shape) # non-hurricane random velocities (kph)
      non_hurricane_random_velocities = RVS[,i]
      non_hurricane_random_probabilities = U[,i]
    }
    else if (type_pdf_rvs[i] == "lmomco::quagpa") { # Generalized Pareto Distribution from lmomco (Only for non-hurricane Random Velocities)
      shape = as.double(st[wind_band_shape])
      location = as.double(st[wind_band_location])
      scale = as.double(st[wind_band_scale])
      #gpa.par <- lmomco::vec2par(c(location, scale, shape), type="gpa")
      gpa.par <- lmomco::vec2par(c(xi = location, alpha = scale, kappa=shape), type="gpa")
      
      #print(s)
      #
      # print(i)
      # print(names_of_rvs[i])
      # print(type_pdf_rvs[i])
      # print(rvs_location[location])
      # print(scale)
      #f <- get(type_pdf_rvs[i])
      package_function = strsplit(type_pdf_rvs[i], "::")
      #f <- get(type_pdf_rvs[i])
      f <- getFromNamespace(package_function[[1]][2], package_function[[1]][1])    
      RVS[,i] <- f(U[,i], gpa.par) # non-hurricane random velocities (kph)
      non_hurricane_random_velocities = RVS[,i]
      non_hurricane_random_probabilities = U[,i]
    }
    else if (type_pdf_rvs[i] == "lmomco::qualn3") { # Log Normal Distribution from lmomco (Only for non-hurricane Random Velocities)
      threshold = as.double(st[wind_band_threshold])
      shape = as.double(st[wind_band_shape])
      location = as.double(st[wind_band_location])
      scale = as.double(st[wind_band_scale])
      ln3.par <- lmomco::vec2par(c(zeta = threshold,  mu = location, sigma = scale), type = "ln3")
      #gpa.par <- lmomco::vec2par(c(location, scale, shape), type="gpa")
      
      #print(s)
      #
      # print(i)
      # print(names_of_rvs[i])
      # print(type_pdf_rvs[i])
      # print(rvs_location[location])
      # print(scale)
      #f <- get(type_pdf_rvs[i])
      package_function = strsplit(type_pdf_rvs[i], "::")
      #f <- get(type_pdf_rvs[i])
      f <- getFromNamespace(package_function[[1]][2], package_function[[1]][1])    
      RVS[,i] <- f(U[,i], ln3.par)# non-hurricane random velocities (kph)
      non_hurricane_random_velocities = RVS[,i]
      non_hurricane_random_probabilities = U[,i]
    }
    else if (type_pdf_rvs[i] == "lmomco::quagum") { # Gumbel Distribution from lmomco (Only for non-hurricane Random Velocities)
      threshold = as.double(st[wind_band_threshold])
      shape = as.double(st[wind_band_shape])
      location = as.double(st[wind_band_location])
      scale = as.double(st[wind_band_scale])
      
      
      gum.par <- lmomco::vec2par(c(xi = location, alpha = scale), type="gum")
      #gpa.par <- lmomco::vec2par(c(location, scale, shape), type="gpa")
      
      #print(s)
      #
      # print(i)
      # print(names_of_rvs[i])
      # print(type_pdf_rvs[i])
      # print(rvs_location[location])
      # print(scale)
      #f <- get(type_pdf_rvs[i])
      package_function = strsplit(type_pdf_rvs[i], "::")
      #f <- get(type_pdf_rvs[i])
      f <- getFromNamespace(package_function[[1]][2], package_function[[1]][1])    
      RVS[,i] <- f(U[,i], gum.par) # non-hurricane random velocities (kph)
      non_hurricane_random_velocities = RVS[,i]
      non_hurricane_random_probabilities = U[,i]
    }
    else if (type_pdf_rvs[i] == "lmomco::quawei") { # Weibull Distribution from lmomco (Only for non-hurricane Random Velocities)
      threshold = as.double(st[wind_band_threshold])
      shape = as.double(st[wind_band_shape])
      location = as.double(st[wind_band_location])
      scale = as.double(st[wind_band_scale])
      
      
      wei.par <- lmomco::vec2par(c(zeta = location, beta = scale, delta=shape), type="wei")
      #gpa.par <- lmomco::vec2par(c(location, scale, shape), type="gpa")
      
      #print(s)
      #
      # print(i)
      # print(names_of_rvs[i])
      # print(type_pdf_rvs[i])
      # print(rvs_location[location])
      # print(scale)
      #f <- get(type_pdf_rvs[i])
      package_function = strsplit(type_pdf_rvs[i], "::")
      #f <- get(type_pdf_rvs[i])
      f <- getFromNamespace(package_function[[1]][2], package_function[[1]][1])    
      RVS[,i] <- f(U[,i], wei.par) # non-hurricane random velocities (kph)
      non_hurricane_random_velocities = RVS[,i]
      non_hurricane_random_probabilities = U[,i]
    }
    else if (type_pdf_rvs[i] == "lmomco::quagev") { # Generalized Extreme Value from lmomco (Only for non-hurricane Random Velocities)
      shape = as.double(st[wind_band_shape])
      location = as.double(st[wind_band_location])
      scale = as.double(st[wind_band_scale])
      #gpa.par <- lmomco::vec2par(c(location, scale, shape), type="gpa")
      gev.par <- lmomco::vec2par(c(xi = location, alpha = scale, kappa=shape), type="gev")
      
      #print(s)
      #
      # print(i)
      # print(names_of_rvs[i])
      # print(type_pdf_rvs[i])
      # print(rvs_location[location])
      # print(scale)
      #f <- get(type_pdf_rvs[i])
      package_function = strsplit(type_pdf_rvs[i], "::")
      #f <- get(type_pdf_rvs[i])
      f <- getFromNamespace(package_function[[1]][2], package_function[[1]][1])    
      RVS[,i] <- f(U[,i], gev.par) # non-hurricane random velocities (kph)
      non_hurricane_random_velocities = RVS[,i]
      non_hurricane_random_probabilities = U[,i]
    }
    else if (type_pdf_rvs[i] == "interpolation_hurricane") { # To simulate hurricane hazards, (only for Hurricane Velocities)
                                                   # doing interpolation with hazard curve and random probabilities as input
      #browser()
      VH = as.double(st[bands_h]) # Note: Input was in kph

      print("Velocities Hurricanes:")
      print(VH)
      # #Check data with problems in V
      if (any(!is.na(VH))){
        if (sum(VH) == 0 | sum(VH) < 0) {
          VH = rep(NA, length(bands_h))
        }
      }
      
      
      #Organize given hazard curve points in a datagrame from VH and mri_h
      h = data.frame(rl=VH, prob = 1/mri_h)
      
      #Add 600 points (detailed) to the hazard curve with extrapolation
      #create hurricane hazard curve (detailed >>> 1:600)
      #if (is.na(h$rl[1])){ # check if hurricane data is NA
      if (any(is.na(h$rl)) | any(is.na(h$prob))){ # check if data frame hurricane columns have NA values
        #only one NA value >>>> avoid extrapola!
        print("a")
        hc_h_prob = rep(NA, length(rl600))
      } else{
        print("b")
        hc_h_prob = extrapola(x=h$rl, y=h$prob, xout=rl600)  
      }
      
      hc_h = data.frame(rl=rl600, prob=hc_h_prob)
      rownames(hc_h) <- rl600
      #Do not forget to check extreme values in probs
      hc_h$prob[hc_h$prob < 0] = 0
      hc_h$prob[hc_h$prob > 1] = 1
      
      hurricane_random_probabiities = U[,i]
      
      #Interpolate from probabilities to return velocities
      if (any(is.na(hc_h$rl)) | any(is.na(hc_h$prob))){ # check if data frame hurricane columns have NA values
        #only one NA value >>>> avoid extrapola!
        hurricane_random_velocities = rep(NA, length(hurricane_random_probabiities))
        RVS[,i] = hurricane_random_velocities
        
      } else{
        hurricane_random_velocities = extrapola(x=hc_h$prob, y=hc_h$rl, xout=hurricane_random_probabiities)
        hurricane_random_velocities[hurricane_random_velocities<0]=0
        RVS[,i] = hurricane_random_velocities
      }
    }
    # else if (type_pdf_rvs[i] == "interpolation_combined") { # To simulate hurricane hazards, (only for Hurricane Velocities)
    #   combined_random_probabilities = U[,i]
    # }
  }

  ##Summary of the interpolation process to obtain combined hazard
  ##
  # #Hurricane                            (some points in the Excel)
  #   #Initial points of hazard curve
  #     h 					                      (from Excel)
  #   #curve with 600 points
  #     hc_h 					                    (probabilities: interpolation using h)
  #                                       (velocities: 1:600)
  #   #all points
  #     hurricane_random_velocities 		  (interpolation using hc_h )
  #     hurricane_random_probabilities  		(LHS)
  # 
  # #Non hurricane
  #   #all points
  #     non_hurricane_random_probabilities 	(LHS)
  #     non_hurricane_random_velocities 	  (fitting non-hurricane random probabilities with pdf parameters)
  #         OR
  #     nh 					                        (from non_hurricane_random_probabilities , non_hurricane_random_velocities 
  #   #curve with 600 points
  #     hc_nh					                      (probabilities: interpolation using nh)
  #                                         (velocities: 1:600)
  # 
  # #Combined
  #   #curve with 600 points
  #     hc_c					                      (probabilities: combining probs of hc_h and hc_nh with 600 points)
  #                                         (velocities: 1:600)
  #   #all points
  #     combined_random_probabilities		    (combining non_hurricane_random_probabilities and hurricane_random_probabiities)
  #     combined_random_velocities 		      (interpolation from hc_c)
  # #
  # 
  
  
  # Non-Hurricane hazard curve
  #
  #Construct hazard curve for non-hurricanes with velocity
  #values between 1 and 600 (step 1), using
  #the whole amount of velocities and probabilities coming from simulations
  #and stored in y: non_hurricane_random_velocities, x: non_hurricane_random_probabilities
  nh = data.frame(rl=non_hurricane_random_velocities, prob=(1 - non_hurricane_random_probabilities))
  
  #create non-hurricane hazard curve (values between 1 and 600)
  if (any(is.na(nh$rl)) | any(is.na(nh$prob))){ # check if dataframe hurricane columns have NA values
    #only one NA value >>>> avoid extrapola!
    hc_nh_prob = rep(NA, length(rl600))
  } else{
    hc_nh_prob = extrapola(x=nh$rl, y=nh$prob, xout=rl600) #Not check for NA values because non-hurricane has to have rl value in all cells
  }
  
  #hc_nh is less detailed hc compared with nh,
  #but the interpolation will be performed with hc_nh
  hc_nh = data.frame(rl=rl600, prob=hc_nh_prob)
  rownames(hc_nh) <- rl600
  #Do not forget to check extreme values in probs
  hc_nh$prob[hc_nh$prob < 0] = 0
  hc_nh$prob[hc_nh$prob > 1] = 1
  
  #Combined hazard curve
  # 
  #Create combined hazard curve using hc_nh and hc_h
  #create combined hazard curve (detailed >>> 1:600)
  #Note: Calculation of combined hazard curve, only if hurricane data is available:
  
  if (any(is.na(hc_h$prob)) | any(is.na(hc_h$rl))){ #<<< Not hurricane data available
    combined_random_velocities = non_hurricane_random_velocities
  }else{ #Only in this section we interpolate hurricana & non-hurricane hazard curve
    if (any(is.na(hc_nh$prob)) | any(is.na(hc_nh$rl))){
      #if (is.na(hc_h$prob[1])){ #only check the first probability in the data frame (corresponding to velocity 1)
      #check the hurricanes data.frame >> Be Aware hc_h
      #hc_c_prob = hc_nh$prob
      hc_c_prob = rep(NA, length(rl600))
      print ("Error: null values in hc_h")
    } else {
      #pec = (1 - ((1-peh)*(1-penh)))
      hc_c_prob = (1 - ((1-hc_h$prob)*(1-hc_nh$prob)))     #This probabilities are for the whole 
    }
    
    hc_c = data.frame(rl=rl600, prob=hc_c_prob)
    rownames(hc_c) <- rl600
    #Do not forget to check extreme lower values in probs
    hc_c$prob[hc_c$prob<0] = 0
    
    # Combined random probabilities comes from hurricane and non-nurricane random probabilities
    combined_random_probabilities = (1 - ((1-hurricane_random_probabiities)*(1-non_hurricane_random_probabilities)))
    
    #Interpolate rl in combined hazard curve based on combined_random_probabilities
    if (any(is.na(hc_c$prob)) | any(is.na(hc_h$rl))){
      combined_random_velocities  = rep(NA, length(combined_random_probabilities))
    } else {
      combined_random_velocities  = extrapola(x=hc_c$prob, y=hc_c$rl, xout=combined_random_probabilities)  
    }
  }
  
  #Note that  velocities_c = non_hurricane_random_velocities when no data for non-hurricane
  velocities_c = combined_random_velocities * 0.277778 # Change units in velocity from KPH to M/S, but not in the Output (the outputs will be shown in Kph), but
  # the calculations will be in m/s
  
  #RVS[,i] = velocities_nh * 0.277778  #>>>> Do no change units in the dataset (The Output will be in Kph, calculations here in m/s)
  mean_vc = mean(combined_random_velocities) #Means in KPH
  max_vc = max(combined_random_velocities)
  min_vc = min(combined_random_velocities)
  sd_vc = sd(combined_random_velocities) 
  cov_vc = sd_vc/mean_vc   # C.O.V in KPH
  
  

  # the calculations will be in m/s
  
  # ScatterPlots
  #v <- c('a','b','c','e')
  station = as.integer(st[wind_band_station])
  
  #if (contar %in% plot){
  if (station %in% plot){
      library(dplyr)
    
    # Scatter Plot
      not_all_na <- function(x) any(!is.na(x)) #Function to remove columns full of NA
      
      #temp <-RVS %>% select(where(not_all_na)) #removing columns with NA (all values) from RVS
      
      temp <- RVS %>% as.data.frame() %>% select(where(not_all_na)) %>% data.matrix()
      
      station = as.integer(st[wind_band_station])
      name_pdf = paste0(output_path, "/", "scatterplot_", contar, "_", station, ".pdf")
      if (file.exists(name_pdf)) {
        file.remove(name_pdf)
      }
      pdf(name_pdf)
      #browser()
      pairs(temp, pch = ".", cex=0.1, labels=names_of_rvs, lower.panel = NULL, main="Ramdom Realizations")
      dev.off()

    # Hurricane Hazard Curve
      if (!any(is.na(hc_h$prob)) & !any(is.na(hc_h$rl))){
        #browser()
        name_pdf = paste0(output_path, "/", "hazard_curve_hurricane", contar, "_", station, ".pdf")
        if (file.exists(name_pdf)) {
          file.remove(name_pdf)
        }
        pdf(name_pdf)
        #browser()
        plot(hc_h$prob, hc_h$rl, col="red", lwd=3, type="l", xlab="Exceedance Probabilities", ylab="Velocities (kph)", 
              main="Hurricane Hazard")
        points(h$prob, h$rl, col="blue", pch="o", cex=1)
        points(hurricane_random_probabiities, hurricane_random_velocities, col="black", pch=".", cex=0.1)
        legend("topright", legend=c("Hurricane Hazard Curve", "Input Hazard from Excel", "Random Realizations"),
               col=c("red", "blue", "black"), cex=0.8, pch=c(NA, "o", "."), lty = c(1, NA, NA), lwd=c(2, NA, NA))
        
        dev.off()
      }
      
    # Non-Hurricane hazard curve
      if (!any(is.na(hc_nh$prob)) & !any(is.na(hc_nh$rl))){  
        name_pdf = paste0(output_path, "/", "hazard_curve_nonhurricane", contar, "_", station, ".pdf")
        if (file.exists(name_pdf)) {
          file.remove(name_pdf)
        }
        pdf(name_pdf)
        plot(hc_nh$prob, hc_nh$rl, col="red", lwd=3, type="l", xlab="Exceedance Probabilities", ylab="Velocities (kph)", 
              main="Non-hurricane hazard")
        points(( 1 - non_hurricane_random_probabilities), non_hurricane_random_velocities, col="black", pch=".", cex=0.1)
        legend("topright", legend=c("Non-Hurricane Hazard Curve", "Random Realizations"),
               col=c("red", "black"), cex=0.8, pch = c(NA, "."), lty = c(1, NA), lwd=c(2, NA))
        #points(h$prob, h$rl, col="blue", pch=18, cex=2)
        dev.off()
      }
      
    # Combined hazard curve
      if (!any(is.na(hc_h$prob)) & !any(is.na(hc_h$rl))){    #NOTE: checking with hc_h because hc_c may not exist 
                                                             # if no data for hurricanes!
        name_pdf = paste0(output_path, "/", "hazard_curve_combined", contar, "_", station, ".pdf")
        if (file.exists(name_pdf)) {
          file.remove(name_pdf)
        }
        pdf(name_pdf)
        plot(hc_c$prob, hc_c$rl, col="red", type="l", lwd=3, xlab="Exceedance Probabilities", ylab="Velocities (kph)", 
              main="Combined Hazard")
        points(combined_random_probabilities, combined_random_velocities, col="black", pch=".", cex=0.1)
        points((1- non_hurricane_random_probabilities), non_hurricane_random_velocities, col="blue", pch="o", cex=0.1)
        points(hurricane_random_probabiities, hurricane_random_velocities, col="green", pch="x", cex=0.1)
        legend("topright", legend=c("Combined Hazard Curve", "Combined Probabilities", "Non-Hurricane Random Realizations", "Hurricane Random Realizations"),
               col=c("red", "black", "blue", "green"), cex=0.8, pch=c(NA, ".", "o", "x"), lty = c(1, NA, NA, NA), lwd=c(2, NA, NA, NA))
        #points(h$prob, h$rl, col="blue", pch=18, cex=2)
        dev.off()
        #browser()
        colnames(hc_h)[colnames(hc_h) == 'prob'] <- 'Hurricane'
        colnames(hc_nh)[colnames(hc_nh) == 'prob'] <- 'Non-hurricane'
        colnames(hc_c)[colnames(hc_c) == 'prob'] <- 'Combined'
        
        
        data1 = merge(x = hc_h, y = hc_nh, by = "rl", all=TRUE)
        data2 = merge(x = data1, y = hc_c, by = "rl", all=TRUE)
        data2 = data2[data2$rl < 300, ]
        
        dat.m <- reshape2::melt(data2, id.vars = "rl")
        #browser()
        labels = paste0(data2$rl)
        dat.m <- within(dat.m, rl <- factor(rl, labels = labels))
        pal = RColorBrewer::brewer.pal(3, "Dark2")
        title = paste0("Hazard Curves.\n", "ERA5 Cell: ", station)
        
        p <- ggplot(dat.m, aes(value, y=rl, colour=variable, group=variable)) +
                    geom_point(size=0.02) + 
                    geom_line(data=dat.m[!is.na(dat.m$value),]) +
                    scale_colour_manual(values = pal) +
                    labs(title=title, x="Exceedance Probability", y = "Velocity (kph)", colour="Hazard Curve") + 
                    theme(plot.title = element_text(size=10)) +
                    #theme(axis.text.x = element_text(angle = 90))+
                    scale_y_discrete(breaks=c(50, 100, 150, 200, 250), labels=c(50, 100, 150, 200, 250))
        
        name_pdf = paste0(output_path, "/", "hazard_curve_combined_ggplot", contar, "_", station, ".pdf")
        if (file.exists(name_pdf)) {
          file.remove(name_pdf)
        }
        pdf(name_pdf)
        print(p)
        dev.off()
      }
  }
  
  # Get determinitic values from inputs
  # names_of_deterministics = c("Kzt", "Ke") to variables
  # values_of_deterministics = c(1, 1)
  for (i in 1:length(names_of_deterministics)) {
    if (names_of_deterministics[i] == "Kzt"){
      Kzt = values_of_deterministics[i]
    }
    else if (names_of_deterministics[i] == "Ke") {
      Ke = values_of_deterministics[i]
    }
  } 
  
  #Assign random realizations to variables
  #names_of_rvs = c("Kh", "Kd", "GCp", "GCpi", "R") to variables
  for (i in 1:length(names_of_rvs)) {
    if (names_of_rvs[i] == "Kh"){
      Kh = RVS[,i]
    }
    else if (names_of_rvs[i] == "Kd") {
      Kd = RVS[,i]
    }
    else if (names_of_rvs[i] == "GCp") {
      GCp = RVS[,i]
    }
    else if (names_of_rvs[i] == "GCpi") {
      GCpi = RVS[,i]
    }
    else if (names_of_rvs[i] == "R") {
      R = RVS[,i] * 1000  #>>>>>>>> NOTE:convert from kN/m2 to N/m2 using 1000
      RVS[,i] = R  #>>>> Change units in the dataset
      mean_r = mean(R)
      max_r = max(R)
      min_r = min(R)
      sd_r = sd(R)
      cov_r = sd_r/mean_r
    }
    else if (names_of_rvs[i] == "VNH") {
      #velocities_nh = RVS[,i] * 0.277778 # Change units in velocity from KPH to M/S, but not in the Output (the outputs will be shown in Kph), but
                                      # the calculations will be in m/s
                               
      #RVS[,i] = velocities_nh * 0.277778  #>>>> Do no change units in the dataset (The Output will be in Kph, calculations here in m/s)
      mean_vnh = mean(RVS[,i]) #Means in KPH
      max_vnh = max(RVS[,i])
      min_vnh = min(RVS[,i])
      sd_vnh = sd(RVS[,i]) 
      cov_vnh = sd_vnh/mean_vnh   # C.O.V in KPH
    }
    else if (names_of_rvs[i] == "VH") {
      #velocities_nh = RVS[,i] * 0.277778 # Change units in velocity from KPH to M/S, but not in the Output (the outputs will be shown in Kph), but
      # the calculations will be in m/s
      
      #RVS[,i] = velocities_nh * 0.277778  #>>>> Do no change units in the dataset (The Output will be in Kph, calculations here in m/s)
      mean_vh = mean(RVS[,i]) #Means in KPH
      max_vh = max(RVS[,i])
      min_vh = min(RVS[,i])
      sd_vh = sd(RVS[,i]) 
      cov_vh = sd_vh/mean_vh   # C.O.V in KPH
    }
  }
  
  #Add mean_v and cov_v to the output
  the_means = c("min_R"=min_r, "mean_R"=mean_r, "max_R"=max_r, 
                "min_VNH"=min_vnh, "mean_VNH"=mean_vnh, "max_VNH"=max_vnh, 
                "min_VH"=min_vh, "mean_VH"=mean_vh, "max_VH"=max_vh,
                "min_VC"=min_vc, "mean_VC"=mean_vc, "max_VC"=max_vc)
  
  the_covs = c("cov_R"=cov_r, "cov_VNH"=cov_vnh, "cov_VH"=cov_vh, "cov_VC"=cov_vc)
  
  # ******1) FIRST OPTION: Velocity as R.V. # Hurricane and Non-hurricane as R.V.S
  #
  #
  
  qh = 0.613 * Kh * Kzt *  Kd * Ke * (velocities_c^2)
  p1 = qh * (GCp - GCpi)  # N/m2 (positive GCpi)
  p2 = qh * (GCp - (-1*GCpi))  # N/m2 (negative GCpi)
  p = pmax(abs(p1), abs(p2))
  RVS = cbind(RVS, p)
  colnames(RVS)[ncol(RVS)] = "S"
  
  #Save means and c.o.v
  mean_s_rv = mean(RVS[, ncol(RVS)])
  max_s_rv = max(RVS[, ncol(RVS)])
  min_s_rv = min(RVS[, ncol(RVS)])
  
  sd_s_rv = sd(RVS[, ncol(RVS)])
  cov_s_rv = sd_s_rv/mean_s_rv
  the_means = c(the_means, "min_S"=min_s_rv, "mean_S"=mean_s_rv, "max_S"=max_s_rv)
  the_covs = c(the_covs, "cov_S"=cov_s_rv)
  
  
  #Calculate probability of failure as the times load (p) > capacity divided by number of samples
  pf_this_row = sum(abs(p) > abs(R)) / number_of_samples
  RVS = cbind(RVS, abs(p) > abs(R))
  colnames(RVS)[ncol(RVS)] = "f"
  
  pf = c("pf"=pf_this_row)
  #
  beta_this_row = qnorm((1-pf_this_row))
  beta = c("b"=beta_this_row)
  
  # ******2) SECOND OPTION: Fixed Values of Velocity (including non-hurricane and hurricane).
  #                        Note: Valid probability of failure, but given fixed velocities, so this failure probabilities are not the
  #                              final probability of failure of the structure
  #
  # 
  # Read the velocities from bands_c
  #V = NULL
  # for (band in bands_c) {
  #  V = c(V, as.double(st[band]))  #st[]: the value of each row in st for specific band (previous attribute with same order)
  # }
  #VC = as.double(st[bands_c]) # Note: Input was in kph
  VC = as.double(st[bands_fixed_vel]) # Note: Input was in kph
  print("Velocities Combined - Fixed in simulations:")
  print(VC)
  # #Check data with problems in V
  if (any(!is.na(VC))){
    if (sum(VC) == 0 | sum(VC) < 0) {
      VC = rep(NA, length(bands_fixed_vel))
    }
  }
  
  #RVS[,ncol(RVS)] = (RVS[,ncol(RVS)]) #>>>>>>>> NOTE:convert from kN/m2 to N/m2 using 1000 
  #>>>>>>>> CAPACITY HAVE TO BE THE LAST COLUMN!
  #capacity = RVS[,ncol(RVS)]
  # Calculate pf with LHS - MCS
  #pf = NULL
  #beta = NULL
  for (j in 1:length(VC)) {
    each_v = VC[j] * 0.277778 #>>>>>>> Velocity: NOTE: Conversion from Kph to m/s
    rl = mri_fixed_vel[j]
    #for (each_v in V) {
    # Parameters related to wind
    #
    # Apply formula to calculate wind velocity pressure Kh:
    # kh = 0.613 * Kh * Kzt *  Kd * Ke * (V^2)
    #kh = 0.613 * RVS[,1] * values_of_deterministics[1] *  RVS[,2] * values_of_deterministics[2] * (each_v*each_v)
    c("Kh", "Kd", "GCp", "GCpi", "R")
    qh = 0.613 * Kh * Kzt *  Kd * Ke * (each_v^2)
    #
    # Parameters related to the structure
    #
    # Apply formula to wind pressure at the building component sheathing panel [ 8 ft * 4 ft]
    # I don't need to use area of the panel, because capacity is in N/m2 and p in N/m2
    # p = kh * (GCp - GCpi) # p is wind pressure or load N/m2
    p1 = qh * (GCp - GCpi)  # N/m2 (positive GCpi)
    p2 = qh * (GCp - (-1*GCpi))  # N/m2 (negative GCpi)
    p = pmax(abs(p1), abs(p2))
    RVS = cbind(RVS, p)
    colnames(RVS)[ncol(RVS)] = paste0("S_", rl)
    
    #save means and c.o.v
    mean_s = mean(RVS[, ncol(RVS)])

    sd_s = sd(RVS[, ncol(RVS)])
    cov_s = sd_s/mean_s
    the_means = c(the_means, mean_s)
    names(the_means)[length(the_means)] = paste0("mean_s_", rl)
    the_covs = c(the_covs, cov_s)
    names(the_covs)[length(the_covs)] = paste0("cov_s_", rl)
    
    #Calculate probability of failure as the times load (p) > capacity divided by number of samples
    pf_this_row = sum(abs(p) > abs(R)) / number_of_samples
    RVS = cbind(RVS, abs(p) > abs(R))
    colnames(RVS)[ncol(RVS)] = paste0("f_", rl)
    
    pf = c(pf, pf_this_row)
    names(pf)[length(pf)] = paste0("pf_", rl)
    #
    beta_this_row = qnorm((1-pf_this_row))
    beta = c(beta, beta_this_row)
    names(beta)[length(beta)] = paste0("b_", rl)
    #colnames(pf)[ncol(pf)] = paste0("pf_", rl)
  }
  
  print(paste0("LHS Uniform for ", number_of_rvs, " variables"))
  print(U)
  
  print(paste0("MCS for ", number_of_rvs, " random variables:"))
  print("1) Probability of Failure and Betas for fixed velocities at given MRI")
  print("2) Probability of Failure and Betas for velocities as random variable")
  print("--Note: Velocities are shown in Kph (source units), but internally are processed in m/s (multiplied by 0.277778)")
  
  print(RVS)
  return(c(the_means, the_covs, pf, beta))
}



stats_data <- function(data, data_label="Raw Data", prefix="all", numberofplots=numberofplots, exel_file=statsfile_OUT)
  {
  #If there are records in the raw data time series, 
  # 1) Generate statistics by years, weeks, months, gaps, and save the results  
  #      to the Excel file raw_data_station_ID_statistics.xlsx (variable 'statsfile'), 
  #      sheets all_years, all_weeks, all_months, all_gaps respectively.
  # 2) Generate the time series plot for raw data (thunderstorm and non-thunderstorm)
  if (length(data$date.time) > 0) {
    #Print raw.data histogram
    title= paste("Frequency Histogram of ",  data_label, "\n", 
                 "Station: ", number, sep="")
    print(hist(data$speed.kph, probability = FALSE, col="cadetblue3", main=title))
    mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
    numberofplots = numberofplots + 1
    
    #Use the function 'generate_stats_time_serie' to create the statistics based on four parameters:
    # data: tibble data frame with time series
    # variable: column name of time series (for instance: "speed.kph")
    # time: column name with date time (POSIXct) column  (for instance: data$date.time)
    # index: period for the statistics (for instance: "years", "months", "weeks")
    years = generate_stats_time_serie(data, "speed.kph", data$date.time, "years")
    months = generate_stats_time_serie(data, "speed.kph", data$date.time, "months")
    weeks = generate_stats_time_serie(data, "speed.kph", data$date.time, "weeks")
    
    #myxts = na.omit(xts(x=select(data, "speed.kph"), order.by = data$date.time))
    #Split dataset by year
    #xts5_yearly <- split(myxts,f="years") #Convert to [[]]
    #sc = lapply(xts5_yearly, cumsum) #Calcular suma cumulativa but still [[]]
    #do.call(rbind,  sc) #Unstack [[]] and leave all in one index []
    
    #statistics <- list()
    #statistics[[1]] = years
    #statistics[[2]] = months
    #statistics[[3]] = weeks
    #lapply(statistics, write, statsfile, append=TRUE)
    
    #library(xlsx)
    #Write to a different sheet name: all_years, all_months, all_weeks
    #write.xlsx(years, file=statsfile, sheetName="all_years", row.names=TRUE)
    addWorksheet(exel_file, paste0(prefix, "_years"))
    writeData(exel_file, sheet = paste0(prefix, "_years"), x = years)
    
    #write.xlsx(months, file=statsfile, sheetName="all_months", append=TRUE, row.names=TRUE)
    addWorksheet(exel_file, paste0(prefix, "_months"))
    writeData(exel_file, sheet = paste0(prefix, "_months"), x = months)
    
    #write.xlsx(weeks, file=statsfile, sheetName="all_weeks", append=TRUE, row.names=TRUE)
    addWorksheet(exel_file, paste0(prefix, "_weeks"))
    writeData(exel_file, sheet = paste0(prefix, "_weeks"), x = weeks)
    
    #Write gaps statistics 
    # search time differences in days between consecutive samples 
    # greather than threshold in days (last parameter next function)
    thresholdindays = 30
    holesindays = locate_holes_time_serie(data, "speed.kph", data$date.time, thresholdindays)
    if (length(holesindays) == 0){
      holesindays = "No holes!"
    }
    #write.xlsx(holesindays, file=statsfile, sheetName=paste0("all_gaps",thresholdindays,"days"), append=TRUE, row.names=FALSE)  
    addWorksheet(statsfile_OUT, paste0(prefix, "_gaps",thresholdindays,"days"))
    writeData(statsfile_OUT, sheet = paste0(prefix, "_gaps",thresholdindays,"days"), x = holesindays)
    
    #Plot time series
    #library(xts)
    #myxts = na.omit(xts(x=select(data, "speed.kph"), order.by = data$date.time))
    #par(oma = c(2,0,0,0))
    #main=paste0("Time Series Plot for Raw.Data\nStation: ", number, " - Wind Velocity [Km/h]")
    #print(plot.xts(myxts, main=main, major.ticks="year", format.labels = "%b-%d\n%Y",
    #         col="green", legend.loc = "top", cex.main=0.2))
    #mtext(side = 1, text = paste0("Page ",numberofplots, " - Time Series Plot for Raw.Data - Station: ", number), outer = TRUE)
    print(plotxts(data=data, variable="speed.kph", time=data$date.time,
                  cex.main=0.2, major.ticks="years",
                  xlab=paste0("Page ",numberofplots, " - Time Series Plot for ", data_label, " - Station: ", number),
                  main = paste0("Station ID: ",  number, "\nWind Velocity [Km/h]")))
    #assign(paste0("myprint", numberofplots), recordPlot())
    #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))
    
    #Increase the number of plots after printing in the PDF
    numberofplots = numberofplots + 1
  }
  return(numberofplots)
}

yearly_maxima_and_graphs <- function(data=raw.data.nt, 
                                     data_label=paste0("Non-thunderstorm raw data for Station ID:", number), 
                                     prefix="nt", 
                                     numberofplots=numberofplots,
                                     tipicalReturnPeriods = c(10,20,50,100,250,500,700,1000,1700,3000,7000),
                                     exel_file=fnfitted_OUT,
                                     ym_mle_gev=nt_ym_mle_gev,
                                     ym_lmom_gev=nt_ym_lmom_gev)
{
  if (length(data$date.time) > 0) {
    #Apply yearly maxima method and print its graphics
    #tipicalReturnPeriods = c(10,20,50,100,250,500,700,1000,1700,3000,7000)
    myextrRemesyearlymaxima = alexys_extRemes_yearlymaxima(x=data, RPs=tipicalReturnPeriods,
                                                           variable.description=data_label)
    #write.xlsx(myextrRemesyearlymaxima$a, file=fnfitted, sheetName="nt_rawdata_RL_yearlymaxima_GEV", append=TRUE, row.names=TRUE)
    addWorksheet(exel_file, paste0(prefix, "_rawdata_RL_yearlymaxima_GEV"))
    writeData(exel_file, sheet = paste0(prefix, "_rawdata_RL_yearlymaxima_GEV"), x = myextrRemesyearlymaxima$a)
    
    numberofplots = myextrRemesyearlymaxima$b
    
    #sheet nt_rawdata_RL_yearlymaxima_GEV (row 2)(row 1 are names)
    #function: alexys_extRemes_yearlymaxima
    #package: extRemes::fevd(ams, method="MLE", type="GEV") >>> extRemes::return.level
    #method: mle
    ymmlegev = myextrRemesyearlymaxima$a[1,1:11]
    ym_mle_gev[i,10:20]=ymmlegev
    ym_mle_gev[i,3]=myextrRemesyearlymaxima$a[1,13] # location
    ym_mle_gev[i,4]=myextrRemesyearlymaxima$a[1,14] # scale
    ym_mle_gev[i,5]=myextrRemesyearlymaxima$a[1,15] # #shape
    
    #sheet nt_rawdata_RL_yearlymaxima_GEV(row 3)(row 1 are names)
    #function: alexys_extRemes_yearlymaxima
    #package: fit_lmom <- extRemes::fevd(ams, method = "Lmoments", type="GEV") >>> extRemes::return.level
    #method: lmoments
    ymlmomgev = myextrRemesyearlymaxima$a[2,1:11]
    ym_lmom_gev[i,10:20]=ymlmomgev
    ym_lmom_gev[i,3]=myextrRemesyearlymaxima$a[2,13] # location
    ym_lmom_gev[i,4]=myextrRemesyearlymaxima$a[2,14] # scale
    ym_lmom_gev[i,5]=myextrRemesyearlymaxima$a[2,15] # #shape
  }
  return(list("numberofplots"=numberofplots, "ym_mle_gev"=ym_mle_gev, "ym_lmom_gev"=ym_lmom_gev))
}


#Write "t" to csv, but changing to one data per day (the maximun)
#Write "nt" to csv, but changing to one data per day (the maximun)
tnt_csv_1perday <- function(date, series, check, type){
  if (check) {
    #Write "t" to csv, but changing to one data per day (the maximun)
    data = data.frame(date=date, series=series)
    data = as_tibble(data)
    library(xts)
    library(dplyr)
    select <- dplyr::select
    myxts = na.omit(xts(x=select(data, "series"), order.by = data$date))
    endp = endpoints(myxts,on="days")
    period = period.apply(myxts,INDEX=endp,FUN=max)
    #indexFormat(period) <- "%Y-%m-%d"
    period2 = data.frame(date=format(index(period),"%Y-%m-%d"), series=period$series, stringsAsFactors =FALSE)
    rownames(period2) = NULL
    #period2 = as.xts(period2$speed.kph, order.by=as.Date(period2$date,"%Y-%m-%d"))
    #colnames(period2) = c("speed.kph")
    #write.zoo(period2,sep=";",file=paste0("zoo_", type, number, ".csv"))
    #write.table(period2,file=paste0(type, number, ".csv"),sep=";", row.names=FALSE)
  }
}



#plot histogram (thunderstorm or non-thunderstorm)
plot_frequency_histogram <- function(series=imp.vals$nt.series, data_label="Non-Thunderstorm", numberofplots=numberofplots, check=length(series) > 0)
{
  if (check){
    #Print raw.data histogram
    title= paste("Frequency Histogram of Decluster and Thresholding ", data_label, "\n", 
                 "Station: ", number, sep="")
    print(hist(series, probability = FALSE, col="cadetblue3", main=title))
    mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
    numberofplots = numberofplots + 1
  }
  return(numberofplots)
}

#Save time series to text file
save_time_series <- function(date=imp.vals$nt.series.dt, series=imp.vals$nt.series, prefix="dnt_", outputpath, check=length(series) > 0){
  if (check){
    #save declustered nonthunderstorm and thunderstorm time series
    mydnt=data.frame(dnt=series, time =date)
    #write mydnt declustered nonthunderstorm data
    write.table(mydnt,file=paste0(outputpath, prefix, number, ".csv"),sep=";", row.names=FALSE)
  }
}

#Fit gumbel curve using different options
#save parameters to excelfile
#plot the results
#  - Send to PDF (FittedModel_ID.pdf) graphics for declustered (nt or t) (Gumbel fittings and GPD-Poisson-GPD extremeStat results)
#  - Fitted Gumbel Probability Density Curve - Log-Likelihood(Gumbel) - Optim (nll-optim)
#  - Declustered - Non-Thunderstorm - fitdistrplus-fitdist(gumbel)
fit_gumbel_different_options <- function(date=imp.vals$nt.series.dt, series=imp.vals$nt.series, obsperyears = imp.vals$n.nthunders.per.year,
                                         threshold=nt.thresh, shape=0, excelfile=fnfitted_OUT, prefix="nt", data_label="Non-Thunderstorm",
                                         numberofplots=numberofplots, check=length(series) > 0){
  if (check){
    ## 1)evd::fpot (check it)
    #
    library(evd)
    library(lubridate)
    potdata = data.frame(time=lubridate::decimal_date(date[1:length(series)]), obs=series)
    #obsperyears = length(series)/(imp.vals$total.time/365.25)
    #obsperyears = imp.vals$n.nthunders.per.year
    M2 <- evd::fpot(potdata$obs, threshold = threshold, cmax=FALSE, npp=obsperyears, model="pp", shape = shape, std.err = FALSE)
    myfit = cbind("loc" = M2$estimate[1], "scale" = M2$estimate[2])
    #If have time please review this pp some day!!
    #write.xlsx(myfit, file=fnfitted, sheetName="nt_pp-evd", append=TRUE, row.names=FALSE, col.names=TRUE)
    #M2Ploc <- profile(M2, conf=0.975)
    #plot(M2Ploc)
    #M2Pscale <- profile(M2, which = "scale", conf=0.975, mesh=c(0.001))
    #plot(M2Pscale)
    #If have time please review this pp some day!!
    #par(mfrow = c(2,2))
    #plot(M2)
    #mtext(side = 1, text = paste0("Page ", numberofplots, " - Declustered - Non-Thunderstorm - Package EVD - Fitting a PP. Location: ",
    #        round(M2$estimate[1], digits=2), ". Scale: ", round(M2$estimate[2], digits = 2)),
    #      outer = TRUE)
    #numberofplots = numberofplots + 1
    
    ## 2) function fGumbel & evd::fgev
    #
    #Use function fGumbel to estimate parameters
      require(evd)
      library
      cat(number)
      #Note fit.evd (shape=0) = fit.new 
      #fit.evd <- evd::fgev(x=series, shape = 0.0)
      fit.evd <- evd::fgev(x=series)
      fit.new <- fGumbel(series)
      #myfit = cbind("evd::fgev" = c(fit.evd$estimate, "deviance" = fit.evd$deviance),
      #              "fGumbel" = c(fit.new$estimate, fit.new$deviance))
      myfit = cbind("params>" =c(names(fit.evd$estimate), "deviance"), "evd::fgev" = c(fit.evd$estimate, "deviance" = fit.evd$deviance),
                    "params>" = c(names(fit.new$estimate), "deviance"), "fGumbel" = c(fit.new$estimate, fit.new$deviance))
      locbyfgev = fit.evd$estimate[1]
      scalebyfgev = fit.evd$estimate[2]
      
      #write.xlsx(myfit, file=fnfitted, sheetName="nt_evd-fgev_fGumbel_Gumbel", append=TRUE, row.names=TRUE)
      addWorksheet(excelfile, paste0(prefix, "_evd-fgev_fGumbel_Gumbel"))
      writeData(excelfile, sheet = paste0(prefix, "_evd-fgev_fGumbel_Gumbel"), x = myfit)
    
    #
    
    ## 3)Use function logLH
    #
    #Estimators of moments method???
    #using bbmle::mle2 (maximum likelihood estimator)
      mu = mean(series) + (0.45006 * sd(series))
      sigma = (sd(series)*sqrt(6))/pi
      library(bbmle)
      est <- bbmle::mle2(logLH, start = list(mu = mu, sigma = sigma), data = list(x = series))
      intervals = confint(est, level = 0.95)
      myfit = list("mu2.5" = intervals["mu",1], "mu(location)"=est@coef[1], "mu97.5" = intervals["mu",2],
                   "sigma2.5" = intervals["sigma",1], "sigma(scale)"=est@coef[2], "sigma97.5" = intervals["sigma",2])
      myfit = rbind(names(myfit), myfit)
      #write.xlsx(myfit, file=fnfitted, sheetName="nt_bbmle-mle2_Gumbel", append=TRUE, row.names=TRUE)
      addWorksheet(excelfile, paste0(prefix, "_bbmle-mle2_Gumbel"))
      writeData(excelfile, sheet = paste0(prefix, "_bbmle-mle2_Gumbel"), x = myfit)
    
    ## 4)Use the minus log-likelihood (function mllGumbel) and optim
    #
      mllToBeOptimized <- function(par)
        mllGumbel(par[1], par[2], series)
      mle <- optim(c(mu, sigma), mllToBeOptimized)$par
      
      #dgumbel <- function(x,mu,sigma){ # PDF
      #  exp((mu - x)/sigma - exp((mu - x)/sigma))/sigma
      #}
      par(mfrow = c(1,1))
      hist (series, probability = TRUE, col='cadetblue3',
            xlab=paste0("Declustered - ", data_label, " Series"), main="Data Histogram and Fitted Gumbel Probability Density Curve")
      curve(dgumbel1(x, mle[1], mle[2]), col = "red", add = TRUE)
      mtext(side = 1, text = paste0("Page ", numberofplots, " - Log-Likelihood(Gumbel) - Optim (nll-optim). Location: ",
                                    round(mle[1], digits=2), ". Scale: ", round(mle[2], digits=2)), outer = TRUE)
      #legend("topright", c("Data", "Fitted"), fill=c("cadetblue3", "red"))
      #legend("topright", c("Data", "Fitted"), col=c("cadetblue3", "red"), lwd=c(15,1))
      legend("topright", c("Data-Empirical", "Fitted-Theoretical"),
             bty = "n",
             col = c("cadetblue3", "red"),
             lty = c(0, 1), lwd = c(0, 1),
             pch = c(22, NA),
             pt.bg = c("cadetblue3", NA),
             pt.cex = 2)
      #assign(paste0("myprint", numberofplots), recordPlot())
      #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))
      numberofplots = numberofplots + 1
      
      #Write results of option 4
      myfit = list("mu(location)"=mle[1], "sigma(scale)"=mle[2])
      myfit = rbind(names(myfit), myfit)
      #write.xlsx(myfit, file=fnfitted, sheetName="nt_nll-optim_Gumbel", append=TRUE, row.names=TRUE)
      addWorksheet(excelfile, paste0(prefix, "_nll-optim_Gumbel"))
      writeData(excelfile, sheet = paste0(prefix, "_nll-optim_Gumbel"), x = myfit)
    
    ## 5) library(fitdistrplus)
    #
      #Use package library(fitdistrplus)
      #Use mle
      #starting values for mu (location) and sigma (scale) comes from number 3
      library(fitdistrplus)
      gumbel.fit <- fitdistrplus::fitdist(series, "gumbel1", start=list(mu=mu, sigma=sigma), method="mle")
      gofstat(gumbel.fit, discrete=FALSE) # goodness-of-fit statistics
      par(cex=1.2, bg="white")
      plot(gumbel.fit, lwd=2, col="cadetblue3")
      mtext(side = 1, text = paste0("Page ", numberofplots, " - Declustered - ", data_label, " - fitdistrplus-fitdist(gumbel). Location: ",
                                    round(gumbel.fit$estimate["mu"], digits = 2), ". Scale: ", round(gumbel.fit$estimate["sigma"], digits = 2)),
            outer = TRUE)
      #assign(paste0("myprint", numberofplots), recordPlot())
      #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))
      numberofplots = numberofplots + 1
      myfit = list("mu(location)"=gumbel.fit$estimate["mu"], "sigma(scale)"=gumbel.fit$estimate["sigma"])
      myfit = rbind(names(myfit), myfit)
      #Write results of option 5
      #write.xlsx(myfit, file=fnfitted, sheetName="nt_fitdistrplus-fitdist_Gumbel", append=TRUE, row.names=TRUE)
      addWorksheet(excelfile, paste0(prefix, "_fitdistrplus-fitdist_Gumbel"))
      writeData(excelfile, sheet = paste0(prefix, "_fitdistrplus-fitdist_Gumbel"), x = myfit)
  }
  return(numberofplots)
}


#Fitting generalized patetto using function
#alexys_extremes (package: extRemes)
#write to excel file
pot_gp <- function(series=imp.vals$nt.series, threshold=nt.thresh, tipicalReturnPeriods= c(10,20,50,100,250,500,700,1000,1700,3000,7000),
                   npy=imp.vals$n.nthunders.per.year, excelfile=fnfitted_OUT, prefix="nt", check=length(series) > 0, 
                   pot_mle_gp=nt_pot_mle_gp){
  if (check) {
    #extRemes Alexys
    #library(extRemes)
    #tipicalReturnPeriods = c(10,20,50,100,250,500,700,1000,1700,3000,7000)
    #npy=imp.vals$n.nthunders.per.year
    myextrRemes = alexys_extRemes(series, threshold=threshold,
                                  RPs=tipicalReturnPeriods, npy=npy)
    #write.xlsx(myextrRemes, file=fnfitted, sheetName="nt_extRemes_GP", append=TRUE, row.names=TRUE)
    addWorksheet(excelfile, paste0(prefix, "_extRemes_GP"))
    writeData(excelfile, sheet = paste0(prefix, "_extRemes_GP"), x = myextrRemes)
    
    #function alexys_extRemes (POT - MLE - GP - extRemes::fevd)
    #  extRemes::fevd(x, method="MLE", type="GP"..)
    #sheet nt_extRemes_GP
    #method: mle
    #browser()
    gppotmle = myextrRemes[ ,1:11]
    pot_mle_gp[i,10:20]=gppotmle
    pot_mle_gp[i,3]=myextrRemes[ ,12] #threshold = location
    pot_mle_gp[i,4]=myextrRemes[ ,13] #alpha scale
    pot_mle_gp[i,5]=myextrRemes[ ,14] #shape NA
  }
  return(pot_mle_gp)
  
}

#extRemes berry
#Fitting many probability distributions functions using extremeStat package,
#which is based on package lmomco for the fittings
#write to excel file
pot_extremeStat <- function(series=imp.vals$nt.series, threshold=nt.thresh, tipicalReturnPeriods= c(10,20,50,100,250,500,700,1000,1700,3000,7000),
                   npy=imp.vals$n.nthunders.per.year, excelfile=fnfitted_OUT, prefix="nt", check=length(series) > 0,
                   pot_lmon_gum=nt_pot_lmon_gum,
                   pot_lmon_wei=nt_pot_lmon_wei,
                   pot_lmom_ln3=nt_pot_lmom_ln3,
                   pot_lmom_gpa=nt_pot_lmom_gpa,
                   pot_lmom_exp=nt_pot_lmom_exp,
                   pot_lmom_gam=nt_pot_lmom_gam,
                   pot_lmom_kap=nt_pot_lmom_kap,
                   pot_lmom_revgum=nt_pot_lmom_revgum,
                   pot_lmom_gev=nt_pot_lmom_gev){
  if (check){
    library(extremeStat)
    #npy=imp.vals$n.nthunders.per.year   #Number of observations per year
    #w = length(series)/npy  #Fitting period: Total observations divided in npy
    #Observations over threshold
    overthresh = series > threshold
    #overthreshold = imp.vals$t.series >= z2
    #lambda = length(series[overthresh])/w
    #tipicalReturnPeriods = c(10,20,50,100,250,500,700,1000,1700,3000,7000)
    p = (1 - (1/(npy*tipicalReturnPeriods)))
    
    truncate = 1 - (sum(overthresh)/length(series))
    d <- distLquantile(series, truncate=truncate, probs=p, quiet=TRUE, list=TRUE)
    
    #plotLquantile(d, breaks=50, xlab="Declustered - Non-Thunderstorm - plotLquantile {extremeStat}")
    #mtext(side = 1, text = paste0("Page ",
    #      numberofplots, " - Station: ", number), outer = TRUE)
    #numberofplots = numberofplots + 1
    
    #write.xlsx(d$quant, file=fnfitted, sheetName="nt_distLquantile_quant", append=TRUE, row.names=TRUE)
    addWorksheet(excelfile, paste0(prefix, "_distLquantile_quant"))
    myDF <- data.frame(pdf = row.names(d$quant), d$quant)
    writeData(excelfile, sheet = paste0(prefix, "_distLquantile_quant"), x = myDF)
    
    #write.xlsx(capture.output(d$parameter), file=fnfitted, sheetName="nt_distLquantile_parameters", append=TRUE, row.names=TRUE)
    addWorksheet(excelfile, paste0(prefix, "_distLquantile_parameters"))
    writeData(excelfile, sheet = paste0(prefix, "_distLquantile_parameters"), x = capture.output(d$parameter))
    
    dlf <- distLextreme(series, quiet=TRUE, RPs=tipicalReturnPeriods, npy=npy, truncate=truncate)
    
    #plotLextreme(dlf, log=TRUE, legargs=list(cex=0.6, bg="transparent"), xlab="Return Period - RP", ylab="Velocidades [Km/h]", xlim=c(10,7000), ylim=c(20,250))
    #mtext(side = 1, text = paste0("Page ",
    #                              numberofplots, " - Declustered - Non-Thunderstorm - plotLextreme {extremeStat} - Station: ", number), outer = TRUE)
    #numberofplots = numberofplots + 1
    #write.xlsx(dlf$returnlev, file=fnfitted, sheetName="nt_distLextreme_returnlev", append=TRUE, row.names=TRUE)
    addWorksheet(excelfile, paste0(prefix, "_distLextreme_returnlev"))
    myDF <- data.frame(pdf = row.names(dlf$returnlev), dlf$returnlev)
    writeData(excelfile, sheet = paste0(prefix, "_distLextreme_returnlev"), x = myDF)
    
    #nt pot gumbel return levels
    gumrl = as.numeric(dlf$returnlev[which(rownames(dlf$returnlev) == "gum"), ])
    weirl = as.numeric(dlf$returnlev[which(rownames(dlf$returnlev) == "wei"), ])
    ln3rl = as.numeric(dlf$returnlev[which(rownames(dlf$returnlev) == "ln3"), ])
    gparl = as.numeric(dlf$returnlev[which(rownames(dlf$returnlev) == "gpa"), ])
    exprl = as.numeric(dlf$returnlev[which(rownames(dlf$returnlev) == "exp"), ])
    gamrl = as.numeric(dlf$returnlev[which(rownames(dlf$returnlev) == "gam"), ])
    kaprl = as.numeric(dlf$returnlev[which(rownames(dlf$returnlev) == "kap"), ])
    revgumrl = as.numeric(dlf$returnlev[which(rownames(dlf$returnlev) == "revgum"), ])
    gevrl = as.numeric(dlf$returnlev[which(rownames(dlf$returnlev) == "gev"), ])
    
    pot_lmon_gum[i,10:20]=gumrl
    pot_lmon_wei[i,10:20]=weirl
    pot_lmom_ln3[i,10:20]=ln3rl
    pot_lmom_gpa[i,10:20]=gparl
    pot_lmom_exp[i,10:20]=exprl
    pot_lmom_gam[i,10:20]=gamrl
    pot_lmom_kap[i,10:20]=kaprl
    pot_lmom_revgum[i,10:20]=revgumrl
    pot_lmom_gev[i,10:20]=gevrl
    
    
    #write.xlsx(capture.output(dlf$parameter), file=fnfitted, sheetName="nt_distLextreme_parameter", append=TRUE, row.names=TRUE)
    addWorksheet(excelfile, paste0(prefix, "_distLextreme_parameter"))
    writeData(excelfile, sheet = paste0(prefix, "_distLextreme_parameter"), x = capture.output(dlf$parameter))
    #browser()
    #nt pot gumbel parameters
    pot_lmon_gum[i,3]=dlf$parameter$gum$para[1] #xi location
    pot_lmon_gum[i,4]=dlf$parameter$gum$para[2] #alpha scale
    pot_lmon_gum[i,5]=NA #shape NA
    
    pot_lmon_wei[i,3]=dlf$parameter$wei$para[1] #zeta location
    pot_lmon_wei[i,4]=dlf$parameter$wei$para[2] #beta scale
    pot_lmon_wei[i,5]=dlf$parameter$wei$para[3] #delta shape
    
    pot_lmom_ln3[i,3]=dlf$parameter$ln3$para[2] #mulog location
    pot_lmom_ln3[i,4]=dlf$parameter$ln3$para[3] #sigmalog scale
    pot_lmom_ln3[i,5]=dlf$parameter$ln3$para[1] #zeta lowerbounds
    
    pot_lmom_gpa[i,3]=dlf$parameter$gpa$para[1] #xi location
    pot_lmom_gpa[i,4]=dlf$parameter$gpa$para[2] #alpha scale
    pot_lmom_gpa[i,5]=dlf$parameter$gpa$para[3] #kappa shape
    
    pot_lmom_exp[i,3]=dlf$parameter$exp$para[1] #xi location
    pot_lmom_exp[i,4]=dlf$parameter$exp$para[2] #alpha scale
    pot_lmom_exp[i,5]=NA #shape NA
    
    pot_lmom_gam[i,3]=NA #location NA
    pot_lmom_gam[i,4]=dlf$parameter$gam$para[2] #beta scale
    pot_lmom_gam[i,5]=dlf$parameter$gam$para[1] #alpha shape
    
    pot_lmom_kap[i,3]=dlf$parameter$kap$para[1] #xi location
    pot_lmom_kap[i,4]=dlf$parameter$kap$para[2] #alpha scale
    pot_lmom_kap[i,5]=dlf$parameter$kap$para[3] #kappa shape1
    pot_lmom_kap[i,21]=dlf$parameter$kap$para[4] #h shape2
    
    pot_lmom_revgum[i,3]=dlf$parameter$revgum$para[1] #xi location
    pot_lmom_revgum[i,4]=dlf$parameter$revgum$para[2] #alpha scale
    pot_lmom_revgum[i,5]=NA #shape NA
    
    pot_lmom_gev[i,3]=dlf$parameter$gev$para[1] #xi location
    pot_lmom_gev[i,4]=dlf$parameter$gev$para[2] #alpha scale
    pot_lmom_gev[i,5]=dlf$parameter$gev$para[3] #kappa shape
    #_________________________________________________
  }
  return(list("pot_lmon_gum"=pot_lmon_gum, "pot_lmon_wei"=pot_lmon_wei, 
              "pot_lmom_ln3"=pot_lmom_ln3, "pot_lmom_gpa"=pot_lmom_gpa, 
              "pot_lmom_exp"=pot_lmom_exp, "pot_lmom_gam"=pot_lmom_gam, 
              "pot_lmom_kap"=pot_lmom_kap, "pot_lmom_revgum"=pot_lmom_revgum, 
              "pot_lmom_gev"=pot_lmom_gev))
}

#In file raw_data_station_ID_statistics.xlsx (variable 'statsfile') create the statistics sheets for thunderstorm (? = t) and non-thunderstorm (? = nt):
  #  - declu_?_years
  #  - declu_?_weeks
  #  - declu_?_months
  #  - declu_?_gaps30days
data_statistics <- function(date=imp.vals$nt.series.dt, series=imp.vals$nt.series, 
                            excelfile=statsfile_OUT, prefix="nt", check=length(series) > 0){
  if (check){
    #Statistics work
    ntds = data.frame(date, series)
    names(ntds) = c("time", "series")
    ntds = as_tibble(ntds)
    years = generate_stats_time_serie(ntds, "series", ntds$time, "years")
    months = generate_stats_time_serie(ntds, "series", ntds$time, "months")
    weeks = generate_stats_time_serie(ntds, "series", ntds$time, "weeks")
    
    #write.xlsx(years, file=statsfile, sheetName="declu_nt_years", append=TRUE, row.names=TRUE)
    addWorksheet(excelfile, "declu_nt_years")
    writeData(excelfile, sheet = "declu_nt_years", x = years)
    
    #write.xlsx(months, file=statsfile, sheetName="declu_nt_months", append=TRUE, row.names=TRUE)
    addWorksheet(excelfile, paste0("declu_", prefix, "_months"))
    writeData(excelfile, sheet = paste0("declu_", prefix, "_months"), x = months)
    
    #write.xlsx(weeks, file=statsfile, sheetName="declu_nt_weeks", append=TRUE, row.names=TRUE)
    addWorksheet(excelfile, paste0("declu_", prefix, "_weeks"))
    writeData(excelfile, sheet = paste0("declu_", prefix, "_weeks"), x = weeks)
    
    #Search time differences in days between consecutive samples greater than threshold in days (last parameter next function)
    thresholdindays = 30
    holesindays = locate_holes_time_serie(ntds, "series", ntds$time, thresholdindays)
    
    #write.xlsx(holesindays, file=statsfile, sheetName=paste0("declu_nt_gaps",thresholdindays,"days"), append=TRUE, row.names=FALSE)
    addWorksheet(excelfile, paste0("declu_", prefix, "_gaps", thresholdindays,"days"))
    writeData(excelfile, sheet = paste0("declu_", prefix, "_gaps",thresholdindays,"days"), x = holesindays)
  }
}

# lot time series
#  Send to PDF (FittedModel_ID.pdf): Declustered Non-Thunderstorm ('t') or Thunderstorm ('t') Time Series
plot_timeseries <- function(series=imp.vals$nt.series, date=imp.vals$nt.series.dt, data_label="Non-Thunderstorm ('nt')", numberofplots=numberofplots, check=length(series) > 0){
  if (check){
    ntds = data.frame(date, series)
    names(ntds) = c("time", "series")
    ntds = as_tibble(ntds)
    #Plot time serie
    print(plotxts(data=ntds, variable="series", time=ntds$time, cex.main=0.2, major.ticks="years",
                  xlab=paste0("Page ",numberofplots, " - Declustered ", data_label, " Time Series - Station: ", number),
                  main = paste0("Station ID: ",  number, "\nWind Velocity [Km/h]")))
    #assign(paste0("myprint", numberofplots), recordPlot())
    #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))				
    numberofplots = numberofplots + 1
  }
  return(numberofplots)
}



#Fit multiple pdf using fitdistplus
fit_multiple_pdf_fitdistplus <- function(date=imp.vals$nt.series.dt, series=imp.vals$nt.series, obsperyears = imp.vals$n.nthunders.per.year,
                                         threshold=nt.thresh, prefix="nt", data_label="Non-Thunderstorm",
                                         numberofplots=numberofplots, number=number, variable_description= "Wind Velocity [Km/h]", check=length(series) > 0,
                                         fdp_gum=nt_pot_fdp_gum,
                                         #fdp_wei=nt_pot_fdp_wei,
                                         #fdp_ln3=nt_pot_fdp_ln3,
                                         #fdp_gpa=nt_pot_fdp_gpa,
                                         #fdp_exp=nt_pot_fdp_exp,
                                         #fdp_gam=nt_pot_fdp_gam,
                                         #fdp_kap=nt_pot_fdp_kap,
                                         #fdp_revgum=nt_pot_fdp_revgum,
                                         #fdp_gev=nt_pot_fdp_gev, 
                                         tipicalReturnPeriods = c(10,20,50,100,250,500,700,1000,1700,3000,7000),
                                         tipicalProbabilities = 1-1/(obsperyears*tipicalReturnPeriods), 
                                         i = i, #consecutive
                                         x = x,
                                         y = y,
                                         cellindexlr = cellindexlr,
                                         lonindex = lonindex,
                                         latindex = latindex,
                                         lonlatindex = lonlatindex,
                                         average_time_per_year = 1-(zzz[i,6]),
                                         maxW = z8,
                                         niter_boost = 1001,
                                         fdp_gum_gof=nt_pot_fdp_gum_gof,
                                         fdp_gof_short=nt_pot_fdp_gof_short
                                         ){
  if (check){
    #1) Plot empirical density and cumulative distribution
    #print(plotdist(series, histo = TRUE, demp = TRUE))
    
    #browser()
    par(mfrow=c(1,2))
    par(oma=c(3,0,3,0)) #Outer Margins
    par(cex=1.2, bg="white")
    #par(mar=c(5,4,4,2) + 0.1)
    hist(series, freq = F, xlab = data_label, ylab="Density", main="Histogram")
    lines(density(series))
    cdf = ecdf(series)
    plot(cdf, xlab = data_label, ylab="Probability", main="CDF")
    mtext(side = 1, line=1, text = paste0("Page ", numberofplots, " - Declustered - ", data_label), outer = TRUE)
    mtext(side = 3, line=1, text = paste0("Station ID ", number, " - ", variable_description), outer = TRUE)
    numberofplots = numberofplots + 1
    
    #2) Cullen and Frey graph
    #
    par(mfrow=c(1,1))
    par(oma=c(3,0,3,0)) #Outer Margins
    par(cex=1.2, bg="white")
    descdist(series, boot = 1000)
    mtext(side = 1, line=1, text = paste0("Page ", numberofplots, " - Declustered - ", data_label), outer = TRUE)
    mtext(side = 1, line=2, text = "Package 'fitdistrplus'", outer = TRUE, cex = 0.8)
    mtext(side = 3, line=1, text = paste0("Station ID ", number, " - ", variable_description), outer = TRUE)
    numberofplots = numberofplots + 1
    
    #to initialize parameters
    mu = mean(series) + (0.45006 * sd(series))
    sigma = (sd(series)*sqrt(6))/pi
    
    
    #3) 
    
    #3.1) GUMBEL pot_mme_fdp_gum
    
    #mle (Maximum Likelihood Estimation)
      library(actuar) #gumbel comes from library actuar
      pgumbel = actuar::pgumbel
      dgumbel = actuar::dgumbel
      qgumbel = actuar::qgumbel
      rgumbel = actuar::rgumbel
      #kth raw moment
      mgumbel = actuar::mgumbel
      #moment generating function in t.
      mgfgumbel = actuar::mgfgumbel
      

      #browser()
      mle_gum <- fitdist(series, "gumbel", start=list(alpha=mu, scale=sigma))
      summary(mle_gum)
      #
      par(oma=c(3,0,3,0)) #Outer Margins
      par(cex=1.2, bg="white")
      plot(mle_gum)
      mtext(side = 1, line=1, text = paste0("Page ", numberofplots, " - Declustered - ", data_label, ". Fitting actuar::Gumbel. Location: NA. Scale: ",
                                            round(mle_gum$estimate["scale"], digits = 2), ". Shape: ", round(mle_gum$estimate["shape"], digits = 2)),
                                            outer = TRUE)
      mtext(side = 1, line=2, text = "Package 'fitdistrplus'. Maximum Likelihood Estimation - MLE", outer = TRUE, cex = 0.8)
      mtext(side = 3, line=1, text = paste0("Station ID ", number, " - ", variable_description), outer = TRUE)
      numberofplots = numberofplots + 1
      
      #goodness-of-fit
      gof_mle_gum = gofstat(mle_gum)
      
      #Confidence Intervals - Boostrap
      mle_gum.B <- bootdist(mle_gum, niter=niter_boost)
      mle_gum.B.su <- summary(mle_gum.B)
      #
      par(oma=c(3,0,3,0)) #Outer Margins
      par(cex=1.2, bg="white")
      plot(mle_gum.B)
      mtext(side = 1, line=1, text = paste0("Page ", numberofplots, " - Declustered - ", data_label, ". Fitting actuar::Gumbel"), outer = TRUE)
      mtext(side = 1, line=2, text = "Package 'fitdistrplus'.MLE. Bootstrap (uncertainty in parameters estimation)", outer = TRUE, cex = 0.8)
      mtext(side = 3, line=2, text = paste0("Station ID ", number, " - ", variable_description), outer = TRUE)
      numberofplots = numberofplots + 1
      
      #Estimation of mle_gum.B.quantiles
      mle_gum.B.qu = quantile(mle_gum.B, probs = tipicalProbabilities)
      
    #mge (Maximum Goodness-of-fit estimation) -- adr (Anderson-Darling Right Tail)
      mgeadr_gum <- fitdist(series, "gumbel", method = "mge", gof = "ADR", start = list(alpha = mu, scale = sigma))
      summary(mgeadr_gum)
      #
      par(oma=c(3,0,3,0)) #Outer Margins
      par(cex=1.2, bg="white")
      plot(mgeadr_gum)
      mtext(side = 1, line=1, text = paste0("Page ", numberofplots, " - Declustered - ", data_label, ". Fitting actuar::Gumbel. Location: NA. Scale: ",
                                            round(mgeadr_gum$estimate["scale"], digits = 2), ". Shape: ", round(mgeadr_gum$estimate["shape"], digits = 2)),
            outer = TRUE)
      mtext(side = 1, line=2, text = "Package 'fitdistrplus'. Maximum Goodness-of-fit Estimation - MGE. Anderson-Darling Right Tail - ADR", outer = TRUE, cex = 0.8)
      mtext(side = 3, line=1, text = paste0("Station ID ", number, " - ", variable_description), outer = TRUE)
      numberofplots = numberofplots + 1
      
      #goodness-of-fit
      gof_mgeadr_gum = gofstat(mgeadr_gum)
      
      #Confidence Intervals - Boostrap
      mgeadr_gum.B <- bootdist(mgeadr_gum, niter=niter_boost)
      mgeadr_gum.B.su <- summary(mgeadr_gum.B)
      #
      par(oma=c(3,0,3,0)) #Outer Margins
      par(cex=1.2, bg="white")
      plot(mgeadr_gum.B)
      mtext(side = 1, line=1, text = paste0("Page ", numberofplots, " - Declustered - ", data_label, ". Fitting actuar::Gumbel"), outer = TRUE)
      mtext(side = 1, line=2, text = "Package 'fitdistrplus'. MGE-ADR. Bootstrap (uncertainty in parameters estimation)", outer = TRUE, cex = 0.8)
      mtext(side = 3, line=2, text = paste0("Station ID ", number, " - ", variable_description), outer = TRUE)
      numberofplots = numberofplots + 1
      
      #Estimation of mle_gum.B.quantiles
      mgeadr_gum.B.qu = quantile(mgeadr_gum.B, probs = tipicalProbabilities)
      
      
    #mge (Maximum Goodness-of-fit estimation) -- ad2r (2nd Order Anderson-Darling Right Tail)
      #browser()
      mgead2r_gum <- fitdist(series, "gumbel", method = "mge", gof = "AD2R", start = list(alpha = mu, scale = sigma))
      summary(mgead2r_gum)
      #
      par(oma=c(3,0,3,0)) #Outer Margins
      par(cex=1.2, bg="white")
      plot(mgead2r_gum)
      mtext(side = 1, line=1, text = paste0("Page ", numberofplots, " - Declustered - ", data_label, ". Fitting actuar::Gumbel. Location: NA. Scale: ",
                                            round(mgead2r_gum$estimate["scale"], digits = 2), ". Shape: ", round(mgead2r_gum$estimate["shape"], digits = 2)),
            outer = TRUE)
      mtext(side = 1, line=2, text = "Package 'fitdistrplus'. Maximum Goodness-of-fit Estimation - MGE. 2nd Order Anderson-Darling Right Tail - AD2R", outer = TRUE, cex = 0.8)
      mtext(side = 3, line=1, text = paste0("Station ID ", number, " - ", variable_description), outer = TRUE)
      numberofplots = numberofplots + 1
      
      #goodness-of-fit
      gof_mgead2r_gum = gofstat(mgead2r_gum)
      
      #Confidence Intervals - Boostrap
      mgead2r_gum.B <- bootdist(mgead2r_gum, niter=niter_boost)
      mgead2r_gum.B.su <- summary(mgead2r_gum.B)
      #
      par(oma=c(3,0,3,0)) #Outer Margins
      par(cex=1.2, bg="white")
      plot(mgead2r_gum.B)
      mtext(side = 1, line=1, text = paste0("Page ", numberofplots, " - Declustered - ", data_label, ". Fitting actuar::Gumbel"), outer = TRUE)
      mtext(side = 1, line=2, text = "Package 'fitdistrplus'. MGE - AD2R. Bootstrap (uncertainty in parameters estimation)", outer = TRUE, cex = 0.8)
      mtext(side = 3, line=2, text = paste0("Station ID ", number, " - ", variable_description), outer = TRUE)
      numberofplots = numberofplots + 1
      
      #Estimation of mle_gum.B.quantiles
      mgead2r_gum.B.qu = quantile(mgead2r_gum.B, probs = tipicalProbabilities)
    
    #mme - Moment Matching Estimation
      #Empirical moment function
      #browser()
      memp <- function(x, order) sum(x^order)/length(x)
      
      mme_gum <- fitdist(series, "gumbel", method = "mme", order=1:2, memp=memp, 
                         start = list(alpha = mu, scale = sigma), lower=c(1e-6, 1e-6), upper = c(Inf, Inf))
      summary(mme_gum)
      #
      par(oma=c(3,0,3,0)) #Outer Margins
      par(cex=1.2, bg="white")
      plot(mme_gum)
      mtext(side = 1, line=1, text = paste0("Page ", numberofplots, " - Declustered - ", data_label, ". Fitting actuar::Gumbel. Location: NA. Scale: ",
                                            round(mme_gum$estimate["scale"], digits = 2), ". Shape: ", round(mme_gum$estimate["shape"], digits = 2)),
            outer = TRUE)
      mtext(side = 1, line=2, text = "Package 'fitdistrplus'. Moment Matching Estimation - MME", outer = TRUE, cex = 0.8)
      mtext(side = 3, line=1, text = paste0("Station ID ", number, " - ", variable_description), outer = TRUE)
      numberofplots = numberofplots + 1
      
      #goodness-of-fit
      gof_mme_gum = gofstat(mme_gum)
      
      #Confidence Intervals - Boostrap
      mme_gum.B <- bootdist(mme_gum, niter=niter_boost)
      mme_gum.B.su <- summary(mme_gum.B)
      #
      par(oma=c(3,0,3,0)) #Outer Margins
      par(cex=1.2, bg="white")
      plot(mme_gum.B)
      mtext(side = 1, line=1, text = paste0("Page ", numberofplots, " - Declustered - ", data_label, ". Fitting actuar::Gumbel"), outer = TRUE)
      mtext(side = 1, line=2, text = "Package 'fitdistrplus'. MME. Bootstrap (uncertainty in parameters estimation)", outer = TRUE, cex = 0.8)
      mtext(side = 3, line=2, text = paste0("Station ID ", number, " - ", variable_description), outer = TRUE)
      numberofplots = numberofplots + 1
      
      #Estimation of mle_gum.B.quantiles
      mme_gum.B.qu = quantile(mme_gum.B, probs = tipicalProbabilities)
    
    #All Gumbel in one graphic
    par(mfrow = c(1, 1))
    par(oma=c(3,0,3,0)) #Outer Margins
    par(cex=1.2, bg="white")
    plot.legend <- c("Gumbel MLE", "Gumbel MGE-ADR", "Gumbel MGE-A2R", "Gumbel MME")
    denscomp(list(mle_gum, mgeadr_gum, mgead2r_gum, mme_gum), legendtext = plot.legend)
    #qqcomp(list(mle_gum, mgeadr_gum, mgead2r_gum, mme_gum), legendtext = plot.legend)
    #cdfcomp(list(mle_gum, mgeadr_gum, mgead2r_gum, mme_gum), legendtext = plot.legend)
    #ppcomp(list(mle_gum, mgeadr_gum, mgead2r_gum, mme_gum), legendtext = plot.legend)
    mtext(side = 1, line=1, text = paste0("Page ", numberofplots, " - Declustered - ", data_label, ". Fitting actuar::Gumbel"), outer = TRUE)
    mtext(side = 1, line=2, text = "Package 'fitdistrplus'. Goodness-of-fit Gumbel (All Fittings)", outer = TRUE, cex = 0.8)
    mtext(side = 3, line=1, text = paste0("Station ID ", number, " - ", variable_description), outer = TRUE)
    numberofplots = numberofplots + 1

    par(mfrow = c(1, 1))
    par(oma=c(3,0,3,0)) #Outer Margins
    par(cex=1.2, bg="white")
    plot.legend <- c("Gumbel MLE", "Gumbel MGE-ADR", "Gumbel MGE-A2R", "Gumbel MME")
    #denscomp(list(mle_gum, mgeadr_gum, mgead2r_gum, mme_gum), legendtext = plot.legend)
    qqcomp(list(mle_gum, mgeadr_gum, mgead2r_gum, mme_gum), legendtext = plot.legend)
    #cdfcomp(list(mle_gum, mgeadr_gum, mgead2r_gum, mme_gum), legendtext = plot.legend)
    #ppcomp(list(mle_gum, mgeadr_gum, mgead2r_gum, mme_gum), legendtext = plot.legend)
    mtext(side = 1, line=1, text = paste0("Page ", numberofplots, " - Declustered - ", data_label, ". Fitting actuar::Gumbel"), outer = TRUE)
    mtext(side = 1, line=2, text = "Package 'fitdistrplus'. Goodness-of-fit Gumbel (All Fittings)", outer = TRUE, cex = 0.8)
    mtext(side = 3, line=1, text = paste0("Station ID ", number, " - ", variable_description), outer = TRUE)
    numberofplots = numberofplots + 1

    par(mfrow = c(1, 1))
    par(oma=c(3,0,3,0)) #Outer Margins
    par(cex=1.2, bg="white")
    plot.legend <- c("Gumbel MLE", "Gumbel MGE-ADR", "Gumbel MGE-A2R", "Gumbel MME")
    #denscomp(list(mle_gum, mgeadr_gum, mgead2r_gum, mme_gum), legendtext = plot.legend)
    #qqcomp(list(mle_gum, mgeadr_gum, mgead2r_gum, mme_gum), legendtext = plot.legend)
    cdfcomp(list(mle_gum, mgeadr_gum, mgead2r_gum, mme_gum), legendtext = plot.legend)
    #ppcomp(list(mle_gum, mgeadr_gum, mgead2r_gum, mme_gum), legendtext = plot.legend)
    mtext(side = 1, line=1, text = paste0("Page ", numberofplots, " - Declustered - ", data_label, ". Fitting actuar::Gumbel"), outer = TRUE)
    mtext(side = 1, line=2, text = "Package 'fitdistrplus'. Goodness-of-fit Gumbel (All Fittings)", outer = TRUE, cex = 0.8)
    mtext(side = 3, line=1, text = paste0("Station ID ", number, " - ", variable_description), outer = TRUE)
    numberofplots = numberofplots + 1

    par(mfrow = c(1, 1))
    par(oma=c(3,0,3,0)) #Outer Margins
    par(cex=1.2, bg="white")
    plot.legend <- c("Gumbel MLE", "Gumbel MGE-ADR", "Gumbel MGE-AD2R", "Gumbel MME")
    #denscomp(list(mle_gum, mgeadr_gum, mgead2r_gum, mme_gum), legendtext = plot.legend)
    #qqcomp(list(mle_gum, mgeadr_gum, mgead2r_gum, mme_gum), legendtext = plot.legend)
    #cdfcomp(list(mle_gum, mgeadr_gum, mgead2r_gum, mme_gum), legendtext = plot.legend)
    ppcomp(list(mle_gum, mgeadr_gum, mgead2r_gum, mme_gum), legendtext = plot.legend)
    mtext(side = 1, line=1, text = paste0("Page ", numberofplots, " - Declustered - ", data_label, ". Fitting actuar::Gumbel"), outer = TRUE)
    mtext(side = 1, line=2, text = "Package 'fitdistrplus'. Goodness-of-fit Gumbel (All Fittings)", outer = TRUE, cex = 0.8)
    mtext(side = 3, line=1, text = paste0("Station ID ", number, " - ", variable_description), outer = TRUE)
    numberofplots = numberofplots + 1
    
    #Fill array of Gumbel fitting
      fdp_gum[i,1] = x #x
      fdp_gum[i,2] = y #y
      fdp_gum[i,3] = cellindexlr#cellindexlr
      fdp_gum[i,4] = lonindex#lonindex
      fdp_gum[i,5] = latindex#latindex
      fdp_gum[i,6] = lonlatindex#lonlatindex
      fdp_gum[i,7] = NA # empty
      fdp_gum[i,8] = i # consecutive
      fdp_gum[i,9] = obsperyears # imp.vals$n.nthunders.per.year or imp.vals$n.thunders.per.year # t or nt _average_events_per_year
      fdp_gum[i,10] = average_time_per_year #1-(zzz[i,6]) #nt_average_time_per_year(Ant) = 1-At as TIME units are in YEARS in this code
      fdp_gum[i,11] = maxW #z8 #Max W for optimal threshold
      fdp_gum[i,12] = number #Station number
      fdp_gum[i,13] = threshold #z5 #threshold
      #mle
        #mle parameters
          fdp_gum[i,14]= mle_gum$estimate["alpha"] #alpha location mle
          fdp_gum[i,15]= mle_gum$estimate["scale"] #scale mle
          fdp_gum[i,16]= NA #shape mle
        #bootstrap mle parameters MEDIAN
          fdp_gum[i,17]= mle_gum.B.su$CI[, "Median"]["alpha"] #alpha location MEDIAN mle
          fdp_gum[i,18]= mle_gum.B.su$CI[, "Median"]["scale"] # scale MEDIAN mle
          fdp_gum[i,19]= NA # shape MEDIAN mle
        #bootstrap mle parameters 2.5%
          fdp_gum[i,20]= mle_gum.B.su$CI[, "2.5%"]["alpha"] # alpha location 2.5% mle
          fdp_gum[i,21]= mle_gum.B.su$CI[, "2.5%"]["scale"] # scale 2.5% mle
          fdp_gum[i,22]= NA # shape 2.5% mle
        #bootstrap mle parameters 97.5%
          fdp_gum[i,23]= mle_gum.B.su$CI[, "97.5%"]["alpha"] # alpha location 97.5% mle
          fdp_gum[i,24]= mle_gum.B.su$CI[, "97.5%"]["scale"] # scale 97.5% mle
          fdp_gum[i,25]= NA # shape 97.5% mle
        #bootstrap mle standard error of parameters
          fdp_gum[i,26]= mle_gum.B.su$fitpart$sd["alpha"] #alpha location standard error mle
          fdp_gum[i,27]= mle_gum.B.su$fitpart$sd["scale"] # scale standard error mle
          fdp_gum[i,28]= NA # shape standard error mle
        #mle goodness-of-fit
          fdp_gum[i,29]= mle_gum$loglik #Loglikelihood # mle
          fdp_gum[i,30]= mle_gum$aic #AIC # mle
          fdp_gum[i,31]= mle_gum$bic #BIC # mle
          fdp_gum[i,32]= gof_mle_gum$chisq #Chi Smle_gum.B.quared
          fdp_gum[i,33]= gof_mle_gum$chisqpvalue #Chi Smle_gum.B.quared pvalue
          fdp_gum[i,34]= gof_mle_gum$cvm #Cramer-von Mises statistic
          fdp_gum[i,35]= gof_mle_gum$ad # Anderson-Darling statistic
          fdp_gum[i,36]= gof_mle_gum$ks #Kolmogorov-Smirnov statistic
          fdp_gum[i,37]= NA #empty
          fdp_gum[i,38]= NA #empty
        #mle mle_gum.B.quantiles (tipicalProbabilities)
          fdp_gum[i,39:49] = as.numeric(mle_gum.B.qu$quantiles)  #non-hurricane return levels mle_gum.B.quantile fitdistplus mle
        #mle mle_gum.B.quantiles median (tipicalProbabilities)
          fdp_gum[i,50:60]= as.numeric(mle_gum.B.qu$quantmedian) #non-hurricane return levels mle_gum.B.quantile median fitdistplus mle
        #mle mle_gum.B.quantiles confidence intervals 2.5 % (tipicalProbabilities)
          fdp_gum[i,61:71]= as.numeric(mle_gum.B.qu$quantCI["2.5 %", ]) #non-hurricane return levels mle_gum.B.quantile confidence intervals 2.5 % fitdistplus mle
        #mle mle_gum.B.quantiles confidence intervals 97.5 % (tipicalProbabilities)
          fdp_gum[i,72:82]= as.numeric(mle_gum.B.qu$quantCI["97.5 %", ]) #non-hurricane return levels mle_gum.B.quantile confidence intervals 97.5 % fitdistplus mle
      #mge-adr
        #mgeadr parameters
          fdp_gum[i,83]= mgeadr_gum$estimate["alpha"] #alpha location mgeadr
          fdp_gum[i,84]= mgeadr_gum$estimate["scale"] #scale mgeadr
          fdp_gum[i,85]= NA #shape mgeadr
        #bootstrap mgeadr parameters MEDIAN
          fdp_gum[i,86]= mgeadr_gum.B.su$CI[, "Median"]["alpha"] # alpha location MEDIAN mgeadr
          fdp_gum[i,87]= mgeadr_gum.B.su$CI[, "Median"]["scale"] # scale MEDIAN mgeadr
          fdp_gum[i,88]= NA # shape MEDIAN mgeadr
        #bootstrap mgeadr parameters 2.5%
          fdp_gum[i,89]= mgeadr_gum.B.su$CI[, "2.5%"]["alpha"] #alpha location 2.5% mgeadr
          fdp_gum[i,90]= mgeadr_gum.B.su$CI[, "2.5%"]["scale"] # scale 2.5% mgeadr
          fdp_gum[i,91]= NA # shape 2.5% mgeadr
        #bootstrap mgeadr parameters 97.5%
          fdp_gum[i,92]= mgeadr_gum.B.su$CI[, "97.5%"]["alpha"] #alpha location 97.5% mgeadr
          fdp_gum[i,93]= mgeadr_gum.B.su$CI[, "97.5%"]["scale"] # scale 97.5% mgeadr
          fdp_gum[i,94]= NA # shape 97.5% mgeadr
        #bootstrap mgeadr standard error of parameters
          fdp_gum[i,95]= NA #mgeadr_gum.B.su$fitpart$sd["alpha"] # alpha location standard error mgeadr
          fdp_gum[i,96]= NA #mgeadr_gum.B.su$fitpart$sd["scale"] # scale standard error mgeadr
          fdp_gum[i,97]= NA #shape standard error mgeadr
        #mgeadr goodness-of-fit
          fdp_gum[i,98]= mgeadr_gum$loglik #Loglikelihood # mgeadr
          fdp_gum[i,99]= mgeadr_gum$aic #AIC # mgeadr
          fdp_gum[i,100]= mgeadr_gum$bic #BIC # mgeadr
          fdp_gum[i,101]= gof_mgeadr_gum$chisq #Chi Smgeadr_gum.B.quared
          fdp_gum[i,102]= gof_mgeadr_gum$chisqpvalue #Chi Smgeadr_gum.B.quared pvalue
          fdp_gum[i,103]= gof_mgeadr_gum$cvm #Cramer-von Mises statistic
          fdp_gum[i,104]= gof_mgeadr_gum$ad # Anderson-Darling statistic
          fdp_gum[i,105]= gof_mgeadr_gum$ks #Kolmogorov-Smirnov statistic
          fdp_gum[i,106]= NA # empty
          fdp_gum[i,107]= NA # empty
        #mgeadr mgeadr_gum.B.quantiles (tipicalProbabilities)
          fdp_gum[i,108:118] = as.numeric(mgeadr_gum.B.qu$quantiles)  #non-hurricane return levels mgeadr_gum.B.quantile fitdistplus mgeadr
        #mgeadr mgeadr_gum.B.quantiles median (tipicalProbabilities)
          fdp_gum[i,119:129]= as.numeric(mgeadr_gum.B.qu$quantmedian) #non-hurricane return levels mgeadr_gum.B.quantile median fitdistplus mgeadr
        #mgeadr mgeadr_gum.B.quantiles confidence intervals 2.5 % (tipicalProbabilities)
          fdp_gum[i,130:140]= as.numeric(mgeadr_gum.B.qu$quantCI["2.5 %", ]) #non-hurricane return levels mgeadr_gum.B.quantile confidence intervals 2.5 % fitdistplus mgeadr
        #mgeadr mgeadr_gum.B.quantiles confidence intervals 97.5 % (tipicalProbabilities)
          fdp_gum[i,141:151]= as.numeric(mgeadr_gum.B.qu$quantCI["97.5 %", ]) #non-hurricane return levels mgeadr_gum.B.quantile confidence intervals 97.5 % fitdistplus mgeadr
      #mge-ad2r
        #mgead2r parameters
          fdp_gum[i,152]= mgead2r_gum$estimate["alpha"] #alpha location mgead2r
          fdp_gum[i,153]= mgead2r_gum$estimate["scale"] #scale mgead2r
          fdp_gum[i,154]= NA #shape mgead2r
        #bootstrap mgead2r parameters MEDIAN
          fdp_gum[i,155]= mgead2r_gum.B.su$CI[, "Median"]["alpha"] #alpha location MEDIAN mgead2r
          fdp_gum[i,156]= mgead2r_gum.B.su$CI[, "Median"]["scale"] # scale MEDIAN mgead2r
          fdp_gum[i,157]= NA # shape MEDIAN mgead2r
        #bootstrap mgead2r parameters 2.5%
          fdp_gum[i,158]= mgead2r_gum.B.su$CI[, "2.5%"]["alpha"] #alpha location 2.5% mgead2r
          fdp_gum[i,159]= mgead2r_gum.B.su$CI[, "2.5%"]["scale"] # scale 2.5% mgead2r
          fdp_gum[i,160]= NA # shape 2.5% mgead2r
        #bootstrap mgead2r parameters 97.5%
          fdp_gum[i,161]= mgead2r_gum.B.su$CI[, "97.5%"]["alpha"] #apha location 97.5% mgead2r
          fdp_gum[i,162]= mgead2r_gum.B.su$CI[, "97.5%"]["scale"] # scale 97.5% mgead2r
          fdp_gum[i,163]= NA # shape 97.5% mgead2r
        #bootstrap mgead2r standard error of parameters
          fdp_gum[i,164]= NA #mgead2r_gum.B.su$fitpart$sd["alpha"] #alpha location standard error mgead2r
          fdp_gum[i,165]= NA #mgead2r_gum.B.su$fitpart$sd["scale"] #scale standard error mgead2r
          fdp_gum[i,166]= NA #shape standard error mgead2r
        #mgead2r goodness-of-fit
          fdp_gum[i,167]= mgead2r_gum$loglik #Loglikelihood # mgead2r
          fdp_gum[i,168]= mgead2r_gum$aic #AIC # mgead2r
          fdp_gum[i,169]= mgead2r_gum$bic #BIC # mgead2r
          fdp_gum[i,170]= gof_mgead2r_gum$chisq #Chi Smgead2r_gum.B.quared
          fdp_gum[i,171]= gof_mgead2r_gum$chisqpvalue #Chi Smgead2r_gum.B.quared pvalue
          fdp_gum[i,172]= gof_mgead2r_gum$cvm #Cramer-von Mises statistic
          fdp_gum[i,173]= gof_mgead2r_gum$ad # Anderson-Darling statistic
          fdp_gum[i,174]= gof_mgead2r_gum$ks #Kolmogorov-Smirnov statistic
          fdp_gum[i,175]= NA # empty
          fdp_gum[i,176]= NA # empty
        #mgead2r mgead2r_gum.B.quantiles (tipicalProbabilities)
          fdp_gum[i,177:187] = as.numeric(mgead2r_gum.B.qu$quantiles)  #non-hurricane return levels mgead2r_gum.B.quantile fitdistplus mgead2r
        #mgead2r mgead2r_gum.B.quantiles median (tipicalProbabilities)
          fdp_gum[i,188:198]= as.numeric(mgead2r_gum.B.qu$quantmedian) #non-hurricane return levels mgead2r_gum.B.quantile median fitdistplus mgead2r
        #mgead2r mgead2r_gum.B.quantiles confidence intervals 2.5 % (tipicalProbabilities)
          fdp_gum[i,199:209]= as.numeric(mgead2r_gum.B.qu$quantCI["2.5 %", ]) #non-hurricane return levels mgead2r_gum.B.quantile confidence intervals 2.5 % fitdistplus mgead2r
        #mgead2r mgead2r_gum.B.quantiles confidence intervals 97.5 % (tipicalProbabilities)
          fdp_gum[i,210:220]= as.numeric(mgead2r_gum.B.qu$quantCI["97.5 %", ]) #non-hurricane return levels mgead2r_gum.B.quantile confidence intervals 97.5 % fitdistplus mgead2r
      #mme
        #mme parameters
          fdp_gum[i,221]= mme_gum$estimate["alpha"] #alpha location mme
          fdp_gum[i,222]= mme_gum$estimate["scale"] #scale mme
          fdp_gum[i,223]= NA #shape mme
        #bootstrap mme parameters MEDIAN
          fdp_gum[i,224]= mme_gum.B.su$CI[, "Median"]["alpha"] #alpha location MEDIAN mme
          fdp_gum[i,225]= mme_gum.B.su$CI[, "Median"]["scale"] # scale MEDIAN mme
          fdp_gum[i,226]= NA # shape MEDIAN mme
        #bootstrap mme parameters 2.5%
          fdp_gum[i,227]= mme_gum.B.su$CI[, "2.5%"]["alpha"] #alpha location 2.5% mme
          fdp_gum[i,228]= mme_gum.B.su$CI[, "2.5%"]["scale"] # scale 2.5% mme
          fdp_gum[i,229]= NA # shape 2.5% mme
        #bootstrap mme parameters 97.5%
          fdp_gum[i,230]= mme_gum.B.su$CI[, "97.5%"]["alpha"] #alpha location 97.5% mme
          fdp_gum[i,231]= mme_gum.B.su$CI[, "97.5%"]["scale"] #scale 97.5% mme
          fdp_gum[i,232]= NA # shape 97.5% mme
        #bootstrap mme standard error of parameters
          fdp_gum[i,233]= NA #mme_gum.B.su$fitpart$sd["alpha"] #alpha location standard error mme
          fdp_gum[i,234]= NA #mme_gum.B.su$fitpart$sd["scale"] # scale standard error mme
          fdp_gum[i,235]= NA # shape standard error mme
        #mme goodness-of-fit
          fdp_gum[i,236]= mme_gum$loglik #Loglikelihood # mme
          fdp_gum[i,237]= mme_gum$aic #AIC # mme
          fdp_gum[i,238]= mme_gum$bic #BIC # mme
          fdp_gum[i,239]= gof_mme_gum$chisq #Chi Smme_gum.B.quared
          fdp_gum[i,240]= gof_mme_gum$chisqpvalue #Chi Smme_gum.B.quared pvalue
          fdp_gum[i,241]= gof_mme_gum$cvm #Cramer-von Mises statistic
          fdp_gum[i,242]= gof_mme_gum$ad # Anderson-Darling statistic
          fdp_gum[i,243]= gof_mme_gum$ks #Kolmogorov-Smirnov statistic
          fdp_gum[i,244]= NA # empty
          fdp_gum[i,245]= NA # empty
        #mme mme_gum.B.quantiles (tipicalProbabilities)
          fdp_gum[i,246:256] = as.numeric(mme_gum.B.qu$quantiles)  #non-hurricane return levels mme_gum.B.quantile fitdistplus mme
        #mme mme_gum.B.quantiles median (tipicalProbabilities)
          fdp_gum[i,257:267] = as.numeric(mme_gum.B.qu$quantmedian) #non-hurricane return levels mme_gum.B.quantile median fitdistplus mme
        #mme mme_gum.B.quantiles confidence intervals 2.5 % (tipicalProbabilities)
          fdp_gum[i,268:278]= as.numeric(mme_gum.B.qu$quantCI["2.5 %", ]) #non-hurricane return levels mme_gum.B.quantile confidence intervals 2.5 % fitdistplus mme
        #mme mme_gum.B.quantiles confidence intervals 97.5 % (tipicalProbabilities)
          fdp_gum[i,279:289] = as.numeric(mme_gum.B.qu$quantCI["97.5 %", ]) #non-hurricane return levels mme_gum.B.quantile confidence intervals 97.5 % fitdistplus mme

     #Fit array of Goodness-of-fit Gumbel long
      
      fdp_gum_gof[i,1] = i # consecutive
      fdp_gum_gof[i,2] = number #Station number
      
      fdp_gum_gof[i,3] = "actuar::Gumbel" #function name and package
      fdp_gum_gof[i,4] = "fitdistrplus::fitdist" #function fitting and package
      #mle
        fdp_gum_gof[i,5] = "fitdist(method='mle')" #function fitting method
        fdp_gum_gof[i,6] = "fitdist(gof=NA)" #function goodness-of-fit method
        fdp_gum_gof[i,7] = "fitdist(optim.method='Nelder-Mead')" #function optim.method method
        
        fdp_gum_gof[i,8]= mle_gum$aic #AIC # mle
        fdp_gum_gof[i,9]= mle_gum$bic #BIC # mle
        
        fdp_gum_gof[i,10]= gof_mle_gum$cvm #Cramer-von Mises statistic
        fdp_gum_gof[i,11]= gof_mle_gum$cvmtest #Cramer-von Mises test
        fdp_gum_gof[i,12]= gof_mle_gum$ad #Anderson-Darling statistic 
        fdp_gum_gof[i,13]= gof_mle_gum$adtest #Anderson-Darling test
        fdp_gum_gof[i,14]= gof_mle_gum$ks #Kolmogorov-Smirnov statistic
        fdp_gum_gof[i,15]= gof_mle_gum$kstest #Kolmogorov-Smirnov test
        
      #mge-adr
        fdp_gum_gof[i,16] = "fitdist(method='mge')" #function fitting method
        fdp_gum_gof[i,17] = "fitdist(gof='ADR')" #function goodness-of-fit method
        fdp_gum_gof[i,18] = "fitdist(optim.method='Nelder-Mead')" #function optim.method method
        
        fdp_gum_gof[i,19]= mgeadr_gum$aic #AIC # mle
        fdp_gum_gof[i,20]= mgeadr_gum$bic #BIC # mle
        
        fdp_gum_gof[i,21]= gof_mgeadr_gum$cvm #Cramer-von Mises statistic
        fdp_gum_gof[i,22]= gof_mgeadr_gum$cvmtest #Cramer-von Mises test
        fdp_gum_gof[i,23]= gof_mgeadr_gum$ad #Anderson-Darling statistic
        fdp_gum_gof[i,24]= gof_mgeadr_gum$adtest #Anderson-Darling test
        fdp_gum_gof[i,25]= gof_mgeadr_gum$ks #Kolmogorov-Smirnov statistic
        fdp_gum_gof[i,26]= gof_mgeadr_gum$kstest #Kolmogorov-Smirnov test
      
      #mge-ad2r
        fdp_gum_gof[i,27] = "fitdist(method='mge')" #function fitting method
        fdp_gum_gof[i,28] = "fitdist(gof='AD2R')" #function goodness-of-fit method
        fdp_gum_gof[i,29] = "fitdist(optim.method='Nelder-Mead')" #function optim.method method
        
        fdp_gum_gof[i,30]= mgead2r_gum$aic #AIC # mle
        fdp_gum_gof[i,31]= mgead2r_gum$bic #BIC # mle
        
        fdp_gum_gof[i,32]= gof_mgead2r_gum$cvm #Cramer-von Mises statistic
        fdp_gum_gof[i,33]= gof_mgead2r_gum$cvmtest #Cramer-von Mises test
        fdp_gum_gof[i,34]= gof_mgead2r_gum$ad #Anderson-Darling statistic
        fdp_gum_gof[i,35]= gof_mgead2r_gum$adtest #Anderson-Darling test
        fdp_gum_gof[i,36]= gof_mgead2r_gum$ks #Kolmogorov-Smirnov statistic
        fdp_gum_gof[i,37]= gof_mgead2r_gum$kstest #Kolmogorov-Smirnov test
      
      #mme
        fdp_gum_gof[i,38] = "fitdist(method='mme')" #function fitting method
        fdp_gum_gof[i,39] = "fitdist(gof=NA)" #function goodness-of-fit method
        fdp_gum_gof[i,40] = "fitdist(optim.method='Nelder-Mead')" #function optim.method method
        
        fdp_gum_gof[i,41]= mme_gum$aic #AIC # mle
        fdp_gum_gof[i,42]= mme_gum$bic #BIC # mle
        
        fdp_gum_gof[i,43]= gof_mme_gum$cvm #Cramer-von Mises statistic
        fdp_gum_gof[i,44]= gof_mme_gum$cvmtest #Cramer-von Mises test
        fdp_gum_gof[i,45]= gof_mme_gum$ad #Anderson-Darling  statistic
        fdp_gum_gof[i,46]= gof_mme_gum$adtest #Anderson-Darling  test
        fdp_gum_gof[i,47]= gof_mme_gum$ks #Kolmogorov-Smirnov statistic
        fdp_gum_gof[i,48]= gof_mme_gum$kstest #Kolmogorov-Smirnov test
      
      
      #Fit array of Goodness-of-fit Gumbel short
      #mle
        fdp_gof_short[i,1] = i # consecutive
        fdp_gof_short[i,2] = number #Station number
        fdp_gof_short[i,3]= mle_gum$aic #AIC # mle
        fdp_gof_short[i,4]= mle_gum$bic #BIC # mle
        fdp_gof_short[i,5]= gof_mle_gum$cvm #Cramer-von Mises statistic
        fdp_gof_short[i,6]= gof_mle_gum$ad #Anderson-Darling statistic 
        fdp_gof_short[i,7]= gof_mle_gum$ks #Kolmogorov-Smirnov statistic
  
      #mge-adr
        fdp_gof_short[i,8]= mgeadr_gum$aic #AIC # mle
        fdp_gof_short[i,9]= mgeadr_gum$bic #BIC # mle
        fdp_gof_short[i,10]= gof_mgeadr_gum$cvm #Cramer-von Mises statistic
        fdp_gof_short[i,11]= gof_mgeadr_gum$ad #Anderson-Darling statistic
        fdp_gof_short[i,12]= gof_mgeadr_gum$ks #Kolmogorov-Smirnov statistic
      
      
      #mge-ad2r
        fdp_gof_short[i,13]= mgead2r_gum$aic #AIC # mle
        fdp_gof_short[i,14]= mgead2r_gum$bic #BIC # mle
        fdp_gof_short[i,15]= gof_mgead2r_gum$cvm #Cramer-von Mises statistic
        fdp_gof_short[i,16]= gof_mgead2r_gum$ad #Anderson-Darling statistic
        fdp_gof_short[i,17]= gof_mgead2r_gum$ks #Kolmogorov-Smirnov statistic
      
      #mme
        fdp_gof_short[i,18]= mme_gum$aic #AIC # mle
        fdp_gof_short[i,19]= mme_gum$bic #BIC # mle
        fdp_gof_short[i,20]= gof_mme_gum$cvm #Cramer-von Mises statistic
        fdp_gof_short[i,21]= gof_mme_gum$ad #Anderson-Darling  statistic
        fdp_gof_short[i,22]= gof_mme_gum$ks #Kolmogorov-Smirnov statistic
        
    #3.2) WEIBULL pot_mle_fdp_wei
    
    #3.3) LOGNORMAL 3 pot_mle_fdp_ln3
    
    #3.4) Generalized Pareto pot_mle_fdp_gpa
    
    #3.5) Exponential pot_mle_fdp_exp
    
    #3.6) Gamma pot_mle_fdp_gam
    
    #3.7) Kappa pot_mle_fdp_kap
    
    #3.8) Rev Gumbel pot_mle_fdp_revgum
    
    #3.9) GENERALIZED EXTREME VALUE pot_mle_fdp_gev
      par(mfrow=c(1,1))
      par(oma=c(0,0,0,0)) #Outer Margins
      #par(mar=c(5.1, 4.1, 4.1, 2.1))
  }  
  return(list("numberofplots"=numberofplots,  
              "fdp_gum"=fdp_gum, 
              "fdp_gum_gof"=fdp_gum_gof, "fdp_gof_short"=fdp_gof_short))
}


#read hurricane rasters
read_hurricane_rasters <- function(path= paste0(input_path, "/h_rasters/"),
                                   pattern = "*.*.tif$"){
  rasterfilesera5 = list.files(path=path, pattern=pattern)
  h_st = NA
  h_sf = NA
  h = list("h_st"=h_st, "h_sf"=h_sf)
  if(length(rasterfilesera5)>0){
    rasterfilesera5 = paste0(path, "/", rasterfilesera5)
    rasterfilesera5 = stringr::str_sort(rasterfilesera5, numeric = TRUE) #sort string by numbers inside
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
    h = list("h_st"=h_st, "h_sf"=h_sf)
  }
  return(h)
}

alex <- function(){
  return(c(1, 2))
}