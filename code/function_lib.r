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

dgumbel <- function(x,mu,sigma){ # PDF
  exp((mu - x)/sigma - exp((mu - x)/sigma))/sigma
}

pgumbel <- function(q,mu,sigma){ # CDF
  exp(-exp(-((q - mu)/sigma)))
}

qgumbel <- function(p, mu, sigma){ # quantile function
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
ReadWindISDStation <- function (ncin, lonindex, latindex, ntime, timestamp) {
  
  #statera5_xts = na.omit(xts(x= ncvar_get(ncin, 'fg10', start=c(lonindex, latindex,1), count=c(1,1,ntime)), order.by = timestamp))
  statera5 = data.frame(ncvar_get(ncin, 'fg10', start=c(lonindex, latindex,1), count=c(1,1,ntime)))
  
  colnames(statera5) = "mps"
  statera5$mps  = statera5$mps * 3.6  #from mts/seg to km/hour
  statera5$thunder_flag = "nt"
    
  #filename <- paste(path, "raw_data_station_", station.number, ".txt", sep="")
  #raw.data <- read.table(file=filename,
  #                       header=TRUE,
  #                       colClasses=c("character", "numeric", "character"))
  #date.time <- as.POSIXct(raw.data[, "date_time"], tz="GMT", usetz=TRUE)
  date.time <- timestamp
  speed.kph <- statera5$mps
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
