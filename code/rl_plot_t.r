#Return levels and plots considering the whole dataset as:
# ONLY thunderstorm
# - Calculation of return values (different methods)
# - Graphics to PDF
# - In raw_data_station_id_fitted.xlsx (variable fnfitted) write sheet 't_POT-Poisson-GPD-Equivalent'
if (length(imp.vals$t.series.dt) > 0) {
  #___
  #___a) POT-Poisson Process using Shape parameter!!
  #___   Intensity Function: NIST.SP.500-301.pdf, page 29, shape parameter almost zero (0.00001)
  #___   Return Values: NIST.SP.500-301.pdf, equation before equation 4 (page 16)
  #___     As (At) is ONE
  #___   PDF: NIST.SP.500-301.pdf. Equation after equation 2, page 15
  #___     Domain in magnitude: y1: min (t series), y2: max (t series)
  #___   Equivalent POT-Poisson-GPD (NIST.SP.500-301.pdf):
  #         Scale: equation after equation 18 page 39 (NIST.SP.500-301.pdf)
  #         Location: Equal to threshold of POT-PP (gpdLocation)
  #___   CDF: NIST.SP.500-301.pdf. Equation 5, page 17
  #___   Write return values: zzz[i,16:26]
  #___   Plot hazard curve!
  
  #Intensity Function Poisson Process. NIST.SP.500-301.pdf, page 29
    Ip.t <- function(x) {1/z4 *(1+0.00001*((x-z3)/z4))^((-1/0.00001)-1)}
    
    
    #At is same parameter As of equation before equation 4 (NIST.SP.500-301.pdf, page 16).
    #Here (ONLY thunderstorm) it is not (imp.vals$n.thunders.per.year*(1/24)) 
    #But it must be equal to ONE when in the dataset there is only THUNDERSTORM data,
    # but Pintar station (722287) is working based in days(365) (BIG ISSUE TO SOLVE IN THESIS)
    #Meaning: Typical amount of thunderstorm in a year, but here always is ONE
    #         As "ant" = 0, then at = 365 -0 or at = 1 -0
    #at =  365 #If time is in days, but no match with other methods: 
    #  - Gumbel Quantile 
    #  - Formula Yn PP (Ecuation 18 pag 39 of NIST.SP.500-301.pdf)
    #  - Formula Yn GPD equivalent (Equation 15 pag 39 of NIST.SP.500-301.pdf)
    at = 1   #It does not mather if time is in days, but match with other methods:
    #  - Gumbel Quantile 
    #  - Formula Yn PP (Ecuation 18 pag 39 of NIST.SP.500-301.pdf)
    #  - Formula Yn GPD equivalent (Equation 15 pag 39 of NIST.SP.500-301.pdf)
    
    #Here (ONLY thunderstorm) it is not imp.vals$n.thunders.per.year*(1/24) 
    #Same parameter As of equation before equation 4 (NIST.SP.500-301.pdf, page 16). 
    #But it is equal to ONE when in the dataset there is only THUNDERSTORM data
    #Meaning: Typical amount of thunderstorm in a year, but here always is ONE
    
  #Return Level Poisson Process
  #  representing equation 4, page 16 (NIST.SP.500-301.pdf), but using Ip.t
    returnlevel_equation <- function(lower, upper, at){ #lower is the value of velocity to integrate with it to upper (infinite)
      integrated.t <- integrate(Ip.t, lower=lower, upper=upper)$value
      value = at * integrated.t
    }
    
  #Calculate exceedance probability for velocities 1 to 600 using Poisson Process
    paP = sapply(yvels, returnlevel_equation, upper=Inf, at=at)
  
  #PDF function of Poisson Process
  #Probability density according to NIST.SP.500-301.pdf. Equation after equation 2, page 15
    densityPoissonProcess <- function (velocity, series, threshold){ 
      y1 = min(series)
      y2 = max(series)
      #  t1 = min(serie.dt)
      #  t2 = max(serie.dt)
      integrand <- Ip.t
      #integrated <- integrate(integrand, lower=y1, upper=y2)$value
      integrated <- integrate(integrand, lower=threshold, upper=y2)$value
      #Time part of the integral is not included here!!XXX IT IS NOT IMPORTANT BECAUSE At is equal to 1
      density <- Ip.t(velocity)/integrated  # Equation after equation 2, page 15
    }
    
  #Calculate PDF
  #Calculate density function values for velocities y1 to y2 (domain in velocity) using Poisson Process
  #dfP = sapply(yvels, densityPoissonProcess, upper=Inf)
    yv = seq(from=min(imp.vals$t.series), to= max(imp.vals$t.series), length.out=1000)
    dfP = sapply(yv, densityPoissonProcess, series=imp.vals$t.series, threshold=z2)


  #Plot PDF
  #Plot: Page 2: Density Function from Intensity Function of Poisson Process
  #Plot dfP - probability density function using intensity function of Poisson Process
    library(RColorBrewer)
    cols <- brewer.pal(9,"Set1")
    plot(x= yv, y=dfP, xlab="Declustered - Thunderstorm - Velocities [Km/h]", ylab="Density Function - Poisson Process",
        main=paste("Density Function from Intensity Function of Poisson Process\n", "Location=", round(z3,2), " Scale=", round(z4,2), "\n", "Station:", number),
        type="l", col=cols[3], lwd=4)
    mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
    hist(imp.vals$t.series, probability = TRUE, add=TRUE, col="cadetblue3")
    lines(x= yv, y=dfP, col=cols[3], lwd=4)
    curve(dgpd_pp(x, location=z3, shape=0.00000001, scale=z4, threshold=z2), col=cols[5], add=TRUE, lwd=2, lty= 2)
    legend("topright", c("Data-Empirical", "Fitted-Theoretical POT-Poisson Process", "Fitted-Theoretical POT-Poisson-GPD Equivalence"),
          bty = "n",
          col = c("cadetblue3", cols[3], cols[5]),
          lty = c(0,1,2), lwd = c(0,4,2),
          pch = c(22, NA, NA),
          pt.bg = c("cadetblue3", NA, NA),
          pt.cex = 2)
    #assign(paste0("myprint", numberofplots), recordPlot())
    #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))		 
    numberofplots = numberofplots + 1
  
  #Equivalent POT-Poisson-GPD of POT-PP
  # - Strategy: Use fitdistrplus::gofstat and stats::ks.test to get goodness-of-fit statistics of the POT-PP, but as it is not possible to get
  #             easily the functions dpp(density), ppp(distriution) and qpp(quantile) of Poisson Process,
  #             then the equivalent GPD will be used:
  #     - 1) Define own functions for dgpd(density):'dmygpd', pgpd(distriution):'pmygpd' and qgpd(quantile):'qmygpd' 
  #          using GPD equations with constant dummy variable. See functions in function_lib.r. 
  #          Note: Be aware that equations for GPD are defined in help of function 'GPD' of package 'extraDistr' or 'gpd' in 'evd',
  #          but different equations are defined in:
  #                          - hysj_40_02_0165.pdf (Parameter estimation for 3-parameter
  #                                                 generalized pareto distribution by the
  #                                                 principle of maximum entropy (POME))
  #                          - rjef1_10_8.pdf
  #                          - os-2016-47.pdf
  #                          - Hosking, Wallis_1987_Parameter and quantile estimation for the generalized Pareto distribution
  #          so, the used functions are the 3-parameters generalized pareto distribution (hysj_40_02_0165.pdf)
  #     - 2) Calculate equivalent parameters between POT-PP and POT-Poisson-GPD (NIST.SP.500-301.pdf):
  #         Scale: equation after equation 18 page 39 (NIST.SP.500-301.pdf) for scale of POT-Poisson-GPD (gpdScale)
  #         Location: Equal to threshold of POT-PP (gpdLocation)
  #     - 3) As the function fitdistrplus::gofstat (to calculate goodness of fit statistics) need the fitted distribution with 
  #          desired parameters values(gpdScale, gpdLocation),
  #          the function fitdistrplus::fitdist was used solve this requirement:
  #           a) To fit own GPD defined functions which have a in addition to scale, location and shape, a dummy variable ('dmygpd', 'pmygpd' and 'qmygpd')
  #           b) Send fixed parameters (fix.arg=param5) (gpdScale, gpdLocation and shape=0.00001) to force calculation in only a dummy parameter 
  #              (start=param6) which is not interfering in the results as in the equations the dummy paramter is cancelled (dummy/dummy), 
  #              in this way we are fitting a distribution and forcing to return desired parameters due to the use of a dummy variable, 
  #              which does not alter anything in the resulting function! 
  #           c) Using the method "mle" and optim.method "L-BFGS-B"
  #       The result is the equivalent GPD in 'fp' variable with parameters gpdScale, gpdLocation and shape=0.00001
  #       Be aware that the resulting value of dummy parameters is not affecting the final fitted GPD, because in equations: dummy/dummy
    gpdScale = (-0.00001)*((-z4/0.00001)+z3-z2)
    gpdLocation = get("z2")
    attr(gpdLocation, "names") = NULL
    param5 = list(scale = gpdScale, location=gpdLocation, shape=0.00001)
    param6 = list(dummy = 1)
    #Fit equivalent GPD using MLE and forcing all parameters
    #Be aware that this results in fp was created with mygpd(related to frequency), but inside fp mass or density values are available
      fp <- fitdistrplus::fitdist(imp.vals$t.series, "mygpd",
                                  start=param6, fix.arg=param5, lower = c(0,0),
                                  method="mle", optim.method = "L-BFGS-B")
      #print(fp)
      #summary(fp)
      #cdfcomp(fp)
      #denscomp(fp)
      #ppcomp(fp)
      #qqcomp(fp)
      #quantile(fp, probs = c(0.05, 0.1, 0.2))

    # goodness-of-fit statistics
      mygof= fitdistrplus::gofstat(fp, discrete=FALSE) 
    
    #Kolmogorov smirnov test 
    mykstest = stats::ks.test(imp.vals$t.series, y="pmygpd", location=gpdLocation, scale=gpdScale, shape=0.00001, dummy=1)
    
    # Plot equivalent POT-Poisson-GPD using fp
    # Four graphics:
    # Left-top: Empirical and theoretical dens.
    # Left-bottom: Empirical and theoretical CDFs
    # Right-top: Q−Q plot
    # Right-bottom: P−P plot
      par(cex=1.2, bg="white")
      plot(fp, lwd=2, col="cadetblue3")
      
      mtext(side = 1, text = paste0("Page ", numberofplots, " - Declustered - Thunderstorm - POT-Poisson-GPD Equivalent. Location: ",
                                    round(fp$fix.arg$location, digits = 2), ". Scale: ", round(fp$fix.arg$scale, digits = 2),
                                    ". Shape: ", round(fp$fix.arg$shape, digits = 2)), outer = TRUE)
      #assign(paste0("myprint", numberofplots), recordPlot())
      #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))								
      numberofplots = numberofplots + 1
    
    # Calculation of RMSE
    # Be aware that RMSE calculation is related to empirical results
    # and not related to mass or density values because the use of min and max in empCDF(mx)
    # that is why we use pmygpd and empCDF
      mx <- seq(min(imp.vals$t.series), max(imp.vals$t.series), length = 10000)
      empCDF <- stats::ecdf(imp.vals$t.series)
      #D of Kolmogorov Smirnov
      #max(abs(empCDF(mx) - pmygpd(mx, scale = fp$fix.arg$scale, location =fp$fix.arg$location, shape = 0.00001, dummy=1)))
      #rmse
      rmse = sqrt(mean((empCDF(mx) - pmygpd(mx, scale = fp$fix.arg$scale, location =fp$fix.arg$location, shape = 0.00001, dummy=1))^2))
    
    # Write all results (GOF, KSTEST, RMSE, Parameters) to raw_data_station_id_fitted.xlsx (variable fnfitted)
      myfitgpd = list("Fit_fitdistrplus::fitdist" = capture.output(fp),
                    "GoodnessOfFit_fitdistrplus::gofstat"=capture.output(mygof),
                    "KolmogorovSmirnovTest_stats::ks.test"=capture.output(mykstest),
                    "RMSE"=capture.output(rmse))
      #write.xlsx(capture.output(myfitgpd), file=fnfitted, sheetName="t_POT-Poisson-GPD-Equivalent", append=TRUE, row.names=TRUE)
      addWorksheet(fnfitted_OUT, "t_POT-Poisson-GPD-Equivalent")
      writeData(fnfitted_OUT, sheet = "t_POT-Poisson-GPD-Equivalent", x = capture.output(myfitgpd))

  ##New Idea - Alexys
  #Probability density with the join density according to NIST.SP.500-301.pdf equation 2, page 15.
  #Same equation in Smith 2004. Extreme Values in Finance, Telecommunications, and the Environment,
  #Equation 1.18
  
  #Ip <- function(x) {1/z4 *(1+0.00001*((x-z3)/z4))^((-1/0.00001)-1)}
  
  #joindensityPoissonProcess <- function (serie, serie.dt){  #y1 and y2 are velocities (min and max in dataset),
  #                                                        #t1 and t2 are times (min and max in dataset)
  #  y1 = min(serie)
  #  y2 = max(serie)
  #  t1 = min(serie.dt)
  #  t2 = max(serie.dt)
  #  intensityvalues = lapply (serie, Ip)
  #  product = prod(intensityvalues)
  #  integrand <- Ip
  #  integrated <- integrate(integrand, lower=y1, upper=y2)$value
  #  joindensity <- product * exp(integrated*-1)
  #    Ip.t(lower)/integrated  # Equation after equation 2, page 15
  #}
  
  #CDF
  # Cumulative distribution function according to NIST.SP.500-301.pdf. Equation 5, page 17
  # The integral in the formula does not depend of time (only magnitude)
    distributionPoissonProcess <- function(velocity, threshold){  #lower is the value of velocity to integrate with it up to infinite
      integrand <- Ip.t
      integrated_tresh_to_vel <- integrate(integrand, lower=threshold, upper=velocity)$value
      integrated_tresh_to_inf <- integrate(integrand, lower=threshold, upper=Inf)$value
      #distribution <- 1- (integrated_tresh_to_vel/integrated_tresh_to_inf)#NIST.SP.500-301.pdf. Equation 5, page 17
      #Be ware I am not using 1 - () as is stated in equation 5
      distribution <- (integrated_tresh_to_vel/integrated_tresh_to_inf)#NIST.SP.500-301.pdf. Equation 5, page 17
    }
  
  #Calculate cumulative distribution function values for velocities y1 to y2 (Domain PP) using Poisson Process
  #DfP = sapply(yvels, distributionPoissonProcess, upper=Inf, threshold=z2)
    DfP = sapply(yv, distributionPoissonProcess, threshold=z2)
  
  #Plot: Cumulative distribution function with intensity function of Poisson Process
  #  - 1) Plot Cumulative distribution function with intensity function of Poisson Process (NIST.SP.500-301.pdf. Equation 5, page 17)
  #  - 2) Plot data-empirical cumulative distribution function (but related to mass or density): stats::ecdf(imp.vals$t.series)
  #  - 3) Plot equivalent Pot-Poisson-GPD cumulative distribution function (related to mass or density): pgpd_pp
  #
    library(RColorBrewer)
    cols <- brewer.pal(9,"Set1")
    plot(x= yv, y=DfP, xlab="Declustered - Thunderstorm - Velocities [Km/h]", ylab="Distribution Function - Poisson Process",
        main=paste("Cumulative Distribution Function from Intensity Function of Poisson Process\n", "Location=", round(z3,2), " Scale=", round(z4,2), "\n", "Station:", number),
        type="l", col=cols[3], lwd=4)
    mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
    #h = hist(imp.vals$t.series, probability = TRUE, plot=FALSE)
    #h$counts <- cumsum(h$counts)/sum(h$counts)
    #plot(h, add=TRUE)
    plot(stats::ecdf(imp.vals$t.series), col="cadetblue3", add=TRUE)
    lines(x= yv, y=DfP, col=cols[3], lwd=4)
    curve(pgpd_pp(x, location=z3, shape=0.00000001, scale=z4, threshold=z2), col=cols[5], add=TRUE, lwd=2, lty= 2)
    legend("bottomright", c("Data-Empirical", "Fitted-Theoretical POT-Poisson Process", "Fitted-Theoretical POT-Poisson-GPD Equivalence"),
          bty = "o",
          col = c("cadetblue3", cols[3], cols[5]),
          lty = c(1, 1, 2), lwd = c(1, 4, 2),
          pch = c(21, NA, NA),
          pt.bg = c("cadetblue3", NA, NA),
          pt.cex = 1, box.lwd = 0, box.col = "white", bg = "white")
    box(lty = 1, col = 'black', lwd=0.5)
    #assign(paste0("myprint", numberofplots), recordPlot())
    #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))  
    numberofplots = numberofplots + 1
  
  #Plot: Page 4
  #Histogram and fitted density function from density function of NIST.SP.500-301.pdf. Equation after equation 2, page 15
  #Whitout Substracting Threshold
  overthreshold = imp.vals$t.series >= z2 #Be sure only data above threshold are included
  #hist(imp.vals$t.series[overthreshold], prob=TRUE, main=paste("Fitted density function from Intensity Function of Poisson Process\nWhitout Substracting Threshold\nStation:", number, sep=" "))
  #lines(x=yvels, y=dfP, col="red")
  #mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
  #numberofplots = numberofplots + 1
  
  #Plot: Page 5
  #Histogram and fitted density function from density function of NIST.SP.500-301.pdf. Equation after equation 2, page 15
  #Subtracting Threshold
  #overthreshold = imp.vals$t.series >= z2 #Be sure only data above threshold are included
  #hist(imp.vals$t.series[overthreshold]-nt.thresh, prob=TRUE, main=paste("Fitted density function from Intensity Function of Poisson Process\nSubstracting Threshold\nStation:", number, sep=" "))
  #lines(x=yvels, y=dfP, col="red")
  #mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
  #numberofplots = numberofplots + 1
  
  #Calculate velocities (return values) for typical return periods using Poisson Process
    veocitiesfortypicalreturnperiodsP <- approx(x=paP, y=yvels, xout = typicalExcedenceProbabilities)$y  #Interpolate tipical excedence probabilities to get velocities using Poisson
  
  #Write return values to array zzz
    zzz[i,16:26] = veocitiesfortypicalreturnperiodsP #Velocities for typical return periods Poisson
  
  #Create and Plot Hazards Curve
  # 1) POT-PP Intensity function integral (x = 1/paP, y= yvels), (yvels are return levels from 1 to 600)
  #    paP calculated from function returnlevel_equation (equation 4, page 16 -NIST.SP.500-301.pdf) 
  #    using Ip (NIST.SP.500-301.pdf, page 28, shape parameter almost zero (0.00001))
  # 2) POT-PP formula (returnlevelPP) (Equation 18 page 39 (NIST.SP.500-301.pdf))
  # 3) POT-Poisson-GPD equivalence (returnlevelGPD) (Equation 15 page 39 - NIST.SP.500-301.pdf)
    library(RColorBrewer)
    cols <- brewer.pal(9,"Set1")
    maxy = max(veocitiesfortypicalreturnperiodsP) + 0.15*max(veocitiesfortypicalreturnperiodsP)
    plot(x= 1/paP, y=yvels, xlab="Return Periods (Years) - Poisson Process Intensity Function",
        ylab="Velocities Km/h", main=paste("Declustered - Thunderstorms - Hazard Curve\n", "Location=", round(z3,2), " Scale=", round(z4,2), "\n", "Station:", number),
        xlim=c(0,10000), ylim = c(0, maxy), type="l", col=cols[3], lwd=3)
    #curve (returnlevelPP(x, location=z3, shape=0.00001, scale=z4), col="red", add=TRUE, lwd=6)
    myx= 1:10000
    myrlpp = sapply(myx, returnlevelPP, location=z3, shape=0.00001, scale=z4)
    lines(myx, myrlpp, col=cols[4], lwd=2, lty=5)
    
    #curve (returnlevelGPD(x, location=z3, shape=0.00001, scale=z4, threshold=z2), col="black", add=TRUE, lwd=2)
    myrlgpd2 = sapply(myx, returnlevelGPD, location=z3, shape=0.00001, scale=z4, threshold=z2)
    lines(myx, myrlgpd2, col=cols[5], lwd=1, lty=2)
    points(x= tipicalReturnPeriods, y= veocitiesfortypicalreturnperiodsP, col=cols[9], pch=20)
    text(x = tipicalReturnPeriods, y = veocitiesfortypicalreturnperiodsP,
        labels = paste0("(",tipicalReturnPeriods,",",round(veocitiesfortypicalreturnperiodsP, digits=1),")"),
        cex=0.8, pos = 4)
    mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
    legend("bottomright", c("POT-PP (Intensity Function Integral)","POT-PP (formula)", "POT-Poisson-GPD equivalence"),
          bty = "n",
          col = cols[3:5],
          lty = c(1, 5, 2), lwd = c(3, 2, 1),
          pch = c(NA, NA, NA),
          pt.bg = c(NA, NA, NA),
          pt.cex = c(0,0,0))
    box(lty = 1, col = 'black', lwd=0.5)
    #assign(paste0("myprint", numberofplots), recordPlot())
    #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))
    numberofplots = numberofplots + 1
  
  
  #rlgpd = sapply(1:7000, returnlevelGPD, location=z3, shape=0.00001, scale=z4, threshold=z2)
  #plot(x= 1:7000, y=rlgpd, col="black", type="l")
  #plot(x= myx, y=myrlgpd2, col="black", type="l")





  #___
  #___b) Poisson Process - Gumbel like tile length. Shape = 0. 
  #___   Intensity Function: NIST.SP.500-301.pdf, page 32, equation 10 (shape parameter equal to zero)
  #___   Return Values: NIST.SP.500-301.pdf, equation before equation 4 (page 16)
  #___     As (At) is 1
  #___   Write return values: zzz[i,27:37]  
  #___   Plot hazard curve!
  #___
  
  
  #Intensity Function Poisson Process - Gumbel like tile length. Shape = 0. NIST.SP.500-301.pdf, page 32, equation 10
    Ig.t <- function(x) {1/z4 * exp(-(x-z3)/z4)}
    #at =  1 # It is not imp.vals$n.thunders.per.year*(1/24)   
                                                   #Same parameter As of equation before equation 4 (NIST.SP.500-301.pdf, page 16). 
                                                   #But it is equal to ONE when in the dataset there is only THUNDERSTORM data
                                                   #Meaning: Typical amount of thunderstorm in a year, but here always is ONE
  #Return values
  #Representing equation 4, page 16 (NIST.SP.500-301.pdf), but using Ig.t
    returnlevel_equation_gumbel <- function(lower, upper, at){  #lower is the value of velocity to integrate with it up to infinite
      integrated.t <- integrate(Ig.t, lower=lower, upper=upper)$value
      value = at * integrated.t
    }
  
  #Calculate excedence probability for velocities 1 to 600 using Poisson Process - Gumbel Intensity Function
    paG = sapply(yvels, returnlevel_equation_gumbel, upper=Inf, at=at)
  
  #Calculation of return values for Poisson Process - Gumbel Intensity Function
  # calculate velocities for typical return periods
    veocitiesfortypicalreturnperiodsG <- approx(x=paG, y=yvels, xout = typicalExcedenceProbabilities)$y  #Interpolate tipical excedence probabilities to get velocities using Gumbel
    zzz[i,27:37] = veocitiesfortypicalreturnperiodsG #Velocities for typical return periods Gumbel Tile Length Intensity Function
  
  #Plot Hazard Curve
  # using Intensity Function of Gumbel like tail length
    maxy = max(veocitiesfortypicalreturnperiodsG) + 0.15*max(veocitiesfortypicalreturnperiodsG)
    library(RColorBrewer)
    cols <- brewer.pal(9,"Set1")
    plot(x= 1/paG, y=yvels, xlab="Return Periods (Years) - Gumbel like tail Intensity Function of Poisson Process",
        ylab="Velocities Km/h", main=paste("Declustered - Thunderstorms - Hazard Curve\n", "Location=", round(z3,2), " Scale=", round(z4,2), "\n", "Station:", number),
        xlim=c(0,10000), ylim=c(0, maxy), type="l", col=cols[3], lwd=3)
    myx= 1:10000
    myrlpp = sapply(myx, returnlevelPP, location=z3, shape=0.00001, scale=z4)
    lines(myx, myrlpp, col=cols[4], lwd=2, lty=5)
    #curve (returnlevelPP(x, location=z3, shape=0.00001, scale=z4), col="red", add=TRUE, lwd=6)
    myrlgpd2 = sapply(myx, returnlevelGPD, location=z3, shape=0.00001, scale=z4, threshold=z2)
    lines(myx, myrlgpd2, col=cols[5], lty=2, lwd=1)
    #curve (returnlevelGPD(x, location=z3, shape=0.00001, scale=z4, threshold=z2), col="black", add=TRUE, lwd=2)
    points(x= tipicalReturnPeriods, y= veocitiesfortypicalreturnperiodsG, col=cols[9], pch=20)
    text(x = tipicalReturnPeriods, y = veocitiesfortypicalreturnperiodsG, labels = paste0("(",tipicalReturnPeriods,",",round(veocitiesfortypicalreturnperiodsG, digits=1),")"), cex=0.8, pos = 4)
    mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
    legend("bottomright", c("POT-PP (Gumbel like - Intensity Function Integral)","POT-PP (formula)", "POT-Poisson-GPD equivalence"),
          bty = "n",
          col = cols[3:5],
          lty = c(1, 5, 2), lwd = c(3, 2, 1),
          pch = c(NA, NA, NA),
          pt.bg = c(NA, NA, NA),
          pt.cex = c(0,0,0))
    box(lty = 1, col = 'black', lwd=0.5)
    #assign(paste0("myprint", numberofplots), recordPlot())
    #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))  
    numberofplots = numberofplots + 1
  
  #___
  #___c) Using Gumbel Quantile replacing Poisson Process fitted parameters (location=z3, scale=z4)
  #___   Plot PDF of Gumbel distribution with parameters of Poisson Process: THEY WILL NOT FIT WITH DATA, 
  #___     because GUMBEL PDF is not equivalent to Poisson Process (2D) PDF
  #___   Plot CDF of Gumbel distributon with parameters of Poisson Process: THEY WILL NOT FIT WITH DATA, 
  #___     because GUMBEL CDF is not equivalent to Poisson Process (2D) CDF
  #___   Plot HISTOGRAM first then PDF of Gumbel distribution with parameters of Poisson Process: THEY WILL NOT FIT WITH DATA, 
  #___     because GUMBEL PDF is not equivalent to Poisson Process (2D) PDF
  #___   Calculation of return levels using Quantile function of Gumbel 
  #        with Poisson Process parameters
  #        and probability equal to (1-(1/mri). Be aware that lambda is not included in probabilities, 
  #        and write the return values to zzz[i,38:48]
  #___   Plot of Hazard Curve
  #        using gumbel quantile function
  #        using POT-PP formula (Equation 18 page 39 NIST.SP.500-301.pdf)
  #        using POT-Poisson-GPD equivalence formula (Equation 15 page 39 NIST.SP.500-301.pdf)

  #Plot 
  #  - PDF of Gumbel distribution with parameters of Poisson Process
  #  - Data histogram of thunderstorm (They will not fit with Gumbel PDF because is not equivalent to Poisson Process (2D) PDF)
    #library(RcmdrMisc)
    .x <- seq(-300, 300, length.out=1000)
    dfG = RcmdrMisc::dgumbel(.x, location=z3, scale=z4)
    plot(.x, dfG, col="red",
        xlab="Thunderstorms - Velocities Km/h", ylab="Density Function - Gumbel Distribution",
        main=paste("Gumbel Density Function, but using parameters of Poisson Process\n", "Location=", round(z3,2), " Scale=", round(z4,2), "\n", "Station:", number),
        type="l", lwd=1, ylim=c(0,0.08))
    #plotDistr(.x, dgumbel(.x, location=0, scale=1), cdf=FALSE, xlab="x",
    #          ylab="Density", main=paste("Gumbel Distribution:  Location=0, Scale=1"))
    mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
    
    pdfG <- function(x) {1/z4 *exp(-(x-z3)/z4)*exp(-exp(-(x-z3)/z4))}
    #curve(pdfG, add=T)
    hist(imp.vals$t.series[overthreshold], prob=TRUE, add=T, col="cadetblue3")
    lines(.x, dfG, col="red")
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
  
  #pdfG <- function(x) {1/z4 *exp(-(x-0)/z4)*exp(-exp(-(x-0)/z4))}
  #curve(pdfG, add=T)
  
  #Plot
  #  - CDF cumulative distribution function with Gumbel distribution using parameters of Poisson Process
  #  - Empirical CDF using thunderstorm data. THEY WILL NOT FIT WITH DATA, 
  #      because GUMBEL CDF is not equivalent to Poisson Process (2D) CDF
    .x <- seq(-300, 300, length.out=1000)
    DfG = RcmdrMisc::pgumbel(.x, loc=z3, scale=z4)
    plot(.x, DfG,
        xlab="Thunderstorms - Velocities Km/h", ylab="Cumulative Distribution Function - Gumbel Distribution",
        main=paste("Gumbel Cumulative Distribution, but using parameters of Poisson Process\n", "Location=", round(z3,2), " Scale=", round(z4,2), "\n", "Station:", number),
        type="l", col="red", lwd=2)
    #lines(yvels, DfG, col="red", lty=4)
    mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
    plot(stats::ecdf(imp.vals$t.series), col="cadetblue3", add=TRUE)
    lines(.x, DfG, col="red")
    legend("bottomright", c("Data-Empirical", "Fitted-Theoretical"),
          bty = "o",
          col = c("cadetblue3", "red"),
          lty = c(1, 1), lwd = c(1, 2),
          pch = c(21, NA),
          pt.bg = c("cadetblue3", NA),
          pt.cex = 1, box.lwd = 0, box.col = "white", bg = "white")
    box(lty = 1, col = 'black', lwd=0.5)
    #assign(paste0("myprint", numberofplots), recordPlot())
    #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds")) 
    numberofplots = numberofplots + 1
  
  #Plot
  #  - Histogram of thunderstorm
  #  - Fitted density function of Gumbel distribution. THEY WILL NOT FIT (Not equivalent GUMBEL PDF and PP PDF)
    overthreshold = imp.vals$t.series >= z2 #Be sure only data above threshold are included
    #imp.vals$t.series[imp.vals$t.series > t.thresh]
    hist(imp.vals$t.series[overthreshold], prob=TRUE, xlab="Declustered Thunderstorm", col="cadetblue3",
        main=paste("Fitted Gumbel density function using parameters of Poisson Process\n", "Location=", round(z3,2), " Scale=", round(z4,2), "\n", "Station:", number))
    curve(RcmdrMisc::dgumbel(x, location=z3, scale=z4), add=TRUE, col="red", lwd=2)
    #lines(x=yvels, y=dfP, col="green")
    #lines(densityPoissonProcess(lower=x, upper=inf), add = TRUE)
    mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
    legend("topright", c("Data-Empirical", "Fitted-Theoretical"),
          bty = "n",
          col = c("cadetblue3", "red"),
          lty = c(0, 1), lwd = c(0, 2),
          pch = c(22, NA),
          pt.bg = c("cadetblue3", NA),
          pt.cex = 2)
    #assign(paste0("myprint", numberofplots), recordPlot())
    #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))		 
    numberofplots = numberofplots + 1
  
  #Return values using Quantile Function of Gumbel
  # Following section "Using the Fitted Model" of Extreme Wind Speeds: Overview (https://www.itl.nist.gov/div898/winds/overview.htm)
  # Calculation of Velocities for Return Periods using Percent Point Function of Gumbel (Quantiles: qgumbel)
  # But in that reference they use lambda (average number of events per year) for POT, 
  # and HERE we are not using lambda, i.e. that is to say as if it were the calculation of return levels for yearly maxima
  #    ->>>> It is related to the use of At equal to one (It is not considering time in the calculations here??)
  #    ->>>> If we use lamda it could be equivalent to the calculation of return levels using POT-Poisson-GPD!! 
  #           (XXX try do to it to test!!) >>> Function: gumbelVelocitiesQuantileFunctionNpy and in addition test Berry solution with
  #                                            function gumbelVelocitiesQuantileFunctionBerry
  
    #Using this function HERE!
    gumbelVelocitiesQuantileFunction <- function(mri){  #mri: return intervals from 1 to 3000
      RcmdrMisc::qgumbel((1-(1/mri)), location=z3, scale=z4)
    }
    
    #Not used here, it is meant to try the use of lambda
    gumbelVelocitiesQuantileFunctionLambda <- function(mri, npy, numberofsamples, numberofsamplesoverthreshold){  #mri: return intervals from 1 to 3000
      #npy #Number of observations per year
      #w = length(imp.vals$nt.series)/npy  #Fitting period: Total observations divided in npy
      w = numberofsamples/npy
      #Observations over threshold
      #overthresh = imp.vals$nt.series > nt.thresh
      #overthreshold = imp.vals$t.series >= z2
      #lambda = length(imp.vals$nt.series[overthresh])/w
      lambda = numberofsamplesoverthreshold/w
      RcmdrMisc::qgumbel((1-(1/(lambda*mri))), location=z3, scale=z4)
    }
    
    #Not used here, it is meant to try the use of lambda
    gumbelVelocitiesQuantileFunctionNpy <- function(mri, npy){  #mri: return intervals from 1 to 3000
      RcmdrMisc::qgumbel((1-(1/(npy*mri))), location=z3, scale=z4)
    }
    
    #Not used here, it it meant to try the use of lambda and truncate with Berry method!
    gumbelVelocitiesQuantileFunctionBerry <- function(mri, npy, truncate){  #mri: return intervals from 1 to 3000
      probs = 1 -(1/(mri*npy))
      probs2 = (probs-truncate)/(1-truncate)
      RcmdrMisc::qgumbel(probs2, location=z3, scale=z4)
    }

    #Calculation of return values
    #Calculation of velocities for Typical Return Periods using Quantile Function of Gumbel Distribution
    #veocitiesfortypicalreturnperiodsQG = sapply(tipicalReturnPeriods, function(x){qgumbel((1-(1/x)), location=z3, scale=z4)})
    veocitiesfortypicalreturnperiodsQG = sapply(tipicalReturnPeriods, gumbelVelocitiesQuantileFunction)
  
  #Next option whitout using Lambda
  #npy=imp.vals$n.thunders.per.year
  #veocitiesfortypicalreturnperiodsQG = sapply(tipicalReturnPeriods, gumbelVelocitiesQuantileFunctionLambda,
  #                                            npy=npy, numberofsamples= length(imp.vals$t.series),
  #                                            numberofsamplesoverthreshold= length(imp.vals$t.series[overthreshold]) )
  
  #This option using npy
  #npy=imp.vals$n.thunders.per.year
  #veocitiesfortypicalreturnperiodsQG = sapply(tipicalReturnPeriods, gumbelVelocitiesQuantileFunctionNpy, npy=npy)
  
  
  #This option using berry
  #npy=imp.vals$n.thunders.per.year
  #truncate = 1 - (sum(overthreshold)/length(imp.vals$t.series))
  #veocitiesfortypicalreturnperiodsQG = sapply(tipicalReturnPeriods, gumbelVelocitiesQuantileFunctionBerry, npy=npy, truncate=truncate)
  
    #Write the return values to the matrix zzz
    zzz[i,38:48] = veocitiesfortypicalreturnperiodsQG #Velocities for typical return periods using Gumbel quantile function
  
  #Plot of Hazard Curve
  #  - Using gumbel quantile function
  #  - Using POT-PP formula (Equation 18 page 39 NIST.SP.500-301.pdf)
  #  - POT-Poisson-GPD equivalence formula (Equation 15 page 39 NIST.SP.500-301.pdf)
    #Calculation of velocities for return periods from 1 to 3000
    mri = 1:10000
    allVelGumbelQuantileFunction = sapply(mri, gumbelVelocitiesQuantileFunction)
    library(RColorBrewer)
    cols <- brewer.pal(9,"Set1")
    #Hazard curve using quantile function of Gumbel distribution
    #Plot: Page 11
    plot(x= mri, y=allVelGumbelQuantileFunction, xlab="Return Periods (Years) - Gumbel Quantile Function using parameters of Poisson Process",
        ylab="Velocities Km/h", main=paste("Declustered - Thunderstorms - Hazard Curve\n", "Location=", round(z3,2), " Scale=", round(z4,2), "\n", "Station:", number),
        xlim=c(0,10000), type="l", col=cols[3], lwd=3)
    myx= 1:10000
    myrlpp = sapply(myx, returnlevelPP, location=z3, shape=0.00001, scale=z4)
    lines(myx, myrlpp, col=cols[4], lwd=2, lty=5)
    #curve (returnlevelPP(x, location=z3, shape=0.00001, scale=z4), col="red", add=TRUE, lwd=6)
    myrlgpd2 = sapply(myx, returnlevelGPD, location=z3, shape=0.00001, scale=z4, threshold=z2)
    lines(myx, myrlgpd2, col=cols[5], lwd=1, lty=2)
    #curve (returnlevelGPD(x, location=z3, shape=0.00001, scale=z4, threshold=z2), col="black", add=TRUE, lwd=2)
    points(x= tipicalReturnPeriods, y= veocitiesfortypicalreturnperiodsQG, col=cols[9], pch=20)
    text(x = tipicalReturnPeriods, y = veocitiesfortypicalreturnperiodsQG, labels = paste0("(",tipicalReturnPeriods,",",round(veocitiesfortypicalreturnperiodsQG, digits=1),")"), cex=0.8, pos = 4)
    mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
    legend("bottomright", c("Gumbel Quantile","POT-PP (formula)", "POT-Poisson-GPD equivalence"),
          bty = "n",
          col = cols[3:5],
          lty = c(1, 5, 2), lwd = c(3, 2, 1),
          pch = c(NA, NA, NA),
          pt.bg = c(NA, NA, NA),
          pt.cex = c(0,0,0))
    box(lty = 1, col = 'black', lwd=0.5)
    #assign(paste0("myprint", numberofplots), recordPlot())
    #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))  
    numberofplots = numberofplots + 1

  #___
  #___d) Using POT-Poisson-GPD with package XXX (This part is missing, need to be done!)
  #___   Write the return values to the Excel file
  #        zzz[i,49:59]
}
