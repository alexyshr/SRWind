#Return levels and plots considering the whole dataset as:
# thunderstorm and non-thunderstorm, i.e. both(t, and nt) exists
  #___a) POT-Poisson Process using Shape parameter!!
  #___   Intensity Function: NIST.SP.500-301.pdf, page 29, shape parameter almost zero (0.00001)
  #___   Return Values: NIST.SP.500-301.pdf, equation before equation 4 (page 16)
  #___     At (As) is imp.vals$n.thunders.per.year*(1/24)
  #___     Ant (Ans) is 365 - imp.vals$n.thunders.per.year*(1/24)
  #___   Write return values to zzz[zz,104:114]
  #___   Return level graphic to PDF
if (length(imp.vals$t.series.dt) > 0) {
  #Graphics for non-thunderstorm
  if (length(imp.vals$nt.series.dt) > 0) {
    #Intensity Function Poisson Process. NIST.SP.500-301.pdf, page 29
    Ip.t <- function(x) {1/z4 *(1+0.00001*((x-z3)/z4))^((-1/0.00001)-1)}
    Ip.nt <- function(x) {1/z7 *(1+0.00001*((x-z6)/z7))^((-1/0.00001)-1)}
    at = imp.vals$n.thunders.per.year*(1/24)*(1/365) #Same parameter As of equation before equation 4 (NIST.SP.500-301.pdf, page 16). 
                                             #Units in Days!!
                                             #Typical amount of thunderstorm in a year
    ant = 1-at#365-at #Same parameter Ans of equation before equation 4 (NIST.SP.500-301.pdf, page 16).
                 #Typical amount of non-thunderstorm in a year. 
                 #If At = 0, then Ans = 365-0
                 #Units in days

    #Representing equation 4, page 16 (NIST.SP.500-301.pdf), but using Ip.t and Ip.nt
    returnlevel_equation <- function(lower, upper, at, ant){ #lower is the value of velocity to integrate with it to infinite
      integrated.t <- integrate(Ip.t, lower=lower, upper=upper)$value
      integrated.nt <- integrate(Ip.nt, lower=lower, upper=upper)$value
      value = (at * integrated.t) + (ant * integrated.nt)
      #Alexys AIS
      valor = c(value, 100*(at * integrated.t)/value, 100*(ant * integrated.nt)/value)
    }

    #Calculate excedence probability for velocities 1 to 600 using Poisson Process
    probabilitiesreturnlevel = sapply(yvels, returnlevel_equation, upper=Inf, at=at, ant=ant)
    #Alexys AIS
    #veocitiesfortypicalreturnperiodsP <- approx(x=probabilitiesreturnlevel, y=yvels, xout=typicalExcedenceProbabilities)$y  #Interpolate tipical excedence probabilities to get velocities using Poisson
    veocitiesfortypicalreturnperiodsP <- approx(x=probabilitiesreturnlevel[1,], y=yvels, xout=typicalExcedenceProbabilities)$y  #Interpolate tipical excedence probabilities to get velocities using Poisson
    percentage_pt_fortypicalreturnperiodsP <- approx(x=probabilitiesreturnlevel[1,], y=probabilitiesreturnlevel[2,], xout=typicalExcedenceProbabilities)$y
    percentage_pnt_fortypicalreturnperiodsP <- approx(x=probabilitiesreturnlevel[1,], y=probabilitiesreturnlevel[3,], xout=typicalExcedenceProbabilities)$y
    
    #Write return values to matrix zzz
    zzz[i,104:114] = veocitiesfortypicalreturnperiodsP #Velocities for typical return periods Poisson
    zzz[i,115:125] = percentage_pt_fortypicalreturnperiodsP
    zzz[i,126:136] = percentage_pnt_fortypicalreturnperiodsP
    #Plot return level curve
    maxy = max(veocitiesfortypicalreturnperiodsP) + 0.15*max(veocitiesfortypicalreturnperiodsP)
    #Hazards Curves using Intensity Function of Poisson Process
    plot(x= 1/probabilitiesreturnlevel[1,], y=yvels, xlab="Return Periods (Years) - Poisson Process Intensity Function",
         ylab="Velocities Km/h", main=paste("Declustered - Thunderstorms and Non-Thunderstorm - Hazard Curve - Station:", number, sep=" "),
         xlim=c(0,10000), ylim = c(0, maxy), type="l", col="blue", lwd=2)
    points(x= tipicalReturnPeriods, y= veocitiesfortypicalreturnperiodsP, col="red", pch=20)
    text(x = tipicalReturnPeriods, y = veocitiesfortypicalreturnperiodsP, labels = paste0("(",tipicalReturnPeriods,",",round(veocitiesfortypicalreturnperiodsP, digits=1),")"), cex=0.8, pos = 4)
    mtext(side = 1, text = paste0("Page ", numberofplots), outer = TRUE)
    legend("bottomright", c("Hazard Curve"),
           bty = "n",
           col = "blue",
           lty = c(1), lwd = c(2),
           pch = c(NA),
           pt.bg = c(NA),
           pt.cex = c(0))
    box(lty = 1, col = 'black', lwd=0.5)
    #assign(paste0("myprint", numberofplots), recordPlot())
    #saveRDS(eval(parse(text=paste0("myprint", numberofplots))), paste0(outputpath, "myprint", numberofplots, ".rds"))	
    numberofplots = numberofplots + 1
  }
}
