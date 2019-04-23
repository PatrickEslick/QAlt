library(lubridate)
library(dataRetrieval)

#Take a daily data frame and subset it by only the complete years
preCheck <- function(dailyData) {
  dates <- dailyData$date
  #Find the starting and ending years
  firstYear <- year(min(dates))
  lastYear <- year(max(dates))
  years <- firstYear:lastYear
  firstDay <- as.Date(paste(firstYear, "-01-01", sep=""))
  lastDay <- as.Date(paste(lastYear, "-12-31", sep=""))
  #Generate a list of dates from the first of the first year to the last of the last year
  completeDates <- seq(from=firstDay, to=lastDay, by=1)
  completeYears <- year(completeDates)
  #Check if the dates in the dataframe are are in completeDates
  present <- completeDates %in% dates
  complete <- data.frame(date = completeDates, year=completeYears, present=present)
  isComplete <- vector()
  for(i in years) {
    if(all(complete[complete$year==i,"present"])) {
      isComplete[length(isComplete)+1] <- TRUE
    } else {
      isComplete[length(isComplete)+1] <- FALSE
    }
  }
  keepDates <- complete[complete$year %in% years[isComplete],"date"]
  dailyData <- dailyData[dailyData$date %in% keepDates,]
}

#Give a vector of the rolling means for a given number of days
rollX <- function(dailyData, days) {
  #Initialize a vector to hold each rolling sum
  rolling_mean <- vector(mode="numeric")
  #For each 3-day sum
  for(i in 1:(nrow(dailyData)-days)) {
    rolling_mean[length(rolling_mean) + 1] <- mean(dailyData[i:(i+(days-1)),"discharge"])
  }
  #Return the lowest 3-day sum
  return(rolling_mean)
}

#Find the average discharge for a given month
avgMonth <- function(dailyData, month) {
  dailyData$month <- month(dailyData$date)
  avg <- mean(dailyData[dailyData$month==month,"discharge"])
  return(avg)
}

#Find the coefficient of variation for a given
coefVar <- function(dailyData) {
  q <- dailyData$discharge
  cv <- (sd(q)/mean(q)) * 100
  return(cv)
}

richardsBaker <- function(dailyData) {
  qq <- vector()
  for(i in 2:nrow(dailyData)) {
    qq[length(qq) + 1] <- abs(dailyData$discharge[i] - dailyData$discharge[i-1])
  }
  rb <- sum(qq)/sum(dailyData$discharge)
  return(rb)
}

calcP10P50P90 <- function(station = NULL, dailyData = NULL) {
  
  if(is.null(station)) {
    if(is.null(dailyData)) {
      return(c(0, 0, 0))
    }
    dailyData <- preCheck(dailyData)
    q <- quantile(dailyData$discharge, c(0.10, 0.50, 0.90))
  } else {
    dailyData <- readNWISdv(station, "00060")[,c("Date", "X_00060_00003")]
    names(dailyData) <- c("date", "discharge")
    if(station=="06893300") {
      dailyData <- dailyData[dailyData$date > as.Date("1984-12-31"),]
    }
    dailyData <- preCheck(dailyData)
    q <- quantile(dailyData$discharge, c(0.10, 0.50, 0.90))
  }
  return(q)
}

#Output number of peaks (3X the mean) and the average magnitude thereof
peakStats <- function(dailyData, peakThresh) {
  inPeak <- dailyData$discharge > peakThresh
  inPeak <- as.numeric(inPeak)
  diff <- inPeak[1]
  for(i in 2:length(inPeak)) {
    diff[i] <- inPeak[i] - inPeak[i-1]
  }
  nPeaks <- length(diff[diff==1])
  magnitude <- mean(dailyData$discharge[as.logical(inPeak)])
  output <- c(nPeaks, magnitude)
  return(output)
}

#Output 3 statistics related to the estimated p10
##Number of flow pulses less than the 10th percentile
##Average duration (in days) of flow pulses less than the 10th percentile
##Average magnitude of flow pulses less than the 10th percentile
p10Stats <- function(dailyData, p10) {

  inPulse <- dailyData$discharge < p10
  inPulse <- as.numeric(inPulse)
  diff <- inPulse[1]
  for(i in 2:length(inPulse)) {
    diff[i] <- inPulse[i] - inPulse[i-1]
  }
  nPulses <- length(diff[diff==1])
  magnitude <- mean(dailyData$discharge[as.logical(inPulse)])
  duration <- sum(inPulse)/nPulses
  output <- c(nPulses, magnitude, duration)
  return(output)
  
}

#Output 3 statistics related to the estimated p90
##Number of flow pulses geater than the 90th percentile
##Average duration (in days) of flow pulses greater than the 90th percentile
##Average magnitude of flow pulses greater than the 90th percentile
p90Stats <- function(dailyData, p90) {
  
  inPulse <- dailyData$discharge > p90
  inPulse <- as.numeric(inPulse)
  diff <- inPulse[1]
  for(i in 2:length(inPulse)) {
    diff[i] <- inPulse[i] - inPulse[i-1]
  }
  nPulses <- length(diff[diff==1])
  magnitude <- mean(dailyData$discharge[as.logical(inPulse)])
  duration <- sum(inPulse)/nPulses
  output <- c(nPulses, magnitude, duration)
  return(output)
  
}

tqmean <- function(dailyData) {
  Qmean <- mean(dailyData$discharge)
  nExcede <- sum(dailyData$discharge > Qmean)
  tqmean <- nExcede/nrow(dailyData)
  return(tqmean)
}

riseFallRate <- function(dailyData) {
  change <- vector()
  for(i in 2:nrow(dailyData)) {
    change[length(change) + 1] <- dailyData$discharge[i] - dailyData$discharge[i-1]
  }
  rises <- change[change>0]
  falls <- abs(change[change<0])
  riseRate <- mean(rises)
  fallRate <- mean(falls)
  return(c(riseRate, fallRate))
}

drainageArea <- function(site) {
  siteInfo <- readNWISsite(site)
  drainArea <- siteInfo$drain_area_va
  return(drainArea)
}

#Calculate all stats for a single year of data
yearStats <- function(dailyData, drainArea, station, p10p50p90) {
  
  #Some preliminary calculations
  p10 <- p10p50p90[1]
  p50 <- p10p50p90[2]
  p90 <- p10p50p90[3]
  roll3 <- rollX(dailyData, 3)
  roll7 <- rollX(dailyData, 7)
  roll30 <- rollX(dailyData, 30)
  roll90 <- rollX(dailyData, 90)
  peakOut <- peakStats(dailyData, peakThresh = (p50 * 3))
  riseFall <- riseFallRate(dailyData)
  p10output <- p10Stats(dailyData, p10)
  p90output <- p90Stats(dailyData, p90)
  
  #Run the stats
  P50 <- median(dailyData$discharge)/drainArea
  AVG_ANNUAL <- mean(dailyData$discharge)/drainArea
  CV_FLOW <- coefVar(dailyData)
  AVG_JAN <- avgMonth(dailyData, 1)/drainArea
  AVG_FEB <- avgMonth(dailyData, 2)/drainArea
  AVG_MAR <- avgMonth(dailyData, 3)/drainArea
  AVG_APR <- avgMonth(dailyData, 4)/drainArea
  AVG_MAY <- avgMonth(dailyData, 5)/drainArea
  AVG_JUN <- avgMonth(dailyData, 6)/drainArea
  AVG_JUL <- avgMonth(dailyData, 7)/drainArea
  AVG_AUG <- avgMonth(dailyData, 8)/drainArea
  AVG_SEP <- avgMonth(dailyData, 9)/drainArea
  AVG_OCT <- avgMonth(dailyData, 10)/drainArea
  AVG_NOV <- avgMonth(dailyData, 11)/drainArea
  AVG_DEC <- avgMonth(dailyData, 12)/drainArea
  MIN_1DAY <- min(dailyData$discharge)/drainArea
  MIN_3DAY <- min(roll3)/drainArea
  MIN_7DAY <- min(roll7)/drainArea
  MIN_30DAY <- min(roll30)/drainArea
  MIN_90DAY <- min(roll90)/drainArea
  MAX_1DAY <- max(dailyData$discharge)/drainArea
  MAX_3DAY <- max(roll3)/drainArea
  MAX_7DAY <- max(roll7)/drainArea
  PUL_NO_P10 <- p10output[1]
  PUL_DUR_P10 <- p10output[3]
  PUL_MAG_P10 <- p10output[2]/drainArea
  PUL_NO_P90 <- p90output[1]
  PUL_DUR_P90 <- p90output[3]
  PUL_MAG_P90 <- p90output[2]/drainArea
  PEAK_NUM <- peakOut[1]
  PEAK_MAG <- peakOut[2]/drainArea
  RB_INDEX <- richardsBaker(dailyData)
  TQMEAN <- tqmean(dailyData)
  RISE_RATE <- riseFall[1]/drainArea
  FALL_RATE <- riseFall[2]/drainArea
  
  #Collect the stats in a vector
  stats <- c(P50, AVG_ANNUAL, CV_FLOW, AVG_JAN, AVG_FEB, AVG_MAR, AVG_APR, AVG_MAY, AVG_JUN, AVG_JUL, AVG_AUG,
             AVG_SEP, AVG_OCT, AVG_NOV, AVG_DEC, MIN_1DAY, MIN_3DAY, MIN_7DAY, MIN_30DAY, MIN_90DAY, MAX_1DAY,
             MAX_3DAY, MAX_7DAY, PUL_NO_P10, PUL_DUR_P10, PUL_MAG_P10, PUL_NO_P90, PUL_DUR_P90, PUL_MAG_P90,
             PEAK_NUM, PEAK_MAG, RB_INDEX, TQMEAN, RISE_RATE, FALL_RATE)
  names(stats) <- c("P50", "AVG_ANNUAL","CV_FLOW", "AVG_JAN", "AVG_FEB", "AVG_MAR", "AVG_APR", "AVG_MAY", "AVG_JUN", 
                    "AVG_JUL", "AVG_AUG", "AVG_SEP", "AVG_OCT", "AVG_NOV", "AVG_DEC", "MIN_1DAY", "MIN_3DAY",
                    "MIN_7DAY", "MIN_30DAY", "MIN_90DAY", "MAX_1DAY", "MAX_3DAY", "MAX_7DAY",
                    "PUL_NO_P10", "PUL_DUR_P10", "PUL_MAG_P10", "PUL_NO_P90", "PUL_DUR_P90", "PUL_MAG_P90",
                    "PEAK_NUM", "PEAK_MAG","RB_INDEX", "TQMEAN","RISE_RATE", "FALL_RATE")
  return(stats)
}

calcAllYears <- function(dailyData, site, p10p50p90) {
  
  #Find the years for which the calculation will be done
  dailyData$year <- year(dailyData$date)
  firstYear <- min(dailyData$year)
  lastYear <- max(dailyData$year)
  
  #Find the peak threshold and drainage area
  drainArea <- drainageArea(site)
  
  #Calculate stats for each year and add them to the list
  yrStats <- list()
  for(i in firstYear:lastYear) {
    temp <- dailyData[dailyData$year==i,]
    yrStats[[length(yrStats) + 1]] <- yearStats(temp, drainArea, site, p10p50p90)
  }
  
  #Convert the list into a data frame
  statsOut <- data.frame(matrix(unlist(yrStats), nrow=length(yrStats[[1]])))
  names(statsOut) <- paste("Y", as.character(firstYear:lastYear), sep="")
  row.names(statsOut) <- c("P50", "AVG_ANNUAL", "CV_FLOW", "AVG_JAN", "AVG_FEB", "AVG_MAR", "AVG_APR", "AVG_MAY", "AVG_JUN", 
                           "AVG_JUL", "AVG_AUG", "AVG_SEP", "AVG_OCT", "AVG_NOV", "AVG_DEC", "MIN_1DAY", "MIN_3DAY",
                           "MIN_7DAY", "MIN_30DAY", "MIN_90DAY", "MAX_1DAY", "MAX_3DAY", "MAX_7DAY",
                           "PUL_NO_P10", "PUL_DUR_P10", "PUL_MAG_P10", "PUL_NO_P90", "PUL_DUR_P90", "PUL_MAG_P90",
                           "PEAK_NUM", "PEAK_MAG","RB_INDEX", "TQMEAN","RISE_RATE", "FALL_RATE")
  return(statsOut)
  
}

#Proudce a duration curve from a vector of dates and corresponding flows. 
#dates - date object
#flows - a numerical vector
#yrs - an integer vector of years
#normalize - the value of 
durationPlot <- function(dates, flows, yrs, normalize = 1, log=FALSE) {
  
  flowData <- data.frame(dates, flows)
  flowData$year <- year(flowData$dates)
  flowData <- flowData[flowData$year %in% yrs,]
  
  flowData$normFlows <- flowData$flows/normalize
  
  quant <- seq(from = 0.01, to = 0.99, by=0.01)
  flowQuantiles <- data.frame(quant, normFlows = quantile(flowData$normFlows, quant))
  flowQuantiles$quant <- rev(flowQuantiles$quant) * 100
  
  if(normalize == 1) {
    ylab <- "Streamflow, in cfs"
  } else {
    ylab <- "Streamflow, normalized by drainage area, in cfs/sq. mile"
  }
  ttl <- paste0("Excedence plot, ", min(yrs), "-", max(yrs))
  
  exPlot <- ggplot(data = flowQuantiles) +
    geom_line(aes(x=quant, y=normFlows), color="steelblue") + 
    geom_point(aes(quant, y=normFlows), color="blue") +
    scale_x_continuous(breaks = seq(0, 100, 25), minor_breaks = 0) +
    theme(panel.background = element_rect(fill="white"),
          panel.grid.major = element_line(color="grey50", linetype = "solid"),
          panel.grid.minor = element_line(color="grey50", linetype = "dashed"),
          title = element_text(size = 16),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14)) +
    labs(title=ttl, x = "Frequency of exceedance, in percent", y=ylab)
  
  if(log)
    exPlot <- exPlot + scale_y_log10()
  
  return(exPlot)
  
}

#Function to calculate total flow volume for a period of time
flowVolume <- function(flowData) {
  
  flowData$dailyCF <- flowData$discharge * 86400
  totalVolume <- sum(flowData$dailyCF)
  return(totalVolume)
  
}

flowComponentPlot <- function(dates, flows, title = "Flow components") {
  
  month_letter <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
  month_start <- as.numeric(as.character(as.Date(c("2001-01-01", "2001-02-01", "2001-03-01", "2001-04-01",
                                                  "2001-05-01", "2001-06-01", "2001-07-01", "2001-08-01",
                                                  "2001-09-01", "2001-10-01", "2001-11-01", "2001-12-01")),
                            format = "%j"))
  
  data <- data.frame(date = dates, Q = flows)
  data$J <- as.numeric(as.character(data$date, format = "%j"))
  min <- e95 <- e75 <- e10 <- e5 <- vector(length = 72, mode = "numeric")
  for(i in 1:72) {
    qntl <- quantile(data$Q[data$J %in% (((i-1) * 5) + 1):(((i-1) * 5) + 5)],
                     probs = c(0, 0.05, 0.25, 0.90, 0.95))
    min[i] <- qntl[1]
    e95[i] <- qntl[2]
    e75[i] <- qntl[3]
    e10[i] <- qntl[4]
    e5[i] <- qntl[5]
  }
  
  plotData <- data.frame(D = (0:71)*5 + 1, min, e95, e75, e10, e5)
  
  plt <- ggplot(data = plotData) +
    geom_ribbon(aes(x = D, ymin = min, ymax = e95, fill = "mygreen")) +
    geom_ribbon(aes(x = D, ymin = e95, ymax = e75, fill = "mypurple")) +
    geom_ribbon(aes(x = D, ymin = e75, ymax = e10, fill = "myblue")) +
    geom_ribbon(aes(x = D, ymin = e10, ymax = e5, fill = "myred")) +
    scale_x_continuous(breaks = month_start, labels = month_letter) +
    scale_fill_manual("", values = c(mygreen = "#4daf4a", mypurple = "#984ea3", 
                                     myblue = "#377eb8", myred = "#e41a1c"),
                      labels = c("Min-5%", "5%-25%", "25%-90%", "90%-95%")) +
    ylab("Discharge (cfs)") + xlab(NULL) + ggtitle(title) +
    theme(panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(color = "grey80"))
 
  return(plt)
   
}



