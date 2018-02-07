#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dataRetrieval)
library(lubridate)
library(ggplot2)
source("helpers.R")
colors <- c("blue3", "red", "springgreen3", "pink2", "burlywood4")
sym <- c(15, 8, 19, 17, 3)
statChoices <-  c("P50", "AVG_ANNUAL", "CV_FLOW", "AVG_JAN", "AVG_FEB", "AVG_MAR", "AVG_APR", "AVG_MAY", "AVG_JUN", 
                    "AVG_JUL", "AVG_AUG", "AVG_SEP", "AVG_OCT", "AVG_NOV", "AVG_DEC", "MIN_1DAY", 
                    "MIN_7DAY", "MIN_30DAY", "MIN_90DAY", "MAX_1DAY", "MAX_3DAY", "MAX_7DAY",
                    "PUL_NO_P10", "PUL_DUR_P10", "PUL_MAG_P10", "PUL_NO_P90", "PUL_DUR_P90", "PUL_MAG_P90",
                    "PEAK_NUM", "PEAK_MAG","RB_INDEX", "TQMEAN","RISE_RATE", "FALL_RATE")


# Define server logic
shinyServer(function(input, output) {
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  p10p50p90 <- reactive({
    input$go
    sites <- isolate(input$sites)
    if(sites != "") {
      stations <- isolate(strsplit(sites, ",")[[1]])
      p10p50p90 <- list()
      for(i in stations) {
        p10p50p90[[i]] <- calcP10P50P90(station=i)
      }
      p10p50p90
    }
  })
  
  dailyData <- reactive({
    
    input$go
    
    sites <- isolate(input$sites)
    dateRange <- isolate(input$dateRange)
    if(sites != "") {
      
      #Return daily values of flow for all sites listed in input$sites
      dates <- c(paste(dateRange[1], "-01-01", sep=""), paste(dateRange[2], "-12-31", sep=""))
      stations <- isolate(strsplit(sites, ",")[[1]])
      dailyData <- list()
      statsOut <- list()
      for(i in stations) {
        raw <- readNWISdv(i, "00060", dates[1], dates[2])
        raw <- raw[,c("Date", "X_00060_00003")]
        names(raw) <- c("date", "discharge")
        raw <- preCheck(raw)
        dailyData[[i]] <- raw
      }
    } else {
      dailyData <- list()
    }
    return(dailyData)
  })
  
  allStats <- reactive({
    
    # input$go
    
    sites <- isolate(input$sites)
    dateRange <- isolate(input$dateRange)
    if(length(dailyData()) > 0) {
      
      #Return daily values of flow for all sites listed in input$sites
      dates <- c(paste(dateRange[1], "-01-01", sep=""), paste(dateRange[2], "-12-31", sep=""))
      stations <- isolate(strsplit(sites, ",")[[1]])
      statsOut <- list()
      for(i in stations) {
        statsOut[[i]] <- calcAllYears(dailyData()[[i]], i, p10p50p90()[[i]])
      }
    } else {
      statsOut <- list()
    }
    return(statsOut)
  })
  
  output$label1 <- renderText({
    
    input$go
    
    stations <- isolate(strsplit(input$sites, ",")[[1]])
    if(length(stations) > 0) {
      nm <- readNWISsite(stations[1])$station_nm
      label <- paste("Site:", nm)
    } else {
      label <- "Site: "
    }
    label
  })
  
  output$label2 <- renderText({
    
    input$go
    
    stations <- isolate(strsplit(input$sites, ",")[[1]])
    if(length(stations) > 1) {
      nm <- readNWISsite(stations[2])$station_nm
      label <- paste("Site:", nm)
    } else {
      label <- "Site: "
    }
    label
  })
  
  output$label3 <- renderText({
    
    input$go
    
    stations <- isolate(strsplit(input$sites, ",")[[1]])
    if(length(stations) > 2) {
      nm <- readNWISsite(stations[3])$station_nm
      label <- paste("Site:", nm)
    } else {
      label <- "Site: "
    }
    label
  })
  
  output$plot2statSelect <- renderUI({
    if(!(input$same2As1)) {
      selectizeInput("plotStats2", "Stats to plot:", choices=statChoices, multiple=TRUE)
    } else {
      selectizeInput("plotStats2", "Stats to plot:", choices=statChoices, multiple=TRUE,
                     selected = input$plotStats1)
    }
  })
  
  output$plot3statSelect <- renderUI({
    if(!(input$same3As1)) {
      selectizeInput("plotStats3", "Stats to plot:", choices=statChoices, multiple=TRUE)
    } else {
      selectizeInput("plotStats3", "Stats to plot:", choices=statChoices, multiple=TRUE,
                     selected = input$plotStats1)
    }
  })
  
  output$table1 <- renderTable({
    if(length(allStats())>0)
      outTable <- allStats()[[1]]
  }, rownames=TRUE)
  
  output$plot1 <- renderPlot({

    stats <- input$plotStats1
    stations <- isolate(strsplit(input$sites, ",")[[1]])
    
    allPlotStats <- stats
    if(input$forceScales) {
      if(!is.null(input$plotStats2))
        allPlotStats <- c(allPlotStats, input$plotStats2)
      if(!is.null(input$plotStats3))
        allPlotStats <- c(allPlotStats, input$plotStats3)
    }
    
    if(length(stations) > 0) {
      if(!is.null(stats)) {
        
        data <- data.frame(t(allStats()[[1]]))
        data$year <- as.integer(substr(row.names(data), 2, 5))
        
        #Calculate plotting limits
        yMax <- 1.2 * max(data[,stats])
        if(input$forceScales) {
          yMax <- vector()
          for(i in 1:length(allStats())) {
            d <- data.frame(t(allStats()[[i]]))
            yMax[i] <- max(d[,allPlotStats])
          }
          yMax <- max(yMax) * 1.2
        }

        #Make the plotting formula
        nStats <- length(stats)
        par(xpd=TRUE, mar=c(6.5, 4.1, 1.1, 2.1))
        plot(x = data$year, y=data[,stats[1]],
             pch=17, col=colors[1], las=1, xaxt="n", ylim=c(0, yMax),
             xlab="Year", ylab=paste(stats, collapse=", "), yaxs="i", cex=1.3)
        lines(x=data$year, y=data[,stats[1]], col=colors[1], cex=1.3)
        if(nStats > 1) {
          for(i in 2:nStats) {
            points(x = data$year, y=data[,stats[i]],col=colors[i],pch=17, cex=1.3)
            lines(x=data$year, y=data[,stats[i]], col=colors[i], cex=1.3)
          }
        }
        axis(1, at=unique(data$year), labels=as.character(unique(data$year)))
        par(mar=c(0,0,0,0))
        legend(min(data$year), -0.25 * yMax, stats, col=colors[1:nStats], pch=17, horiz=TRUE, bty='n')
      }
    }
  })
  
  #Allow user to download the flow data for site 1
  output$downloadFlow1 <- downloadHandler(
    filename = function() {
      stations <- strsplit(input$sites, ",")[[1]]
      paste(stations[1], "_flow.csv", sep="")
    },
    content = function(file) {
      stations <- strsplit(input$sites, ",")[[1]]
      dateRange <- input$dateRange
      dates <- c(paste(dateRange[1], "-01-01", sep=""), paste(dateRange[2], "-12-31", sep=""))
      raw <- readNWISdv(stations[1], "00060", dates[1], dates[2])
      raw <- raw[,c("Date", "X_00060_00003")]
      names(raw) <- c("date", "discharge in cfs")
      raw <- preCheck(raw)
      write.csv(raw, file, row.names=FALSE)
    }
  )
  
  output$downloadStats1 <- downloadHandler(
    filename = function() {
      stations <- strsplit(input$sites, ",")[[1]]
      paste(stations[1], "_stats.csv", sep="")
    },
    content = function(file) {
      stations <- strsplit(input$sites, ",")[[1]]
      table <- allStats()[[stations[1]]]
      write.csv(table, file, row.names=TRUE)
    }
  )
  
  output$table2 <- renderTable({
    if(length(allStats())>1)
      outTable <- allStats()[[2]]
  }, rownames=TRUE)
  
  output$plot2 <- renderPlot({
    stats <- input$plotStats2
    stations <- isolate(strsplit(input$sites, ",")[[1]])
    
    allPlotStats <- stats
    if(input$forceScales) {
      if(!is.null(input$plotStats2))
        allPlotStats <- c(allPlotStats, input$plotStats2)
      if(!is.null(input$plotStats3))
        allPlotStats <- c(allPlotStats, input$plotStats3)
    }
    
    if(length(stations) > 0) {
      if(!is.null(stats)) {
        
        data <- data.frame(t(allStats()[[2]]))
        data$year <- as.integer(substr(row.names(data), 2, 5))
        
        #Calculate plotting limits
        yMax <- 1.2 * max(data[,stats])
        if(input$forceScales) {
          yMax <- vector()
          for(i in 1:length(allStats())) {
            d <- data.frame(t(allStats()[[i]]))
            yMax[i] <- max(d[,allPlotStats])
          }
          yMax <- max(yMax) * 1.2
        }
        
        #Make the plotting formula
        nStats <- length(stats)
        par(xpd=TRUE, mar=c(6.5, 4.1, 1.1, 2.1))
        plot(x = data$year, y=data[,stats[1]],
             pch=17, col=colors[1], las=1, xaxt="n", ylim=c(0, yMax),
             xlab="Year", ylab=paste(stats, collapse=", "), yaxs="i", cex=1.3)
        if(nStats > 1) {
          for(i in 2:nStats) {
            points(x = data$year, y=data[,stats[i]],col=colors[i],pch=17, cex=1.3)
            lines(x=data$year, y=data[,stats[i]], col=colors[i], cex=1.3)
          }
        }
        axis(1, at=unique(data$year), labels=as.character(unique(data$year)))
        lines(x=data$year, y=data[,stats[1]], col=colors[1], cex=1.3)
        par(mar=c(0,0,0,0))
        legend(min(data$year), -0.25 * yMax, stats, col=colors[1:nStats], pch=17, horiz=TRUE, bty='n')
      }
    }
  })
  
  #Allow user to download the flow data for site 1
  output$downloadFlow2 <- downloadHandler(
    filename = function() {
      stations <- strsplit(input$sites, ",")[[1]]
      paste(stations[2], "_flow.csv", sep="")
    },
    content = function(file) {
      stations <- strsplit(input$sites, ",")[[1]]
      dateRange <- input$dateRange
      dates <- c(paste(dateRange[1], "-01-01", sep=""), paste(dateRange[2], "-12-31", sep=""))
      raw <- readNWISdv(stations[2], "00060", dates[1], dates[2])
      raw <- raw[,c("Date", "X_00060_00003")]
      names(raw) <- c("date", "discharge in cfs")
      raw <- preCheck(raw)
      write.csv(raw, file, row.names=FALSE)
    }
  )
  
  output$downloadStats2 <- downloadHandler(
    filename = function() {
      stations <- strsplit(input$sites, ",")[[1]]
      paste(stations[2], "_stats.csv", sep="")
    },
    content = function(file) {
      stations <- strsplit(input$sites, ",")[[1]]
      table <- allStats()[[stations[2]]]
      write.csv(table, file, row.names=TRUE)
    }
  )
  
  output$table3 <- renderTable({
    if(length(allStats())>2)
      outTable <- allStats()[[3]]
  }, rownames=TRUE)
  
  output$plot3 <- renderPlot({
    stats <- input$plotStats3
    stations <- isolate(strsplit(input$sites, ",")[[1]])
    
    allPlotStats <- stats
    if(input$forceScales) {
      if(!is.null(input$plotStats2))
        allPlotStats <- c(allPlotStats, input$plotStats2)
      if(!is.null(input$plotStats3))
        allPlotStats <- c(allPlotStats, input$plotStats3)
    }
    
    if(length(stations) > 0) {
      if(!is.null(stats)) {
        
        data <- data.frame(t(allStats()[[3]]))
        data$year <- as.integer(substr(row.names(data), 2, 5))
        
        #Calculate plotting limits
        yMax <- 1.2 * max(data[,stats])
        if(input$forceScales) {
          yMax <- vector()
          for(i in 1:length(allStats())) {
            d <- data.frame(t(allStats()[[i]]))
            yMax[i] <- max(d[,allPlotStats])
          }
          yMax <- max(yMax) * 1.2
        }
        
        #Make the plotting formula
        nStats <- length(stats)
        par(xpd=TRUE, mar=c(6.5, 4.1, 1.1, 2.1))
        plot(x = data$year, y=data[,stats[1]],
             pch=17, col=colors[1], las=1, xaxt="n", ylim=c(0, yMax),
             xlab="Year", ylab=paste(stats, collapse=", "), yaxs="i", cex=1.3)
        lines(x=data$year, y=data[,stats[1]], col=colors[i], cex=1.3)
        if(nStats > 1) {
          for(i in 2:nStats) {
            points(x = data$year, y=data[,stats[i]],col=colors[i],pch=17, cex=1.3)
            lines(x=data$year, y=data[,stats[i]], col=colors[i], cex=1.3)
          }
        }
        axis(1, at=unique(data$year), labels=as.character(unique(data$year)))
        lines(x=data$year, y=data[,stats[1]], col=colors[1], cex=1.3)
        par(mar=c(0,0,0,0))
        legend(min(data$year), -0.25 * yMax, stats, col=colors[1:nStats], pch=17, horiz=TRUE, bty='n')
      }
    }
  })
  
  #Allow user to download the flow data for site 1
  output$downloadFlow3 <- downloadHandler(
    filename = function() {
      stations <- strsplit(input$sites, ",")[[1]]
      paste(stations[3], "_flow.csv", sep="")
    },
    content = function(file) {
      stations <- strsplit(input$sites, ",")[[1]]
      dateRange <- input$dateRange
      dates <- c(paste(dateRange[1], "-01-01", sep=""), paste(dateRange[2], "-12-31", sep=""))
      raw <- readNWISdv(stations[3], "00060", dates[1], dates[2])
      raw <- raw[,c("Date", "X_00060_00003")]
      names(raw) <- c("date", "discharge in cfs")
      raw <- preCheck(raw)
      write.csv(raw, file, row.names=FALSE)
    }
  )
  
  output$downloadStats3 <- downloadHandler(
    filename = function() {
      stations <- strsplit(input$sites, ",")[[1]]
      paste(stations[3], "_stats.csv", sep="")
    },
    content = function(file) {
      stations <- strsplit(input$sites, ",")[[1]]
      table <- allStats()[[stations[3]]]
      write.csv(table, file, row.names=TRUE)
    }
  )
  
  output$combinedPlot <- renderPlot({
    
    input$go
    
    allData <- allStats()
    stations <- isolate(strsplit(input$sites, ",")[[1]])
    stats <- input$combinedStats

    if(length(stations) > 0 & !is.null(stats)) {
      
      nStats <- length(stats)
      
      labels <- vector()
      for(i in stations) {
        labels[length(labels) + 1] <- readNWISsite(i)$station_nm
      }
      
      #Find the overall plotting window 
      yMax <- vector()
      for(i in names(allData)) {
        yMax[length(yMax) + 1] <- max(t(allData[[i]])[,stats], na.rm=TRUE)
      }
      yMax <- max(yMax) * 1.2
      
      print(yMax)
      
      #Plot the first stat series from the first site
      data <- allData[[stations[1]]]
      data <- data.frame(t(data))
      data$year <- as.integer(substr(row.names(data), 2, 5))
      
      par(xpd=TRUE, mar=c(5.1, 4.1, 1.1, 20.1))
      plot(x = data$year, y=data[,stats[1]],
           pch=sym[1], col=colors[1], las=1, xaxt="n", ylim=c(0, yMax),
           xlab="Year", ylab=paste(stats, collapse=", "), yaxs="i", cex=1.3)
      lines(x=data$year, y=data[,stats[1]], col=colors[1], cex=1.3)
      #Plot any remaining stats for the first site
      if(nStats > 1) {
        for(i in 2:nStats) {
          points(x = data$year, y=data[,stats[i]],col=colors[1], pch=sym[i], cex=1.3)
          lines(x=data$year, y=data[,stats[i]], col=colors[1], cex=1.3)
        }
      }
      #Plot all of the stat series for the other sites (if there are other sites)
      if(length(stations) > 1) {
        for(i in 2:length(stations)) {
          data <- allData[[stations[i]]]
          data <- data.frame(t(data))
          data$year <- as.integer(substr(row.names(data), 2, 5))
          for(j in 1:nStats) {
            points(x=data$year, y=data[,stats[j]], col=colors[i], pch=sym[j], cex=1.3)
            lines(x=data$year, y=data[,stats[j]], col=colors[i], cex=1.3)
          }
        }
      }
      # par(mar=c(0,0,0,0))
      xPos <- max(data$year) + (max(data$year) - min(data$year)) * 0.05
      yPos <- yMax
      legend(xPos, yPos, legend=stats, col="black", pch=sym[1:nStats], bty='n')
      yPos = yPos * 0.5
      legend(xPos, yPos, legend=labels, col=colors[1:length(stations)], pch=17, bty='n')
      axis(1, at=unique(data$year), labels=as.character(unique(data$year)))
    }
  })
  
  output$whichTS <- renderUI({
    
    input$go
    
    stations <- isolate(strsplit(input$sites, ",")[[1]])
    if(length(stations) > 0) {
      nwis_sites <-readNWISsite(stations)
      nms <- nwis_sites$station_nm
      nos <- nwis_sites$site_no
    } else {
      nms <- vector()
    }

    if(length(nms) > 0) {
      ch <- split(nos, nms)
    } else {
      ch <- ""
    }
    
    selectInput("stationTS", "View time series for", choices=ch, width="100%")
    
  })
  
  output$timeSeries <- renderPlot({
    
    if(input$stationTS != "") {

      data <- dailyData()[[input$stationTS]]
      data$date <- as.Date(data$date)
      
      p <- ggplot() +
        geom_line(data=data, aes(x = date, y = discharge), color="blue") +
        scale_x_date() +
        theme(panel.background=element_blank(), panel.grid.major=element_line(color="black"),
              panel.grid.minor=element_line(color="black", linetype="dashed")) 
      if(input$tsLog) {
        p <- p + scale_y_log10()
      } else {
        p <- p + scale_y_continuous()
      }
  
      p
    }
    
  })
  
  observe({
    brush <- input$tsBrush
    if (!is.null(brush)) {
      ranges$x <- c(as.Date(brush$xmin, origin="1970-01-01"), as.Date(brush$xmax, origin="1970-01-01"))
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$zoomedPlot <- renderPlot({
    
    if(input$stationTS != "") {
      
      data <- dailyData()[[input$stationTS]]
      data$date <- as.Date(data$date)
      
      p <- ggplot() +
        geom_line(data=data, aes(x = date, y = discharge), color="blue") +
        theme(panel.background=element_blank(), panel.grid.major=element_line(color="black"),
              panel.grid.minor=element_line(color="black", linetype="dashed")) 
      if(!(is.null(ranges$x))) {
        if(input$zpLog) {
          p <- p + scale_y_log10(limits=ranges$y) + scale_x_date(limits=ranges$x)
        } else {
          p <- p + scale_y_continuous(limits=ranges$y) + scale_x_date(limits=ranges$x)
        }
      } else {
        p <- p + scale_x_date()
        if(input$zpLog) {
          p <- p + scale_y_log10()
        } else {
          p <- p + scale_y_continuous()
        }
      }
     
      p 
    }
    
  })
  
})
