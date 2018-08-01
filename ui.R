library(shiny)
library(shinydashboard)
statChoices <-  c("P50", "AVG_ANNUAL", "CV_FLOW", "AVG_JAN", "AVG_FEB", "AVG_MAR", "AVG_APR", "AVG_MAY", "AVG_JUN", 
                  "AVG_JUL", "AVG_AUG", "AVG_SEP", "AVG_OCT", "AVG_NOV", "AVG_DEC", "MIN_1DAY", 
                  "MIN_7DAY", "MIN_30DAY", "MIN_90DAY", "MAX_1DAY", "MAX_3DAY", "MAX_7DAY",
                  "PUL_NO_P10", "PUL_DUR_P10", "PUL_MAG_P10", "PUL_NO_P90", "PUL_DUR_P90", "PUL_MAG_P90",
                  "PEAK_NUM", "PEAK_MAG","RB_INDEX", "TQMEAN","RISE_RATE", "FALL_RATE")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Streamflow alteration"),
  
  #Controls for generating the table and plots
  fluidRow(
    column(2,textInput("sites", "Site numbers:", value="06893300,06893390,06892360"),
           helpText("Up to 3, comma separated"),
           checkboxInput("forceScales", "Force Equivalent Scales")),
    column(2, sliderInput("dateRange","Years:", 1999, 2017, c(2014, 2016), step=1, sep="")),
    column(1, actionButton("go", "Go"))
  ),
  fluidRow(
    helpText("Note: Incomplete years of data will not be shown or used in calculations")
  ),
#Output the tables and plots
  tabsetPanel(
    tabPanel("Sites",
      fluidRow(
        column(4,
          textOutput("label1"),
          tabsetPanel(
            tabPanel("Table",
              box(width=NULL,
                div(style='overflow-x: scroll',tableOutput("table1"))
              )
            ),
            tabPanel("Plot",
              selectizeInput("plotStats1", "Stats to plot:", choices=statChoices, multiple=TRUE),
              plotOutput("plot1")         
            ),
            tabPanel("Exceedance Plot",
              fluidRow(
                column(6,
                  uiOutput("yearSelect1")   
                ),
                column(4,
                  checkboxInput("exNorm1", "Normalize by drainage area", value=FALSE)
                ),
                column(2,
                  checkboxInput("exLog1", "Log scale", value=FALSE)
                )
              ),
              plotOutput("exPlot1")
            ),
            tabPanel("Download",
              helpText("-"),
              downloadButton("downloadFlow1", "Daily Flow"),
              downloadButton("downloadStats1", "Stats Table")
            )
          )
        ),
        column(4,
          textOutput("label2"),
          tabsetPanel(
            tabPanel("Table",
              box(width=NULL,
                div(style='overflow-x: scroll',tableOutput("table2"))
              )
            ),
            tabPanel("Plot",
              fluidRow(
                column(7,uiOutput("plot2statSelect")),
                column(4, helpText("- "), checkboxInput("same2As1", "Use Plot 1 Selection"))
              ),
              plotOutput("plot2")         
            ),
            tabPanel("Exceedance Plot",
              fluidRow(
                column(6,
                  uiOutput("yearSelect2")   
                ),
                column(4,
                  checkboxInput("exNorm2", "Normalize by drainage area", value=FALSE)
                ),
                column(2,
                  checkboxInput("exLog2", "Log scale", value=FALSE)
                )
              ),
              plotOutput("exPlot2")
            ),
            tabPanel("Download",
              helpText("-"),
              downloadButton("downloadFlow2", "Daily Flow"),
              downloadButton("downloadStats2", "Stats Table")
            )
          )      
        ),
        column(4,
          textOutput("label3"),
          tabsetPanel(
            tabPanel("Table",
              box(width=NULL,
                div(style='overflow-x: scroll',tableOutput("table3"))
              )
            ),
            tabPanel("Plot",
              fluidRow(
                column(7,uiOutput("plot3statSelect")),
                column(4, helpText("- "), checkboxInput("same3As1", "Use Plot 1 Selection"))
              ),
              plotOutput("plot3")         
            ),
            tabPanel("Exceedance Plot",
              fluidRow(
                column(6,
                  uiOutput("yearSelect3")   
                ),
                column(4,
                  checkboxInput("exNorm3", "Normalize by drainage area", value=FALSE)
                ),
                column(2,
                  checkboxInput("exLog3", "Log scale", value=FALSE)
                )
              ),
              plotOutput("exPlot3")
            ),
            tabPanel("Download",
              helpText("-"),
              downloadButton("downloadFlow3", "Daily Flow"),
              downloadButton("downloadStats3", "Stats Table")
            )
          )       
        )
      )
    ),
    tabPanel("Combined Plot",
      selectizeInput("combinedStats", "Stats to plot:", choices=statChoices, multiple=TRUE),
      column(8, plotOutput("combinedPlot", height=600))
    ),
    tabPanel("Time Series",
      fluidRow(
        column(4,
          uiOutput("whichTS")
        )
      ),
      checkboxInput("tsLog", label="Log10 scale for Y-Axis", value=TRUE),
      fluidRow(
        column(12, 
          plotOutput("timeSeries", width="100%", height="400px", 
                     brush = brushOpts(id = "tsBrush", resetOnNew = TRUE, direction="x"))
        )
      ),
      checkboxInput("zpLog", label="Log10 scale for Y-Axis", value=TRUE),
      fluidRow(
        column(12, 
          plotOutput("zoomedPlot", width="100%", height="400px")
        )
      )
    ),
    tabPanel("Explanation and references",
      fluidRow(
        downloadButton("downloadExplanation", "Download PDF")
      )        
    )
  )
))
