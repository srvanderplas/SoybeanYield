library(shiny)


# When updating the applet, be sure to run GenerateDataObjects.R if the data has changed. 
# GenerateDataObjects.R contains code to pre-process the data files - 
# this does not need to happen every time the applet is loaded, but should happen when the
# underlying data files change. 

# Data/uiStart.rda and Data/serverStart.rda depend on GenerateDataObjects

# Variables to populate the drop-down menus in the tool
load("Data/uiStart.rda")

# Intro.R creates HTML from the 4 text files in ./Data (text, titles, captions, and figures). 
# This code creates the expandable panels on the "Introduction" page of the applet. 
source("Code/Intro.R")

tool <- function(){
  tabPanel(
    "Tool", 
    tags$style(type="text/css", "label {font-size: 14px;}"),
    tags$head(tags$link(href="addons.css", rel="stylesheet")),
    tags$head(tags$link(href="bootstrap.min.css", rel="stylesheet")),
    wellPanel(
      fluidRow(
        column(2, offset=1, 
               br(),
               p("Select a variable to compare up to 3 different values for that variable.")
              ),
        column(2,
               radioButtons("compare", label="Comparison Variable", 
                            choices=c("Location"="Location", 
                                      "Planting Date"="PlantDay", 
                                      "Maturity Group"="MG"), 
                            inline=F)
        ),
        column(6,
          fluidRow(
            p(align="center", "Click on the boxes below to see what options are available.")
          ),
          fluidRow(
            column(4, div(align="center", uiOutput("location"))),
            column(4, div(align="center", uiOutput("planting"))),
            column(4, div(align="center", uiOutput("maturity")))
          ),
          fluidRow(
            p(align="center", "You can type or select options to add them (use backspace to remove a selected option)."
            )
          )
        )
      )
    ), 
    fluidRow(
      column(10, 
             plotOutput("DevelopmentPlot", height=400),
             div(align="center", 
                 helpText(strong("Fig. 1"), "Development timeline of soybean cultivars. Outliers (if applicable) are shown as unfilled circles.", br(), "About these plots:", a(href="https://en.wikipedia.org/wiki/Box_plot", "Box plot"), ", ", a(href="https://en.wikipedia.org/wiki/Violin_plot", "Violin plot"))
             )
      ),
      column(2, br(), br(), 
             wellPanel(
               h4("Plot Options"),
               radioButtons("plottype", "Plot Type", choices=c("Box Plot" = 1, "Violin Plot" = 2), selected=1), 
               checkboxInput("facets", "Show sub-plots", value=FALSE)
             )
      )
    ),
    fluidRow(
      column(5, 
             plotOutput("YieldByMGPlot", height=400),
             div(align="center", helpText(strong("Fig. 2"), "Relationship between relative yield and soybean cultivar (maturity group) for a specific planting date(s) and location(s). For Location and Planting Date comparison variables, vertical dotted line(s) indicates the appropriate maturity group for use in a specific location and/or planting date."))), 
      column(2, br(),br(),
             wellPanel(
               strong("Relative Yield by MG Plot:"),
               radioButtons("plottype2", "Type", choices=c("Fitted Line"=2, "Box Plot" = 1), selected=2),
               br(),
               strong("Both Plots:"),
               checkboxInput("points", label="Show Points"),
               checkboxInput("ci", label="Show 95% Prediction Interval"),
               checkboxInput("failed", label="Include Failed Trials?"),
               checkboxInput("newdata2", "Show 2014 measured data (if available)", value=FALSE)
             )
      ),
      column(5,
             plotOutput("YieldByPlantingPlot", height=400),
             div(align="center", helpText(strong("Fig. 3"), "Relationship between relative yield and date of planting for a specific cultivar (maturity group) and location. For Location and Maturity Group comparison variables, vertical dotted line(s) indicates the optimum planting date(s) for maximizing yield under a particular combination of cultivar and location.")))
    ),
    img(src="Footer.jpg", width='100%', height='auto') 
  )
}




# Define UI for application that plots random distributions
shinyUI(
  navbarPage(
    title="Soybean Planting Decision Tool", 
    tabPanel("Introduction", 
             div(align="center", h1("Understanding interactions between soybean planting date and maturity across environments"), br()),
             fluidRow(
               column(width=3, 
                      img(src="PhotoIIISoybeanemergen.jpg", width='100%', height='auto'),
                      br(), br(),
                      div(class="panel-group", id="accordion", role="tablist", "aria-multiselectable"="true",
                          HTML(paste0(list.of.panels[17:20], collapse="\n"))
                      ),
                      br(), br(),
                      img(src="PhotoI.jpg", width='100%', height='auto'),
                      br(), br()
               ),
               column(width=9,
                      div(class="panel-group", id="accordion", role="tablist", "aria-multiselectable"="true",
                              HTML(paste0(list.of.panels[1:16], collapse="\n")) 
                      )
               )
             ),
             img(src="Footer.jpg", width='100%', height='auto') 
             ),
    tool(),
    inverse=TRUE
))