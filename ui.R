library(shiny)
load("Data/uiStart.rda")

# Define UI for application that plots random distributions
shinyUI(fluidPage(
#   theme="http://bootswatch.com/simplex/bootstrap.css",
  tags$style(type="text/css",
             "label {font-size: 14px;}"
  ),
#   tags$head(tags$style(type="text/css", "label.radio { display: inline; width: 25%; margin-right:2%;}")), # format checkbox inputs in 3 columns with some padding.
  
  # Application title
  titlePanel("Soybean Phenology and Relative Yield"),
  
  wellPanel(
    fluidRow(
      column(4, offset=1, p("Select a variable to compare up to 3 different values for that variable. You may only select one variable at a time to compare in this way.")),
      column(3, offset=2, radioButtons("compare", label="Comparison Variable", 
                             choices=c("Location"="Location", "Planting Date"="PlantDay", "Maturity Group"="MG"), 
                             inline=TRUE))
    ), br(),
    fluidRow(
      column(4, offset=1, p("Click on the boxes below to see what options are available."),
                p("You can type or select options to add them (use backspace to remove a selected option)."),
                p("You may select up to three values for the variable chosen as the comparison variable.")),
      column(2,div(align="center", uiOutput("location"))),
      column(2,div(align="center", uiOutput("planting"))),
      column(2,div(align="center", uiOutput("maturity")))
    )),
  fluidRow(
    column(10, 
      plotOutput("DevelopmentPlot", height=400),
      div(align="center", 
          helpText(strong("Fig. 1"), "Development timeline of soybean cultivars.", br(), "About these plots:", a(href="https://en.wikipedia.org/wiki/Box_plot", "Box plot"), ", ", a(href="https://en.wikipedia.org/wiki/Violin_plot", "Violin plot"))
          )
      ),
    column(2, br(), br(), 
           wellPanel(
             h4("Plot Options"),
             radioButtons("plottype", "Plot Type", choices=c("Box Plot" = 1, "Violin Plot" = 2), selected=1), 
             br(), 
             checkboxInput("facets", "Show Facets", value=FALSE),
             checkboxInput("newdata", "Show 2014 measured data (if available)", value=FALSE))
           )
    ),
  fluidRow(
    column(5, 
           plotOutput("YieldByMGPlot", height=400),
           div(align="center", helpText(strong("Fig. 2"), "Relationship between relative yield and soybean cultivar for a specific planting date(s) and location(s). Vertical dotted line(s) indicates the appropriate maturity group for use in a specific location and/or planting date."))), 
    column(2, br(),br(),
           wellPanel(
             h4("Plot Options"),
             h5("Relative Yield by MG Plot:"),
             radioButtons("plottype2", "Type", choices=c("Fitted Line"=2, "Box Plot" = 1), selected=2),
             br(),
             h5("Both Plots:"),
             checkboxInput("points", label="Show Points"),
             checkboxInput("failed", label="Include Failed Trials?"),
             checkboxInput("newdata2", "Show 2014 measured data (if available)", value=FALSE),
             br())
           ),
    column(5,
           plotOutput("YieldByPlantingPlot", height=400),
           div(align="center", helpText(strong("Fig. 3"), "Relationship between relative yield and date of planting for a specific cultivar and location. Vertical dotted line(s) indicates the optimum planting date(s) for maximizing yield under a particular combination of cultivar and location.")))
    )
))