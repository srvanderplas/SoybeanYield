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
          helpText("This plot shows the temporal distribution of each maturity stage by maturity group and planting date. 
                    The boxes shown indicate the 25th, and 75th percentiles, with an additional line indicating the median.")
          )
      ),
    column(2, br(), br(), 
           wellPanel(
             h4("Phenology Plot Options"),
             radioButtons("plottype", "Plot Type", choices=c("Box Plot" = 1, "Violin Plot" = 2), selected=1), 
             br(), 
             checkboxInput("facets", "Show Facets", value=FALSE))
           )
    ),
  fluidRow(
    column(4, offset=1, 
           plotOutput("YieldByMGPlot", height=300)), 
    column(2, br(),br(),
           wellPanel(h4("Plot Options"),
                     h5("Relative Yield by MG Plot:"),
                     radioButtons("plottype2", "Type", choices=c("Fitted Line"=2, "Box Plot" = 1), selected=2),
                     br(),
                     h5("Both Plots:"),
                     checkboxInput("points", label="Show Points"),
                     checkboxInput("failed", label="Include Failed Trials?"))),
    column(4,
           plotOutput("YieldByPlantingPlot", height=300))
    ),
  fluidRow(
    column(6,
           div(align="center", helpText("This plot shows the decline in (relative) yield with increasing maturity group, for a specified planting date."))
           ),
    column(6,
           div(align="center", helpText("This plot shows the decline in (relative) yield with later planting dates, for a specified maturity group."))
           )
    )
))