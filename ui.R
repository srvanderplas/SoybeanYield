library(shiny)
load("Data/uiStart.rda")

# Define UI for application that plots random distributions
shinyUI(fluidPage(
  theme="http://bootswatch.com/simplex/bootstrap.css",
  tags$style(type="text/css",
             "label {font-size: 14px;}"
  ),
#   tags$head(tags$style(type="text/css", "label.radio { display: inline; width: 25%; margin-right:2%;}")), # format checkbox inputs in 3 columns with some padding.
  
  # Application title
  titlePanel("Soybean Yield"),
  
  wellPanel(
    fluidRow(
      column(2, offset=1, radioButtons("compare", label="Comparison Variable", 
                                       choices=c("Location"="Location", "Planting Date"="PlantDay", "Maturity Group"="MG"))),
      column(2,div(align="center", 
                   selectizeInput("location", label="Select location(s)", 
                                  choices=locations, selected="Ames", 
                                  multiple=TRUE, options=list(maxItems=3)))),
      column(2,div(align="center", 
                   selectizeInput("planting", label="Select planting date", 
                                  choices=planting.date, selected="5-Jun", 
                                  multiple=TRUE, options=list(maxItems=3)))),
      column(2,div(align="center", 
                   selectizeInput("maturity", label="Select maturity group", 
                                  choices=0:5, selected=0, multiple=TRUE, options=list(maxItems=3)))),
      column(2,div(align="center", 
                   checkboxInput("failed", label="Include failed trials?")))
    )),
  fluidRow(
    column(6,
           # Output - side 1
           plotOutput("DevelopmentPlot", height=500),
           div(align="center", 
               helpText("This plot shows the temporal distribution of each maturity stage by maturity group and planting date. 
                           The boxes shown indicate the 25th, and 75th percentiles, with an additional line indicating the median."))
    ),
    column(6,
           # Output - side 2a
           plotOutput("YieldByMGPlot", height=250),
           div(align="center", helpText("This plot shows the decline in (normalized) yield with increasing maturity group, for a specified planting date.")),
           # Output - side 2b
           plotOutput("YieldByPlantingPlot", height=250), 
           div(align="center", helpText("This plot shows the decline in (normalized) yield with later planting dates, for a specified maturity group."))
    )
  )
    wellPanel(
      fluidRow(
        column(2, offset=1, h4("Testing Plot Options")),
        column(2,radioButtons("plottype", label="Phenology Plot type", 
                              choices=c("Box Plot" = 3, "Violin Plot" = 2, "Tile Density Plot (old style)" = 1))), 
        column(1,checkboxInput("facets", label="Show facets in Phenology Plot")),
        column(2,radioButtons("intervaltype", label="Interval type", choices=c("confidence", "prediction"), selected="prediction")),
        column(1,radioButtons("pvalue", label="Conf. Level", choices=c("90%"=.95, "95%"=.975, "99%"=.995), selected=.975)),
        column(1,checkboxInput("points", label="Show Points"))
      ))
))