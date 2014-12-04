library(shiny)
load("Data/uiStart.rda")

# Define UI for application that plots random distributions
shinyUI(fluidPage(
  theme="http://bootswatch.com/simplex/bootstrap.min.css",
  tags$style(type="text/css",
             "label {font-size: 12px;}",
             ".recalculating {opacity: 1.0;}"
  ),
  
  # Application title
  titlePanel("Soybean Yield"),
  wellPanel(
    fluidRow(
      column(3,div(align="center", 
                   selectizeInput("location", label="Select location(s)", 
                                  choices=locations, selected="Ames", 
                                  multiple=TRUE, options=list(maxItems=3)))),
      column(3,div(align="center", 
                   selectizeInput("planting", label="Select planting date", 
                                  choices=planting.date, selected="5-Jun", 
                                  multiple=TRUE, options=list(maxItems=1)))),
      column(3,div(align="center", 
                   selectizeInput("maturity", label="Select maturity group", 
                                  choices=0:5, selected=0, multiple=FALSE))),
      column(3,div(align="center", 
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
))