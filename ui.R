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
      column(3, p("Select a variable to compare up to 3 different values for that variable. You may only select one variable at a time to compare in this way.")),
      column(6, p("Click on the boxes below to see what options are available. You can type or select options to add them, and use the backspace key to remove a selected option. You may select up to three values for the variable chosen as the comparison variable, and one value for the other two variables.")), 
      column(3, p("Check this box to show the data for the relative yield plots."))
      ),br(),
    fluidRow(
      column(3, 
             radioButtons("compare", label="Comparison Variable", 
                          choices=c("Location"="Location", "Planting Date"="PlantDay", "Maturity Group"="MG"), inline=TRUE)),
      column(2,div(align="center", 
                   selectizeInput("location", label="Select location(s)", 
                                  choices=locations, selected="Ames", 
                                  multiple=TRUE, options=list(maxItems=3)))),
      column(2,div(align="center", 
                   selectizeInput("planting", label="Select planting date(s)", 
                                  choices=planting.date, selected="15-May", 
                                  multiple=TRUE, options=list(maxItems=3)))),
      column(2,div(align="center", 
                   selectizeInput("maturity", label="Select maturity group(s)", 
                                  choices=0:5, selected=2, multiple=TRUE, options=list(maxItems=3)))), 
      column(2, offset=1, checkboxInput("points", label="Show Points"))
    )),
  fluidRow(
    plotOutput("DevelopmentPlot", height=400),
    div(align="center", 
        helpText("This plot shows the temporal distribution of each maturity stage by maturity group and planting date. 
                 The boxes shown indicate the 25th, and 75th percentiles, with an additional line indicating the median."))),
  fluidRow(
    column(6,
           plotOutput("YieldByMGPlot", height=300),
           div(align="center", helpText("This plot shows the decline in (relative) yield with increasing maturity group, for a specified planting date."))
           ),
    column(6,
           # Output - side 2b
           plotOutput("YieldByPlantingPlot", height=300), 
           div(align="center", helpText("This plot shows the decline in (relative) yield with later planting dates, for a specified maturity group."))
           )
    ),
    wellPanel(
      fluidRow(
        column(2, offset=1, h4("Testing Plot Options")),
        column(1,radioButtons("intervaltype", label="Interval type", choices=c("confidence", "prediction"), selected="prediction")),
        column(2, helpText("Confidence intervals show the variability around the average value, prediction intervals show the variability around a single predicted value.")),
        column(1,radioButtons("pvalue", label="Conf. Level", choices=c("90%"=.95, "95%"=.975, "99%"=.995), selected=.975)),
        column(2, helpText("The confidence level is a measure of the accuracy of the interval. If we have 95% confidence, then we expect that of 100 intervals we construct, 95 will contain the true mean or predicted value."))
      ))
))