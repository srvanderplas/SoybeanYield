library(shiny)
load("Data/uiStart.rda")
source("Code/Intro.R")

tool <- function(){
  tabPanel(
    "Tool", 
    tags$style(type="text/css", "label {font-size: 14px;}"),
    wellPanel(
      fluidRow(
        column(4, 
               br(),
               p("Select a variable to compare up to 3 different values for that variable. You may only select one variable at a time to compare in this way."),
               br(),
               p("Click on the boxes below to see what options are available."),
               p("You can type or select options to add them (use backspace to remove a selected option)."),
               p("You may select up to three values for the variable chosen as the comparison variable.")),
        
        column(2, radioButtons("compare", label="Comparison Variable", 
                               choices=c("Location"="Location", "Planting Date"="PlantDay", "Maturity Group"="MG"), 
                               inline=F)),
        column(2, 
               div(align="center", uiOutput("location")),
               div(align="center", uiOutput("planting")),
               div(align="center", uiOutput("maturity"))),
        column(4, div(align="center", helpText("Logo(s) go here")))
      )
    ), 
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
               checkboxInput("facets", "Show sub-plots", value=FALSE)
             )
      )
    ),
    fluidRow(
      column(5, 
             plotOutput("YieldByMGPlot", height=400),
             div(align="center", helpText(strong("Fig. 2"), "Relationship between relative yield and soybean cultivar for a specific planting date(s) and location(s). Vertical dotted line(s) indicates the appropriate maturity group for use in a specific location and/or planting date."))), 
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
             div(align="center", helpText(strong("Fig. 3"), "Relationship between relative yield and date of planting for a specific cultivar and location. Vertical dotted line(s) indicates the optimum planting date(s) for maximizing yield under a particular combination of cultivar and location.")))
    )
  )
}




# Define UI for application that plots random distributions
shinyUI(
  navbarPage(
    title="Soybean Planting Decision Tool",
    tabPanel("Introduction", 
             column(width=3, 
                    img(src="PhotoIIISoybeanemergen.png", width='100%', height='auto'),
                    br(),
                    br(),
                    img(src="PhotoI.png", width='100%', height='auto')),
             column(width=9,
                    div(class="panel-group", id="accordion", role="tablist", "aria-multiselectable"="true",
                        HTML(paste0(list.of.panels, collapse="\n"))
                        )
                    )
             ),
    tool(),
    inverse=TRUE
))