library(shiny)


# When updating the applet, be sure to run GenerateDataObjects.R if the data has changed. 
# GenerateDataObjects.R contains code to pre-process the data files - 
# this does not need to happen every time the applet is loaded, but should happen when the
# underlying data files change. 

# Data/uiStart.rda and Data/serverStart.rda depend on GenerateDataObjects

# Variables to populate the drop-down menus in the tool
load("Data/uiStart.rda")

# Intro.R creates HTML from the 4 text files in ./Data (text, titles, captions, and figures). 
# This code creates the text/HTML for the expandable panels on the "Introduction" page of the applet. 
# Specifically, creates list.of.panels 
source("Code/Intro.R")
# Once the text, figures, etc. are stable, this function call 
# could be moved into GenerateDataObjects.R
# So long as the object list.of.panels is saved in uiStart.rda

# About Shiny/Bootstrap integration: 
# Bootstrap (and Shiny) divide a page into 12 columns, 
# so that these columns can be rearranged for mobile devices.
# Every input belongs in a column so that it can be resized. 
# http://getbootstrap.com/css/
# in Shiny:
#    fluidRow() allows columns to be rearranged for mobile browsers.
#    fixedRow() would make a column that would not rearrange.

# Function to print footer in each tab
footer <- function(){
  img(src="Footer.jpg", width='100%', height='auto')
}

# Define header to be used across all tabs
header <- function(){
  
  # CSS files and other links which belong in the header
  
  tags$head(
    # CSS for bootstrap modifications: 
    #    Change the size of the title and Tab labels, 
    #    make input labels a bit larger.
    tags$link(href="addons.css", rel="stylesheet"),
    
    # Bootstrap CSS for animated panels, etc. 
    # Reference: http://getbootstrap.com/css/
    tags$link(href="bootstrap.min.css", rel="stylesheet")
  )
  
} # end header function definition


# I define the intro and tool tabs in functions
# because it's much easier to make the code modular
# when each tab is in a separate space and clearly delineated 

intro <- function(){
  tabPanel(
    "Introduction", # name
    
    # Intro page title
    div(
      align="center", 
      h1("Understanding interactions between soybean planting date and maturity across environments")
    ), 
    
    # Space
    br(),
    
    # Everything but the footer and title in this row!
    fluidRow(
      
      # Side panel/column with pictures and information about the project itself
      column(
        width=3, 
        
        # Cute baby soybean sprout
        img(src="PhotoIIISoybeanemergen.jpg", width='100%', height='auto'),
        
        br(), 
        br(),
        
        # Expandable set of tab panels with 
        # Disclaimer, Acknowledgements, Contact Info, and References
        div(
          class="panel-group", 
          id="accordion", 
          role="tablist", 
          "aria-multiselectable"="true", # Can multiple panels be expanded at once?
          
          # Output HTML of the last 4 tab panels
          HTML(paste0(list.of.panels[17:20], collapse="\n"))
        ),
        
        br(), 
        br(),
        
        # Fuzzy soybean in the field
        img(src="PhotoI.jpg", width='100%', height='auto'),
        
        br(), 
        br()
        
      ), # End sidebar column
      
      # This column contains the expandable panels. 
      column(
        width=9,
        div(
          class="panel-group", 
          id="accordion", 
          role="tablist", 
          "aria-multiselectable"="true", # Can multiple panels be expanded at once?
          
          # Output HTML of the first 16 tab panels, with matching figures, etc.
          HTML(paste0(list.of.panels[1:16], collapse="\n")) 
        )
      ) # End column with expandable panels
      
    ), # End content row
    
    # Footer image with ISU Extension and copyright info. 
    footer()
    
  ) # End tabPanel definition
  
} # End intro() function definition

tool <- function(){
  tabPanel(
    "Tool", 
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