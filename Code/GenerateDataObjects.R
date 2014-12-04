# code to generate data objects for the Shiny applet
library(ggplot2)
library(reshape2)
library(dplyr)
library(lubridate)
library(stringr)

yield <- read.csv("./Data/IowaAnalysis2.csv", stringsAsFactors=F)


locations <- unique(yield$Location)
planting.date <- unique(yield$Planting)
maturity <- unique(yield$MG)
save(locations, planting.date, maturity, file="Data/uiStart.rda")
