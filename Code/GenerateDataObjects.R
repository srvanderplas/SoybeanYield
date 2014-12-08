# code to generate data objects for the Shiny applet
library(ggplot2)
library(reshape2)
library(dplyr)
library(lubridate)
library(stringr)

yield <- read.csv("./Data/SoybeanYieldData.csv", stringsAsFactors=F)
yield$id <- 1:nrow(yield)
yield$PlantDay <- yield$Planting
yield$Planting2 <- dmy(paste0(yield$Planting, "-2000"))
yield$PlantDay <- factor(yield$PlantDay, levels=unique(yield$PlantDay)[order(ydm(paste0("2000-", unique(yield$PlantDay))))])
yield$Date.of.first.frost2 <- dmy(paste0(yield$Date.of.first.frost, "-2000"))

longyield <- melt(yield, id.vars=c(1,2,3,5,17:20), measure.vars=c(4,6:10), variable.name="Stage", value.name="Date")
longyield$Date <- paste0(longyield$Date, "-2000")
longyield$Date <- dmy(longyield$Date)
longyield$Stage <- factor(longyield$Stage, 
                          levels=c("Planting", "VE", "R1", "R4", "R7", "R8"), 
                          labels=c("Planting", "Emergence", "Flowering", 
                                   "Start Grain\nFilling", "Maturity", "R8"))
# Leave out R8 for now
longyield <- filter(longyield, Stage!="R8")
# Remove NAs
# longyield <- filter(longyield, !is.na(Date))

save(yield, longyield, file="Data/serverStart.rda")

# Get options for ui.R
locations <- unique(yield$Location)
planting.date <- unique(yield$Planting)
planting.date <- planting.date[order(ydm(paste0("2000-", planting.date)))]
maturity <- unique(yield$MG)
save(locations, planting.date, maturity, file="Data/uiStart.rda")
