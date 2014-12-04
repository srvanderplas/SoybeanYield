library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)
library(lubridate)
library(stringr)
library(splines)

load("Data/serverStart.rda")

shinyServer(function(input, output, session) {
  
  # Plot of development progress
  output$DevelopmentPlot <- renderPlot({
    
    longdata.sub <- filter(longyield, MG==input$maturity & 
                             Location == input$location & 
                             PlantDay%in%input$planting)
    
    plant.dates <- ydm(paste0("2000-", input$planting))
    second(longdata.sub$Date) <- (longdata.sub$Date%in%plant.dates)*sample(1:60, nrow(longdata.sub), replace=T)
    
    plot <- ggplot() + 
      stat_density(aes(x=Date, y=Stage, alpha=..scaled..), 
                   data=longdata.sub, fill="green4", geom="tile", position="identity") + 
      scale_alpha_continuous(range=c(0,.9), guide="none") 
    
    if(length(input$planting)<2){

      plant.dates.df <- data.frame(x=plant.dates, y=.5, yend=1.5)
      
      plot <- plot + 
        geom_segment(aes(x=x, xend=x, y=y, yend=yend), data=plant.dates.df, colour="darkgreen")
    }
    
    quantile.sub <- longdata.sub %>% 
      group_by(Location, PlantDay, MG, Stage) %>% 
      summarize(q25=quantile(Date, .25),
                q50=quantile(Date, .5), 
                q75=quantile(Date, .75)) %>%
      filter(Stage != "Planting")
    
    frost.date.df <- data.frame(frost.date = 
                                  mean(filter(yield, Location == input$location & 
                                                PlantDay %in% input$planting & 
                                                MG==input$maturity)$Date.of.first.frost2), 
                                y=.5, label="Avg. Frost Date")
    
    plot <- plot +
      geom_segment(aes(x=q25, xend=q75, 
                       y=as.numeric(Stage)-.25, 
                       yend=as.numeric(Stage)-.25), 
                   data=quantile.sub) +
      geom_segment(aes(x=q25, xend=q75, 
                       y=as.numeric(Stage)+.25, 
                       yend=as.numeric(Stage)+.25), 
                   data=quantile.sub) +
      geom_segment(aes(x=q75, xend=q75, 
                       y=as.numeric(Stage)-.25, 
                       yend=as.numeric(Stage)+.25), 
                   data=quantile.sub) +
      geom_segment(aes(x=q50, xend=q50, 
                       y=as.numeric(Stage)-.25, 
                       yend=as.numeric(Stage)+.25), 
                   data=quantile.sub) +
      geom_segment(aes(x=q25, xend=q25, 
                       y=as.numeric(Stage)-.25, 
                       yend=as.numeric(Stage)+.25), 
                   data=quantile.sub) + 
      geom_segment(aes(x=frost.date, xend=frost.date, y=y+.25, yend=y+5), data=frost.date.df, linetype=2) + 
      geom_text(aes(x=frost.date, y=y, label=label), data=frost.date.df, hjust=1, vjust=0) + 
      xlab("") +
#       scale_x_datetime(limits=ymd("2000-04-01", "2000-10-15"))+
      theme_bw() + theme(panel.grid.major.x=element_line(color="grey40")) + 
      ggtitle(paste0("Development Timeline if Planted on ", input$planting, 
                     " (MG=", input$maturity, ")")) + 
      facet_grid(Location~.)
    
    print(plot)
  })

  
  output$YieldByMGPlot <- renderPlot({
    plotdata <- filter(yield, 
                       Location%in%input$location &
                         PlantDay%in%input$planting)
    if(!input$failed){
      plotdata <- filter(plotdata, Comment!="failure")
    }
    
    
    plotdata$nyield <- plotdata$Yield/max(plotdata$Yield)
    plotdata$jitterMG <- jitter(plotdata$MG)
    bx3 <- cbind(I=1, ns(plotdata$jitterMG, df=3)) 
    cubicspline3 <- lm(data=plotdata, nyield~bx3-1)
    spline.data <- data.frame(jitterMG=plotdata$jitterMG)
    spline.data <- cbind(spline.data, predict(cubicspline3, se.fit=T, interval="prediction"))
    spline.data$fit.lwr <- pmax(spline.data$fit.lwr, 0)
    spline.data$fit.upr <- pmin(spline.data$fit.upr, 1)
    
    
    plot <- 
      ggplot() + 
        geom_ribbon(data=spline.data, aes(x=jitterMG, ymin=fit.lwr, ymax=fit.upr), alpha=.25, fill="green4") + 
        geom_point(aes(x=jitterMG, y=nyield), data=plotdata) + 
        geom_line(data=spline.data, aes(x=jitterMG, y=fit.fit)) + 
        facet_wrap(~Location) + 
        ylab("Normalized Yield") + 
        ylim(c(0,1)) + 
        xlab("Maturity Group") + 
        theme_bw() + ggtitle(paste0("Normalized Yield by Maturity Group (Planted on ", input$planting, ")"))
    
    print(plot)
  })
  
  output$YieldByPlantingPlot <- renderPlot({
    plotdata <- filter(yield, 
                       Location%in%input$location &
                         MG%in%input$maturity)
    if(!input$failed){
      plotdata <- filter(plotdata, Comment!="failure")
    }
    
    plotdata$nyield <- plotdata$Yield/max(plotdata$Yield)
    plotdata$jitterDate <- yday(plotdata$Planting2)
    
    bx5 <- cbind(I=1, ns(plotdata$jitterDate, df=5)) 
    cubicspline5 <- lm(data=plotdata, nyield~bx5-1)
    spline.data <- data.frame(jitterDate=plotdata$jitterDate)
    spline.data <- cbind(spline.data, predict(cubicspline5, se.fit=T, interval="prediction"))
    spline.data$fit.lwr <- pmax(spline.data$fit.lwr, 0)
    spline.data$fit.upr <- pmin(spline.data$fit.upr, 1)
    
    plot <- 
      ggplot() + 
        geom_ribbon(data=spline.data, aes(x=jitterDate, ymin=fit.lwr, ymax=fit.upr), alpha=.25, fill="green4") + 
        geom_jitter(data=plotdata, aes(x=jitterDate, y=nyield), alpha=.5) + 
        geom_line(data=spline.data, aes(x=jitterDate, y=fit.fit)) + 
        facet_wrap(~Location) + 
        ylab("Normalized Yield") + 
        ylim(c(0,1)) + 
        scale_x_continuous("", breaks=c(92, 122, 153, 183, 214, 245), labels=c("Apr", "May", "Jun", "Jul", "Aug", "Sept")) + 
        theme_bw() + ggtitle(paste0("Normalized Yield by Planting Date (Maturity Group ", input$maturity, ")"))
    
    print(plot)
  })
})