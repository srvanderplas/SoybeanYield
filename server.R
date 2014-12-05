library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)
library(lubridate)
library(stringr)
library(splines)

load("Data/serverStart.rda")

shinyServer(function(input, output, session) {
  
  observe({
    if(input$compare=="Location"){
      updateSelectizeInput(session, "location", options=list(maxItems=3))
    } else {
      updateSelectizeInput(session, "location", 
                           selected=input$location[1], 
                           options=list(maxItems=1))
    }  
    
    if(input$compare=="PlantDay"){
      updateSelectizeInput(session, "planting", options=list(maxItems=3))
    } else {
      updateSelectizeInput(session, "planting", 
                           selected=input$planting[1], 
                           options=list(maxItems=1))
    }  
    
    if(input$compare=="MG"){
      updateSelectizeInput(session, "maturity", options=list(maxItems=3))
    } else {
      updateSelectizeInput(session, "maturity", 
                           selected=input$maturity[1], 
                           options=list(maxItems=1))
    }  
    
  })
  
  # Plot of development progress
  output$DevelopmentPlot <- renderPlot({
    
    longdata.sub <- filter(longyield, MG==input$maturity & 
                             Location == input$location & 
                             PlantDay%in%input$planting) 
    longdata.sub$facet <- longdata.sub[,input$compare]
    
    plant.dates <- ydm(paste0("2000-", input$planting))
    second(longdata.sub$Date) <- (longdata.sub$Date%in%plant.dates)*sample(1:2, nrow(longdata.sub), replace=T)
    
    if(length(input$planting)<2){
      plant.dates.df <- data.frame(x=plant.dates, y=.5, yend=1.5, facet=unique(longdata.sub$facet))
    } else {
      plant.dates.df <- data.frame(x=plant.dates, y=.5, yend=1.5, facet=unique(input$planting))
    }
    
    quantile.sub <- longdata.sub %>% 
      group_by(Location, PlantDay, MG, Stage, facet) %>% 
      summarize(q25=quantile(Date, .25),
                q50=quantile(Date, .5), 
                q75=quantile(Date, .75))
    
    frost.date.df <- data.frame(frost.date.lb = 
                                  quantile(filter(yield, Location == input$location & 
                                                PlantDay %in% input$planting & 
                                                MG==input$maturity)$Date.of.first.frost2, .25), 
                                frost.date.ub = 
                                  quantile(filter(yield, Location == input$location & 
                                                    PlantDay %in% input$planting & 
                                                    MG==input$maturity)$Date.of.first.frost2, .75),
                                frost.date = 
                                  quantile(filter(yield, Location == input$location & 
                                                    PlantDay %in% input$planting & 
                                                    MG==input$maturity)$Date.of.first.frost2, .5), 
                                y=.5, label="First Frost Likely")
    plot1 <- ggplot() + 
      geom_violin(aes(x=Stage, y=Date, fill=factor(facet), color=factor(facet)), 
                  alpha=.3, data=longdata.sub, scale="width", adjust=2) +
      scale_color_brewer(gsub("PlantDay", "Planting\nDate", input$compare), palette="Set1") + coord_flip() + 
      scale_fill_brewer(gsub("PlantDay", "Planting\nDate", input$compare), palette="Set1") + coord_flip() + 
      geom_rect(aes(ymin=frost.date.lb, ymax=frost.date.ub, xmin=-Inf, xmax=Inf), alpha=.1, fill="black", data=frost.date.df) +  
      geom_segment(aes(y=frost.date, yend=frost.date, x=y+.25, xend=Inf), data=frost.date.df, linetype=2) + 
      geom_text(aes(y=frost.date, x=y, label=label), data=frost.date.df, hjust=1, vjust=0) + 
      xlab("") + ylab("") + 
      theme_bw() + theme(panel.grid.major.x=element_line(color="grey40")) + 
      ggtitle(paste0("Development Timeline if Planted on ", input$planting, 
                     " (MG=", input$maturity, ")"))
    if(input$facets){
      plot1 <- plot1 + facet_wrap(~facet)
    }

    plot2 <- ggplot() + 
      geom_crossbar(aes(x=Stage, y=q50, ymin=q25, ymax=q75, fill=factor(facet), color=factor(facet)), 
                    alpha=.3, position="dodge", data=quantile.sub) +
      coord_flip() + 
      scale_color_brewer(gsub("PlantDay", "Planting\nDate", input$compare), palette="Set1") + 
      scale_fill_brewer(gsub("PlantDay", "Planting\nDate", input$compare), palette="Set1") + 
      geom_rect(aes(ymin=frost.date.lb, ymax=frost.date.ub, xmin=-Inf, xmax=Inf), alpha=.1, fill="black", data=frost.date.df) +  
      geom_segment(aes(y=frost.date, yend=frost.date, x=y+.25, xend=Inf), data=frost.date.df, linetype=2) + 
      geom_text(aes(y=frost.date, x=y, label=label), data=frost.date.df, hjust=1, vjust=0) + 
      xlab("") + ylab("") + 
      theme_bw() + theme(panel.grid.major.x=element_line(color="grey40")) + 
      ggtitle(paste0("Development Timeline if Planted on ", input$planting, 
                     " (MG=", input$maturity, ")"))
    if(input$facets){
      plot2 <- plot2 + facet_wrap(~facet)
    }
    
    quantile.sub <-  quantile.sub %>% filter(Stage != "Planting")
    plot <- ggplot() + 
      stat_density(aes(x=Date, y=Stage, alpha=..scaled..), 
                   data=longdata.sub, geom="tile", fill="green4", position="identity") + 
      scale_alpha_continuous(range=c(0,.9), guide="none") + 
      geom_segment(aes(x=x, xend=x, y=y, yend=yend), data=plant.dates.df, colour="darkgreen") + 
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
      geom_rect(aes(xmin=frost.date.lb, xmax=frost.date.ub, ymin=-Inf, ymax=Inf), alpha=.1, fill="black", data=frost.date.df) +  
      geom_segment(aes(x=frost.date, xend=frost.date, y=y+.25, yend=Inf), data=frost.date.df, linetype=2) + 
      geom_text(aes(x=frost.date, y=y, label=label), data=frost.date.df, hjust=.5, vjust=0) + 
      xlab("") + ylab("") + 
      #       scale_x_datetime(limits=ymd("2000-04-01", "2000-10-15"))+
      theme_bw() + theme(panel.grid.major.x=element_line(color="grey40")) + 
      ggtitle(paste0("Development Timeline if Planted on ", input$planting, 
                     " (MG=", input$maturity, ")")) + 
      facet_grid(facet~.)
    
    

    plots <- list(plot, plot1, plot2)
    print(plots[[as.numeric(input$plottype)]])

  
  output$YieldByMGPlot <- renderPlot({
    plotdata <- filter(yield, 
                       Location%in%input$location &
                       PlantDay%in%input$planting)
    if(input$compare!="MG"){
      plotdata$facet <- plotdata[,input$compare]
    } else {
      plotdata$facet <- NA
    }
    
    if(!input$failed){
      plotdata <- filter(plotdata, Comment!="failure")
    }
    
    plotdata <- plotdata %>% group_by(facet) %>% mutate(nyield=Yield/max(Yield))
    plotdata$jitterMG <- jitter(plotdata$MG)
    
    spline.data <- plotdata %>% group_by(facet) %>% do({
      bx3 <- cbind(I=1, ns(.$jitterMG, df=3)) 
      cubicspline3 <- lm(data=., nyield~bx3-1)
      tmp <- data.frame(jitterMG=.$jitterMG)
      tmp <- cbind(tmp, suppressWarnings(predict(cubicspline3, se.fit=T, interval=input$intervaltype, level=as.numeric(input$pvalue))))
      tmp
    })
    
    if(input$points){
      plot <- ggplot() + 
        geom_jitter(data=plotdata, aes(x=jitterMG, y=nyield), alpha=.25) 
    } else {
      plot <- ggplot()
    }
    
    if(sum(is.na(plotdata$facet))>0){
      plot <- plot + 
        geom_line(data=spline.data, aes(x=jitterMG, y=fit.lwr), linetype=2) + 
        geom_line(data=spline.data, aes(x=jitterMG, y=fit.upr), linetype=2)  + 
        geom_line(data=spline.data, aes(x=jitterMG, y=fit.fit))
    } else {
      plot <- plot + 
        geom_line(data=spline.data, aes(x=jitterMG, y=fit.lwr, colour=factor(facet)), linetype=2) + 
        geom_line(data=spline.data, aes(x=jitterMG, y=fit.upr, colour=factor(facet)), linetype=2)  + 
        geom_line(data=spline.data, aes(x=jitterMG, y=fit.fit, colour=factor(facet))) +
        scale_colour_brewer(gsub("PlantDay", "Planting\nDate", input$compare),palette="Set1")
    }
    plot <- plot + 
        scale_y_continuous(breaks=c(0, .25, .5, .75, 1), name="Relative Yield", limits=c(0, 1.05)) + 
        scale_x_continuous(breaks=0:5, labels=0:5, name="Maturity Group") + 
        theme_bw() + 
        ggtitle(paste0("Relative Yield by Maturity Group"))
    
    print(plot)
  })
  
  output$YieldByPlantingPlot <- renderPlot({
    plotdata <- filter(yield, 
                       Location%in%input$location &
                         MG%in%input$maturity)
    if(input$compare!="PlantDay"){
      plotdata$facet <- plotdata[,input$compare]
    } else {
      plotdata$facet <- NA
    }
    
    if(!input$failed){
      plotdata <- filter(plotdata, Comment!="failure")
    }
    
    plotdata$nyield <- plotdata$Yield/max(plotdata$Yield)
    plotdata$jitterDate <- yday(plotdata$Planting2)
    
    spline.data <- plotdata %>% group_by(facet) %>% do({
      bx5 <- cbind(I=1, ns(.$jitterDate, df=5)) 
      cubicspline5 <- lm(data=., nyield~bx5-1)
      tmp <- data.frame(jitterDate=.$jitterDate)
      tmp <- cbind(tmp, suppressWarnings(predict(cubicspline5, se.fit=T, interval=input$intervaltype, level=as.numeric(input$pvalue))))
      tmp
    })
    
    if(input$points){
      plot <- ggplot() + 
        geom_jitter(data=plotdata, aes(x=jitterDate, y=nyield), alpha=.25) 
    } else {
      plot <- ggplot()
    }
    
    if(sum(is.na(plotdata$facet))>0){
      plot <- plot + 
        geom_line(data=spline.data, aes(x=jitterDate, y=fit.lwr), linetype=2) + 
        geom_line(data=spline.data, aes(x=jitterDate, y=fit.upr), linetype=2) + 
        geom_line(data=spline.data, aes(x=jitterDate, y=fit.fit))
    } else {
      plot <- plot + 
        geom_line(data=spline.data, aes(x=jitterDate, y=fit.lwr, colour=factor(facet)), linetype=2) + 
        geom_line(data=spline.data, aes(x=jitterDate, y=fit.upr, colour=factor(facet)), linetype=2) + 
        geom_line(data=spline.data, aes(x=jitterDate, y=fit.fit, colour=factor(facet))) + 
        scale_colour_brewer(gsub("PlantDay", "Planting\nDate", input$compare),palette="Set1") 
    }
    plot <- plot + 
        scale_y_continuous(breaks=c(0, .25, .5, .75, 1), name="Relative Yield", limits=c(0, 1.05)) + 
        scale_x_continuous("", breaks=c(92, 122, 153, 183, 214, 245), 
                           labels=c("Apr", "May", "Jun", "Jul", "Aug", "Sept")) + 
        theme_bw() + 
        ggtitle(paste0("Relative Yield by Planting Date"))
    
    print(plot)
  })
})