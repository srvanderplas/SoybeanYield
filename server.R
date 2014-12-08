library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)
library(lubridate)
library(stringr)
library(splines)

load("Data/serverStart.rda")

fix.na.data <- function(df){
  ret <- unique(df[,c("Location", "PlantDay", "MG", "Stage")])
  if(sum(!is.na(df$Date))>0){
    ret$text <- " " 
  } else {
    ret$text <- "Not Achieved"
  }
  return(ret)
}

# enlarge font for session 
theme_set(theme_bw(24))

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
    
    longdata.sub <- filter(longyield, MG%in%input$maturity & 
                             Location%in%input$location & 
                             PlantDay%in%input$planting) 
    longdata.sub$Location <- factor(longdata.sub$Location, levels=input$location, ordered = T)
    longdata.sub$facet <- longdata.sub[,input$compare]
    
    if(nrow(longdata.sub)==0){
      plot <- ggplot() + 
        geom_text(aes(x=0, y=0, label="Please input\na location,\nplanting date,\n and maturity group."), size=20) +
        xlab("") + ylab("") + theme_bw() + 
        theme(axis.text=element_blank(), axis.ticks=element_blank()), title=element_blank()
    } else {
      
      textdata <- longdata.sub%>%group_by(Location, PlantDay, MG, Stage) %>% do(fix.na.data(.))
      textdata <- merge(textdata, longdata.sub%>%group_by(Stage)%>%summarize(y=mean(Date, na.rm=T), ymax=max(Date, na.rm=T)))
      if(sum(is.na(textdata$text))>0){
        textdata$text[is.na(textdata$text)] <- " "
      }
      textdata$facet <- textdata[,input$compare]
      
      plant.dates <- ydm(paste0("2000-", input$planting))
      second(longdata.sub$Date) <- (longdata.sub$PlantDay%in%input$planting)*(longdata.sub$Stage=="Planting")*sample(1:2, nrow(longdata.sub), replace=T)
      
      if(length(input$planting)<2){
        plant.dates.df <- data.frame(x=plant.dates, y=.5, yend=1.5, facet=unique(longdata.sub$facet))
      } else {
        plant.dates.df <- data.frame(x=plant.dates, y=.5, yend=1.5, facet=unique(input$planting))
      }
      
      quantile.sub <- longdata.sub %>% 
        group_by(Location, PlantDay, MG, Stage, facet) %>% 
        summarize(q25=floor_date(quantile(Date, .25, na.rm=T), "day"),
                  q50=floor_date(quantile(Date, .5, na.rm=T), "day"), 
                  q75=floor_date(quantile(Date, .75, na.rm=T), "day"))
      
      frost.date.df <- filter(yield, MG%in%input$maturity & 
                                Location%in%input$location & 
                                PlantDay%in%input$planting) %>% 
        group_by(Location, PlantDay, MG) %>% 
        summarize(frost.date=floor_date(quantile(Date.of.first.frost2, .5, na.rm=T), "day"), 
                  y=.5, 
                  label="First Frost Likely") %>% as.data.frame()
      
      frost.date.df$frost.date.lb <- floor_date(quantile(filter(yield, MG%in%input$maturity & 
                                                                  Location%in%input$location & 
                                                                  PlantDay%in%input$planting)$Date.of.first.frost2, 
                                                         .25, na.rm=T), "day")
      frost.date.df$frost.date.ub <- floor_date(quantile(filter(yield, MG%in%input$maturity & 
                                                                  Location%in%input$location & 
                                                                  PlantDay%in%input$planting)$Date.of.first.frost2, 
                                                         .75, na.rm=T), "day")
      frost.date.df$textlabel <- median(frost.date.df$frost.date)
      frost.date.df$facet <- frost.date.df[,input$compare]
      
      plot <- ggplot() + 
        geom_crossbar(aes(x=Stage, y=q50, ymin=q25, ymax=q75, fill=factor(facet), color=factor(facet), width=0.9), 
                      alpha=.3, position=position_dodge(), data=quantile.sub) + 
        geom_text(aes(x=Stage, y=ymax, ymax=ymax, label=text, color=factor(facet)), data=textdata, position=position_dodge(width=0.9), hjust=1) +
        coord_flip() + 
        scale_color_brewer(gsub("PlantDay", "Planting\nDate", input$compare), palette="Set1") + 
        scale_fill_brewer(gsub("PlantDay", "Planting\nDate", input$compare), palette="Set1") + 
        geom_rect(aes(ymin=frost.date.lb, ymax=frost.date.ub, xmin=-Inf, xmax=Inf), alpha=.1, fill="black", data=frost.date.df) +  
        geom_segment(aes(y=frost.date, yend=frost.date, x=y+.25, xend=Inf, color=facet), data=frost.date.df, linetype=2) + 
        geom_text(aes(y=textlabel, x=y, label=label), data=frost.date.df, hjust=1, vjust=0) + 
        xlab("") + ylab("") + 
        geom_vline(aes(xintercept=seq(.5, 5.5, 1)), colour="grey30") +
        theme_bw() + 
        theme(plot.title=element_text(size=18), 
              axis.text = element_text(size = 16), 
              legend.title=element_text(size=16), 
              legend.text=element_text(size=14),
              panel.grid.major.x=element_line(color="grey40"), 
              panel.grid.minor.y=element_line(color="black"))
      
      if(input$compare=="Location") {
        plot <- plot + ggtitle(paste0("Development Timeline if Planted on ", input$planting, 
                                      " (MG=", input$maturity, ")"))
      } else if(input$compare=="MG"){
        plot <- plot + ggtitle(paste0("Development Timeline if Planted on ", input$planting, ")"))
      } else {
        plot <- plot + ggtitle(paste0("Development Timeline if Planted on ", input$planting, ")"))
      }
    }
    print(plot)
  })

  
  output$YieldByMGPlot <- renderPlot({
    plotdata <- filter(yield, 
                       Location%in%input$location &
                       PlantDay%in%input$planting)
    if(input$compare!="MG"){
      plotdata$facet <- plotdata[,input$compare]
    } else {
      plotdata$facet <- NA
    }
    
#     if(!input$failed){
      plotdata <- filter(plotdata, Comment!="failure")
#     }
    
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
        theme(plot.title = element_text(size = 18), 
              legend.title = element_text(size = 16), 
              legend.text = element_text(size = 14), 
              axis.text = element_text(size = 14), 
              axis.title = element_text(size = 16)) + 
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
    
#     if(!input$failed){
      plotdata <- filter(plotdata, Comment!="failure")
#     }
    
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
        theme(plot.title = element_text(size = 18), 
              legend.title = element_text(size = 16), 
              legend.text = element_text(size = 14), 
              axis.text = element_text(size = 14), 
              axis.title = element_text(size = 16)) + 
        ggtitle(paste0("Relative Yield by Planting Date"))
    
    print(plot)
  })
})