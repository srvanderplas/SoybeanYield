library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
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
        theme(axis.text=element_blank(), axis.ticks=element_blank(), title=element_blank())
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
        do(data.frame(q25=floor_date(quantile(.$Date, .25, na.rm=T), "day"), 
                      q50=floor_date(quantile(.$Date, .5, na.rm=T), "day"),
                      q75=floor_date(quantile(.$Date, .75, na.rm=T), "day")))
        
         
      
      yield.sub <- filter(yield, MG%in%input$maturity & 
                            Location%in%input$location & 
                            PlantDay%in%input$planting)
      yield.sub$facet <- yield.sub[,input$compare]
      
      guidelines <- expand.grid(xintercept=seq(.5, 5.5, 1), facet=unique(yield.sub$facet))
      
      frost.date.df <- yield.sub %>% 
        group_by(Location, PlantDay, MG, facet) %>% 
        do(data.frame(frost.date=floor_date(quantile(.$Date.of.first.frost2, .5, na.rm=T), "day"), 
                      y=.5, 
                      label="First Frost Likely")) %>%
        as.data.frame()
      hour(frost.date.df$frost.date) <- sample(0:11, nrow(frost.date.df))
      
      frost.date.df$frost.date.lb <- floor_date(quantile(filter(yield, MG%in%input$maturity & 
                                                                  Location%in%input$location & 
                                                                  PlantDay%in%input$planting)$Date.of.first.frost2, 
                                                         .25, na.rm=T), "day")
      frost.date.df$frost.date.ub <- floor_date(quantile(filter(yield, MG%in%input$maturity & 
                                                                  Location%in%input$location & 
                                                                  PlantDay%in%input$planting)$Date.of.first.frost2, 
                                                         .75, na.rm=T), "day")
      frost.date.df$med.frost <- floor_date(quantile(filter(yield, MG%in%input$maturity & 
                                                              Location%in%input$location & 
                                                              PlantDay%in%input$planting)$Date.of.first.frost2, 
                                                     .5, na.rm=T), "day")
      frost.date.df$textlabel <- floor_date(median(frost.date.df$frost.date), "day")
      
      if(input$plottype=="1"){
        plot <- ggplot() + 
          geom_crossbar(aes(x=Stage, y=q50, ymin=q25, ymax=q75, fill=factor(facet), color=factor(facet), width=0.9), 
                        alpha=.3, position=position_dodge(), data=quantile.sub) 
      } else {
        plot <- ggplot() + 
          geom_violin(aes(x=Stage, y=Date, fill=factor(facet), color=factor(facet)), 
                      alpha=.3, data=longdata.sub, scale="width", adjust=2)
      }
      
      label_facet <- function(x, y){
        paste0(gsub("PlantDay", "Planting\nDate", input$compare), ": ", y)
      }
      
      if(input$facets){
        plot <- plot + facet_grid(.~facet, labeller=labeller(facet=label_facet))
      }
      
      plot <- plot + 
        geom_text(aes(x=Stage, y=ymax, ymax=ymax, label=text, color=factor(facet)), data=textdata, position=position_dodge(width=0.9), hjust=1) +
        coord_flip() + 
        scale_color_brewer(gsub("PlantDay", "Planting\nDate", input$compare), palette="Set1") + 
        scale_fill_brewer(gsub("PlantDay", "Planting\nDate", input$compare), palette="Set1") + 
        geom_rect(aes(ymin=frost.date.lb, ymax=frost.date.ub, xmin=-Inf, xmax=Inf), alpha=.05, fill="black", data=frost.date.df) +  
        geom_text(aes(y=textlabel, x=y, label=label), data=frost.date.df, hjust=1, vjust=0, size=6) + 
        xlab("") + ylab("") + 
        geom_vline(aes(xintercept=xintercept), data=guidelines) + 
        theme_bw() + 
        theme(plot.title=element_text(size=18), 
              axis.text = element_text(size = 16), 
              legend.title=element_text(size=16), 
              legend.text=element_text(size=14),
              panel.grid.major.x=element_line(color="grey40"), 
              panel.grid.minor.y=element_line(color="black")) +  
        ggtitle("Development Timeline of Soybeans")
      
      if(input$compare=="Location") {
        plot <- plot + 
          geom_segment(aes(y=frost.date, yend=frost.date, x=y+.25, xend=Inf, 
                           color=factor(facet)), data=frost.date.df, linetype=2)
      } else {
        plot <- plot + 
          geom_segment(aes(y=med.frost, yend=med.frost, x=y+.25, xend=Inf), data=frost.date.df[1,], linetype=2)
      }
    }
    print(plot)
  })

  
  output$YieldByMGPlot <- renderPlot({
    plotdata <- filter(yield, 
                       Location%in%input$location &
                       PlantDay%in%input$planting)
    plotdata$Location <- factor(plotdata$Location, levels=input$location, ordered=T)
    if(input$compare!="MG"){
      plotdata$facet <- plotdata[,input$compare]
    } else {
      plotdata$facet <- NA
    }
    
    if(!input$failed){
      plotdata <- filter(plotdata, Comment!="failure")
    }
    
    plotdata <- plotdata %>% group_by(facet) %>% mutate(nyield=Yield/max(Yield)) %>% as.data.frame
    plotdata$jitterMG <- jitter(plotdata$MG)
    
    spline.data <- plotdata %>% group_by(facet) %>% do({
      set.seed(9852996)
      bx3 <- cbind(I=1, ns(.$jitterMG, df=3)) 
      cubicspline3 <- lm(data=., nyield~bx3-1)
      tmp <- data.frame(MG=.$MG, jitterMG=.$jitterMG)
      tmp <- cbind(tmp, suppressWarnings(predict(cubicspline3, se.fit=T, interval="prediction", level=.95)))
      tmp
    })

    spline.max <- spline.data %>% group_by(facet) %>% do({
      .[which.max(.$fit.fit),]
    })
  
    if(nrow(spline.max)>1 & length(unique(spline.max$MG))<nrow(spline.max)){
      spline.max$MG <- spline.max$MG + seq(-.05, .05, length.out = nrow(spline.max))
    }    
    
    if(input$points){
      plot <- ggplot() + 
        geom_point(data=plotdata, aes(x=jitterMG, y=nyield), alpha=.25) 
    } else {
      plot <- ggplot()
    }
    if(input$plottype2=="2"){
      if(sum(is.na(plotdata$facet))>0){
        plot <- plot + 
          geom_line(data=spline.data, aes(x=jitterMG, y=fit.lwr), linetype=2) + 
          geom_line(data=spline.data, aes(x=jitterMG, y=fit.upr), linetype=2)  + 
          geom_line(data=spline.data, aes(x=jitterMG, y=fit.fit), size=2)
      } else {
        plot <- plot + 
          geom_line(data=spline.data, aes(x=jitterMG, y=fit.lwr, colour=factor(facet)), linetype=2) + 
          geom_line(data=spline.data, aes(x=jitterMG, y=fit.upr, colour=factor(facet)), linetype=2)  + 
          geom_line(data=spline.data, aes(x=jitterMG, y=fit.fit, colour=factor(facet)), size=2, alpha=1/sqrt(nrow(spline.max))) +
          scale_colour_brewer(gsub("PlantDay", "Planting\nDate", input$compare),palette="Set1") + 
          geom_segment(data=spline.max, aes(x=MG, y=fit.fit, xend=MG, yend=0, colour=factor(facet), ymax=fit.fit), linetype=3, size=2)
      }
    } else {
      if(sum(is.na(plotdata$facet))>0){
        plot <- plot + 
          geom_boxplot(data=plotdata, aes(x=MG, y=nyield, group=round_any(MG, 1)), fill=NA)
      } else {
        plot <- plot + 
          geom_boxplot(data=plotdata, aes(x=MG, y=nyield, colour=factor(facet), 
                                          group=interaction(factor(facet), round_any(MG, 1))), 
                       fill=NA, position=position_dodge()) + 
          scale_colour_brewer(gsub("PlantDay", "Planting\nDate", input$compare),palette="Set1")
        if(length(unique(plotdata$facet))>1)
          plot <- plot + geom_vline(aes(xintercept=-1:5+.5), colour="grey30")
      }
    }
    
    plot <- plot + 
        scale_y_continuous(breaks=c(0, .25, .5, .75, 1), name="Relative Yield", limits=c(0, 1.1)) + 
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
    plotdata$Location <- factor(plotdata$Location, levels=input$location, ordered=T)
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
      set.seed(9852996)
      bx5 <- cbind(I=1, ns(.$jitterDate, df=5)) 
      cubicspline5 <- lm(data=., nyield~bx5-1)
      tmp <- data.frame(jitterDate=.$jitterDate)
      tmp <- cbind(tmp, suppressWarnings(predict(cubicspline5, se.fit=T, interval="prediction", level=.95)))
      tmp
    })

    spline.max <- spline.data %>% group_by(facet) %>% do({
      .[which.max(.$fit.fit),]
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
        geom_line(data=spline.data, aes(x=jitterDate, y=fit.fit), size=2)
    } else {
      plot <- plot + 
        geom_line(data=spline.data, aes(x=jitterDate, y=fit.lwr, colour=factor(facet)), linetype=2) + 
        geom_line(data=spline.data, aes(x=jitterDate, y=fit.upr, colour=factor(facet)), linetype=2) + 
        geom_line(data=spline.data, aes(x=jitterDate, y=fit.fit, colour=factor(facet)), size=2, alpha=1/sqrt(nrow(spline.max))) + 
        scale_colour_brewer(gsub("PlantDay", "Planting\nDate", input$compare),palette="Set1") + 
        geom_segment(data=spline.max, aes(x=jitterDate, y=fit.fit, xend=jitterDate, yend=0, colour=factor(facet)), linetype=3, size=2)
    }
    
    plot <- plot + 
        scale_y_continuous(breaks=c(0, .25, .5, .75, 1), name="Relative Yield", limits=c(0, 1.1)) + 
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