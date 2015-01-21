yield <- read.csv("./Data/IowaAnalysis3.csv", stringsAsFactors=F)

library(ggplot2)
library(reshape2)
library(dplyr)
library(lubridate)
library(stringr)
tmp <- subset(yield, Location=="Ames")
tmp$id <- 1:nrow(tmp)
tmp2 <- melt(tmp, id.vars=c(1,2,3,5,17:19), measure.vars=c(4,6:10), variable.name="Stage", value.name="Date")
tmp2$Date <- paste0(tmp2$Date, "-2000")
tmp2$Date <- dmy(tmp2$Date)
tmp2$Date.of.first.frost <- paste0(tmp2$Date.of.first.frost, "-2000")
tmp2$Date.of.first.frost <- dmy(tmp2$Date.of.first.frost)
tmp2$Stage <- factor(tmp2$Stage, levels=c("Planting", "VE", "R1", "R4", "R7", "R8"))

summary.tmp <- tmp2 %>% group_by(Stage, MG) %>% summarize(q25=quantile(Date, .25, na.rm=T), 
                                                          q50=quantile(Date, .5, na.rm=T), 
                                                          q75=quantile(Date, .75, na.rm=T))

qplot(data=subset(summary.tmp, MG==0), x=Stage, y=q50, ymin=q25, ymax=q75, geom="crossbar") + 
#   facet_grid(MG~., labeller=label_both) + 
  coord_flip() + ggtitle("Development by Maturity Group") + xlab("Time") 

ggplot(data=subset(tmp2, MG==5 & !is.na(Date))) + 
  stat_density(aes(x=Date, y=Stage, alpha=..scaled..), fill="darkgreen", geom="tile", position="identity") + 
  scale_alpha_identity() +  
  geom_segment(aes(x=q25, xend=q75, 
                   y=as.numeric(Stage)-.25, 
                   yend=as.numeric(Stage)-.25), 
               data=subset(summary.tmp, MG==5)) +
  geom_segment(aes(x=q25, xend=q75, 
                   y=as.numeric(Stage)+.25, 
                   yend=as.numeric(Stage)+.25), 
               data=subset(summary.tmp, MG==5)) +
  geom_segment(aes(x=q75, xend=q75, 
                   y=as.numeric(Stage)-.25, 
                   yend=as.numeric(Stage)+.25), 
               data=subset(summary.tmp, MG==5)) +
  geom_segment(aes(x=q50, xend=q50, 
                   y=as.numeric(Stage)-.25, 
                   yend=as.numeric(Stage)+.25), 
               data=subset(summary.tmp, MG==5)) +
  geom_segment(aes(x=q25, xend=q25, 
                   y=as.numeric(Stage)-.25, 
                   yend=as.numeric(Stage)+.25), 
               data=subset(summary.tmp, MG==5)) +
#   facet_grid(MG~., labeller=label_both) + 
  theme_bw() + theme(panel.grid.major.x=element_line(color="grey30")) + 
  ggtitle("Development by Maturity Group") + xlab("Time") 

yield$Planting2 <- paste0(yield$Planting, "-2000")
yield$Planting2 <- yday(dmy(yield$Planting2))-92
ggplot(data=yield, aes(x=MG, y=Yield, group=interaction(Location, Planting2))) + 
  geom_jitter(alpha=.1) + 
  geom_smooth(aes(colour=Planting2), method="loess", se=F) + 
  scale_colour_gradient2("Planting Date: \nDays After April 1", low="steelblue3", mid="springgreen4", midpoint=62, high="yellow") +
  facet_wrap(~Location) + 
  theme_bw() + ggtitle("Yield by Maturity Group and Planting Date")


yield$Planting2 <- paste0(yield$Planting, "-2000")
yield$Planting2 <- dmy(yield$Planting2)
ggplot(data=subset(yield, Location=="Ames" & MG==2 & Yield>0), aes(x=Planting2, y=Yield, group=Location)) + 
  geom_jitter(alpha=.4) + 
  geom_smooth(aes(colour=Location), method="loess") + 
#   facet_wrap(~MG) + 
  theme_bw() + ggtitle("Yield by Planting Date and Maturity Group")



library(splines)
bx5 <- cbind(I=1, ns(plotdata$jitterDate, df=5)) 
cubicspline5 <- lm(data=plotdata, nyield~bx5-1)


spline.data <- data.frame(jitterDate=plotdata$jitterDate)
bx5.pred <- data.frame(I=1, ns(spline.data$jitterDate, df=5))
spline.data <- cbind(spline.data, predict(cubicspline5, newdata=bx5.pred, se.fit=T, interval="prediction"))

ggplot() + 
  geom_jitter(data=plotdata, aes(x=jitterDate, y=nyield), alpha=.5) + 
  geom_ribbon(data=spline.data, aes(x=jitterDate, ymin=fit.lwr, ymax=fit.upr), alpha=.25) + 
  geom_line(data=spline.data, aes(x=jitterDate, y=fit.fit)) + 
  facet_wrap(~Location) + 
  theme_bw() + ggtitle(paste0("Normalized Yield by Planting Date (Maturity Group ", input$maturity2b, ")"))


plotdata <- filter(yield, 
                   Location%in%c("Sutherland", "Ames", "Crawford") &
                   MG%in%c(0, 1, 2, 3, 4, 5))

plotdata <- filter(plotdata, Comment!="failure")
plotdata$Location <- factor(plotdata$Location, levels=c("Sutherland", "Ames", "Crawford"), ordered=T)

if(input$compare!="PlantDay"){
  plotdata$facet <- plotdata[,input$compare]
} else {
  plotdata$facet <- NA
}

plotdata$nyield <- plotdata$Yield/max(plotdata$Yield)
plotdata$jitterDate <- yday(plotdata$Planting2)

spline.data <- plotdata %>% group_by(MG, Location) %>% do({
  set.seed(9852996)
  bx5 <- cbind(I=1, ns(.$jitterDate, df=5)) 
  cubicspline5 <- lm(data=., nyield~bx5-1)
  tmp <- data.frame(jitterDate=.$jitterDate)
  tmp <- cbind(tmp, suppressWarnings(predict(cubicspline5, se.fit=T, interval="prediction", level=.95)))
  tmp
})

spline.max <- spline.data %>% group_by(MG, Location) %>% do({
  .[which.max(.$fit.fit),]
})

plot <- ggplot(data=plotdata) + 
  facet_grid(MG~Location) + 
  geom_jitter(data=plotdata, aes(x=jitterDate, y=nyield), alpha=.25) + 
  geom_line(data=spline.data, aes(x=jitterDate, y=fit.lwr, colour=factor(MG)), linetype=2) + 
  geom_line(data=spline.data, aes(x=jitterDate, y=fit.upr, colour=factor(MG)), linetype=2) + 
  geom_line(data=spline.data, aes(x=jitterDate, y=fit.fit, colour=factor(MG)), size=2, alpha=1/3) + 
  scale_colour_brewer("MG",palette="Set1") + 
  geom_segment(data=spline.max, aes(x=jitterDate, y=fit.fit, xend=jitterDate, yend=0, colour=factor(MG)),
               linetype=3, size=2) + 
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


plotdata <- filter(yield, 
                   Location%in%c("Sutherland", "Ames", "Crawford") &
                   PlantDay%in%c("5-Apr", "15-Apr", "5-May", "15-May", "5-Jun", "15-Jun", "5-Jul", "15-Jul"))
plotdata$Location <- factor(plotdata$Location, levels=c("Sutherland", "Ames", "Crawford"), ordered=T)
plotdata <- filter(plotdata, Comment!="failure")
plotdata <- plotdata %>% group_by(Location, PlantDay) %>% mutate(nyield=Yield/max(Yield)) %>% as.data.frame
plotdata$jitterMG <- jitter(plotdata$MG, amount=.2)

spline.data <- plotdata %>% group_by(Location, PlantDay) %>% do({
  set.seed(9852996)
  bx3 <- cbind(I=1, ns(.$jitterMG, df=3)) 
  cubicspline3 <- lm(data=., nyield~bx3-1)
  tmp <- data.frame(MG=.$MG, jitterMG=.$jitterMG)
  tmp <- cbind(tmp, suppressWarnings(predict(cubicspline3, se.fit=T, interval="prediction", level=.95)))
  tmp
})

spline.max <- spline.data %>% group_by(Location, PlantDay) %>% do({
  .[which.max(.$fit.fit),]
})

if(nrow(spline.max)>1 & length(unique(spline.max$MG))<nrow(spline.max)){
  spline.max$MG <- spline.max$MG + seq(-.05, .05, length.out = nrow(spline.max))
}    

plotdata$facet <- factor(plotdata$PlantDay, levels=c("5-Apr", "15-Apr", "5-May", "15-May", "5-Jun", "15-Jun", "5-Jul", "15-Jul"), ordered=T)
spline.data$facet <- factor(spline.data$PlantDay, levels=c("5-Apr", "15-Apr", "5-May", "15-May", "5-Jun", "15-Jun", "5-Jul", "15-Jul"), ordered=T)
spline.max$facet <- factor(spline.max$PlantDay, levels=c("5-Apr", "15-Apr", "5-May", "15-May", "5-Jun", "15-Jun", "5-Jul", "15-Jul"), ordered=T)

plot <- ggplot(data=plotdata) + 
  facet_grid(PlantDay ~ Location)  + 
  geom_point(data=plotdata, aes(x=jitterMG, y=nyield), alpha=.25) + 
  geom_line(data=spline.data, aes(x=jitterMG, y=fit.lwr, colour=factor(facet)), linetype=2) + 
  geom_line(data=spline.data, aes(x=jitterMG, y=fit.upr, colour=factor(facet)), linetype=2)  + 
  geom_line(data=spline.data, aes(x=jitterMG, y=fit.fit, colour=factor(facet)), size=2, alpha=1/sqrt(nrow(spline.max))) +
  scale_colour_brewer("Planting\nDate", palette="Set1") + 
  geom_segment(data=spline.max, aes(x=MG, y=fit.fit, xend=MG, yend=0, colour=factor(facet), ymax=fit.fit), linetype=1, size=2) + 
  scale_y_continuous(breaks=c(0, .25, .5, .75, 1), name="Relative Yield", limits=c(0, 1.1)) + 
  scale_x_continuous(breaks=0:5, labels=0:5, name="Maturity Group") + 
  theme_bw() + 
  theme(plot.title = element_text(size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14), 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.position="bottom",
        legend.direction="horizontal") + 
  ggtitle(paste0("Relative Yield by Maturity Group"))