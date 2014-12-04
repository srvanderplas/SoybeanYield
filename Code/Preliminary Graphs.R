yield <- read.csv("./Data/IowaAnalysis2.csv", stringsAsFactors=F)

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
