library(ggplot2)
library(reshape2)
library(gdata)

g=read.csv("data/experiment_size_selectivity.csv")
g$Length<-g$Length*100
head(g)

############################################
#####Drawings
############################################

#histograms
ggplot(g,aes(x=Length))+
  geom_bar(aes(fill=Treatment))+
  facet_grid(Treatment~.)+
  theme_classic()

ggplot(data=g,aes(x=Length, y=Treatment,point_color=Treatment,color=Treatment,fill=Treatment)) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 3, size = 0.25,
    position = position_points_jitter(height = 0),
    aes(x = Length), 
    alpha = .8, color = "white")+
  theme_ridges(center=TRUE)+
  scale_fill_manual(values = c("gray", "red","blue"), labels = c("Control", "Grouper","Lionfish")) +
  scale_color_manual(values = c("gray", "red","blue"), guide = "none") +
  scale_discrete_manual("point_color", values = c("gray", "red","blue"), guide = "none") +
  xlab("Survivor length [Standard Length mm]")+
  ylab("Predator Treatment")
  # scale_x_continuous(trans=log10_trans())
  # scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))   

ggsave("figures/size_selectivity/ridgeline_preysize_postpredation.png")



 
############################################
#####Analysis
############################################
contrasts(g$Treatment)=cbind(c(-2,1,1),c(0,-1,1))
contrasts(g$Treatment)


tapply(g$Length,list(g$Treatment),mean)

m1<-lm(Length~Treatment,data=g)
summary(m1)

kruskal.test(g$Length~g$Treatment)

#just the little guys there's limited signal as well 
g2<-subset(g,Length<150)

m1<-lm(Length~Treatment,data=g2)
summary(m1)

kruskal.test(g2$Length~g2$Treatment)

tapply(g2$Length,list(g2$Treatment),mean)




###

library(zipcode)
library(tidyverse)
library(maps)
library(viridis)
library(ggthemes)
library(albersusa)#installed via github
#data
fm<-Export <- read_csv("~/Downloads/Export (1).csv")#the file we just downloaded
data(zipcode)
fm$zip<- clean.zipcodes(fm$zip)
#size by zip
fm.zip<-aggregate(data.frame(count=fm$FMID),list(zip=fm$zip,county=fm$County),length)
fm<- merge(fm.zip, zipcode, by='zip')

###

library(maptools)
library(RColorBrewer)
library(ggmap)
library(plyr)

area <- readShapePoly("canada.shp")

area.points <- fortify(area)


loc="Toronto"

mapImage <- get_map(location=loc,
                    color="color",
                    zoom=10,
                    maptype="roadmap")

extents <- geocode(loc, output="more")

area.zoom <- subset(area.points, 
                    extents$west  <= long & long <= extents$east &
                      extents$south <= lat  & lat  <= extents$north)

ggmap(mapImage) +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group,
                   fill = id),
               data = area.zoom,
               color = 'white',
               #fill = 'black',
               alpha = 0.4) +
  coord_map(projection="mercator")+
  labs(x = "Longitude",
       y = "Latitude")
