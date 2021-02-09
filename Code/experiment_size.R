library(ggplot2)
library(reshape2)
library(gdata)
library(tidyverse)
library(ggridges)

#load data

g=read.csv("data/experiment_size_selectivity.csv")
g$Length<-g$Length*100
head(g)

#Summary of data

sizetab<-
  g%>%
  group_by(Treatment2,Tank_unique)%>%
  summarize(n=n(),mean(Length))

sizetab %>% gt()

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
    quantile_lines=TRUE,
    quantile_fun=function(x,...)mean(x),
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 3, size = 0.25,
    position = position_points_jitter(height = 0,width=0.5),
    aes(x = Length), 
    alpha = .8, color = "white")+
  theme_ridges(center=TRUE)+
  scale_fill_manual(values = c("#ABA950", "#6190AB","#AD262B"), labels = c("Control", "Grouper","Lionfish")) +
  scale_color_manual(values = c("#ABA950", "#6190AB","#AD262B"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#ABA950", "#6190AB","#AD262B"), guide = "none") +
  xlab("Survivor length [standard Length mm]")+
  ylab("Predator treatment")
  # scale_x_continuous(trans=log10_trans())
  # scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))   

ggsave("figures/size_selectivity/ridgeline_preysize_postpredation.pdf")



 
############################################
#####Analysis
############################################
g$Treatment<-as.factor(g$Treatment)
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

