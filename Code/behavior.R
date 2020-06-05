library(tidyverse)
library(ggplot2)
library(reshape2)
library(gdata)
library(lme4)
library(nlme)

#average number of gobies visible by treatment 
#average prop time spent in same quad as predator 
#average prop time predators spent in same quad as refuge

x=read.csv("data/behavior.csv")

############################################
#####Is a similar proportion fo the tank visible? 
############################################

df<-data.frame(tapply(x$prop.visible,list(x$cam_name,x$ttt),mean))
df$camera=rownames(df)
df <- pivot_longer(df,cols=c(1:4))
df<-subset(df,value!="NA")
names(df)<-c("camera","ttt","propvis")

ggplot(df, aes(x= factor(ttt), y=propvis)) +
  stat_summary(aes(fill=ttt),colour="black",fun = mean, geom = "bar", position="dodge")+
  stat_summary(fun.data = mean_cl_normal, geom = "linerange")+
  theme_classic()+
  xlab("Treatment")+
  ylab("Proportion Visible")+
  ggtitle("")

ggsave("figures/behavior/propvisible.png")


x2<-drop.levels(subset(x,ttt!="Control"))
x4 <- subset(x2, cam_name != "TS4 by Jo" & cam_name != "TS5" & cam_name != "Jasmine")
with(x4,table(cam_name,tank))

contrasts(x4$ttt)=cbind(c(-1,-1,2),c(-1,1,0))
contrasts(x4$ttt)

m5 <- lme(prop.visible~ttt,random=~1|cam_name/video_time,data=x4)
summary(m5)

############################################
#####Are a similar number visible by ttt? 
############################################


df<-tapply(x$num_vis,list(x$cam_name,x$ttt),mean)
df<-melt(df)
df<-subset(df,value!="NA")
names(df)<-c("camera","ttt","num_vis")


ggplot(df, aes(x= factor(ttt), y=num_vis)) +
  stat_summary(aes(fill=ttt),colour="black",fun = mean, geom = "bar", position="dodge")+
  stat_summary(fun.data = mean_cl_normal, geom = "linerange")+
  theme_classic()+
  xlab("Treatment")+
  ylab("Number Visible")+
  ggtitle("Hiding_averaged by treatment")

ggsave("figures/behavior/hiding.png")



############################################
#####Are a similar number visible by ttt? 
############################################


df<-tapply(x$num_vis,list(x$cam_name,x$ttt),mean)
df<-melt(df)
df<-subset(df,value!="NA")
names(df)<-c("camera","ttt","num_vis")

#Averageing across each camera(tank)

ggplot(df, aes(x= factor(ttt), y=num_vis)) +
  stat_summary(aes(fill=ttt),colour="black",fun = mean, geom = "bar", position="dodge")+
  stat_summary(fun.data = mean_cl_normal, geom = "linerange")+
  theme_classic()+
  xlab("Treatment")+
  ylab("Number Visible")+
  ggtitle("#Hiding")

ggsave("figures/behavior/visible.png")

#ignoring individual tank effects 
ggplot(x, aes(x= factor(ttt), y=num_vis)) +
  stat_summary(aes(fill=ttt),colour="black",fun = mean, geom = "bar", position="dodge")+
  stat_summary(fun.data = mean_cl_normal, geom = "linerange")+
  theme_classic()+
  xlab("Treatment")+
  ylab("Number Visible")+
  ggtitle("#Hiding_ignoring_replication")

ggsave("figures/behavior/visibile_ignoringtankpseudo.pdf")

#set orthogonal contrasts, treatment versus control and btw preds
x2<-drop.levels(subset(x,ttt!="Control"))
#x2[,c]

contrasts(x2$ttt)=cbind(c(-1,-1,2),c(-1,1,0))
contrasts(x2$ttt)


m1 <-lmer(num_vis~ttt+(1|tank),data=x2)
summary(m1)

m2 <-lme(num_vis~ttt,random=~1|cam_name,data=x2)

m3 <- lme(num_vis~ttt,random=~1|cam_name/video_time,data=x2)

with(x2,table(cam_name,tank))

x3 <- subset(x2, cam_name != "TS4 by Jo" & cam_name != "TS5" & cam_name != "Jasmine")
with(x3,table(cam_name,tank))

contrasts(x3$ttt)=cbind(c(-1,-1,2),c(-1,1,0))
contrasts(x3$ttt)

m4 <- lme(num_vis~ttt,random=~1|cam_name/video_time,data=x3)
summary(m4)
