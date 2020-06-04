library(tidyverse)
library(gdata)
library(maps)


############################################
#####Mortality Plots
############################################

x=read.csv("data/js_as_expt1.csv")
#drop one NA row
x<-x[-16,]

#get rid of dead gobies
x$t0_alive<-10-x$dead
x$mort<-x$eaten/x$t0_alive

#mean survival in all ttt
tapply(x$mort,list(x$ttt_2),mean)
tapply(x$mort,list(x$ttt),mean)

ggplot(x, aes(x= factor(ttt), y=mort)) +
  stat_summary(aes(fill=ttt),colour="black",fun.y = mean, geom = "bar", position="dodge")+
  stat_summary(fun.data = mean_cl_normal, geom = "linerange")+
  theme_classic()+
  xlab("Treatment")+
  ylab("Proportion Mortality")+
  ggtitle("
#KW test:Chi-squared = 17.7279
#p-value = 0.0001414
, wilcox_lfvsgrouper:W = 81.5, p-value = 0.6593")

ggsave("Survivalplot_v1.pdf")


############################################
#####Analysis
############################################

#whacky distribution and heteroschedas
ggplot(x,aes(x=eaten,group=ttt))+
  geom_bar(aes(fill=ttt))+
  facet_wrap(~ttt)+
  theme_classic()+
  xlab("Number of Gobies Eaten")+
  ylab("Frequency")

# ggsave("frequency of gobies eaten.pdf")

hist(asin(sqrt(x$mort)))
boxplot(asin(sqrt(x$mort))~ttt)

#set up table of killed and initial 
stab<-data.frame("eaten"=x$eaten,"survived"=x$t0_alive,"trial"=x$trial,"ttt"=x$ttt)
ttt<-drop.levels(x$ttt)
survtab<-cbind(x$eaten,x$t0_alive)


#set orthogonal contrasts, treatment versus control and btw preds
contrasts(stab$ttt)=cbind(c(-2,1,1),c(0,-1,1))
contrasts(stab$ttt)

m1<-glm(survtab~ttt,family=binomial)
summary(m1)

#trying to fit with trial as random effect
m2<-glmer(survtab~ttt+(1|trial),family="binomial",data=x)
summary(m2)


kruskal.test(x$mort~x$ttt)




#Chi-squared = 17.7279
#p-value = 0.0001414

xl<-subset(x,ttt=="Lionfish")
xg<-subset(x,ttt=="Grouper")
wilcox.test(xg$mort,xl$mort)






