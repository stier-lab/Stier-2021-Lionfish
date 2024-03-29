library(tidyverse)
library(gdata)
library(gt)
library(lme4)
library(multcomp)


############################################
#####Mortality Plots
############################################

x=read.csv("data/js_as_expt1.csv")
#drop one NA row
x<-x[-16,]

# make table of sample sizes by treatment and trial

x %>%
  group_by(trial, ttt_2) %>%
  summarise(
    n = n()
  ) %>% 
  pivot_wider(
    names_from = trial,
    values_from = n,
    names_prefix = "Number of replicates: trial "
  )

#get rid of dead gobies
x$t0_alive<-10-x$dead
x$mort<-x$eaten/x$t0_alive

#mean survival in all ttt
tapply(x$mort,list(x$ttt_2),mean)
tapply(x$mort,list(x$ttt),mean)

ggplot(x, aes(x= factor(ttt), y=mort)) +
  stat_summary(aes(fill=ttt),colour="black",fun = mean, geom = "bar", position="dodge")+
  stat_summary(fun.data = mean_cl_normal, geom = "linerange")+
  theme_classic()+
  xlab("Treatment")+
  ylab("Proportion Mortality")+
  ggtitle("
#KW test:Chi-squared = 17.7279
#p-value = 0.0001414
, wilcox_lfvsgrouper:W = 81.5, p-value = 0.6593")

ggsave("figures/survival/Survivalplot_v1.pdf")


ggplot(x, aes(x= factor(ttt), y=mort)) +
  geom_jitter(width=0.1,height=0,aes(color=ttt,pch=ttt))+
  stat_summary(colour="black",fun = mean, geom = "point", size=3)+
  stat_summary(fun.data = mean_cl_normal, geom = "linerange")+
  theme_classic()+
  # theme(axis.text=element_text(size=12),
        # axis.title=element_text(size=14,face="bold"))+
  xlab("Predator treatment")+
  ylab("Proportion goby mortality")+
  scale_color_manual(values = c("#ABA950", "#6190AB","#AD262B"),labels = c("Control", "Grouper","Lionfish"))

ggsave("figures/survival/Survivalplot_v2.pdf")


ggplot(x, aes(x= factor(ttt_2), y=mort)) +
  geom_jitter(width=0.1,height=0,aes(color=ttt,pch=ttt))+
  stat_summary(colour="black",fun = mean, geom = "point", size=3)+
  stat_summary(fun.data = mean_cl_normal, geom = "linerange")+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Treatment")+
  ylab("Proportion Mortality")+
  scale_color_manual(values = c("#00AFBB", "#E7B800","gray"),labels = c("Control", "Grouper","Lionfish"))

ggsave("figures/survival/Survivalplot_scarus_v1.png")


# 
# ggdensity(x, x = "mort",
#           add = "mean", rug = TRUE,
#           color = "ttt", fill = "ttt",
#           palette = c("#00AFBB", "#E7B800","gray"))

# +
#   ggtitle("
# #KW test:Chi-squared = 17.7279
# #p-value = 0.0001414
# , wilcox_lfvsgrouper:W = 81.5, p-value = 0.6593")


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


ggplot(x,aes(x=ttt_2,y=mort,colour=ttt_2))+
  geom_jitter()+
  facet_wrap(~trial,ncol=1)
  ggsave("figures/survival/survival_by_trial.png")


tab_n<-
  x%>%
  group_by(ttt_2,trial)%>%
  summarize(n=n(),mean(mort))

tab_n_vis <- tab_n %>% gt()


# gtsave(tab_n_vis,"figures/survival/survival_by_trial.png")


#set up table of killed and initial 
stab<-data.frame("eaten"=x$eaten,"initial"=x$t0_alive,"trial"=x$trial,"ttt"=x$ttt)
stab$prop<-stab$eaten/stab$initial
stab$ttt<-as.factor(stab$ttt)

m<-glm(prop~ttt,data=stab,family="quasibinomial")
summary(m)
#Tukey's test post hoc
summary(glht(m, linfct=mcp(ttt="Tukey")))

m<-lm(asin(sqrt(prop))~ttt,data=stab)
summary(m)

#initial contrasts
contrasts(stab$ttt)
#modified contrasts to contrast preds to control and grouper to lionfish
contrasts(stab$ttt)  = cbind(c(-2,1,1),c(0,-1,1))

#linear model
m<-lm(asin(sqrt(prop))~ttt,data=stab)
summary(m)

m<-glm(prop~ttt,data=stab,family="quasibinomial")
summary(m)



ttt<-drop.levels(x$ttt)
survtab<-data.frame(x$eaten,x$t0_alive,x$ttt)
survtab$prop<-survtab$eaten/survtab$t0_alive



alive<-c(rbinom(100,10,prob=0),rbinom(100,10,prob=0.5),rbinom(100,10,prob=0.8))
t<-c(rep("A",100),rep("B",100),rep("C",100))
df<-data.frame(alive,t)
names(df)<-c("alive","t")
df$prop<-df$alive/10   
df$t<-as.factor(t)

m<-glm(prop~t,data=df)
summary(m)

contrasts(df$t)<-cbind(c(-2,1,1),c(0,-1,1))
m<-glm(prop~t,data=df)

summary(m)


#set orthogonal contrasts, treatment versus control and btw preds
# contrasts(stab$ttt)=cbind(c(-2,1,1),c(0,-1,1))
contrasts(stab$ttt)  = cbind(c(-2,1,1),c(0,-1,1))

m1<-glm(survtab~ttt,family=binomial)
summary(m1)

ttt<-as.factor(ttt)
contrasts(ttt) = cbind(c(-2,1,1),c(0,-1,1))

m1<-glm(survtab~ttt,family=binomial)
summary(m1)

#trying to fit with trial as random effect 
#relvant thread with bmb here https://stackoverflow.com/questions/31013260/post-hoc-test-for-glmer

m2<-glmer(survtab~ttt+(1|trial),family="binomial",data=x)
summary(m2)

#double check that 

kruskal.test(x$mort~x$ttt)


#Chi-squared = 17.7279
#p-value = 0.0001414

xl<-subset(x,ttt=="Lionfish")
xg<-subset(x,ttt=="Grouper")
xc<-subset(x,ttt=="Control")
xs<-subset(x,ttt=="Scarus")

wilcox.test(xg$mort,xl$mort)
wilcox.test(xg$mort,xc$mort)
wilcox.test(xc$mort,xl$mort)









