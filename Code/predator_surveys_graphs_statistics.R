#######
## Load Libraries
#######

library(tidyverse) 
library(nlme)
library(gt)
library(lme4)
library(janitor)

# plot coral head focused predator survey data
countdat <- read.csv("data/2015 field survey fish counts.csv", header=TRUE)
names(countdat)

#subset out predatos and divide by coral head area in m2
pred_densities <- tibble(cbind("site"=countdat$site,countdat[,c(13:16)]/countdat$head.area.m2))

#pred_densities <- select(countdat,p.volitans,h.puella,h.nigricans,c.cruentatus)

dat.predator.comparison <- gather(pred_densities, key, value, -site)
names(dat.predator.comparison) <- c("site","species","density") 


#######
## Plot Speies by 95% CI
#######

ggplot(dat.predator.comparison,aes(x=species,y=density))+
geom_jitter(aes(pch=site))+
  xlab("Predator Species")+
  ylab("Predator Density (fish per m2)")+
  theme_classic()+
  theme(axis.text.x=element_text(face="italic")) +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

ggsave("figures/pred_survey/pred_survey_dotplot_roving native vs invasive predators.png")


ggplot(dat.predator.comparison,aes(x=species,y=density))+
  stat_summary(fun.data= mean_cl_normal,lty=2)+
  xlab("Predator Species")+
  ylab("Predator Density (fish per m2)")+
  theme_classic()+
  theme(axis.text.x=element_text(face="italic"))+
  theme(axis.text.x = element_text(angle = 45,hjust=1))

ggsave("figures/pred_survey/pred_survey_mean_CI_roving native vs invasive predators.png")


#######
## Plot Speies by 95% CI without zeros 
#######

pred_dat <-dat.predator.comparison %>%
  filter(density>0)


ggplot(pred_dat, aes(x = species,y=density))+
  geom_dotplot(binaxis = "y", stackdir = "center",position="dodge",fill="white") + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               geom="pointrange", color = "black")+
  xlab("Predator Species")+
  ylab("Predator Density (fish per m2)")+
  theme_classic()+
  theme(axis.text.x=element_text(face="italic"))+
  theme(axis.text.x = element_text(angle = 45,hjust=1)) 

ggsave("figures/pred_survey/pred_survey_mean_CI_nozero_all_sites_roving native vs invasive predators.png")




ggplot(pred_dat, aes(x = species,y=density,pch=site))+
geom_dotplot(binaxis = "y", stackdir = "center",position="dodge",fill="white") + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               geom="pointrange", color = "black")+
  xlab("Predator Species")+
  ylab("Predator Density (fish per m2)")+
  theme_classic()+
  theme(axis.text.x=element_text(face="italic"))+
  theme(axis.text.x = element_text(angle = 45,hjust=1)) 

ggsave("figures/pred_survey/pred_survey_mean_CI_nozero_roving native vs invasive predators.png")


	
#######
## Statistical analyis of predator incidence
#######

names(countdat)


psum<- countdat[,c(3,7,13:16)]

df1<-data.frame(psum[,c(1:2)], psum[,3],rowSums(psum[,4:6]))
names(df1)<-c("site","coralsize","lionfish","native")

#df2<-melt(df1,id.vars=c("site","coralsize")) melt deprecated 
df2 <-  gather(df1, key, value, -site, - coralsize)

m1<-glm(value~key,family="binomial",data=df2)
summary(m1)

m2<-glmer(value~key+(1|site),family="binomial",data=df2)
summary(m2)


#######
## Plot proportion of reefs where lionfish are observed
#######

df3<-data.frame(c("Native","Lionfish"),tapply(df2$value,list(df2$key),sum)/24
)
names(df3)<- c("Predator","Proportion")

ggplot(df3,aes(x=Predator,y=Proportion))+
geom_bar(stat="identity")+
theme_bw()

df4<-data.frame(
    "incidence"=aggregate(df2$value,by=list(Category=df2$site,df2$key),FUN=sum))

df4$reefs_observed = aggregate(df2$value,by=list(Category=df2$site,df2$key),FUN=length)$x
df4$prop_occupancy = (aggregate(df2$value,by=list(Category=df2$site,df2$key),FUN=sum)$x/
  aggregate(df2$value,by=list(Category=df2$site,df2$key),FUN=length)$x)

names(df4) <-c("site","type","observed","counted","fraction")


ggplot(df4,aes(x=type,y=fraction))+
  geom_bar(stat="identity",aes(fill=type))+
  theme_classic()+
  facet_wrap(~site)+
  xlab("Predator Group")+
  scale_fill_manual(values=c("gray","#8FBC8F"))

ggsave("figures/pred_survey/pred_survey_incidence_ native vs invasive predators.png")

### by site
ggplot(dat.predator.comparison, aes(x=species,y=log(density+1))) + 
  geom_boxplot() +
  facet_wrap(~site) +
  #stat_summary(fun.y="median",geom="point") +
  xlab("") +
  ylab("Density\nlog (x+1)") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45,hjust=1))
        
ggsave("figures/pred_survey_by_site_roving native vs invasive predators.png")



ggplot(dat.predator.comparison, aes(x=species,y=log(density+1))) + 
  geom_jitter() +
  facet_wrap(~site) +
  #stat_summary(fun.y="median",geom="point") +
  xlab("") +
  ylab("Density\nlog (x+1)") +
  theme_bw() +
  theme(
    text=element_text(size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2),
    legend.position="none"
  )

m3 <- lme(data=dat.predator.comparison,log(density+1) ~ species, random= ~1|site)
summary(m3)
anova(m3,test="F")

# Linear mixed-effects model fit by REML
# Data: dat.predator.comparison 
# AIC       BIC   logLik
# -20.31207 -5.181335 16.15603
# 
# Random effects:
#   Formula: ~1 | site
# (Intercept)  Residual
# StdDev: 6.782746e-06 0.1894489
# 
# Fixed effects: log(density + 1) ~ species 
# Value Std.Error DF    t-value p-value
# (Intercept)          0.05284010 0.0386711 90  1.3663974  0.1752
# speciesh.puella     -0.03819852 0.0546892 90 -0.6984655  0.4867
# speciesh.nigricans  -0.01655558 0.0546892 90 -0.3027212  0.7628
# speciesc.cruentatus  0.17065113 0.0546892 90  3.1203808  0.0024
# Correlation: 
#   (Intr) spcsh.p spcsh.n
# speciesh.puella     -0.707                
# speciesh.nigricans  -0.707  0.500         
# speciesc.cruentatus -0.707  0.500   0.500 
# 
# Standardized Within-Group Residuals:
#   Min          Q1         Med          Q3         Max 
# -1.17969105 -0.27891471 -0.19152661 -0.07728509  4.40511214 
# 
# Number of Observations: 96
# Number of Groups: 3 

dat.predator.comparison$native.invasive <- c(rep("invasive",24),rep("native",nrow(dat.predator.comparison)-24))
dat.predator.comparison$incidence <- ifelse(dat.predator.comparison$density ==0,0,1)

#total samples by site
dat.predator.comparison %>%
 group_by(site,native.invasive) %>%
 tally()

group_by(dat.predator.comparison, site) %>% mutate(percent = incidence/sum(incidence)))

predtab<-
  dat.predator.comparison%>%
  group_by(site,native.invasive)%>%
  summarize(n=n(),sum(incidence))

predtab %>% gt()

m2 <- lme(data=dat.predator.comparison,log(density+1) ~ native.invasive, random= ~1|site)
summary(m2)

# Linear mixed-effects model fit by REML
# Data: dat.predator.comparison 
# AIC       BIC   logLik
# -16.08229 -5.909109 12.04114
# 
# Random effects:
#   Formula: ~1 | site
# (Intercept) Residual
# StdDev: 5.56805e-06 0.204602
# 
# Fixed effects: log(density + 1) ~ native.invasive 
# Value  Std.Error DF   t-value p-value
# (Intercept)           0.05284010 0.04176421 92 1.2652004  0.2090
# native.invasivenative 0.03863234 0.04822516 92 0.8010828  0.4251
# Correlation: 
#   (Intr)
# native.invasivenative -0.866
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -0.4470750 -0.4470750 -0.4470750 -0.2582579  3.8091312 
# 
# Number of Observations: 96
# Number of Groups: 3

# plot timed predator survey data

setwd("/Users/jameal.samhouri/Dropbox/Lionfish Panama/Code")
timed_pred_dat <- read.csv("2015 Timed predator surveys by observer.csv",header=TRUE)
names(timed_pred_dat)
View(timed_pred_dat)

ggplot(timed_pred_dat, aes(x=Fish,y=log(Total+1))) + 
  geom_jitter() +
  facet_wrap(~Reef) +
  #stat_summary(fun.y="median",geom="point") +
  xlab("") +
  ylab("Number\nlog (x+1)") +
  theme_bw() +
  theme(
    text=element_text(size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2),
    legend.position="none"
  )

timed_pred_dat$native.invasive <- ifelse(timed_pred_dat$Fish == "Pterois volitans","invasive","native")

m3 <- lme(data=timed_pred_dat,log(Total+1) ~ native.invasive, random= ~1|Reef)
summary(m3)

# Linear mixed-effects model fit by REML
# Data: timed_pred_dat 
# AIC     BIC    logLik
# 39.2949 44.1704 -15.64745
# 
# Random effects:
#   Formula: ~1 | Reef
# (Intercept)  Residual
# StdDev:   0.1435216 0.3961389
# 
# Fixed effects: log(Total + 1) ~ native.invasive 
# Value Std.Error DF   t-value p-value
# (Intercept)           0.0770164 0.1558922 23  0.494036   0.626
# native.invasivenative 2.3418043 0.1617230 23 14.480340   0.000
# Correlation: 
#   (Intr)
# native.invasivenative -0.692
# 
# Standardized Within-Group Residuals:
#   Min          Q1         Med          Q3         Max 
# -2.04297410 -0.51016374 -0.09989995  0.67832091  2.11884549 
# 
# Number of Observations: 27
# Number of Groups: 3 

ggplot(timed_pred_dat, aes(x=native.invasive,y=log(Total+1))) + 
  geom_boxplot() +
  #stat_summary(fun.y="median",geom="point") +
  xlab("") +
  ylab("Number\nlog (x+1)") +
  theme_bw() +
  theme(
    text=element_text(size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2),
    legend.position="none"
  )

ggsave("figures/roving native vs invasive predators.pdf")
