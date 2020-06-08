


library(tidyverse) 
library(ggridges)
library(nlme)
library(gt)
library(lme4)
library(scales)


#######
## Load and Organize Data
#######

countdat <- read.csv("data/2015 field survey fish counts.csv")
names(countdat)

countdat$c.personatus.d <- countdat$c.personatus/countdat$head.area.m2
densities <- countdat[,c(9:27)]/countdat$head.area.m2
countdat <- data.frame(countdat,densities)
names(countdat) <- gsub("1","d",names(countdat))


#######
## Frequency Distributions of COPE by site
#######


# http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/

ggplot(countdat, aes(x=c.personatus.d)) +
  geom_histogram(binwidth=25, colour = "black", fill="white") +
  ylab("Number of coral heads") +
  xlab(expression(paste(italic("Coryphopterus personatus"), " density (no. ", m^-2,")", sep=""))) +
  theme_classic()

 ggplot(countdat, aes(x=c.personatus.d)) +
  geom_histogram(binwidth=25, colour = "black", fill="white") +
  facet_wrap(~site) +
  ylab("Number of coral heads") +
  xlab(expression(paste(italic("Coryphopterus personatus"), " density (no. ", m^-2,")", sep=""))) +
  theme_bw() +
  theme_classic()

ggsave("figures/prey_survey/histogram_COPE_bysite.png")



#######
## COPE density releative to other prey
#######

names(countdat)
countdat$otherprey.d <- rowSums(countdat[,c(29:30,32,37:39,41,43,46)]) # leave out elactinus, pomacanthus, bahianus, iserti, capistratus

dat.comparison <- pivot_longer(countdat[,c(3,28,48)],cols=c(2:3))
names(dat.comparison) <- c("site","species","density") 
levels(dat.comparison$species) <- c("Coryphopterus personatus","All other prey")


ggplot(dat.comparison, aes(x=species,y=log(density+1))) + 
  geom_boxplot(aes(fill=species)) +
  #stat_summary(fun.y="median",geom="point") +
  xlab("") +
  ylab("Density\nlog (x+1) transformed\n") +
  theme_classic()+
  facet_wrap(~site)+
  scale_fill_manual(values=c("gray","orange"))


ggsave("figures/prey_survey/boxplot_COPE_otherprey.png")

dat.comparison2 <- dat.comparison %>%
  filter(density>0)

#ridgeline plot alternative with no zeros 
  ggplot(data=dat.comparison2,aes(x=density, y=site,point_color=species,color=species,fill=species)) +
  geom_density_ridges(
    quantile_lines=TRUE,
    quantile_fun=function(x,...)mean(x),
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 3, size = 0.25,
    position = position_points_jitter(height = 0),
    aes(x = density), 
    alpha = .8, color = "white")+
    theme_ridges(center=TRUE)+
    scale_fill_manual(values = c("gray", "orange"), labels = c("Goby", "Other prey")) +
    scale_color_manual(values = c("gray", "orange"), guide = "none") +
    scale_discrete_manual("point_color", values = c("gray", "orange"), guide = "none") +
    xlab("Prey density [fisher per m2")+
    ylab("Reef")+
    # scale_x_continuous(trans=log10_trans())
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))   
  
  ggsave("figures/prey_survey/ridgeline_COPE_otherprey.png")
  
  
  
  
  ggplot(Aus_athletes, aes(x = height, y = sport, color = sex, point_color = sex, fill = sex)) +
    geom_density_ridges(
      jittered_points = TRUE, scale = .95, rel_min_height = .01,
      point_shape = "|", point_size = 3, size = 0.25,
      position = position_points_jitter(height = 0)
    

ggplot(dat.comparison, aes(x = `Mean Temperature [F]`, y = Month, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016')+



m1 <- lme(log(density+1) ~ species, random = ~1|site, data=dat.comparison)
summary(m1)

m2<-glmer(log(density+1) ~ species, random = ~1|site,data=dat.comparison)
summary(m2)

m3 <-lmer(log(density+1) ~species+(1|site),data=dat.comparison)



# Linear mixed-effects model fit by REML
# Data: dat.comparison 
# AIC      BIC    logLik
# 125.5324 132.8469 -58.76618
# 
# Random effects:
#   Formula: ~1 | site
# (Intercept)  Residual
# StdDev:  0.04758146 0.8092751
# 
# Fixed effects: log(density + 1) ~ species 
# Value Std.Error DF  t-value p-value
# (Intercept)            4.073959 0.1676623 44  24.2986       0
# speciesAll other prey -3.122346 0.2336176 44 -13.3652       0
# Correlation: 
#   (Intr)
# speciesAll other prey -0.697
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -2.6037816 -0.4290315  0.1238969  0.4165451  1.7366357 
# 
# Number of Observations: 48
# Number of Groups: 3 

############################################################
############################################################

# STATS FOR DENSITIES OF COPEs RELATIVE TO OTHER PREY

############################################################
############################################################

COPEvsOTHER.ttest <- t.test(log(density+1)~species,data=dat.comparison)
write.csv(data.frame(COPEvsOTHER.ttest$statistic,COPEvsOTHER.ttest$parameter,COPEvsOTHER.ttest$p.value),"t-test for COPE vs all other prey density.csv",row.names=FALSE)

