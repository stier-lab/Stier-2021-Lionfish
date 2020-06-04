
library(tidyverse) 
library(nlme)
library(gt)
library(lme4)


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

histogram_COPE <- ggplot(countdat, aes(x=c.personatus.d)) +
  geom_histogram(binwidth=25, colour = "black", fill="white") +
  ylab("Number of coral heads") +
  xlab(expression(paste(italic("Coryphopterus personatus"), " density (no. ", m^-2,")", sep=""))) +
  theme_classic()

histogram_COPE

setwd("/Users/jameal.samhouri/Dropbox/Lionfish Panama/Figures")
pdf("histogram_COPE.pdf")
histogram_COPE
dev.off()

histogram_COPE_bysite <- ggplot(countdat, aes(x=c.personatus.d)) +
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
  geom_boxplot() +
  #stat_summary(fun.y="median",geom="point") +
  xlab("") +
  ylab("Density\nlog (x+1) transformed\n") +
  theme_bw() +
  theme_classic()


setwd("/Users/jameal.samhouri/Dropbox/Lionfish Panama/Figures")
pdf("plot_COPEs_otherprey.pdf")
plot_COPEs_otherprey
dev.off()

### faceted by site

dat.comparison <- melt(countdat[,c(3,28,48)])
names(dat.comparison) <- c("site","species","density") 
levels(dat.comparison$species) <- c("Coryphopterus personatus","All other prey")

plot_COPEs_otherprey_bysite <- ggplot(dat.comparison, aes(x=species,y=log(density+1))) + 
  geom_boxplot() +
  facet_wrap(~site) +
  #stat_summary(fun.y="median",geom="point") +
  xlab("") +
  ylab("Density\nlog (x+1) transformed\n") +
  theme_bw() +
  theme(
    text=element_text(size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2),
    legend.position="none"
  )
plot_COPEs_otherprey_bysite


setwd("/Users/jameal.samhouri/Dropbox/Lionfish Panama/Figures")
pdf("plot_COPEs_otherprey_by_site.pdf")
plot_COPEs_otherprey_bysite
dev.off()

m1 <- lme(log(density+1) ~ species, random = ~1|site, data=dat.comparison)
summary(m1)

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

