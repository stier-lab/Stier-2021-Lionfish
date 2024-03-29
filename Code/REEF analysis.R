library(tidyverse)
library(Hmisc)
library(janitor)
library(ggrepel)
library(RColorBrewer)

#######
## Load and Organize Data
#######


### Jameal's quick and dirty plots for ICRS 2016 
reef_dat <- clean_names(read_csv("data/REEF lionfish vs graysby 2010-2015.csv")) %>%
  remove_empty(c("rows", "cols")) 
names(reef_dat)
glimpse(reef_dat)
reef_dat$region <- str_to_title(reef_dat$region, locale = "en")

######################################################
############# PUBLICATION ANALYSIS ###################
######################################################


t.test(data = reef_dat, expert_den ~ species)
# Welch Two Sample t-test
# 
# data:  expert_den by species
# t = 2.435, df = 11.253, p-value = 0.03266
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.02957661 0.57042339
# sample estimates:
#   mean in group Graysby mean in group Lionfish 
# 1.975                  1.675 

######################################################
######################################################
######################################################

# hist(reef_dat$expert_den)
# hist(log(reef_dat$expert_den))

# m1 <- glm(data = reef_dat, expert_den ~ species)
# summary(m1)
# 
# summary(aov(data = reef_dat, expert_den ~ species))


#t.test(data = reef_dat, expert_den ~ species,paired=TRUE)

# wilcox.test(data = reef_dat, expert_den ~ species)
# 
# #wilcox.test(data = reef_dat, expert_den ~ species,paired=TRUE) #NS
# 
# kruskal.test(data = reef_dat, expert_den ~ species)

######################################################
############# PUBLICATION FIGURE #####################
######################################################

ggplot(reef_dat, aes(x=species,y=expert_den, colour = species))+
  #geom_point(position= position_dodge(width = 1))+
  stat_summary(fun.data=mean_cl_normal,lty=2, position=position_dodge(width = 1))+
  ylab("Expert Density")+
  xlab("Species") +
  theme_bw() +
  theme(
    text=element_text(size=18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2),
    strip.background =   element_rect(fill = "black", colour = "black"),
    strip.text.x =       element_text(colour="white",size=8),
    strip.text.y =       element_text(angle = -90,colour="white",size=12),
    legend.position = "none"
  )
#ggsave("Lionfish vs graysby by region.pdf", width=12, height=7)
ggsave("figures/reef_data/Lionfish vs graysby.png")

######################################################
######################################################
######################################################

######################################################
############# PAIRS PLOT FIGURE #####################
######################################################

#https://ggrepel.slowkow.com/articles/examples.html
#https://rdrr.io/cran/RColorBrewer/man/ColorBrewer.html
#Color Palettes Based on Fish Species • fishualize
#https://nschiett.github.io/fishualize/index.html

#brewer.pal(n = 12, name = "Set3")[-c(1:2)]

ggplot(
  reef_dat %>%
    mutate(
      pretty_label = case_when(
        species == "Lionfish" ~ region,
        TRUE ~ ""
      )
      ) %>%
    mutate(
      label_font = case_when(
        pretty_label == "Continental Caribbean And Brazil" ~ "bold",
        TRUE ~ "plain"
        )
      )
    ) +
  geom_point(aes(x=species, y=expert_den, colour = region, fill = region), size=2) + #, position=position_nudge(x = -0.2, y = 0)
  geom_path(aes(x=species, y=expert_den, group=region, colour = region)) + #, position=position_nudge(x = -0.2, y = 0)
  geom_text_repel(
    aes(x=species, y=expert_den, label=pretty_label, fontface=label_font, colour = region), 
    alpha=1,
    force = 0.75,
    #nudge_x = 2,
    direction = "y",
    hjust = 0,
    segment.size = 0.1,
    size = 5) +
  stat_summary(aes(x=species, y=expert_den, colour=species),
               fun.data=mean_cl_normal,lty=2, size = 1.5,
               alpha=0.8, position=position_nudge(x = -0.1, y = 0)) + #position=position_dodge(width = 1),
  ylab("Expert Density")+
  #ylim(0,2.6) +
  xlab("") +
  # scale_x_continuous(
  #   breaks = 1:2, labels = c("Graysby", "Lionfish"),
  #   expand = expansion(mult = 0.5)
  # ) +
  scale_x_discrete(expand = expansion(mult = c(0.3,2))) + 
  scale_colour_manual(values = brewer.pal(n = 12, name = "Set3")[-c(1:2)]) + #palette = "Blues", direction = -1, brewer.pal(n = (length(unique(reef_dat$region))+2), name = "Set3")
  scale_fill_manual(values = brewer.pal(n = 12, name = "Set3")[-c(1:2)]) + #palette = "Blues", direction = -1, brewer.pal(n = (length(unique(reef_dat$region))+2), name = "Set3")
  theme_bw() +
  theme(
    text=element_text(size=18),
        axis.text.x = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA,colour = "black",size=2),
        strip.background = element_rect(fill = "black", colour = "black"),
        strip.text.x = element_text(colour="white",size=8),
        strip.text.y = element_text(angle = -90,colour="white",size=12),
        legend.position = "none"
    )
ggsave("figures/reef_data/Lionfish vs graysby by region pairs plot.pdf", width=9, height=7) #
ggsave("figures/reef_data/Lionfish vs graysby pairs plot.png", width=9, height=7)

######################################################
######################################################
######################################################

ggplot(reef_dat, aes(x=species,y=expert_den, group=region, fill = species))+
  geom_bar(stat = "identity")+
  facet_wrap(~region,nrow=4)+
  ylim(0,4)+
  ylab("Expert Density")+
  xlab("Species") +
  theme_bw() +
  theme(
    text=element_text(size=12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2),
    strip.background =   element_rect(fill = "black", colour = "black"),
    strip.text.x =       element_text(colour="white",size=8),
    strip.text.y =       element_text(angle = -90,colour="white",size=12),
    legend.title = element_blank(),
    legend.background = element_rect(colour="black"),
    legend.key = element_blank(),
    legend.position = "none"
  )
#ggsave("Lionfish vs graysby by region.pdf", width=12, height=7)
ggsave("figures/reef_data/Lionfish vs graysby by region colored bars.pdf", width=12, height=7)
ggsave("figures/reef_data/Lionfish vs graysby by region colored bars.png", width=12, height=7)


######## ANALYSIS OF REEF DATA SENT BY CHRISTY ##########
library(ggplot2)
library(readr)
library(dplyr)
# http://ase.tufts.edu/bugs/guide/assets/R%20Cookbook.pdf

setwd("~/Dropbox/Projects/In progress/Adrian/Lionfish Panama/Lionfish Opinion/REEF data")

rm(list=ls())

# read in all records of fish counts
count.df <- read.table("fish_061316.txt",sep="|",header=TRUE,colClasses = "character")[,-1]
head(count.df)

#temp <- read_fwf("fish_061316.txt",skip=1,n_max=100,
#                 fwf_positions(c(2,4,12,21,26,30),c(2,10,19,24,28,30),
#                col_names=c("recordid","formid","geog","speciesid","familyid","abundance")))
#head(temp)

# read in species ID file
sp_names.df <- read.csv("TWAspecies.csv", header=FALSE)
colnames(sp_names.df) <- c("speciesid","common.name","scientific.name","familyid","scientific.family.name")
#sp_names.df$speciesid <- as.character(sp_names.df$speciesid)
head(sp_names.df)

# merge data frames
count.df$speciesid <- as.numeric(count.df$speciesid)
df <- merge(count.df,sp_names.df,by="speciesid")
head(df)

# subset data based on experience of surveyor
experience <- "Expert"

# subset to most recent couple of years
my_years <- c(2013, 2014, 2015)

# subset only to surveys where lionfish were sighted? (conservative test)
  # make a df for each formid (survey), and then subset to surveys that include at least 1 lionfish
df_lion <- 

# choose native species to focus on. start with genera used by Green et al. 2012 "large-bodied competitors" + the genera cephalopholis, hypoplectrus, ocyurus, synodus. then rank them  by mean/median density, and use top 25 spp for comparisons.
unique(df$common.name)
unique(df$scientific.name)
my_genera <- c("Cephalopholis","Epinephelus","Hypoplectrus","Lutjanus","Mycteroperca","Ocyurus","Scorpaena","Synodus")
my_species <- unique(df$scientific.name)[grep(paste(my_genera,collapse="|"),unique(df$scientific.name))]

abund_means <- aggregate(as.numeric(df$abundance), by=list(df$scientific.name), mean, na.rm=TRUE)
my_abund_means <- abund_means[abund_means$Group.1 %in% my_species,]
top25 <- my_abund_means[order(my_abund_means$x, decreasing = TRUE),][1:25,]

# aggregate comparisons by geographic zone code. can use first digit of geog for 8 large regions

# focus on Density score as response variable
# from REEF readme: Density score (D) for each species is a weighted average index based on the frequency of observations in different abundance categories.  Density score is calculated as: D= ((nSx1)+(nFx2)+(nMx3)+(nAx4)) / (nS + nF  + nM + nA), where nS, nF, nM, and nA represented the number of times each abundance category (Single, Few, Many, Abundant) was assigned for a given species.  Values range from 1 to 4.

#acs tinker
library(ggpubr)

d<-read.csv("data/REEF lionfish vs graysby 2010-2015_acs.csv")

ggplot(data=d)+
  geom_point(aes(x=Species, y=Expert.Den,group=Region),size=2) +
  geom_path(aes(x=Species, y=Expert.Den,group=Region)) +

  stat_summary(aes(x=Species, y=Expert.Den),
               fun.data=mean_se,lty=1, size = 1,
               alpha=0.8,colour="grey")+

  ylab("Expert Density")+
  theme_classic() +
  theme(
    text=element_text(size=10),
    axis.text.x = element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # panel.background = element_rect(fill = NA,colour = "black",size=2),
    # strip.background = element_rect(fill = "black", colour = "black"),
    strip.text.x = element_text(colour="white",size=10),
    strip.text.y = element_text(angle = -90,colour="white",size=12),
    legend.position = "none"
  )+
  scale_x_discrete(expand=c(0.2, 0.2))

ggsave("figures/reef_data/stier_pairs.pdf",width=3,height=3)
