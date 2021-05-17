library(tidyverse) 

#######
## Load and Organize Data
#######

countdat <- read_csv("data/2015 field survey fish counts.csv")
names(countdat)
glimpse(countdat)

#######
## coral head size by site
#######

# https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
# The palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(countdat,aes(x=site,y=head.area.m2)) +
  geom_boxplot(aes(fill=site)) +
  theme_classic() +
  xlab("Site") +
  ylab(expression(paste("Coral head area", " (", m^-2,")", sep=""))) +
  scale_fill_manual(values=cbp1) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.position="none")

ggsave("figures/prey_survey/coral_head_area_bp.pdf")
ggsave("figures/prey_survey/coral_head_area_bp.png")

