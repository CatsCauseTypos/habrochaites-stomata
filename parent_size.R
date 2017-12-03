
#install & load needed libraries ----
library(lme4)
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5)) # for centered titles
library(dplyr)
library(cowplot)
library(ggpubr)
# import files ----
parent_size <- read.csv("~/Desktop/length_parents.csv")

head(parent_size)

# stats ----
compare_means(width ~ species,  data = parent_size, method = "t.test") #  significant p 0.022

compare_means(length ~ species,  data = parent_size, method = "t.test") #  significant p 0.0026

compare_means(whole.leaf.length ~ species,  data = parent_size, method = "t.test") #  significant p 0.0069  

# ugh


# facet labels ----
labels <- c(length = "Leaflet Length (cm)", whole_leng = "Whole Leaf Length (cm)", width = "Leaflet Width (cm)")

# fixing spp to discrete
parent_size$species <- as.factor(parent_size$species)

# actual bar graph ----
ggplot(parent_size, aes(x = species, y = all_measure, fill = species, stat = "identity"))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  facet_wrap(~dimension, 
             labeller = labeller(dimension = labels))+
  scale_fill_manual(name="", values = c("darkorange","magenta"),
                    breaks = c("1777","4024"),
                    labels = c("S. habrochaites", "S. lycopersicum"))+
  scale_x_discrete(breaks = c("1777","4024"),
                    labels = c("S. hab", "S. lyco"))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes(y = (all_measure+2),
                   label = sig),
               position = position_dodge(.6),
               color = "black",
               size = 12)+
  theme(legend.position = c(.13,.88),
        legend.direction="vertical",
        legend.title=element_blank(),
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        axis.text.x = element_text(face = "italic"),
        strip.text.x = element_text(size=20),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 1)))+
  ggtitle("Differences in Leaf & Leaflet Size Across Species\n")+
  ylab("Dimensions (cm)")+
  xlab("")+
  coord_cartesian(ylim = c(0, 40))


# so hab is first, but I am tired and hungry and R sucks

# chp1_parents_size.jpeg 1500 (done)