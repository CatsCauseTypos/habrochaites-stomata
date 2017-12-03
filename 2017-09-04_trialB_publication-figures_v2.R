#Date: May-4-2017
#Project: Habrochaites Trial B drought
#Author: Donnelly West - DonWest@UCDavis.edu
#Version: 7.0
# purpose: trying to get all the graphs to look somewhat alike for publication/thesis chapter

#Run this first or your error bars won't work:
#Julin's fancy shamancy code
## PHOTO
plot.summary=function(x) {
  x=na.omit(x)
  y=mean(x)
  sem=sd(x)/sqrt(length(x))
  return(data.frame(
    y=y,
    ymin=y-sem,
    ymax=y+sem
  ))}

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots == 1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#now you have functions! WOOO!


#install & load needed libraries ----
library(lme4)
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5)) # for centered titles
library(dplyr)
library(cowplot)
library(ggpubr)
# import files ----
Trial_b <- read.csv("~/Desktop/Sinha_Lab_stuff/hab-epidermis/Data/trial_b_R_friendly-may-2017.csv")

# we have ab & ad sides for stomata & pavement 

head(Trial_b)
# calculate index ----
Trial_b$Index_ab <- (Trial_b$ab_stomata/Trial_b$ab_pave)
Trial_b$Index_ad <- (Trial_b$ad_stomata/Trial_b$ad_pave)
#Trial_b <- na.omit(Trial_b)
Trial_b$Treatment <- as.factor(Trial_b$Treatment)  

# split data sets ----
Hab <- Trial_b[grepl("habrochaites",Trial_b$Geno),]
Lyc <- Trial_b[grepl("lycopersicum",Trial_b$Geno),]
WTR <- Trial_b[grepl("00",Trial_b$Treatment),]
DRT <- Trial_b[grepl("none",Trial_b$Treatment),]

# checking the column classes
sapply(Trial_b, class)
head(Trial_b)

# stat_summary ----
trialB_stats <- Trial_b %>% 
  group_by(Geno, Treatment) %>% 
  summarise(
    avg_ad_pave = mean(ad_pave),
    var_ad_pave = var(ad_pave),
    avg_ad_stom = mean(ad_stomata), 
    var_ad_stom = var(ad_stomata),
    avg_AB_pave = mean(ab_pave), 
    var_AB_pave = var(ab_pave), 
    avg_AB_stom = mean(ab_stomata),
    var_AB_stom = var(ab_stomata))  

trialB_stats

#write.csv(file = "Trial_B_stats.csv", x = trialB_stats)

# holy cow, the variation is HUGE!!!

head(Trial_b)
# violin peak ----
#new view of old data - b/c wtf is with this project:
ggplot(Trial_b, aes( y = ab_stomata, x = Geno, fill = Treatment)) +
  geom_violin()
ggplot(Trial_b, aes( y = ab_pave, x = Geno, fill = Treatment)) +
  geom_violin()

ggplot(Trial_b, aes( y = ad_pave, x = Geno, fill = Treatment)) +
  geom_violin()

ggplot(Trial_b, aes( y = ad_stomata, x = Geno, fill = Treatment)) +
  geom_violin()


# create theme ----

# scatter plots


# bar plots w hab/lyco
# (just copy/paste >< )
theme(legend.position = c(.18,.88),
      legend.direction="vertical",
      legend.title=element_blank(),
      legend.key.size = unit(1.5, "cm"),
      legend.text = element_text(size = 25),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
      axis.title = element_text(size = 25, face = "bold"),
      axis.text = element_text(size = 25),
      axis.text.x = element_text(face = "italic"),
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 1)))


# scatter plot adaxial to see data (done) ----


b <- ggplot(Trial_b, aes( x = ad_pave, y = ad_stomata, colour = Treatment, shape = Geno) ) +
  geom_point(size=7)+
  theme_bw()+ # this left-justifies title
  scale_color_manual(name="Color Key:", values= c( "darkblue", "darkgoldenrod3"),
                    breaks = c("00_norm","none"),
                               labels = c("Normal Watering","No Water"))+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        legend.text = element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25))+
  ggtitle("Adaxial Pavement vs Stomata Cells\n")+
  ylab("Stomata Cell Counts\n")+
  xlab("\nPavement Cell Counts")+
  scale_shape_discrete(name = "Species:",
                       breaks = c("habrochaites", "lycopersicum"),
                   labels = c("S. habrochaites","S. lycopersicum"))
# chp1_ad_scatter_drought.jpeg 1500 aspect ratio - done
# ggdraw B ----
ggdraw(b)+draw_plot_label("B", size = 30)

# chp1_ad_scatter_drought.png 1500

# scatter plot abaxial (done) ----
a <- ggplot(Trial_b, aes( x = ab_pave, y = ab_stomata, colour = Treatment, shape = Geno) ) +
  geom_point(size=7)+
  theme_bw()+ # this left-justifies title
  scale_color_manual(name="Color Key:", values= c( "darkblue", "darkgoldenrod3"),
                     breaks = c("00_norm","none"),
                     labels = c("Normal Watering","No Water"))+
  theme(legend.position = "none",
        legend.direction="vertical",
        legend.title=element_blank(),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        legend.text = element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25))+
  ggtitle("Abaxial Pavement vs Stomata Cells\n")+
  ylab("Stomata Cell Counts\n")+
  xlab("\nPavement Cell Counts")+
  scale_shape_discrete(name = "Species:",
                       breaks = c("habrochaites", "lycopersicum"),
                       labels = c("S. habrochaites","S. lycopersicum"))

# trialB_ab_scatter.png 800 aspect ratio
# chp1_ab_scatter_drought.jpeg 1500 - done
# ggdraw A ----
ggdraw(a)+draw_plot_label("A", size = 30)
# chp1_ab_scatter_drought.png 1500

# Bar graph adaxial stomata (done) q ----

# got a weird error, so googled it - this fix worked... 
#dev.off()

q <- ggplot(Trial_b, aes(reorder(Geno,ad_stomata, mean), ad_stomata, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes(y = (ad_stomata+3),
                    label = stom_sign_ad),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes(y = (ad_stomata-3.8),
                   label = n),
               position = position_dodge(.6),
               color = "white",
               size = 8)+
  scale_fill_manual(values= c( "darkblue", "darkgoldenrod3"),
                    name = "Treatments:",
                    breaks = c("00_norm", "none"),
                    labels = c("Normal Watering  ","No Water  "))+
  theme(legend.position = "none",
          #c(.18,.88),
        legend.direction="vertical",
        legend.title=element_blank(),
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        axis.text.x = element_text(face = "italic"))+
  ggtitle("Adaxial Stomatal Density")+
  ylab("Average Number \n of Stomata\n")+
  xlab("")+
  coord_cartesian(ylim = c(0, 19))+
  scale_x_discrete(breaks = c("habrochaites", "lycopersicum"),
                   labels = c("S. habrochaites","S. lycopersicum"))

# trialB_ad_stom_bargraph.png aspect 1000

# chp1_ad_stom_drought.jpeg -done

# ??? check stats on ad_stom ----

# bar adaxial pavement (done) s ----

s <- ggplot(Trial_b, aes(reorder(Geno,ad_pave, mean), ad_pave, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values= c( "darkblue", "darkgoldenrod3"),
                    breaks = c("A Normal Watering", "No Water"),
                    labels = c("Normal Watering", "No Water"))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes(y = (ad_pave+30),
                   label = pave_sign_ad),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes(y = (ad_pave-90),
                   label = n),
               position = position_dodge(.6),
               color = "white",
               size = 8)+
  theme(legend.position = "none",
          #c(.18,.88),
        legend.direction="vertical",
        legend.title=element_blank(),
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        axis.text.x = element_text(face = "italic"),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 1)))+
  ggtitle("Adaxial Pavement Cell Density\n")+
  ylab("Average Number of\nPavement Cells")+
  xlab("")+
  coord_cartesian(ylim = c(0, 220))+
  scale_x_discrete(breaks = c("habrochaites", "lycopersicum"),
                   labels = c("S. habrochaites","S. lycopersicum"))

# trialB_ad_pave_bargraph.png aspect 1000
# chp1_ad_pave_drought.jpeg 1500 - done

# bar plot stomatal index adaxial (done) bar_ind_ad ----
# (stomata per pavement cell)

bar_ind_ad <- ggplot(Trial_b, aes(reorder(Treatment), y = Index_ad, x = Geno, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes(y = (Index_ad+.019),
                   label = index_sign_ad),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  scale_fill_manual(values= c( "darkblue", "darkgoldenrod3"),
                    breaks = c("00_norm", "none"),
                    labels = c("Normal Watering", "No Water"))+
  theme(legend.position = "none",
          #c(.15,.88),
        legend.direction="vertical",
        legend.title=element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        axis.text.x = element_text(face = "italic"),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 1)))+
  ggtitle("Adaxial Stomatal Index\n")+
  ylab(" ")+
  xlab(" ")+
  coord_cartesian(ylim = c(0, .11))+
  scale_x_discrete(breaks=c("habrochaites", "lycopersicum"),
                   labels=c("S. habrochaites", "S. lycopersicum"))

# trialB_ad_stom-index.png ratio 1000
# chp1_ad_index_drought.jpeg 1500 (done)


# _______________________________________________________________
# Statistics ----

# significance of AD index: ----
anova(lm(Index_ad~Geno*Treatment, Trial_b)) 

# Response: Index_ad
# Df   Sum Sq   Mean Sq F value    Pr(>F)    
# Geno            1 0.000659 0.0006590  0.3715 0.5437913    
# Treatment       1 0.000456 0.0004565  0.2573 0.6132541    
# Geno:Treatment  1 0.023098 0.0230975 13.0208 0.0005165 ***
#   Residuals      86 0.152555 0.0017739                      


# May 2017:
# Response: Index_ad
# Df   Sum Sq   Mean Sq F value   Pr(>F)   
# Geno            1 0.000571 0.0005714  0.3071 0.581015   
# Treatment       1 0.000394 0.0003935  0.2115 0.646853   
# Geno:Treatment  1 0.021131 0.0211313 11.3556 0.001154 **
#   Residuals      81 0.150731 0.0018609           

# Aug 2017: I think we counted more inbetween first and May analyses?? or excluded samples without complete data sets??

# after we run an anova to see *if* there are differences, we need to figure out between which groups (tukey HSD):

tuke.trialb.index_ad <- TukeyHSD(aov(lm(Index_ad~Geno*Treatment, Trial_b))) 

sum.tuke.trialb.index_ad <- as.data.frame(tuke.trialb.index_ad[3])
# lyco_norm vs hab_norm = not sig
# hab_non vs hab_norm = not sig
# lyco_non vs hab_norm = not
# hab_non vs lyco_norm = not
# lyco_non vs lyco_norm = not
# lyco_non vs hab_non = significant

head(Trial_b)
# t_tests of index data ad ----
# Aug 2017, using ggpubr package
Trial_b %>% 
  group_by(Geno) %>% 
  compare_means(AD_Index ~ Treatment, data = ., method = "t.test") # so this code runs, but I do NOT understand the out put; grepl out the geno's to try it simpler
# this code no longer runs


# hab index ----
compare_means(AD_Index ~ Treatment,  data = Hab, method = "t.test") # significant p 0.031, padj = 0.03056183
compare_means(AB_Index ~ Treatment,  data = Hab, method = "t.test") # signif p = 0.00016, padj = 0.0001592211

# lyco index ----
compare_means(AD_Index ~ Treatment,  data = Lyc, method = "t.test") # significant p 0.0056, padj = 0.005627688
compare_means(AB_Index ~ Treatment,  data = Lyc, method = "t.test") # NOT signif p = 0.5, padj = 0.502816

# between spp - norm
compare_means(AD_Index ~ Geno,  data = WTR, method = "t.test") # NOT significant p 0.067, padj = 0.06658777
compare_means(AD_Index ~ Geno,  data = DRT, method = "t.test") # significant p 0.0048, padj = 0.004836013

compare_means(AB_Index ~ Geno,  data = WTR, method = "t.test") # signif p = 6.7e-12, padj = 6.738585e-12

compare_means(AB_Index ~ Geno,  data = DRT, method = "t.test") # signif p = 5.9e-13, padj = 5.932818e-13


# Personal summary of compare means: ----
# AD_index: 
# hab_none vs hab_wtr = signif *
# lyc_none vs lyc_wtr = signif **
# WTR: lyco vs hab = not sign
# DRT: lyco vs hab = sign **

# hab non  (c)    lyco non (b)    hab wtr (a)    lyc wtr (a) # index_ad_sign2

# need to ask if lyco-non vs hab wtr are signfi ---- ????
 
# these are different than what I have in index_sign_ad (from: sum.tuke.trialb.index_ad):
# hab non  (a)    lyco non (b)    hab wtr (b)    lyc wtr (ab)

# AB_index: 
# hab_none vs hab_wtr = signif ***
# lyc_none vs lyc_wtr = NOT
# WTR: lyco vs hab = sign ****
# DRT: lyco vs hab = sign ****


# and these stats are exactly what's in index_sign_ab already, so stats are consistent regardless of which way I code the tests.

# checking on all of them, just for funsies:
tuke.trialb.index_ad[3]

# which ones have a p of less than 0.05?
sig.sum.tuke.trialb.index_ad <- as.data.frame(sum.tuke.trialb.index_ad[which(sum.tuke.trialb.index_ad$Geno.Treatment.p.adj<.05),])

sig.sum.tuke.trialb.index_ad
# back in october:
# comparison                                      adjusted p value
# hab:No Water-e6203:No Water                   0.04182968
# e6203:Normal Water-e6203:No Water             0.03355455
# hab:Normal Water-hab:No Water                 0.03490362
# hab:Normal Water-e6203:Normal Water           0.03005401

# now in Feb:
# lyco-nowater vs hab-no water 0.02333116
# hab-norm vs hab-nowater 0.03534225

# in May
# Geno.Treatment.diff Geno.Treatment.lwr
# lycop:No Water-hab:No Water             0.03733303


# why are these so different?! I thought we had all the adaxial stuff counted before...
# i think we may have had some 0's in there accidentally because there were some uncounted. kind of thought the na.omit would rid us of them, but y'know me and R don't always agree on stuff like this ><

# significance of AB index: ----
anova(lm(Index_ab~Geno*Treatment, Trial_b)) 

# Response: Index_ab
# Df  Sum Sq Mean Sq  F value    Pr(>F)    
# Geno            1 0.58266 0.58266 197.8852 < 2.2e-16 ***
#   Treatment       1 0.03359 0.03359  11.4083  0.001126 ** 
#   Geno:Treatment  1 0.01094 0.01094   3.7169  0.057368 .  
# Residuals      81 0.23850 0.00294                       

# after we run an anova to see *if* there are differences, we need to figure out between which groups (tukey HSD):

tuke.trialb.index_ab <- TukeyHSD(aov(lm(Index_ab~Geno*Treatment, Trial_b))) 

sum.tuke.trialb.index_ab <- as.data.frame(tuke.trialb.index_ab[3])

# which ones have a p of less than 0.05?
sig.sum.tuke.trialb.index_ab <- as.data.frame(sum.tuke.trialb.index_ab[which(sum.tuke.trialb.index_ab$Geno.Treatment.p.adj<.05),])
# stomatal index AB
sig.sum.tuke.trialb.index_ab

# lyco-no vs hab-no
#hab-norm vs hab -no
# lyco-norm vs hab no
# hab norm vs lyco-no
# lyco-norm vs hab norm

# p-adjusted:
# Geno.Treatment.p.adj
# lycopersicum:00_norm-habrochaites:00_norm               3.378087e-11
# habrochaites:none-habrochaites:00_norm                  1.468079e-03
# lycopersicum:none-habrochaites:00_norm                  3.274965e-10
# habrochaites:none-lycopersicum:00_norm                  2.497813e-11
# lycopersicum:none-habrochaites:none                     2.498879e-11



# significance of stomata adaxial ----
# ANOVA
anova(lm(ad_stomata~Geno*Treatment, Trial_b)) 

# Response: ad_stomata
# Df Sum Sq Mean Sq F value  Pr(>F)   
# Geno            1  158.1  158.11  3.3983 0.06892 . 
# Treatment       1    1.1    1.07  0.0230 0.87979   
# Geno:Treatment  1  399.3  399.27  8.5815 0.00441 **
#   Residuals      81 3768.7   46.53

# same numbers in may 2017


tuke.trialb.stom_ad <- TukeyHSD(aov(lm(ad_stomata~Geno*Treatment, Trial_b))) 
sum.tuke.trialb.stom_ad <- as.data.frame(tuke.trialb.stom_ad[3])

# just to see the layout
tuke.trialb.stom_ad[3]

# to grab the p < 0.05
sig.sum.tuke.trialb.stom_ad <- as.data.frame(sum.tuke.trialb.stom_ad[which(sum.tuke.trialb.stom_ad$Geno.Treatment.p.adj<.05),])

# in Stomata, looks like only the [hab/no water to e6203/no water ] is significantly different
sig.sum.tuke.trialb.stom_ad

# and now in feb & may, it's still the lyco nowater vs hab nowater padj = 0.006274523

# found a problem: https://www.rdocumentation.org/packages/stats/versions/3.4.1/topics/TukeyHSD
# looks like in R, this test requires equal sample sizes. I can either drop down to 6 for everyone or try some sort of repeat/resample thing? ugh ugh ugh
# lsmeans stuff AD_stom ----
# https://stackoverflow.com/questions/12375209/multcomp-tukey-kramer
library(lsmeans)
abst <- as.numeric(Trial_b$ad_stomata) # in the example, they just input these as numeric
spp <- as.character(Trial_b$Geno) # or as character
watr <- as.character(Trial_b$Treatment)
model <- aov(abst ~ spp * watr)

set.seed(2) # makes no difference, unfortunately =/ 

defopt <- options()
options(contrasts=c("contr.sum", "contr.poly"))
print(drop1(aov(model),~.,test="F"))
options <- defopt

#For unbalanced data, pairwise comparisons of adjusted means may be used. Calculation in R [4]:
print(lsmeans(model, list(pairwise ~ spp)), adjust = c("tukey")) #  hab vs lyco p = 0.0804
print(lsmeans(model, list(pairwise ~ watr)), adjust = c("tukey")) # water vs drght p = 0.5551
print(lsmeans(model, list(pairwise ~ spp | watr)), adjust = c("tukey")) 
# water hab vs lyco p = 0.4208
# drght hab vs lyco p= 0.0012 
print(lsmeans(model, list(pairwise ~ watr | spp)), adjust = c("tukey"))
# hab wtr vs hab drght p = 0.0766
# lyco wtr vs lyco drght p = 0.0228


# still don't know how to get the drght hab vs wtr lyco and vice versa ???

# try making a category that splits them into 4 distinct categories?
Trial_b$test <- with(Trial_b, paste0(Treatment, Geno))
# worked


abst <- as.numeric(Trial_b$ad_stomata) # in the example, they just input these as numeric
spp <- as.character(Trial_b$Geno) # or as character
watr <- as.character(Trial_b$Treatment)
ugh <- as.character(Trial_b$test) # does not work

model <- aov(abst ~ ugh)

defopt <- options()
options(contrasts=c("contr.sum", "contr.poly"))
print(drop1(aov(model),~.,test="F"))
options <- defopt

print(lsmeans(model, list(pairwise ~ ugh)), adjust = c("tukey"))

# $`pairwise differences of contrast`
# contrast                                   estimate       SE df t.ratio p.value
# 00_normhabrochaites - 00_normlycopersicum -1.736715 2.146558 81  -0.809  0.8500
# 00_normhabrochaites - nonehabrochaites    -3.501672 1.952533 81  -1.793  0.2840
# 00_normhabrochaites - nonelycopersicum     3.541063 2.146558 81   1.650  0.3571
# 00_normlycopersicum - nonehabrochaites    -1.764957 2.091483 81  -0.844  0.8333
# 00_normlycopersicum - nonelycopersicum     5.277778 2.273681 81   2.321  0.1016
# nonehabrochaites - nonelycopersicum        7.042735 2.091483 81   3.367  0.0063
# these don't agree with the other ones... /sigh 

# significance of Pavement adax ----
anova(lm(ad_pave~Geno*Treatment, Trial_b)) 

# Response: ad_pave
# Df Sum Sq Mean Sq F value   Pr(>F)   
# Geno            1  14253 14253.2  9.5519 0.002738 **
#   Treatment       1  15301 15301.1 10.2542 0.001950 **
#   Geno:Treatment  1   7743  7742.6  5.1887 0.025369 * 
# Residuals      81 120867  1492.2                    


tuke.trialb.pav_ad <- TukeyHSD(aov(lm(ad_pave~Geno*Treatment, Trial_b))) 
sum.tuke.trialb.pav_ad <- as.data.frame(tuke.trialb.pav_ad[3])

tuke.trialb.pav_ad[3]
sig.sum.tuke.trialb.pav_ad <- as.data.frame(sum.tuke.trialb.pav_ad[which(sum.tuke.trialb.pav_ad$Geno.Treatment.p.adj<.05),])

sig.sum.tuke.trialb.pav_ad
# in Pavement, looks like 3 are significantly different:
# hab norm vs e6203 no water, #1
# hab norm vs hab no water, #2 
# hab norm vs e6203 norm #3

# and in feb (&may):
# hab-norm vs hab-nowater #2
# hab-norm vs lyco-nowater # 1
# lyco-norm vs hab-norm #3


# significance of stomata AB ----
# ANOVA
anova(lm(ab_stomata~Geno*Treatment, Trial_b)) 

# Response: ab_stomata
# Df Sum Sq Mean Sq  F value  Pr(>F)    
# Geno            1 112075  112075 289.5325 < 2e-16 ***
#   Treatment       1   2580    2580   6.6654 0.01163 *  
#   Geno:Treatment  1    362     362   0.9341 0.33668    
# Residuals      81  31354     387           

tuke.trialb.stom_ab <- TukeyHSD(aov(lm(ab_stomata~Geno*Treatment, Trial_b))) 
sum.tuke.trialb.stom_ab <- as.data.frame(tuke.trialb.stom_ab[3])

# to grab the p < 0.05
sig.sum.tuke.trialb.stom_ab <- as.data.frame(sum.tuke.trialb.stom_ab[which(sum.tuke.trialb.stom_ab$Geno.Treatment.p.adj<.05),])

# in AB Stomata 
sig.sum.tuke.trialb.stom_ab

# 
# lyco:No Water - hab:No Water                   -69.75641          -85.58112
# lyco:Normal  - hab:No Water               -63.53419          -79.35890
# hab:Normal-lyco:No Water                84.33333           68.09191
# lyco:Normal - hab:Normal            -78.11111          -94.35253
# 
#Geno.Treatment.upr Geno.Treatment.p.adj
# lycopersicum:No Water-habrochaites:No Water                  -53.93170         2.498413e-11
# lycopersicum:Normal Water-habrochaites:No Water              -47.70948         2.500078e-11
# habrochaites:Normal Water-lycopersicum:No Water              100.57476         2.497258e-11
# lycopersicum:Normal Water-habrochaites:Normal Water          -61.86969         2.497458e-11


# significance of Pavement AB ----
anova(lm(ab_pave~Geno*Treatment, Trial_b)) 

# Response: ab_pave
# Df Sum Sq Mean Sq  F value    Pr(>F)    
# Geno            1 231912  231912 135.1481 < 2.2e-16 ***
#   Treatment       1  56472   56472  32.9094  1.61e-07 ***
#   Geno:Treatment  1   8755    8755   5.1022   0.02658 *  
#   Residuals      81 138995    1716                                       


tuke.trialb.pav_ab <- TukeyHSD(aov(lm(ab_pave~Geno*Treatment, Trial_b))) 
sum.tuke.trialb.pav_ab <- as.data.frame(tuke.trialb.pav_ab[3])

sig.sum.tuke.trialb.pav_ab <- as.data.frame(sum.tuke.trialb.pav_ab[which(sum.tuke.trialb.pav_ab$Geno.Treatment.p.adj<.05),])
# Pave AB...
sig.sum.tuke.trialb.pav_ab
# lyco-no vs hab-no
# hab-norm vs hab-no
# lyco-norm vs hab-no
# hab-norm vs lyco-no
# lyco-norm vs hab norm


# Geno.Treatment.upr Geno.Treatment.p.adj
# lycopersicum:No Water-habrochaites:No Water                  -53.95055         7.180740e-09
# habrochaites:Normal Water-habrochaites:No Water              100.16197         6.638177e-07
# lycopersicum:Normal Water-habrochaites:No Water              -26.00611         6.863404e-05
# habrochaites:Normal Water-lycopersicum:No Water              190.52215         2.497902e-11
# lycopersicum:Normal Water-habrochaites:Normal Water          -94.18558         2.501832e-11

# _______________________________________________________________

# Back to graphing ----

# bar ad Pave w significance adaxial (old) ----
# 
# s <- ggplot(Trial_b, aes(reorder(Geno,ad_pave, mean), ad_pave, fill = Treatment))+
#   theme_bw()+
#   stat_summary(fun.data=plot.summary, geom = "bar", width=.5,
#                position = position_dodge(.6))+
#   stat_summary(fun.data=plot.summary, geom = "errorbar", 
#                width=.1, position = position_dodge(.6))+
#   stat_summary(fun.data=plot.summary,
#                geom = "text", 
#                aes( x = Geno, 
#                     y = (ad_pave+25),
#                     label = pave_sign_ad),
#                position = position_dodge(.6),
#                color = "black",
#                size = 6.5)+
#   scale_fill_manual(values= c( "darkblue", "darkgoldenrod3"),
#                     breaks = c("A Normal Watering", "No Water"),
#                     labels = c("Normal Watering", "No Water"))+
#   theme(legend.position = "none",
#         plot.title = element_text(size = 22, face="bold", hjust = .8),
#         axis.title.x = element_text(face="bold", size=14),
#         axis.title.y = element_text(face="bold", size=14) )+
#   ggtitle("Adaxial Pavement Density")+
#   ylab("Average Adaxial Pavement Cells")+
#   xlab("Genotypes")

# bar graph AB pave w sig. (check stats; otherwise done) r ----
r <- ggplot(Trial_b, aes(reorder(Geno,ab_pave, mean), ab_pave, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5,
               position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", 
               width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = Geno, 
                    y = (ab_pave+30),
                    label = pave_sign_ab),
               position = position_dodge(.6),
               color = "black",
               size = 10)+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes(y = (ab_pave-100),
                   label = n),
               position = position_dodge(.6),
               color = "white",
               size = 8)+
  scale_fill_manual(values= c( "darkblue", "darkgoldenrod3"),
                    breaks = c("00_norm", "none"),
                    labels = c("Normal Watering", "No Water"))+
  theme(legend.position = "none",
          #c(.18,.88),
              legend.direction="vertical",
              legend.title=element_blank(),
              legend.key.size = unit(1.5, "cm"),
              legend.text = element_text(size = 25),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
              axis.title = element_text(size = 25, face = "bold"),
              axis.text = element_text(size = 25),
              axis.text.x = element_text(face = "italic"),
              axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 1)))+
  ggtitle("Abaxial Pavement Cell Density\n")+
  ylab("Average Number of\nPavement Cells")+
  xlab("")+
  scale_x_discrete(breaks = c("habrochaites", "lycopersicum"),
                   labels = c("S. habrochaites","S. lycopersicum"))
# ??? check stats on lyco water vs no water ab pave ???
# chp1_AB_pave_bar_drought.jpeg ratio 1500 - done 


# plotting all 4 bar graphs of ab/ad stom/pave density ----
test <- plot_grid(p, q, r,s, labels = c("A","B","C","D"), label_size = 30, label_colour = "midnightblue")
?plot_grid
save_plot("test.png", test, ncol = 2, nrow = 2, base_height =  8 , base_width = 11)
save_plot("test1.png", test, ncol = 2, nrow = 2, base_height = NULL, base_aspect_ratio = 1.375, base_width = 11) # you have to write "null" for it to respect the aspect ratio. fuck you r.
save_plot("cph1_fig8_ab-ad-pave-stom.png", test, ncol = 2, nrow = 2, base_height = NULL, base_aspect_ratio = 1.375, base_width = 8)

# plotting both indecies -----
indecies <- plot_grid(bar_ind_ab, bar_ind_ad, labels = c("A","B"), label_size = 30, label_colour = "midnightblue")
save_plot("cph1_fig9_indicies.png", indecies, ncol = 2, nrow = 1, base_height = NULL, base_aspect_ratio = 1.375, base_width = 8)

# bar AB Stom w significance (done) p ----
p <- ggplot(Trial_b, aes(reorder(Geno,ab_stomata, mean), ab_stomata, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = Geno, 
                    y = (ab_stomata+14),
                    label = stom_sign_ab),
               position = position_dodge(.6),
               color = "black",
               size = 10)+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes(y = (ab_stomata-34),
                   label = n),
               position = position_dodge(.6),
               color = "white",
               size = 8)+
  scale_fill_manual(values= c( "darkblue", "darkgoldenrod3"),
                    breaks = c("00_norm", "none"),
                    labels = c("Normal Watering", "No Water"))+
  theme(legend.position = c(.27,.84),
        legend.direction="vertical",
        legend.title=element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        axis.text.x = element_text(face = "italic"),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 1)))+
  ggtitle("Abaxial Stomatal Density\n")+
  ylab("Average Abaxial \n Stomatal Cells\n")+
  xlab("")+
  scale_x_discrete(breaks = c("habrochaites", "lycopersicum"),
                   labels = c("S. habrochaites","S. lycopersicum"))

# chp1_AB_stom_bar_drought.jpeg ratio 1500 - done 

# bar AD Stom w significance adax ----
q <- ggplot(Trial_b, aes(reorder(Geno,ad_stomata, mean), ad_stomata, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = Geno, 
                    y = (ad_stomata+2.8),
                    label = stom_sign_ad),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  scale_fill_manual(values= c( "darkblue", "darkgoldenrod3"),
                    breaks = c("A Normal Watering", "No Water"),
                    labels = c("Normal Watering", "No Water"))+
  theme(legend.position = "none",
        plot.title = element_text(size = 20, face="bold", hjust = 0.7),
        axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14) )+
  ggtitle("Adaxial Stomatal Density")+
  ylab("Average Adaxial Stomatal Cells")+
  xlab("Genotypes")



# bar AD Index w significance ----

ggplot(Trial_b, aes(reorder(Geno,AD_Index, mean), AD_Index, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = Geno, 
                    y = (AD_Index+.018),
                    label = index_ad_sign2),
               position = position_dodge(.6),
               color = "black",
               size = 6.5)+
  scale_fill_manual(values= c( "darkblue", "darkgoldenrod3"),
                    breaks = c("A Normal Watering", "No Water"),
                    labels = c("Normal Watering", "No Water"))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 20, face="bold", hjust = 0.7),
        axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14))+
  ggtitle("Adaxial Stomatal Index")+
  ylab("Adaxial Stomatal Cells /\n Pavement Cells")+
  xlab("Species")

# bar AB Index w significance (done) bar_ind_ab ----

bar_ind_ab <- ggplot(Trial_b, aes(reorder(Geno,AB_Index, mean), AB_Index, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = Geno, 
                    y = (AB_Index+.05),
                    label = index_sign_ab),
               position = position_dodge(.6),
               color = "black",
               size = 10)+
  scale_fill_manual(values= c( "darkblue", "darkgoldenrod3"),
                    breaks = c("00_norm", "none"),
                    labels = c("Normal Watering", "No Water"))+
  theme(legend.position = c(.27,.85),
        legend.direction="vertical",
        legend.title=element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        axis.text.x = element_text(face = "italic"),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 1)))+
  scale_x_discrete(breaks = c("habrochaites", "lycopersicum"),
                   labels = c("S. habrochaites","S. lycopersicum"))+
  ggtitle("Abaxial Stomatal Index\n")+
  ylab("Stomata/ \nPavement Cells\n")+
  xlab("")

# chp1_AB_index_bar_drought.jpeg ratio 1500 - done 

# # error: removed 55 rows containing non-finite values (stat_summary)
# 
# head(Trial_b)
# Trial_b$Index_ab # look good
# class(Trial_b$Index_ab) # knows it's numeric
# 
# Trial_b$AB_Index # lots of NA's ><
# Trial_b$AB_Index <- na.omit(Trial_b$AB_Index) # error b/c it only has 30 rows vs whole-data of 85 rows /sigh. going to fix it in excel >< FIXED
# class(Trial_b$AB_Index) # knows it's numeric

# bars with y axis limits ----

# AB index
ggplot(Trial_b, aes(reorder(Geno,AB_Index, mean), AB_Index, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = Geno, 
                    y = (AB_Index+.05),
                    label = index_sign_ab),
               position = position_dodge(.6),
               color = "black",
               size = 6.5)+
  scale_fill_manual(values= c( "darkblue", "darkgoldenrod3"),
                    breaks = c("A Normal Watering", "No Water"),
                    labels = c("Normal Watering", "No Water"))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 20, face="bold", hjust = 0.7),
        axis.title.x = element_text(face="bold", size=13),
        axis.title.y = element_text(face="bold", size=13))+
  ggtitle("Abaxial Stomatal Index     ")+
  ylab("Abaxial Stomatal Cells / Pavement Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, .53)) 

### AD index
ggplot(Trial_b, aes(reorder(Geno,AD_Index, mean), AD_Index, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = Geno, 
                    y = (AD_Index+.018),
                    label = index_sign_ad),
               position = position_dodge(.6),
               color = "black",
               size = 6.5)+
  scale_fill_manual(values= c( "darkblue", "darkgoldenrod3"),
                    breaks = c("A Normal Watering", "No Water"),
                    labels = c("Normal Watering", "No Water"))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 20, face="bold", hjust = 0.7),
        axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14))+
  ggtitle("Adaxial Stomatal Index")+
  ylab("Adaxial Stomatal Cells / Pavement Cells")+
  xlab("Species")
#+
 # coord_cartesian(ylim = c(0, .53)) 

### ad pave
ggplot(Trial_b, aes(reorder(Geno,ad_pave, mean), ad_pave, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5,
               position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", 
               width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = Geno, 
                    y = (ad_pave+29),
                    label = pave_sign_ad),
               position = position_dodge(.6),
               color = "black",
               size = 6.5)+
  scale_fill_manual(values= c( "darkblue", "darkgoldenrod3"),
                    breaks = c("A Normal Watering", "No Water"),
                    labels = c("Normal Watering", "No Water"))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 22, face="bold", hjust = .8),
        axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14) )+
  ggtitle("Adaxial Pavement Density     ")+
  ylab("Average Adaxial Pavement Cells")+
  xlab("Genotypes")
#+
 # coord_cartesian(ylim = c(0, 330)) 

### AB pave
ggplot(Trial_b, aes(reorder(Geno,ab_pave, mean), ab_pave, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5,
               position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", 
               width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = Geno, 
                    y = (ab_pave+35),
                    label = pave_sign_ab),
               position = position_dodge(.6),
               color = "black",
               size = 6.5)+
  scale_fill_manual(values= c( "darkblue", "darkgoldenrod3"),
                    breaks = c("A Normal Watering", "No Water"),
                    labels = c("Normal Watering", "No Water"))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 20, face="bold", hjust = .8),
        axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14) )+
  ggtitle("Abaxial Pavement Density")+
  ylab("Average Abaxial Pavement Cells")+
  xlab("Genotypes")
#+
 # coord_cartesian(ylim = c(0, 330)) 

### AB stomata
ggplot(Trial_b, aes(reorder(Geno,ab_stomata, mean), ab_stomata, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = Geno, 
                    y = (ab_stomata+15),
                    label = stom_sign_ab),
               position = position_dodge(.6),
               color = "black",
               size = 6.5)+
  scale_fill_manual(values= c( "darkblue", "darkgoldenrod3"),
                    breaks = c("A Normal Watering", "No Water"),
                    labels = c("Normal Watering", "No Water"))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 22, face="bold", hjust = 0.7),
        axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14) )+
  ggtitle("Abaxial Stomatal Density")+
  ylab("Average Abaxial Stomatal Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 140)) 

### AD Stom 
ggplot(Trial_b, aes(reorder(Geno,ad_stomata, mean), ad_stomata, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = Geno, 
                    y = (ad_stomata+3.5),
                    label = stom_sign_ad),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  scale_fill_manual(values= c( "darkblue", "darkgoldenrod3"),
                    breaks = c("A Normal Watering", "No Water"),
                    labels = c("Normal Watering", "No Water"))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 20, face="bold", hjust = 0.7),
        axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14) )+
  ggtitle("Adaxial Stomatal Density         ")+
  ylab("Average Adaxial Stomatal Cells")+
  xlab("Genotypes") 
#+
 # coord_cartesian(ylim = c(0, 140)) 
  



# LI-COR data begins ----

# load libraries
library(ggplot2)
library(cowplot)
library(dplyr)
library(lme4)
library(lmerTest)
# input licor files ----
daw_dec292015_mary_trimmed <- read.csv("~/Desktop/Sinha_Lab_stuff/hab-epidermis/Data/daw_dec292015_mary_trimmed.csv")

licor <- daw_dec292015_mary_trimmed
head(licor)
licor1 <- read.csv("~/Desktop/Sinha_Lab_stuff/hab-epidermis/Data/daw_jan132016_mary_trimmed1.csv")
licor_all <- read.csv("~/Desktop/Sinha_Lab_stuff/hab-epidermis/Data/daw_amalgamation_mary_trimmed.csv")

# rounding for usefulness later
licor$Tleaf <- round(licor$Tleaf)
licor1$Tleaf <- round(licor1$Tleaf)

# peaking at licor treatments ----
ggplot(licor, aes( Geno, Photo, colour = Treat))+
  geom_jitter()+
  xlab("Genotype")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Species Comparison")+
  scale_color_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c( "darkblue", "goldenrod3")
  )


# boxplot photo vs genotype by treatment final ----
ggplot(licor, aes( Geno, Photo, colour = Treat))+
  geom_boxplot()+
  xlab("Genotype")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Photosynthesis During Treatment")+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 20, face="bold"),
        axis.title.x = element_text(face="bold", size=15),
        axis.title.y = element_text(face="bold", size=15))+
  scale_color_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c("darkblue", "darkgoldenrod3")
  )+
  scale_y_continuous(limits=c(-3,12))+
  scale_x_discrete(breaks=c("e6203", "hab"),
                   labels=c("Domesticated", "Wild"))
####################################################################################
library(ggpubr) ----

head(ToothGrowth)
#Compare two independent groups
compare_means(len ~ supp, data = ToothGrowth)

p <- ggboxplot(ToothGrowth, x = "supp", y = "len",
               color = "supp", palette = "jco",
               add = "jitter")

p +stat_compare_means(method = "t.test")
p+stat_compare_means()
p+ stat_compare_means( aes(label = ..p.signif..), 
                       label.x = 1.5, label.y = 40)
p+stat_compare_means( label = "p.signif", label.x = 1.5, label.y = 40)

#Compare two paired samples
compare_means(len ~ supp, data = ToothGrowth, paired = TRUE)

j<-ggboxplot(ToothGrowth, x = "supp", y = "len",
             color = "supp", palette = "jco",
             add = "jitter")
j+stat_compare_means()
j+stat_compare_means(method = "t.test")

# different ways to display the p value on the graph:
j+stat_compare_means(aes(label = ..p.format..) )
j+stat_compare_means(aes(label = ..p.signif..))
j+stat_compare_means(aes(label = paste0(..method.., "\n", "p =", ..p.format..)))

j + stat_compare_means( aes(label = ..p.signif..), 
                        label.x = 1.5, label.y = 40) # can give it coordinates

j+stat_compare_means( label = "p.signif", label.x = 1.5, label.y = 40) # same thing, just slightly different context.


#Notes: Compare more than two groups ----
compare_means(len ~ dose,  data = ToothGrowth, method = "anova")

# Default method = "kruskal.test" for multiple groups
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "jco")+
  stat_compare_means()
# Change method to anova ----
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "jco")+
  stat_compare_means(method = "anova")

# Perorm pairwise comparisons
compare_means(len ~ dose,  data = ToothGrowth)

# their example of pairwise comparisons: ----
my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)     # Add global p-value

# try to apply it to my data?
# after recovery photo w ttest stats -----
compare_means(Photo ~ Treat,  data = licor1)
# don't think this splits up genotypes...

ggboxplot(licor1, x = "Treat", y = "Photo",
          color = "Treat", palette = "jco")+
  stat_compare_means(method = "t.test")+
  facet_wrap(~Geno, labeller = labeller(Geno = c("e6203" = "lycopersicum", "hab" = "habrochaites")))+
  scale_color_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c("darkblue", "darkgoldenrod3")
  )+
  scale_x_discrete(label = c("",""))+
  xlab("")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Photosynthesis After Recovery")+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18, face="bold",  hjust = 0.5),
        axis.title.y = element_text(face="bold", size=15),
        axis.ticks = element_blank(),
        strip.background = element_rect(colour="white", fill="#FFFFFF"),
        strip.text.x = element_text(size = 12, face = "bold"))

# how the f am I graphing the T-test on there???


# during with stats boxplot ----  
compare_means(Photo ~ Treat,  data = licor, "t.test")
ggboxplot(licor, x = "Treat", y = "Photo",
          color = "Treat", palette = "jco")+
  stat_compare_means(method = "t.test")+
  facet_wrap(~Geno, labeller = labeller(Geno = c("e6203" = "lycopersicum", "hab" = "habrochaites")))+
  scale_color_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c("darkblue", "darkgoldenrod3")
  )+
  scale_x_discrete(label = c("",""))+
  xlab("")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Photosynthesis During Treatment")+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18, face="bold", hjust = 0.5),
        axis.title.y = element_text(face="bold", size=15),
        axis.ticks = element_blank(),
        strip.background = element_rect(colour="white", fill="#FFFFFF"),
        strip.text.x = element_text(size = 12, face = "bold"))




#AD stom index w/ Compare more than two groups ----
compare_means(AD_Index ~ Treatment*Geno,  data = Trial_b, method = "anova")
# thinks it's NS O_o wat?!
head(Trial_b)

Trialb_comparisons <- list( c("a", "b"), c("a", "c"), c("b", "c"), c("a","d"), c("b","d"), c("c","d"))
ggboxplot(Trial_b, x = "btr_lblz", y = "AD_Index",
          color = "Treatment", palette = "jco")+
  stat_compare_means(comparisons = Trialb_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = .3)+
  scale_x_discrete(breaks=c("a", "b", "c","d"),
                   labels=c("lycopersicum", "lycopersicum", "habrochaites", "habrochaites"))


####################################################################################


# after recovery final graph ----
ggplot(licor1, aes( Geno, Photo, colour = Treat))+
  geom_boxplot()+
  xlab("Genotype")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Photosynthesis After Recovery")+
  scale_color_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c("darkblue", "darkgoldenrod3"))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 20, face="bold"),
        axis.title.x = element_text(face="bold", size=15),
        axis.title.y = element_text(face="bold", size=15))+
  scale_y_continuous(limits=c(-3,12))+
  scale_x_discrete(breaks=c("e6203", "hab"),
                   labels=c("lycopersicum", "habrochaites"))


# licor significance of photo by gene & treat ----
anova(lm(Photo~Geno*Treat, licor)) 

# looks like treatment doesn't interact with genotype; however, geno and treat are sign different.

library(lmerTest) # without this, you don't get the following code to work. dunno why. also, only works with * instead of +

tuke.photo <- TukeyHSD(aov(lm(Photo~Geno*Treat, licor))) 
sum.tuke.photo <- as.data.frame(tuke.photo[3])

# all results:
tuke.photo[3]
# significant results:
sig.sum.tuke.photo <- as.data.frame(sum.tuke.photo[which(sum.tuke.photo$Geno.Treat.p.adj<.05),])
sig.sum.tuke.photo

# so this tells us that hab control / e62 control are NOT different; hab drought / e62 drought ARE different; best day ever.

# licor stats photo after recovery: ----
anova(lm(Photo~Geno*Treat, licor1)) 

# looks like NS

library(lmerTest) # without this, you don't get the following code to work. dunno why. also, only works with * instead of + (in july, this ran fine without lmertest /shrug)

tuke.photo.after <- TukeyHSD(aov(lm(Photo~Geno*Treat, licor1))) 
sum.tuke.photo.after <- as.data.frame(tuke.photo.after[3])

# all results:
tuke.photo.after[3]
# significant results:
sig.sum.tuke.photo <- as.data.frame(sum.tuke.photo[which(sum.tuke.photo$Geno.Treat.p.adj<.05),])
sig.sum.tuke.photo

# e drought/e cont, hab dro/e cont, e dro / hab cont, hab dro/hab cont, hab dro/e dro - perfect


# licor tleaf photo treat geno final ----
# during stress
ggplot(licor, aes( Tleaf, Photo, colour = Treat, shape = Geno))+
  geom_jitter(size = 3.2)+
  xlab("Leaf Temperature(C)")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Photosynthesis & Leaf Temperature \nDuring Treatment")+
  theme(legend.position = "bottom",
              plot.title = element_text(size = 20, face="bold"),
              axis.title.x = element_text(face="bold", size=15),
              axis.title.y = element_text(face="bold", size=15))+
  scale_color_manual(name = "Treatment",
                     labels = c("Normal Water", "No Water"),
                     values = c("darkblue", "darkgoldenrod3"))+
  scale_shape_manual(name = "Genotype",
                     labels = c("lycopersicum", "habrochaites"),
                     values = c(16, 17))+
  scale_y_continuous(limits=c(-3,12))



# very nice that it separates by treatment exclusively in the FACE OF GENOTYPE~~ bwahaha


# what about after 'recovery'? final 
ggplot(licor1, aes( Tleaf, Photo, colour = Treat, shape = Geno))+
  geom_jitter(size = 3.2)+
  xlab("Leaf Temperature(C)")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Photosynthesis & Leaf Temperature \nAfter Recovery")+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 20, face="bold"),
        axis.title.x = element_text(face="bold", size=15),
        axis.title.y = element_text(face="bold", size=15))+
  scale_color_manual(name = "Treatment",
                     labels = c("Normal Water", "No Water"),
                     values = c("darkblue", "darkgoldenrod3"))+
  scale_shape_manual(name = "Genotype",
                     labels = c("Domesticated", "Wild"),
                     values = c(16, 17))+
  scale_y_continuous(limits=c(-3,12))


# nice leaf temp vs treat vs spp vs photo
ggplot(licor_all, aes( Tleaf, Photo, colour = Treat))+
  geom_jitter(size = 2.5)+
  facet_grid(~Geno)+
  xlab("Leaf temp (C)")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Leaf Temp Trial B")+
  scale_color_manual(name = "Treatment", values = c("lightslateblue", "deeppink"))

licor$Tleaf

# after recovery
ggplot(licor1, aes( Tleaf, Photo, colour = Treat))+
  geom_jitter()+
  facet_grid(~Geno)+
  xlab("Leaf temp (C)")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Hab Drought Trial B")+
  scale_color_manual(name = "Treatment", values = c("lightslateblue", "deeppink"))


# let's say we want to ask: Is hab photosynthesizing better under drought than e6203?

# box plots grouped by stress 
ggplot(licor, aes(Geno, Photo, colour = Geno))+
  geom_boxplot()+
  facet_grid(~Treat)+
  scale_color_manual(values = c("seagreen3","plum"))+
  ggtitle("Photosynthetic Rates under Stress (Trial B)")+
  xlab("")+
  ylab("Photosynthetic Rate (A)")+
  theme(legend.position = "none",
        plot.title = element_text(size = 22, face="bold"))

# during, within treat, between spp; box plot with t.test ----
compare_means(Photo ~ Geno,  data = licor, "t.test")
# NS norm water; sign under no water
ggboxplot(licor, x = "Geno", y = "Photo",
          color = "Treat", palette = "jco")+
  stat_compare_means(method = "t.test")+
  facet_wrap(~Treat, labeller = labeller(Treat = c("control" = " ", "drought" = " ")))+
  scale_color_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c("darkblue", "darkgoldenrod3")
  )+
  scale_x_discrete(label = c("lycopersicum","habrochaites"))+
  xlab("")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Photosynthesis During Treatment")+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18, face="bold", hjust = 0.5),
        axis.title.y = element_text(face="bold", size=15),
        axis.ticks = element_blank(),
        strip.background = element_rect(colour="white", fill="#FFFFFF"),
        strip.text.x = element_text(size = 12, face = "bold"))

# old graph of recovered (A) faceted by treatment
ggplot(licor1, aes(Geno, Photo, colour = Geno))+
  geom_boxplot()+
  facet_grid(~Treat)+
  scale_color_manual(values = c("seagreen3","plum"))+
  ggtitle("Photosynthetic Rates Recovery (Trial B)")+
  xlab("")+
  ylab("Photosynthetic Rate (A)")+
  theme(legend.position = "none",
        plot.title = element_text(size = 22, face="bold"))
# recovered, within treat, between spp; box plot with t.test ---- 
ggboxplot(licor1, x = "Geno", y = "Photo",
          color = "Treat", palette = "jco")+
  stat_compare_means(method = "t.test")+
  facet_wrap(~Treat, labeller = labeller(Treat = c("control" = " ", "drought" = " ")))+
  scale_color_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c("darkblue", "darkgoldenrod3")
  )+
  scale_x_discrete(label = c("lycopersicum","habrochaites"))+
  xlab("")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Photosynthesis After Recovery")+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18, face="bold", hjust = 0.5),
        axis.title.y = element_text(face="bold", size=15),
        axis.ticks = element_blank(),
        strip.background = element_rect(colour="white", fill="#FFFFFF"),
        strip.text.x = element_text(size = 12, face = "bold"))



# Mary also looked at leaf numbers

# num leaves after vs photo : ----
ggplot(licor_all, aes(num_leaves_after, Photo_after, colour = Treat))+
  geom_jitter()+
  facet_grid(~Geno)+
  scale_color_manual(name = "Treatment", values = c("lightslateblue", "deeppink"))+
  stat_smooth(method = "lm")+
  xlab("Number of Leaves at Rescue")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("LICOR Data from Hab Stomatal Density under Stress (Trial B)")+
  theme(plot.title = element_text(size = 17.2, face="bold"))

# new plot w ttest w num leaves ----
ggboxplot(licor_all, x = "Treat", y = "num_leaves_after",
          color = "Treat", palette = "jco")+
  stat_compare_means(method = "t.test")+
  facet_wrap(~Geno)+
  scale_color_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c("darkblue", "darkgoldenrod3")
  )+
  scale_x_discrete(label = c("Normal Water","No Water"))+
  xlab("Treatments")+
  ylab("Number of Leaves")+
  ggtitle("Expanded Leaves After Recovery")+
  theme(legend.position = "none",
        plot.title = element_text(size = 18, face="bold", hjust = 0.5),
        axis.title.y = element_text(face="bold", size=15),
        axis.title.x = element_text(face="bold", size=15),
        axis.ticks = element_blank(),
        strip.background = element_rect(colour="white", fill="#FFFFFF"),
        strip.text.x = element_text(size = 12, face = "bold"))

# so it looks like hab puts out fewer leaves because of the drought treatment, but photosynthesis doesn't suffer!

ggboxplot(licor_all, x = "Treat", y = "num_leaves",
          color = "Treat", palette = "jco")+
  stat_compare_means(method = "t.test")+
  facet_wrap(~Geno)+
  scale_color_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c("darkblue", "darkgoldenrod3")
  )+
  scale_x_discrete(label = c("Normal Water","No Water"))+
  xlab("Treatments")+
  ylab("Number of Leaves")+
  ggtitle("Expanded Leaves at End of Treatment")+
  theme(legend.position = "none",
        plot.title = element_text(size = 18, face="bold", hjust = 0.5),
        axis.title.y = element_text(face="bold", size=15),
        axis.title.x = element_text(face="bold", size=15),
        axis.ticks = element_blank(),
        strip.background = element_rect(colour="white", fill="#FFFFFF"),
        strip.text.x = element_text(size = 12, face = "bold"))

# but both had reduced expanded leaves at the end of the stress treatment (we didn't measure them before onset of stress... we forgot)

# difference in leaf num btwn spp? ----
ggboxplot(licor_all, x = "Geno", y = "num_leaves",
          color = "Treat", palette = "jco")+
  stat_compare_means(method = "t.test")+
  facet_wrap(~Treat)+
  scale_color_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c("darkblue", "darkgoldenrod3")
  )
# not during stress treatment

# diff in leaf num btwn spp after? ----
ggboxplot(licor_all, x = "Geno", y = "num_leaves_after",
          color = "Treat", palette = "jco")+
  stat_compare_means(method = "t.test")+
  facet_wrap(~Treat)+
  scale_color_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c("darkblue", "darkgoldenrod3")
  )
# nope - no differences


# trying a difference perspective ----

licor_all %>% 
  group_by(Geno, Treat) %>% 
  ggboxplot(., x = "Geno", y = "num_leaves_after", color = "Treat", palette = "jco")+
  stat_compare_means(comparisons = my_comparisons, method = "t.test")+
  stat_compare_means(label.y = 30)

my_comparisons <- list( c("control", "drought"), c("e6203", "hab"))
  
  
    
licor_all %>% 
  group_by(Geno, Treat) %>% 
  compare_means(Photo ~ Treat,  data = .)


# conductance vs leaf temp & photo ----
ggplot(licor_all, aes(Cond, Photo, colour = Tleaf))+
  geom_jitter(size = 3.5)+
  facet_grid(Treat~Geno)+
  scale_colour_gradient( low="yellow", high = "darkorchid", space = "Lab")+
  xlab("Leaflet Water Conductance")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Interactions of Conductance, Photosynthesis, & Leaflet Surface Temperature")+
  theme(plot.title = element_text(size = 15.2, face="bold"))

# conductance to CO2
ggplot(licor_all, aes(y = CndCO2,x = Treat, colour = Geno))+
  geom_jitter(size = 3.5)+
  facet_grid(Treat~Geno)+
  #scale_colour_gradient( low="yellow", high = "darkorchid", space = "Lab")+
  xlab("Leaflet Water Conductance")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Interactions of Conductance, Photosynthesis, & Leaflet Surface Temperature")+
  theme(plot.title = element_text(size = 15.2, face="bold"))

# 11/7 steven says Cond is better than CndCO2 since CndCO2 is already a calculation

ggplot(licor_all, aes(y = Cond,x = Treat, colour = Geno))+
  geom_jitter(size = 3.5)+
  facet_grid(Treat~Geno)+
  #scale_colour_gradient( low="yellow", high = "darkorchid", space = "Lab")+
  ylab("H2O mmol/m2/s (gst)")+
  xlab("Treatment")+
  ggtitle("Leaflet Water Conductance")+
  theme(plot.title = element_text(size = 15.2, face="bold"))

# to do: mult conductance by # of stomata (in 2cm squared area from our licor head measurements)

head(licor_all)

# conductance vs photo ----
ggplot(licor_all, aes(Cond, Photo, colour = Tleaf))+
  geom_jitter(size = 3.5)+
  facet_grid(Treat~Geno, labeller = label_value)+ # this maintains the default labels..
  scale_colour_gradient( low="yellow", high = "darkorchid", space = "Lab")+
  xlab("Leaflet Water Conductance")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Interactions of Conductance, Photosynthesis, & Leaflet Surface Temperature")+
  theme(plot.title = element_text(size = 15.2, face="bold"))

head(licor_all)
# CO2 internal ----
ggplot(licor_all, aes(y = Ci, x = Geno, fill = Treat))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c( "darkgoldenrod3", "darkblue"))+
  theme(legend.position = c(.1,.8))+
  ggtitle("Measured Internal CO2")+
  ylab("Internal CO2")+
  xlab("Genotypes")

# PhiPS2 bar ----
ggplot(licor_all, aes(y = PhiPS2, x = Geno, fill = Treat))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c( "darkgoldenrod3", "darkblue"))+
  theme(legend.position = c(.1,.93))+
  ggtitle("Electrons Moving to Photosytem II")+
  ylab("PhiPS2")+
  xlab("Genotypes")
#  PhiCO2 ----
ggplot(licor_all, aes(y = PhiCO2, x = Geno, fill = Treat))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c( "darkgoldenrod3", "darkblue"))+
  theme(legend.position = c(.9,.8))+
  ggtitle("Carbon Fixation Rate")+
  ylab("PhiCO2")+
  xlab("Genotypes")

# PhiCOs vs PhiPS2
ggplot(licor_all, aes(y = PhiCO2, x = PhiPS2, colour = Treat))+
  geom_jitter(size = 3.5)+
  stat_smooth(method = "lm")+
  facet_grid(Treat~Geno)+
  scale_color_manual(name = "Treatment", values = c( "darkgoldenrod3", "darkblue"))+
  ggtitle("Fixation of Carbon vs Photosystem II Photon Accumulation")+
  theme(plot.title = element_text(size = 15.2, face="bold"))

# e6203 is more efficient under control, but hab is kicking butt under drought. in e6203, doesn't matter how many photons are hanging out, there's no more fixation (indicating H2O is the only limiting factor)


# transpiration  ----
ggplot(licor_all, aes(y = Trans, x = Geno, fill = Treat))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c( "darkgoldenrod3", "darkblue"))+
  theme(legend.position = c(.9,.8))+
  ggtitle("Transpiration Rates Under Stress")+
  ylab("Transpiration Rate (moles per second)")+
  xlab("Genotypes")

# in millimoles
ggplot(licor_all, aes(y = Trmmol, x = Geno, fill = Treat))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c( "darkgoldenrod3", "darkblue"))+
  theme(legend.position = c(.9,.8))+
  ggtitle("Transpiration Rates Under Stress")+
  ylab("Transpiration Rate (millimoles per second)")+
  xlab("Genotypes")



# tranpiration vs photo
ggplot(licor_all, aes(y = Photo, x = Trans, colour = Treat))+
  geom_jitter(size = 3.5)+
  facet_grid(Treat~Geno)+
  scale_color_manual(name = "Treatment", values = c( "darkgoldenrod3", "darkblue"))+
  xlab("Transpiration Rate (molH2O/)")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Transpiration vs Photosynthesis")+
  theme(plot.title = element_text(size = 15.2, face="bold"))

# photosynthesis bar
ggplot(licor_all, aes(y = Photo, x = Geno, fill = Treat))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c( "darkgoldenrod3", "darkblue"))+
  theme(legend.position = c(.9,.8))+
  ggtitle("Photosynthetic Rates Under Stress")+
  ylab("Photosynthetic Rate (A)")+
  xlab("Genotypes")


## 4/12/2016 pondering other measured parameters 
# average stom & pave per plant ----

avg_plant <- Trial_b %>% 
  group_by(Plant.ID) %>% 
  summarise(avg_pave_plnt = mean(Pavement),
            var_pave = var(Pavement), 
            avg_stom_plnt = mean(Stomata),
            var_stom = var(Stomata)
            )
# what to merge by..
head(licor_all)
licor_all$plant.ID
head(avg_plant)
avg_plant$Plant.ID
# merging data sets

all_obs <- merge(x = licor_all, y = avg_plant, by.x = 'plant.ID', by.y = 'Plant.ID', drop = FALSE)

head(all_obs)
# just checking if everything merged correctly
avg_plant[2,] # plant 10
licor_all[,177] # plant.ID
licor_all[29,] # plant 10
all_obs[2,] # plant 10
# looks good!

# Stomata by genotype or photosynthetic rate after recovery
anova(lm(avg_stom_plnt~Geno*Photo_after, all_obs)) 
# no significance.
anova(lm(avg_pave_plnt~Geno*Photo_after, all_obs)) 
# no significance

# avg stomata by genotype or photo under stress
anova(lm(avg_stom_plnt~Geno*Photo, all_obs)) 
# no significance

anova(lm(avg_pave_plnt~Geno*Photo, all_obs)) 
#signif diff between photo & pave

# Stomata by photosynthetic rate
anova(lm(avg_stom_plnt~Photo, all_obs)) 
# no sig

# Pavement by photo
anova(lm(avg_pave_plnt~Photo, all_obs)) 
# signif diff

# leaf # by photo
anova(lm(num_leaves~Geno*Photo, all_obs)) 
# signif diff
anova(lm(num_leaves~num_leaves_after*Photo, all_obs)) 
# signif diff
anova(lm(num_leaves~num_active_ax_Meristems*Photo, all_obs)) 
# signif diff
anova(lm(num_leaves~num_active_ax_Meristems*Photo*Treat*Geno, all_obs)) 

anova(lm(num_leaves_after~num_active_ax_Meristems*Photo*Treat*Geno, all_obs)) 

ggplot(all_obs, aes(x = num_active_ax_Meristems, y = num_leaves, colour = Geno))+
  geom_jitter(size = 3.5)+
  stat_smooth(method = 'lm', se = FALSE)+
  facet_grid(Treat~num_leaves_after, labeller = label_value)
  xlab("Leaflet Water Conductance")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Interactions of Conductance, Photosynthesis, & Leaflet Surface Temperature")+
  theme(plot.title = element_text(size = 15.2, face="bold"))

  
# ____________________

# need to remake the IL stuff too... sigh.
# insert IL data and such? pirate old code ----