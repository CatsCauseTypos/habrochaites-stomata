#Date: 2-3-2017
#Project: Habrochaites Trial B drought -## This is a test edit to play with github
#Author: Donnelly West - DonWest@UCDavis.edu
#Version: 4.0

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


#install & load needed libraries
l


library(lme4)
library(ggplot2)
library(dplyr)
library(cowplot)
# import files ----
Trial_b <- read.csv("~/Desktop/Sinha Lab stuff/Hab IL stomatal stuff/Data/trial_b_R_friendly-2-2017_1.csv")


# we have ab & ad sides for stomata & pavement 

head(Trial_b)

Trial_b$Index_ab <- (Trial_b$ab_stomata/Trial_b$ab_pave)
Trial_b$Index_ad <- (Trial_b$ad_stomata/Trial_b$ad_pave)
Trial_b <- na.omit(Trial_b)
Trial_b$Treatment <- as.factor(Trial_b$Treatment)  

# checking the column classes
sapply(Trial_b, class)




# scatter plot to see data ----

ggplot(Trial_b, aes( x = ad_pave, y = ad_stomata, colour = Treatment, shape = Geno) ) +
  geom_point(size=3.5)+
  theme_bw()+
  scale_color_manual(name="Color Key", values= c( "darkgoldenrod3", "darkblue"))+
  theme(legend.position = "bottom")+
  theme(legend.direction="horizontal")+
  ggtitle("Adaxial Pavement vs Stomata Cells")+
  ylab("Stomata Cell Counts")+
  xlab("Pavement Cell Counts")

# wow, there's really no clear pattern..

# Bar graph adaxial stomata ----

# got a weird error, so googled it - this fix worked... 
#dev.off()

ggplot(Trial_b, aes(reorder(Geno,ad_stomata, mean), ad_stomata, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c( "darkgoldenrod3", "darkblue"))+
  theme(legend.position = c(.15,.85))+
  ggtitle("Adaxial Stomatal Density")+
  ylab("Average Adaxial Stomata Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 17))


# bar adaxial pavement ----

ggplot(Trial_b, aes(reorder(Geno,ad_pave, mean), ad_pave, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c( "darkgoldenrod3", "darkblue"))+
  theme(legend.position = c(.15,.85))+
  ggtitle("Adaxial Pavement Density")+
  ylab("Average Adaxial Pavement Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 200))

# bar plot stomatal index adaxial ----
# (stomata per pavement cell)

ggplot(Trial_b, aes(reorder(Treatment), y = Index_ad, x = Geno, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c("darkgoldenrod3", "darkblue"))+
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 28),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=15))+
  theme(legend.position = c(.5,.87))+
  ggtitle("Stomatal Index")+
  ylab("Adaxial Stomata Cells/Pavement Cells")+
  xlab("Genotypes")+
  theme(axis.text.x = element_text( size = 15))+
  coord_cartesian(ylim = c(0, .11))+
  scale_x_discrete(breaks=c("e6203", "hab"),
                   labels=c("Domesticated", "Wild"))

# significance of index: ----
anova(lm(Index_ad~Geno*Treatment, Trial_b)) 

# Response: Index_ad
# Df   Sum Sq   Mean Sq F value    Pr(>F)    
# Geno            1 0.000659 0.0006590  0.3715 0.5437913    
# Treatment       1 0.000456 0.0004565  0.2573 0.6132541    
# Geno:Treatment  1 0.023098 0.0230975 13.0208 0.0005165 ***
#   Residuals      86 0.152555 0.0017739                      
# ---

# after we run an anova to see *if* there are differences, we need to figure out between which groups (tukey HSD):

tuke.trialb.index_ad <- TukeyHSD(aov(lm(Index_ad~Geno*Treatment, Trial_b))) 

sum.tuke.trialb.index_ad <- as.data.frame(tuke.trialb.index_ad[3])


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

# why are these so different?! I thought we had all the adaxial stuff counted before...


# trying to get significance in the original data frame in order to have it map more easily: 
#tuke.trialb.index$sig <- 
 # lol - this was harder than just asigning arbitrary "a" and "b" and such in excel - f'ing r.. 

# significance of stomata adaxial ----
# ANOVA
anova(lm(ad_stomata~Geno*Treatment, Trial_b)) 

# Response: ad_stomata
# Df Sum Sq Mean Sq F value  Pr(>F)   
# Geno            1  158.1  158.11  3.3983 0.06892 . 
# Treatment       1    1.1    1.07  0.0230 0.87979   
# Geno:Treatment  1  399.3  399.27  8.5815 0.00441 **
#   Residuals      81 3768.7   46.53



tuke.trialb.stom_ad <- TukeyHSD(aov(lm(ad_stomata~Geno*Treatment, Trial_b))) 
sum.tuke.trialb.stom_ad <- as.data.frame(tuke.trialb.stom_ad[3])

# just to see the layout
tuke.trialb.stom_ad[3]

# to grab the p < 0.05
sig.sum.tuke.trialb.stom_ad <- as.data.frame(sum.tuke.trialb.stom_ad[which(sum.tuke.trialb.stom_ad$Geno.Treatment.p.adj<.05),])

# in Stomata, looks like only the [hab/no water to e6203/no water ] is significantly different
sig.sum.tuke.trialb.stom_ad

# and now in feb, it's still the lyco nowater vs hab nowater 

# significance of Pavement adax ----
anova(lm(ad_pave~Geno*Treatment, Trial_b)) 

tuke.trialb.pav_ad <- TukeyHSD(aov(lm(ad_pave~Geno*Treatment, Trial_b))) 
sum.tuke.trialb.pav_ad <- as.data.frame(tuke.trialb.pav_ad[3])

tuke.trialb.pav_ad[3]
sig.sum.tuke.trialb.pav_ad <- as.data.frame(sum.tuke.trialb.pav_ad[which(sum.tuke.trialb.pav_ad$Geno.Treatment.p.adj<.05),])

sig.sum.tuke.trialb.pav_ad
# in Pavement, looks like 3 are significantly different:
# hab norm vs e6203 no water, #1
# hab norm vs hab no water, #2 
# hab norm vs e6203 norm #3

# and in feb:
# hab-norm vs hab-nowater #2
# hab-norm vs lyco-nowater # 1
# lyco-norm vs hab-norm #3


# bar Pave w significance adaxial ----
Trial_b <- read.csv("~/Desktop/Sinha Lab stuff/Hab IL stomatal stuff/Data/trial_b_R_friendly-2-2017_sig.csv")

ggplot(Trial_b, aes(reorder(Geno,ad_pave, mean), ad_pave, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5),
               position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", 
               width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = Geno, 
                    y = (ad_pave+25),
                    label = ad_pave.sig),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  scale_fill_manual(values = c("goldenrod3", "darkblue"))+
  theme(legend.position = c(.099,.9),
        plot.title = element_text(size = 25, face="bold"),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=20) )+
  ggtitle("Adaxial Pavement Density")+
  ylab("Average Adaxial Pavement Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 210))#+
  scale_x_discrete(breaks=c("ly", "hab"),
                   labels=c("Domesticated", "Wild"))

# # how do you reorder by 'control' instead of alphabetical?
# http://stackoverflow.com/questions/20041136/avoid-ggplot-sorting-the-x-axis-while-plotting-geom-bar
# dat <- read.table(text=
#                       "SC_LTSL_BM    16.8275
#                     SC_STSL_BM    17.3914
#                     proB_FrBC_FL   122.1580
#                     preB_FrD_FL    18.5051
#                     B_Fo_Sp    14.4693
#                     B_GC_Sp    15.4986", header = FALSE, stringsAsFactors = FALSE)
#   
# # make V1 an ordered factor
# dat$V1 <- factor(dat$V1, levels = dat$V1)

## try it with my data:
# Trial_b$Geno <- factor(Trial_b$Geno, levels = Trial_b$Geno)
# # so that threw a weird error - don't know 


  
# plot
library(ggplot2)
ggplot(dat,aes(x=V1,y=V2))+geom_bar(stat="identity")
# bar Stom w significance adax ----
ggplot(Trial_b, aes(reorder(Geno,ad_stomata, mean), ad_stomata, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = Geno, 
                    y = (ad_stomata+2.9),
                    label = ad_stom.sig),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  scale_fill_manual(values = c("goldenrod3", "darkblue"))+
  theme(legend.position = c(.11,.85),
              plot.title = element_text(size = 25, face="bold"),
              axis.title.x = element_text(face="bold", size=20),
              axis.title.y = element_text(face="bold", size=20) )+
  ggtitle("Adaxial Stomatal Density")+
  ylab("Average Adaxial Stomatal Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 20))+
  scale_x_discrete(breaks=c("e6203", "hab"),
                   labels=c("Domesticated", "Wild"))

# bar Index w significance ----
ggplot(Trial_b, aes(reorder(Geno,Index, mean), Index, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = Geno, 
                    y = (Index+.018),
                    label = ind.sig),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  scale_fill_manual(values = c("goldenrod3", "darkblue"))+
  theme(legend.position = c(.15,.9),
        plot.title = element_text(size = 25, face="bold"),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=20))+
  ggtitle("Adaxial Stomatal Index")+
  ylab("Adaxial Stomatal Cells / Pavement Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, .15))+
  scale_x_discrete(breaks=c("e6203", "hab"),
                   labels=c("Domesticated", "Wild"))

# LI-COR data begins ----

# load libraries
library(ggplot2)
library(cowplot)
library(dplyr)
library(lme4)
library(lmerTest)
# input licor files ----
daw_dec292015_mary_trimmed <- read.csv("~/Desktop/daw_dec292015_mary_trimmed.csv")

licor <- daw_dec292015_mary_trimmed
head(licor1)
licor1 <- read.csv("~/Desktop/daw_jan132016_mary_trimmed1.csv")
licor_all <- read.csv("~/Desktop/daw_amalgamation_mary_trimmed.csv")

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
  theme(legend.position = c(.9,.9),
        plot.title = element_text(size = 25, face="bold"),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=20))+
  scale_color_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c("darkblue", "darkgoldenrod3")
  )+
  scale_y_continuous(limits=c(-3,12))+
  scale_x_discrete(breaks=c("e6203", "hab"),
                   labels=c("Domesticated", "Wild"))

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
  theme(legend.position = c(.9,.1),
        plot.title = element_text(size = 25, face="bold"),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=20))+
  scale_y_continuous(limits=c(-3,12))+
  scale_x_discrete(breaks=c("e6203", "hab"),
                   labels=c("Domesticated", "Wild"))


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

# looks like treatment doesn't interact with genotype; however, geno and treat are sign different.

library(lmerTest) # without this, you don't get the following code to work. dunno why. also, only works with * instead of +

tuke.photo.after <- TukeyHSD(aov(lm(Photo~Geno*Treat, licor1))) 
sum.tuke.photo.after <- as.data.frame(tuke.photo.after[3])

# all results:
tuke.photo.after[3]
# significant results:
sig.sum.tuke.photo <- as.data.frame(sum.tuke.photo[which(sum.tuke.photo$Geno.Treat.p.adj<.05),])
sig.sum.tuke.photo

# licor tleaf photo treat geno final ----
# during stress
ggplot(licor, aes( Tleaf, Photo, colour = Treat, shape = Geno))+
  geom_jitter(size = 3.2)+
  xlab("Leaf Temperature(C)")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Photosynthesis & Leaf Temperature \nDuring Treatment")+
  theme(legend.position = c(.9,.75),
              plot.title = element_text(size = 25, face="bold"),
              axis.title.x = element_text(face="bold", size=20),
              axis.title.y = element_text(face="bold", size=20))+
  scale_color_manual(name = "Treatment",
                     labels = c("Normal Water", "No Water"),
                     values = c("darkblue", "darkgoldenrod3"))+
  scale_shape_manual(name = "Genotype",
                     labels = c("Domesticated", "Wild"),
                     values = c(16, 17))+
  scale_y_continuous(limits=c(-3,12))



# very nice that it separates by treatment exclusively in the FACE OF GENOTYPE~~ bwahaha


# what about after 'recovery'? final 
ggplot(licor1, aes( Tleaf, Photo, colour = Treat, shape = Geno))+
  geom_jitter(size = 3.2)+
  xlab("Leaf Temperature(C)")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Photosynthesis & Leaf Temperature \nAfter Recovery")+
  theme(legend.position = c(.15,.2),
        plot.title = element_text(size = 25, face="bold"),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=20))+
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

ggplot(licor1, aes(Geno, Photo, colour = Geno))+
  geom_boxplot()+
  facet_grid(~Treat)+
  scale_color_manual(values = c("seagreen3","plum"))+
  ggtitle("Photosynthetic Rates Recovery (Trial B)")+
  xlab("")+
  ylab("Photosynthetic Rate (A)")+
  theme(legend.position = "none",
        plot.title = element_text(size = 22, face="bold"))




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

