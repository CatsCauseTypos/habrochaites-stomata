#Date: 10-18-2016
#Project: Habrochaites Trial B drought
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
library(lme4)
library(ggplot2)
library(dplyr)
library(cowplot)
# import files ----
Trial_b <- read.csv("~/Desktop/trial_b_R_friendly-10-2016.csv")


head(Trial_b)
# looks ok

# checking the column classes
sapply(Trial_b, class)

# fixing weird class things:
Trial_b$AB_Stom <- as.numeric(Trial_b$AB_Stom)
class(Trial_b$AB_Stom)

Trial_b$AB_Pav <- as.numeric(Trial_b$AB_Pav)
class(Trial_b$AB_Pav)

# creating Index ----
Trial_b$Index_AB <- (Trial_b$AB_Stom/Trial_b$AB_Pav)
Trial_b$Index_AD <- (Trial_b$AD_Stom/Trial_b$AD_Pav)

head(Trial_b)

# scatter plot to see data ----

ggplot(Trial_b, aes( x = na.omit(AB_Pav), y = na.omit(AB_Stom), colour = Treatment, shape = Geno)) +
  geom_point(size=3.5)+
  theme_bw()+
  scale_color_manual(name="Color Key", values= c( "darkgoldenrod3", "darkblue"))+
  theme(legend.position = "bottom")+
  theme(legend.direction="horizontal")+
  ggtitle("Abaxial Pavement vs Stomata Cells")+
  ylab("Stomata Cell Counts")+
  xlab("Pavement Cell Counts")

# something weird is happening here... I think it's because all the AB isn't counted for pavement yet??

# adaxial 
ggplot(Trial_b, aes( x = na.omit(AD_Pav), y = na.omit(AD_Stom), colour = Treatment, shape = Geno)) +
  geom_point(size=3.5)+
  theme_bw()+
  scale_color_manual(name="Color Key", values= c( "darkgoldenrod3", "darkblue"))+
  theme(legend.position = "bottom")+
  theme(legend.direction="horizontal")+
  ggtitle("Adaxial Pavement vs Stomata Cells")+
  ylab("Stomata Cell Counts")+
  xlab("Pavement Cell Counts")
# much more normal

# all stomata visualize:
ggplot(Trial_b, aes( x = na.omit(AD_Stom), y = na.omit(AB_Stom), colour = Treatment, shape = Geno)) +
  geom_point(size=3.5)+
  theme_bw()+
  scale_color_manual(name="Color Key", values= c( "darkgoldenrod3", "darkblue"))+
  theme(legend.position = "bottom")+
  theme(legend.direction="horizontal")+
  ggtitle("Abaxial Pavement vs Stomata Cells")+
  ylab("Abaxial Stomata Cell Counts")+
  xlab("Adaxial Stomata Cell Counts")

# attempt to separate treatments...
Trial_b %>% 
  group_by(Treatment) %>% 
  ggplot(aes( x = na.omit(AD_Stom), y = na.omit(AB_Stom), colour = Treatment, shape = Geno))+
  geom_point(size=3.5)+
  scale_color_manual(name="Color Key", values= c( "darkgoldenrod3", "darkblue"))+
  theme(legend.position = "bottom")+
  theme(legend.direction="horizontal")+
  ggtitle("Abaxial Stomata vs Adaxial Stomata Cells")+
  ylab("Abaxial Stomata Cell Counts")+
  xlab("Adaxial Stomata Cell Counts")

# fail.

# Bar graph adaxial stomata ----

# got a weird error, so googled it - this fix worked... 
#dev.off()

ggplot(Trial_b, aes( y = AD_Stom, x = Geno, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c( "darkgoldenrod3", "darkblue"))+
  theme(legend.position = c(.1,.9))+
  ggtitle("Adaxial Stomatal Density")+
  ylab("Average Adaxial Stomata Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 25))

# abaxial

ggplot(Trial_b, aes(Geno, AB_Stom, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c( "darkgoldenrod3", "darkblue"))+
  theme(legend.position = c(.1,.9))+
  ggtitle("Abaxial Stomatal Density")+
  ylab("Average Abaxial Stomata Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 25))

# bar abaxial pavement ----

ggplot(Trial_b, aes(Geno, AB_Pav, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c( "darkgoldenrod3", "darkblue"))+
  theme(legend.position = c(.1,.9))+
  ggtitle("Abaxial Pavement Density")+
  ylab("Average Abaxial Pavement Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 25))

# bar plot stomatal index abaxial ----
# (stomata per pavement cell)

ggplot(Trial_b, aes(reorder(Treatment), y = Index_AB, x = Geno, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c("darkgoldenrod3", "darkblue"))+
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 28),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=15))+
  theme(legend.position = c(.15,.9))+
  ggtitle("Abaxial Stomatal Index")+
  ylab("Abaxial Stomata Cells/Pavement Cells")+
  xlab("Genotypes")+
  theme(axis.text.x = element_text( size = 15))

# bar plot stomatal index adaxial ----
ggplot(Trial_b, aes(reorder(Treatment), y = Index_AD, x = Geno, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c("darkgoldenrod3", "darkblue"))+
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 28),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=15))+
  theme(legend.position = c(.5,.82))+
  ggtitle("Adaxial Stomatal Index")+
  ylab("Adaxial Stomata Cells/Pavement Cells")+
  xlab("Genotypes")+
  theme(axis.text.x = element_text( size = 12))


# significance of index: ----
anova(lm(Index_AD~Geno*Treatment, Trial_b)) 

# there is a genotype by treatment effect, but not geno or treatment individually.

# after we run an anova to see *if* there are differences, we need to figure out between which groups (tukey HSD):

tuke.trialb.index <- TukeyHSD(aov(lm(Index_AD~Geno*Treatment, Trial_b))) 

sum.tuke.trialb.index <- as.data.frame(tuke.trialb.index[3])


# checking on all of them, just for funsies:
tuke.trialb.index[3]

# which ones have a p of less than 0.05?
sig.sum.tuke.trialb.index <- as.data.frame(sum.tuke.trialb.index[which(sum.tuke.trialb.index$Geno.Treatment.p.adj<.05),])

# none

sig.sum.tuke.trialb.index

# significance of stomata ----
# ANOVA
anova(lm(AD_Stom~Geno*Treatment, Trial_b)) 
# not significant
# tuke.trialb.stom <- TukeyHSD(aov(lm(Stomata~Geno*Treatment, Trial_b))) 
# sum.tuke.trialb.stom <- as.data.frame(tuke.trialb.stom[3])
# 
# # just to see the layout
# tuke.trialb.stom[3]
# 
# # to grab the p < 0.05
# sig.sum.tuke.trialb.stom <- as.data.frame(sum.tuke.trialb.stom[which(sum.tuke.trialb.stom$Geno.Treatment.p.adj<.05),])

# in Stomata, looks like only the [hab/no water to e6203/no water ] is significantly different
# sig.sum.tuke.trialb.stom


# significance of Pavement ----
anova(lm(AD_Pav~Geno*Treatment, Trial_b)) 
# not sign.
# tuke.trialb.pav <- TukeyHSD(aov(lm(Pavement~Geno*Treatment, Trial_b))) 
# sum.tuke.trialb.pav <- as.data.frame(tuke.trialb.pav[3])
# 
# tuke.trialb.pav[3]
# sig.sum.tuke.trialb.pav <- as.data.frame(sum.tuke.trialb.pav[which(sum.tuke.trialb.pav$Geno.Treatment.p.adj<.05),])

# sig.sum.tuke.trialb.pav


# bar Pave w significance ----
ggplot(Trial_b, aes(Geno, AD_Pav, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c("goldenrod3", "darkblue"))+
  theme(legend.position = c(.1,.9),
        plot.title = element_text(size = 25, face="bold"),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=8) )+
  ggtitle("Adaxial Pavement Density")+
  ylab("Average Adaxial Pavement Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 210))

# bar Stom w significance ----
ggplot(Trial_b, aes(reorder(Geno,Stomata, mean), Stomata, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = Geno, 
                    y = (Stomata+2.5),
                    label = stom.sig),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  scale_fill_manual(values = c("goldenrod3", "darkblue"))+
  theme(legend.position = c(.1,.9),
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



# conductance vs photo
ggplot(licor_all, aes(Cond, Photo, colour = Tleaf))+
  geom_jitter(size = 3.5)+
  facet_grid(Treat~Geno, labeller = label_value)+ # this maintains the default labels..
  scale_colour_gradient( low="yellow", high = "darkorchid", space = "Lab")+
  xlab("Leaflet Water Conductance")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Interactions of Conductance, Photosynthesis, & Leaflet Surface Temperature")+
  theme(plot.title = element_text(size = 15.2, face="bold"))

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

