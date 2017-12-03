#Date: 3-22-2016
#Project: Habrochaites Pavement cell density in drought - LICOR
#Author: Donnelly West - DonWest@UCDavis.edu
#Version: 2.0

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

# load libraries
library(ggplot2)
library(cowplot)
library(dplyr)
library(lme4)
# input files ----
daw_dec292015_mary_trimmed <- read.csv("~/Desktop/daw_dec292015_mary_trimmed.csv")

data <- daw_dec292015_mary_trimmed

data1 <- read.csv("~/Desktop/daw_jan132016_mary_trimmed1.csv")
data_all <- read.csv("~/Desktop/daw_amalgamation_mary_trimmed.csv")

# rounding for usefulness later
data$Tleaf <- round(data$Tleaf)
data1$Tleaf <- round(data1$Tleaf)

# peaking at the treatments ----
ggplot(data, aes( Geno, Photo, colour = Treat))+
  geom_jitter()+
  xlab("Genotype")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Species Comparison")+
  scale_color_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c("green2", "firebrick1")
    )

# I like the bar graph better
ggplot(data, aes( Geno, Photo, colour = Treat))+
  geom_boxplot()+
  xlab("Genotype")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Species Comparison")+
  scale_color_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c("green2", "firebrick1")
  )
# with ALL data
ggplot(data_all, aes( x = Geno, y = Photo, Photo_after, colour = Treat))+
  geom_boxplot()+
  xlab("Genotype")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Species Comparison")+
  scale_color_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c("green2", "firebrick1")
  )

# how about a bar plot to see them more clearly
ggplot(data, aes( Geno, Photo, fill = Treat))+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.25)+
  xlab("Genotype")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Species Comparison")+
  facet_wrap(~Treat, scales = "free")+
  scale_fill_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c("green2", "firebrick1"))



# together in a bar graph
ggplot(data, aes( Geno, Photo, fill = Treat))+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6), color = "black")+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.25, position = position_dodge(.6))+
  xlab("Genotype")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Species Comparison")+
  scale_fill_manual(
    name = "Treatment",
    labels = c("Normal Water", "No Water"),
    values = c("green2", "firebrick1"))




# me fucking around with linear models:
species.stats <- lm(Photo~Geno,data)
summary(species.stats)
sum.ad.lm <- as.data.frame(summary(ad.lm)$coefficients[,4])

# steven helped me with his code to ask: "does photosynthesis during stress & after recovery change with genotype, treatment or the two together?"
# or mathematically: are photo & photo_after functions of geno and treat or (geno by treatment)
fun <- lm(cbind(Photo, Photo_after)~Geno*Treat, data_all)
summary(manova(fun))


means_photo <- data %>% 
  group_by(Geno, Treat) %>% 
  summarise(mean_photo = mean(Photo))

means_photo1 <- data1 %>% 
  group_by(Geno, Treat) %>% 
  summarise(mean_photo = mean(Photo))


# paired ttest - actually, I want to do an ANOVA now, because there are a lot more comparisons that are interesting.
# format from the internet (http://www.statmethods.net/stats/ttest.html) - t.test(y1,y2,paired=TRUE) # where y1 & y2 are numeric
?t.test
#t.test(set of data 1, set of data2, "two.sided", paired = TRUE)
 # so this code gives no errors, but is not actually doing what I want.. I thought if I gave it the list, and they were 'paired' that it would compare 1 to 1, 2 to 2, but it didn't... think i need an 'apply' type function.

# asked the internet: got this as the answer: apply(sample1,1,function(x) t.test(x,sample2)$p.value) from https://stat.ethz.ch/pipermail/r-help/2008-October/177055.html 
# clearly, I don't understand apply at all... found this article: http://www.r-bloggers.com/using-apply-sapply-lapply-in-r/



# ANOVA ----
anova(lm(Photo~Geno*Treat, data)) 

# just to see:
anova(lm(Photo+Photo_after+num_leaves~Geno*Treat*num_leaves_after, data_all)) 

# my original questions was "are # of leaves different during & after stress" AND "is there an interaction of # leaves with photosynthesis?" ALSO "do recovered plants have correlation between leaf # & photosynthetic rate?"

# so yeah... I need to focus.



# looks like treatment doesn't interact with genotype; however, geno and treat are sign different.

library(lmerTest) # without this, you don't get the following code to work. dunno why. also, only works with * instead of +

tuke.photo <- TukeyHSD(aov(lm(Photo~Geno*Treat, data))) 
sum.tuke.photo <- as.data.frame(tuke.photo[3])

tuke.photo[1] # if you only want to see Genotypes (but remember, it pools treatments)
tuke.photo[2] # if you only want to see Treatments (but remember, it pools genotypes)

sig.sum.tuke.photo <- as.data.frame(sum.tuke.photo[which(sum.tuke.photo$Geno.Treat.p.adj<.05),])

# how do the photosynthetic rates vary with leaf temp by treatment / genotype
ggplot(data, aes( Tleaf, Photo, colour = Geno, shape = Treat))+
  geom_jitter(size = 3.5)+
  xlab("Leaf Temp")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Photosynthesis & Leaf Temperature During Treatment")+
  scale_color_manual(name = "Genotype",
                     labels = c("Domesticated", "Wild"),
                     values = c("lightslateblue", "deeppink"))+
  scale_shape_manual(name = "Treatment",
                     labels = c("Normal Water", "No Water"),
                     values = c(16, 17))
  
# very nice that it separates by treatment exclusively in the FACE OF GENOTYPE~~ bwahaha


# what about after 'recovery'? 
ggplot(data1, aes( Tleaf, Photo, colour = Geno, shape = Treat))+
  geom_jitter(size = 3.5)+
  xlab("Leaf Temp")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Leaf Temp in Trial B After Recovery")+
  scale_color_manual(name = "Genotype",
                     #labels = c("Domesticated", "Wild"),
                     values = c("lightslateblue", "deeppink"))+
  scale_shape_manual(name = "Treatment",
                     labels = c("Normal Water", "No Water"),
                     values = c(16, 17))


# remembering how to facet stuff: http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
# remembering how to use legend titles and such : http://docs.ggplot2.org/0.9.2.1/theme.html

# nice leaf temp vs treat vs spp vs photo
# ggplot(data, aes( Tleaf, Photo, colour = Treat))+
#   geom_jitter(size = 2.5)+
#   facet_grid(~Geno)+
#   xlab("Leaf temp (C)")+
#   ylab("Photosynthetic Rate (A)")+
#   ggtitle("Leaf Temp Trial B")+
#   scale_color_manual(name = "Treatment", values = c("lightslateblue", "deeppink"))

data$Tleaf

# after recovery
ggplot(data1, aes( Tleaf, Photo, colour = Treat))+
  geom_jitter()+
  facet_grid(~Geno)+
  xlab("Leaf temp (C)")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Hab Drought Trial B")+
  scale_color_manual(name = "Treatment", values = c("lightslateblue", "deeppink"))


# let's say we want to ask: Is hab photosynthesizing better under drought than e6203?

# data1 <- data %>% 
#   group_by(Geno, Treat) %>% 
#   mutate(avg_wtf = mean(Photo)) %>% 

# data1$avg_wtf

ggplot(data, aes(Geno, Photo, colour = Geno))+
  geom_boxplot()+
  facet_grid(~Treat)+
  scale_color_manual(values = c("seagreen3","plum"))+
  ggtitle("Photosynthetic Rates under Stress (Trial B)")+
  xlab("")+
  ylab("Photosynthetic Rate (A)")+
  theme(legend.position = "none",
        plot.title = element_text(size = 22, face="bold"))
#+
# annotate(geom="text", label="test",size=3,x=1,y=8) #annoyingly doesn't work with facet

# what about some lovely statistics on those?
#let's try making our own linear model
#(PS- it will compare EVERYTHING by THE FIRST THING, so name your stuff appropriately)
#adaxial


# # trying do linear modeling on these... it's not going well.. (using steven's code that we used on the count data)
# data %>% 
#   group_by(Geno, Treat) %>% 
#   lm(Photo~Geno,.)
# 
# lm(Photo~Geno,data)
# # those two things should give different answers





# # stolen off the internet: 
# boxplot(Photo~Geno*Treat, data, notch=FALSE, 
#         col=(c("gold","darkgreen")),
#         main="zomg", xlab="geno & treat")
# # interesting


### from the internet: http://www.statmethods.net/stats/ttest.html
# independent 2-group t-test
# t.test(y~x) # where y is numeric and x is a binary factor
# # independent 2-group t-test
# t.test(y1,y2) # where y1 and y2 are numeric
# # paired t-test
# t.test(y1,y2,paired=TRUE) # where y1 & y2 are numeric
# # one sample t-test
# t.test(y,mu=3) # Ho: mu=3

##
# this subset works, but I'm not sure how to use any of this subsetting to tell the t.test what I actually want
# data2 <- data %>% 
#   group_by(Geno, Treat) %>% 
#   summarise(avg_photo = mean(Photo), var_photo = var(Photo), avg_leaf = mean(num_leaves), var_leaf = var(num_leaves)) 
# 
# t.test(data2$avg_photo~data2$Geno)

####
#Mary used excel and it was easier...

# t test hab drought vs e6203 drought = 0.000961238
# t test hab drought vs hab control = 3.01176 x 10^-9
# t test 6203 drought vs e6203 control = 5.48253x10^-5
# t test hab control vs e6203 control = 0.29791569

# Mary also looked at leaf numbers

# num leaves after vs photo : ----
ggplot(data_all, aes(num_leaves_after, Photo_after, colour = Treat))+
  geom_jitter()+
  facet_grid(~Geno)+
  scale_color_manual(name = "Treatment", values = c("lightslateblue", "deeppink"))+
  stat_smooth(method = "lm")+
  xlab("Number of Leaves at Rescue")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("LICOR Data from Hab Stomatal Density under Stress (Trial B)")+
  theme(plot.title = element_text(size = 17.2, face="bold"))

