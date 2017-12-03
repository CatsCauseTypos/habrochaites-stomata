#Date: 12-29-2015
#Project: Habrochaites Pavement cell density in drought - LICOR
#Author: Donnelly West - DonWest@UCDavis.edu
#Version: 1.0

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

daw_dec292015_mary_trimmed <- read.csv("~/Downloads/daw_dec292015_mary_trimmed.csv")

data <- daw_dec292015_mary_trimmed

# rounding for usefulness later
data$Tleaf <- round(data$Tleaf)

ggplot(data, aes( Geno, Photo, colour = Treat))+
  geom_jitter()+
  xlab("Genotype")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Hab Drought Trial B")+
  scale_color_manual(name = "Treatment", values = c("lightslateblue", "deeppink"))


ggplot(data, aes( Tleaf, Photo, colour = Geno, shape = Treat))+
  geom_jitter(size = 3.5)+
  xlab("Leaf Temp")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Leaf Temp in Trial B")+
  scale_color_manual(name = "Genotype", values = c("lightslateblue", "deeppink"))

# remembering how to facet stuff: http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
# remembering how to use legend titles and such : http://docs.ggplot2.org/0.9.2.1/theme.html

# nice leaf temp vs treat vs spp vs photo
ggplot(data, aes( Tleaf, Photo, colour = Treat))+
  geom_jitter(size = 2.5)+
  facet_grid(~Geno)+
  xlab("Leaf temp (C)")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("Leaf Temp Trial B")+
  scale_color_manual(name = "Treatment", values = c("lightslateblue", "deeppink"))

data$Tleaf

ggplot(data, aes( Tleaf, Photo, colour = Treat))+
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

#####
#Mary used excel and it was easier...

# t test hab drought vs e6203 drought = 0.000961238
# t test hab drought vs hab control = 3.01176 x 10^-9
# t test 6203 drought vs e6203 control = 5.48253x10^-5
# t test hab control vs e6203 control = 0.29791569

# Mary also looked at leaf numbers

# We should too leaves vs photo :
ggplot(data, aes(num_leaves, Photo, colour = Treat))+
  geom_jitter()+
  facet_grid(~Geno)+
  scale_color_manual(name = "Treatment", values = c("lightslateblue", "deeppink"))+
  stat_smooth(method = "lm")+
  xlab("Number of Leaves at Rescue")+
  ylab("Photosynthetic Rate (A)")+
  ggtitle("LICOR Data from Hab Stomatal Density under Stress (Trial B)")+
  theme(plot.title = element_text(size = 17.2, face="bold"))

