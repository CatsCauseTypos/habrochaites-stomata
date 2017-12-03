#Date: 12-15-2015
#Project: Habrochaites IL Pavement & stomatal cell density 
#Author: Donnelly West - DonWest@UCDavis.edu
#Version: 3.7 USE ME

# to make math work later because R is dumb:
options(stringsAsFactors = FALSE)

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

# get your data - NEWEST DATA 3/7/2016 ----
data <- read.csv("~/Desktop/ILs_R-Friendly Data_mar_2016.csv")
class(data$Pavement) # just checking..

# ways to split up the ab/ad column ----

head(data)

# name.the.new.column <- substring function (the column you want, where.it.should.start, where.it.should.stop)

data$leaf.side <- as.factor(substr(data$ab.ad,1,2)) # this is super-useful to split by abaxial/adaxial sides

data$plant.number <- as.factor(substr(data$Plant_num,2,2)) #Relevant plant # info

class(data$plant.number) #default is as character, so definitely need that extra "as.factor" in there!

head(data) #it worked

# checking the column classes
sapply(data, class)

#separating the data into the leaf sides for simplicity ----
adaxial <- data[grepl("ad",data$leaf.side),]
head(adaxial)

abaxial <- data[grepl("ab",data$leaf.side),]

avg_by_geno <- data %>% 
  group_by(Geno, leaf.side) %>% 
  summarise(avg_pave = mean(Pavement), var_pave = var(Pavement), avg_stom = mean(Stomata), var_stom = var(Stomata)) # these numbers match the original stomatal analyses I did by hand in excel - WHEW!!


# how the heck can I normalize? ----

# first, let's split the avg data by leaf sides:

avg_ad_data <- avg_by_geno[grepl("ad",avg_by_geno$leaf.side),]

avg_ab_data <- avg_by_geno[grepl("ab",avg_by_geno$leaf.side),]

head(avg_ab_data) #worked

#standardize by domesticated parent ----

# abaxial
avg_ab_data$norm.ab <- avg_ab_data$avg_pave/as.numeric(avg_ab_data[1,3])



# adaxial
head(avg_ad_data) # 64.29167
avg_ad_data$norm.ad <- avg_ad_data$avg_pave/as.numeric(avg_ad_data[1,3])

# merging ab/ad ----
data1 <-  merge(x = avg_ab_data, y = avg_ad_data, by.x = 'Geno', by.y = 'Geno', by.x, drop = FALSE)

################################################### significance ----

## But which ILs are actually, significantly different than parent 4024? 

#let's try making our own linear model ----
#(PS- it will compare EVERYTHING by THE FIRST THING, so name your stuff appropriately)

# adaxial stomata

ad.lm <- lm(Stomata~Geno,adaxial)
sum.ad.lm <- as.data.frame(summary(ad.lm)$coefficients[,4])

# adaxial pavement
ad.v.lm <- lm(Pavement~Geno,adaxial)
sum.ad.v.lm <- as.data.frame(summary(ad.lm)$coefficients[,4])

# abaxial stomata
ab.lm <- lm(Stomata~Geno,abaxial)
sum.ab.lm<- as.data.frame(summary(ab.lm)$coefficients[,4])

# abaxial pavement
ab.v.lm <- lm(Pavement~Geno,abaxial)
sum.ab.v.lm<- as.data.frame(summary(ab.lm)$coefficients[,4])


#the summary shows us 'what we expect' from looking at the bar graph of the stomatal counts (the ILs that look to have way more/less stomata than lyco parent are coming out as 'significantly different' than 4024)

#we still need to adjust for false discovery: adjusted p-value

# adjusted p for adaxial stomata ----
ad.adj.p <- data.frame(value=p.adjust(summary(ad.lm)$coefficients[,4], method="fdr")) 
ad.adj.p$Geno=rownames(ad.adj.p) #this makes life easier later

# adjusted p for adaxial pavement
ad.v.adj.p <- data.frame(value=p.adjust(summary(ad.v.lm)$coefficients[,4], method="fdr")) 
ad.v.adj.p$Geno=rownames(ad.v.adj.p) #this makes life easier later

# make lists of the significant ones: adaxial----
# adaxial pavement
ad.v.sigf.p <- as.data.frame(ad.v.adj.p[which(ad.v.adj.p$value<.05),])
write.csv(ad.v.sigf.p,"significant_adaxial_pavement_habGenos.csv") 

# #trying to figure out why 3921 isn't 'significant' ----
# y3922 <- as.data.frame(sum.ad.v.lm[which(sum.ad.v.lm$value<.05),])
# write.csv(y3922,"adaxial_pavement_hab_problems.csv") 
# we think it's because the LM we're running to see if they're significant is slightly different than the SE formula Julin wrote for the graphing function SO when we look at the variation / SE on the graph, it looks the same, but in the LM it's rather different. 3922 had fewer individual plants and more variation (because of that) so it could not be considered 'siginificant' by the adjusted p-value. Now... because we're biologists and we can see darn well that the other two with similar means/variances ARE significant, it might make the most sense to lump 3922 in with the pack.

# adaxial stomatal
ad.sigf.p <- as.data.frame(ad.adj.p[which(ad.adj.p$value<.05),])
write.csv(ad.sigf.p,"significant_adaxial_ILs_hab.csv") #making a file for prosperity

####### abaxial significance ----

#And again for abaxial stomata
ab.adj.p <- data.frame(value=p.adjust(summary(ab.lm)$coefficients[,4], method="fdr")) 
ab.adj.p$Geno=rownames(ab.adj.p) 

# abaxial pavement
ab.v.adj.p <- data.frame(value=p.adjust(summary(ab.v.lm)$coefficients[,4], method="fdr")) 
ab.v.adj.p$Geno=rownames(ab.v.adj.p)
## adjusted p's 
# abaxial pavement
ab.v.sigf.p <- as.data.frame(ab.v.adj.p[which(ab.v.adj.p$value<.05),])
write.csv(ab.v.sigf.p,"significant_abaxial_pavement_habILs.csv") 
# abaxial stomatal
ab.sigf.p <- as.data.frame(ab.adj.p[which(ab.adj.p$value<.05),])
write.csv(ab.sigf.p,"significant_abaxial_stomata_ILs_hab.csv") 




setwd("~/Desktop/")

write.csv(data1, file = "hab_stats_R.csv")
#added parent & significance for both data sets in excel, by hand b/c ><


setwd("~/Desktop/")

data1 <- read.csv("hab_stats_R.csv", head = T) # file back in

head(data1)
#Scatter plot of normalized-to-4024 ad vs ab

ggplot(data1, aes( x = norm.ab, y = norm.ad, colour = Pave_sig) ) +
  geom_jitter()+
  geom_point(size=3.5)+
  theme_bw()+
  scale_color_manual(name="Color Key", values=c("darkorange", "magenta","darkorchid3","darkseagreen2","deepskyblue2","yellow2"))+
  theme(legend.position = "bottom")+
  theme(legend.direction="horizontal")+
  ggtitle("Normalized Pavement Density of S. habrochaites ILs")+
  ylab("Normalized Adaxial Counts")+
  xlab("Normalized Abaxial Counts")


#separating the data into the leaf sides for simplicity
adaxial <- data[grepl("ad",data$leaf.side),]
head(adaxial)

abaxial <- data[grepl("ab",data$leaf.side),]

#Bar graph showing the stomata ILs and parents with error bars
# abaxial
ab.bar <- ggplot(abaxial, aes(reorder(Geno,Stomata, mean), Stomata, fill=Parent))
ab.bar+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("darkorange", "darkblue","magenta"))+
  theme(legend.position = c(.24,.9))+
  theme(legend.direction="horizontal")+
  theme(axis.text.x=element_text(angle=-90,hjust=0))+
  ggtitle("Abaxial Stomatal Density")+
  ylab("Average Abaxial Stomata")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 100))


# abaxial pavement cells

ab.pave <- ggplot(abaxial, aes(reorder(Geno,Pavement, mean), Pavement, fill=Parent))
ab.pave+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("darkorange", "darkblue","magenta"))+
  theme(legend.position = c(.24,.9))+
  theme(legend.direction="horizontal")+
  theme(axis.text.x=element_text(angle=-90,hjust=0))+
  ggtitle("Abaxial Pavement Cell Density")+
  ylab("Average Abaxial Pavement Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 250))



# Adaxial stomatal
ad.bar <- ggplot(adaxial, aes(reorder(Geno,Stomata, mean), Stomata, fill=Parent))
ad.bar+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("orange", "dark blue","magenta"))+
  theme(legend.position = c(.24,.9))+
  theme(legend.direction="horizontal")+
  theme(axis.text.x=element_text(angle=-90,hjust=0))+
  ggtitle("Adaxial Stomatal Density")+
  ylab("Average Adaxial Stomata")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 18))

# adaxial pavement
adaxial %>% 
  na.omit(adaxial) %>% 
  ggplot( aes(reorder(Geno,Pavement, mean), Pavement, fill=Parent)) +
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("orange", "dark blue","magenta"))+
  theme(legend.position = c(.24,.9))+
  theme(legend.direction="horizontal")+
  theme(axis.text.x=element_text(angle=-90,hjust=0))+
  ggtitle("Adaxial Pavement Cell Density")+
  ylab("Average Adaxial Pavement Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 170))

# # wondering what's being removed b/c of missing values..
# adaxial %>% 
#   na.omit(adaxial) %>% 
# ggplot( aes(Geno, Pavement, fill=Parent)) +
#   theme_bw()+
#   stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
#   stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
#   scale_fill_manual(name="Color Key", values = c("orange", "dark blue","magenta"))+
#   theme(legend.position = c(.24,.9))+
#   theme(legend.direction="horizontal")+
#   theme(axis.text.x=element_text(angle=-90,hjust=0))+
#   ggtitle("Adaxial Pavement Cell Density")+
#   ylab("Average Adaxial Pavement Cells")+
#   xlab("Genotypes")
# 
# # when I use 'na omit' I get no more warnings! LA3921 has 8 columns of NAs (doesn't explain '11 removed rows?!) AND all the ILs are present no matter what.



# hand annotated significance in Parental column it goes stomata.pavement
data.sig <- read.csv("hab_stats_R.csv", head = T) # file in



#Scatter plot of normalized-to-4024 ad vs ab

ggplot(data.sig, aes( x = data1$norm.ad, y = data1$norm.ab, colour = Parent) ) +
  geom_jitter()+
  geom_point(size=3.5)+
  theme_bw()+
  scale_colour_manual(name="Color Key: Stomata.Pavement", values = c("darkgreen", "cadetblue4","blue2","darkgoldenrod4","darkorchid3", "darkolivegreen2","darkorange","magenta","yellow2","darkseagreen2"))+
  guides(colour  = guide_legend(nrow = 2))+
  theme(legend.position = "bottom")+
  theme(legend.direction="horizontal")+
  ggtitle("Normalized Pavement Density of S. habrochaites ILs")+
  xlab("Normalized Adaxial Counts")+
  ylab("Normalized Abaxial Counts")

#lol - that's not pretty  

# PCA

#Bar graph with signifigance astrix 

# adaxial stomata
  ad.bar+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("darkorange", "dark blue","magenta"))+
  theme(legend.position = c(.24,.9))+
  theme(legend.direction="horizontal")+
  theme(axis.text.x=element_text(angle=-90,hjust=0))+
  ggtitle("Habrochaites ILs Adaxial Stomatal Density")+
  ylab("Average # Adaxial Stomata")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 20))+
  annotate(geom="text", label="*",size=9.5,x=37,y=15)+ #3913
  annotate(geom="text", label="*",size=9.5,x=38,y=18)+ #3917
  annotate(geom="text", label="*",size=9.5,x=36,y=14.9)+ #3918
  annotate(geom="text", label="*",size=9.5,x=8,y=5)+ #3922
  annotate(geom="text", label="*",size=9.5,x=9,y=5.1)+ #3927
  annotate(geom="text", label="*",size=9.5,x=17,y=7)+ #3932
  annotate(geom="text", label="*",size=9.5,x=3,y=4.3)+ #3933
  annotate(geom="text", label="*",size=9.5,x=13,y=5.9)+ #3935
  annotate(geom="text", label="*",size=9.5,x=20,y=8.2)+ #3943
  annotate(geom="text", label="*",size=9.5,x=1,y=3.3)+ #3944
  annotate(geom="text", label="*",size=9.5,x=12,y=5.9)+ #3945
  annotate(geom="text", label="*",size=9.5,x=18,y=7.5)+ #3949
  annotate(geom="text", label="*",size=9.5,x=16,y=6.75)+ #3953
  annotate(geom="text", label="*",size=9.5,x=11,y=5.98)+ #3954
  annotate(geom="text", label="*",size=9.5,x=15,y=6.2)+ #3958
  annotate(geom="text", label="*",size=9.5,x=19,y=8)+ #3962
  annotate(geom="text", label="*",size=9.5,x=5,y=4.4)+ #3965
  annotate(geom="text", label="*",size=9.5,x=10,y=5.35)+ #3966
  annotate(geom="text", label="*",size=9.5,x=6,y=4.8)+ #3967
  annotate(geom="text", label="*",size=9.5,x=4,y=4.5)+ #3968
  annotate(geom="text", label="*",size=9.5,x=35,y=14.6)+ #3994
  annotate(geom="text", label="*",size=9.5,x=14,y=6)+ #3999
  annotate(geom="text", label="*",size=9.5,x=7,y=4.9)+ #4005
  annotate(geom="text", label="*",size=9.5,x=2,y=4.6) #4008

# abaxial Stomata
#sig: 1777, 13,38,43,44,45,54,66,68,69,94,02,,04,05,10 !!!now: 56, 62, 08 <- something is definitely wrong.

ab.bar+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("darkorange", "dark blue","magenta"))+
  theme(legend.position = c(.24,.9))+
  theme(legend.direction="horizontal")+
  theme(axis.text.x=element_text(angle=-90,hjust=0))+
  ggtitle("hab Abaxial Stomatal Density")+
  ylab("Abaxial Stomata")+
  xlab("Genotypes")+
   coord_cartesian(ylim = c(0, 100)) +
  annotate(geom="text", label="*",size=10,x=36,y=69)+ #1777
  annotate(geom="text", label="*",size=10,x=28,y=48)+ #3913
  annotate(geom="text", label="*",size=10,x=35,y=65)+ #3938
  annotate(geom="text", label="*",size=10,x=26,y=47)+ #3943
  annotate(geom="text", label="*",size=10,x=32,y=53.5)+ #3944
  annotate(geom="text", label="*",size=10,x=30,y=48.3)+ #3945
  annotate(geom="text", label="*",size=10,x=24,y=45)+ #3954
  annotate(geom="text", label="*",size=10,x=34,y=55.8)+ #3966
  annotate(geom="text", label="*",size=10,x=38,y=92.5)+ #3968
  annotate(geom="text", label="*",size=10,x=31,y=51)+ #3969
  annotate(geom="text", label="*",size=10,x=27,y=47)+ #3994
  annotate(geom="text", label="*",size=10,x=25,y=45.5)+ #4002
  annotate(geom="text", label="*",size=10,x=33,y=55)+ #4004
  annotate(geom="text", label="*",size=10,x=37,y=81)+ #4005
  annotate(geom="text", label="*",size=10,x=29,y=50.5) #4010
    