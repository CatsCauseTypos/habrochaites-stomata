#Date: 3-23-2016
#Project: Habrochaites Trial B drought
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


#install & load needed libraries
library(lme4)
library(ggplot2)
library(dplyr)
library(cowplot)

Trial_b <- read.csv("~/Downloads/Trial_b_R_HAPPY_mar_23_2016.csv")

# right now, the data set is only Adaxial, but both pavement and stomata

head(Trial_b)

# name.the.new.column <- substring function (the column you want, where.it.should.start, where.it.should.stop)

Trial_b$leaf.side <- substr(Trial_b$Slide.Side,2,3) # this is super-useful to split by abaxial/adaxial sides
Trial_b$slide.number <- substr(Trial_b$Slide.Side,1,1) # 


# checking the column classes
sapply(data, class)


avg_by_geno <- data %>% 
  group_by(Geno, leaf.side) %>% 
  summarise(avg_pave = mean(Pavement), var_pave = var(Pavement), avg_stom = mean(Stomata), var_stom = var(Stomata)) # these numbers match the original stomatal analyses I did by hand in excel - WHEW!!


# how the heck can I normalize? 

# first, let's split the avg data by leaf sides: 

avg_ad_data <- avg_by_geno[grepl("ad",avg_by_geno$leaf.side),]



avg_ab_data <- avg_by_geno[grepl("ab",avg_by_geno$leaf.side),]

head(avg_ab_data) #worked

avg_ab_data$norm.ab <- avg_ab_data$avg_pave/64.70833 #stole this # from the domesticated parent count (that's what we normalize to) - I know there's a way in R to call a certail value from a specific cell, but I don't remember how

head(avg_ad_data) # 64.29167
avg_ad_data$norm.ad <- avg_ad_data$avg_pave/64.291666

data1 <-  merge(x = avg_ab_data, y = avg_ad_data, by.x = 'Geno', by.y = 'Geno', by.x, drop = FALSE)


setwd("~/Desktop/")

write.csv(data1, file = "hab_stats_R.csv")
#added parent & significance for both data sets in excel, by hand b/c ><

setwd("~/Desktop/")

data1 <- read.csv("hab_stats_R.csv", head = T) # file back in

head(data1)
#Scatter plot of standarized-to-4024 ad vs ab

ggplot(data1, aes( x = norm.ab, y = norm.ad, colour = Pave_sig) ) +
  geom_jitter()+
  geom_point(size=3.5)+
  theme_bw()+
  scale_color_manual(name="Color Key", values=c("darkorange", "magenta","darkorchid3","darkseagreen2","deepskyblue2","yellow2"))+
  theme(legend.position = "bottom")+
  theme(legend.direction="horizontal")+
  ggtitle("Standardized Pavement Density of S. habrochaites ILs")+
  ylab("Standardized Adaxial Counts")+
  xlab("Standardized Abaxial Counts")


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

# remind me what's in data
head(data)

# way to plot abaxial and adaxial side/side
data %>% 
  group_by(leaf.side) %>% 
  ggplot(., aes(reorder(Geno,Stomata, mean), Stomata, fill = Parent ))+
           theme_bw()+
           stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
           stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
           scale_fill_manual(name="Color Key", values = c("orange", "dark blue","magenta"))+
           theme(legend.position = c(.24,.9))+
           theme(legend.direction="horizontal")+
           theme(axis.text.x=element_text(angle=-90,hjust=0))+
           ggtitle("Adaxial Stomatal Density_maybe")+
           ylab("Average Adaxial Stomata")+
           xlab("Genotypes")+
           coord_cartesian(ylim = c(0, 70))

# # par(mfrow) - is this a thing? holy crap, it worked... will it work with ggplot?
# ##  Open a new default device.
# 
get( getOption( "device" ) )()
# 
# ##  Set up plotting in two rows and three columns, plotting along rows first.
# 
 par( mfrow = c( 2, 3 ) )
# 
# ##  The first plot is located in row 1, column 1:
# 
 plot( rnorm( n = 10 ), col = "red", main = "plot 1", cex.lab = 1.1 )
# 
# ##  The second plot is located in row 1, column 2:
# 
 plot( runif( n = 10 ), col = "blue", main = "plot 2", cex.lab = 1.1 )
# 
# ##  The third plot is located in row 1, column 3:
# 
# plot( rt( n = 10, df = 8 ), col = "springgreen4", main = "plot 3",
#       cex.lab = 1.1 )
# 
# ##  The fourth plot is located in row 2, column 1:
# 
# plot( rpois( n = 10, lambda = 2 ), col = "black", main = "plot 4",
#       cex.lab = 1.1 )
# 
# ##  plot.new() skips a position.
# 
plot.new()
# 
# ##  The fifth plot is located in row 2, column 3:
# 
# plot( rf( n = 10, df1 = 4, df2 = 8 ), col = "gray30", main = "plot 5",
#       cex.lab = 1.1 )
# 

# Will it work with ggplot2? nope. but facet wrap does basically the same thing

 ggplot(data, aes(reorder(Geno,Stomata, mean), Stomata, fill=Parent))+
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
   coord_cartesian(ylim = c(0, 180)) +
   facet_wrap(~leaf.side)
 # works, but the reorder gets screwed up by taking the mean of ALL observations vs. by-side-observations; also, the scale CANNOT stay the same zomg
 
 
 ggplot(data, aes(Geno, Pavement, fill=Parent))+
   theme_bw()+
   stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
   stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
   scale_fill_manual(name="Color Key", values = c("orange", "dark blue","magenta"))+
   theme(legend.position = c(.15,.9))+
   theme(legend.direction="horizontal")+
   theme(axis.text.x=element_text(angle=-90,hjust=0))+
   ggtitle("Adaxial Pavement Density")+
   ylab("Average Pavement")+
   xlab("Genotypes")+
   coord_cartesian(ylim = c(0, 200)) +
   facet_wrap(~leaf.side)
 # works, but the reorder gets screwed up by taking the mean of ALL observations vs. by-side-observations; also, the scale CANNOT stay the same zomg 
 
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
  theme(axis.text.x=element_text(angle=-90,hjust=0, vjust = .45))+
  ggtitle("Adaxial Pavement Cell Density")+
  ylab("Average Adaxial Pavement Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 150))
  

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


## But which ILs are actually, significantly different than parent 4024? 

#let's try making our own linear model
#(PS- it will compare EVERYTHING by THE FIRST THING, so name your stuff appropriately)
#adaxial
ad.lm <- lm(Stomata~Geno,adaxial)
summary(ad.lm)
sum.ad.lm <- as.data.frame(summary(ad.lm)$coefficients[,4])

# adaxial pavement
ad.v.lm <- lm(Pavement~Geno,adaxial)
sum.ad.v.lm <- as.data.frame(summary(ad.lm)$coefficients[,4])

#abaxial stomata
ab.lm <- lm(Stomata~Geno,abaxial)
sum.ab.lm<- as.data.frame(summary(ab.lm)$coefficients[,4])

# abaxial pavement
ab.v.lm <- lm(Pavement~Geno,abaxial)
sum.ab.v.lm<- as.data.frame(summary(ab.lm)$coefficients[,4])


#the summary shows us 'what we expect' from looking at the bar graph of the stomatal counts (the ILs that look to have way more/less stomata than lyco parent are coming out as 'significantly different' than 4024)

#we still need to adjust for false discovery: adjusted p-value

# adjusted p for adaxial stomata
ad.adj.p <- data.frame ( #creating a dataframe where..
  value = p.adjust( #the column named "values" is full of adjusted p values from..
    summary(ad.lm)$coefficients[,4], #our initial linear model p values, corrected by...
    method="fdr"))  # fdr method.

# as a side note, bh is stricter, but fdr is okay; people are more familiar with bh in published papers

ad.adj.p$Geno=rownames(ad.adj.p) #this makes life easier later

# adjusted p for adaxial pavement
ad.v.adj.p <- data.frame(value=p.adjust(summary(ad.v.lm)$coefficients[,4], method="fdr")) 
ad.v.adj.p$Geno=rownames(ad.v.adj.p) #this makes life easier later
write.csv(ad.v.adj.p,"significant_adaxial_ILs_hab.csv") #making a file for prosperity

# trying bh correction:
# ad.v.adj.p.bh <- data.frame(value=p.adjust(summary(ad.v.lm)$coefficients[,4], method="BH")) 
# ad.v.adj.p.bh$Geno=rownames(ad.v.adj.p.bh) 
# write.csv(ad.v.adj.p.bh,"significant_adaxial_ILs_hab_bh.csv") #making a file for prosperity

# compared the fdr vs bh for the adaxial pavement data --> identical. like number for number identical. wow.

# false discovery rate/bh correction: it's good that e6203 is severley negative; if it goes to 0, it indicates that there's something wrong with the test.

# ANOVA would be beneficial because you can also see how the ILs look compared to each other and compared to hab without reorganizing everything and doing another linear model 

anova(lm(Stomata~Geno,adaxial)) #verifies that there are statistically significant differences in the data set - WOO!

tuke.test <- TukeyHSD(aov(lm(Stomata~Geno, adaxial))) #makes a giant list of scary..

# Steven's trick to pull out good bits from the scary: 

sum.tuke.test <- as.data.frame(tuke.test[1])

sig.sum.tuke.test <- as.data.frame(sum.tuke.test[which(sum.tuke.test$Geno.p.adj<.05),])

# make lists of the significant ones:
# adaxial pavement
ad.v.sigf.p <- as.data.frame(ad.v.adj.p[which(ad.v.adj.p$value<.05),])
write.csv(ad.v.sigf.p,"significant_adaxial_pavement_habILs.csv") 
# adaxial stomatal
ad.sigf.p <- as.data.frame(ad.adj.p[which(ad.adj.p$value<.05),])
write.csv(ad.sigf.p,"significant_adaxial_ILs_hab.csv") #making a file for prosperity

# PCA 

# make a new file with NO character info (you can leave genotypes, but we have to tell it that we have row names); I took data1, wrote csv, and renamed columns with ab/ad instead of x/y to delete the character columns. 

data_PCA <- read.csv("~/Desktop/hab_ILs_for_PCA.csv", row.names = 1, header = TRUE)

PC <- princomp(data_PCA) #holy crap, it worked.

#Standard deviations:
# Comp.1       Comp.2       Comp.3       Comp.4       Comp.5       Comp.6       Comp.7 
# 5.615567e+02 2.382435e+02 5.186546e+01 3.275421e+01 1.021257e+01 8.547071e+00 4.172739e+00 
# Comp.8       Comp.9      Comp.10 
# 2.539513e+00 9.141850e-07 0.000000e+00 
# 
# 10  variables and  38 observations.

# column 10 contributes nothing; lol

# plot PC just to see

plot(PC) # so we know 1 accounts for most..

biplot(PC) # this looks like stomata and pavement are completely separate, but it may be an artifact of having ab/ad pooled; need to do more split up pca's

# subset PCAs

sub.ad.pc <- princomp(data_PCA, subset = data_PCA[,1:5]) # looks like that's trying to grab rows 1-5 instead of columns 1-5... despite all logic.

sub.ad.pc <- princomp(data_PCA, subset = rep_len(TRUE, nrow(as.matrix(1:5)))) #it ignores this entirely and does a regular pca

?prcomp # looks like this subsetting by rows only (and that explains why princomp was being weird too)

# Steven was all, "maybe we could just pass it the things we want" AND SO WE DID!!!
 data_PCA[,1:5] %>% princomp() %>% biplot() # worked

# looks like the averages / variances aren't really giving us enough spread to see the data do its thing; let's use the raw values next time

##########################
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

# hand annotated significance in Parental column it goes stomata.pavement
data.sig <- read.csv("hab_stats_R_3_2016.csv", head = T) # file in


# HEY YOU - don't forget, you want to add ALL OF THE p-values to their own rows in the data frame!!

#Scatter plot of standardized-to-4024 ad vs ab stomata & pavement

ggplot(data.sig, aes( x = data1$norm.ad, y = data1$norm.ab, colour = signif) ) +
  geom_jitter()+
  geom_point(size=3.5)+
  theme_bw()+
  scale_colour_manual(name="Color Key: Stomata.Pavement", values = c("magenta", "yellow2","darkorchid3", "darkgoldenrod4", "darkolivegreen2","darkseagreen2","gray","blue2", "red", "darkorange"))+
  guides(colour  = guide_legend(nrow = 2))+
  theme(legend.position = "bottom")+
  theme(legend.direction="horizontal")+
  ggtitle("Standardized Pavement & Stomatal Density of S. habrochaites ILs")+
  xlab("Standardized Adaxial Counts")+
  ylab("Standardized Abaxial Counts")

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
#sig: 1777, 13,38,43,44,45,54,66,68,69,94,02,,04,05,10 
#ab.sig<-
  omfg <- ab.bar+
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
    omfg + annotate(geom="text", label="*",size=10,x=26,y=47)+ #3943
  annotate(geom="text", label="*",size=10,x=31,y=49.9)+ #3944
  annotate(geom="text", label="*",size=10,x=30,y=48.3)+ #3945
  annotate(geom="text", label="*",size=10,x=24,y=45)+ #3954
  annotate(geom="text", label="*",size=10,x=34,y=55.8)+ #3966
    annotate(geom="text", label="*",size=10,x=37,y=74.5)+ #3968
    annotate(geom="text", label="*",size=10,x=32,y=51)+ #3969
    annotate(geom="text", label="*",size=10,x=26,y=47)+ #3994
    annotate(geom="text", label="*",size=10,x=25,y=45.5)+ #4002
    annotate(geom="text", label="*",size=10,x=33,y=55)+ #4004
    annotate(geom="text", label="*",size=10,x=38,y=81)+ #4005
    annotate(geom="text", label="*",size=10,x=29,y=50.5) #4010
    