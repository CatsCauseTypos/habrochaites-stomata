#Date: 10-3-2017
#Project: Habrochaites IL Pavement & stomatal cell density - just for thesis
#Author: Donnelly West - DonWest@UCDavis.edu
#Version: 6

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
data <- read.csv("~/Desktop/ILs_R-Friendly Data_mar_2016.csv") # full version
class(data$Pavement) # just checking..
data <- read.csv("~/Desktop/Sinha_Lab_stuff/hab-epidermis/R code/plswork2.csv") # the same file with hand-annotated significance

# ways to split up the ab/ad column ----

head(data)

# name.the.new.column <- substring function (the column you want, where.it.should.start, where.it.should.stop)

data$leaf.side <- as.factor(substr(data$ab.ad,1,2)) # this is super-useful to split by abaxial/adaxial sides

data$plant.number <- as.factor(substr(data$Plant..,2,2)) #Relevant plant # info

# checking the column classes
sapply(data, class)

#separating the data into the leaf sides for simplicity ----
adaxial <- data[grepl("ad",data$leaf.side),]
head(adaxial)

abaxial <- data[grepl("ab",data$leaf.side),]

avg_by_geno <- data %>% 
  group_by(IL, leaf.side) %>% 
  summarise(avg_pave = mean(Pavement), var_pave = var(Pavement), avg_stom = mean(Stomata), var_stom = var(Stomata)) # these numbers match the original stomatal analyses I did by hand in excel - WHEW!!


# how the heck can I normalize? ----

# first, let's split the avg data by leaf sides:

avg_ad_data <- avg_by_geno[grepl("ad",avg_by_geno$leaf.side),]

avg_ab_data <- avg_by_geno[grepl("ab",avg_by_geno$leaf.side),]

head(avg_ab_data) #worked

#standardize by domesticated parent ----

# Pavement

# abaxial
avg_ab_data$norm.ab <- avg_ab_data$avg_pave/as.numeric(avg_ab_data[1,3])

# adaxial
head(avg_ad_data) # 64.29167
avg_ad_data$norm.ad <- avg_ad_data$avg_pave/as.numeric(avg_ad_data[1,3])

# Stomata
# abaxial
avg_ab_data$norm.ab.stom <- avg_ab_data$avg_stom/as.numeric(avg_ab_data[1,5])
# adaxial
head(avg_ad_data) 
avg_ad_data$norm.ad.stom <- avg_ad_data$avg_stom/as.numeric(avg_ad_data[1,5])



# merging ab/ad ----
data1 <-  merge(x = avg_ab_data, y = avg_ad_data, by.x = 'IL', by.y = 'IL', by.x, drop = FALSE)
head(data1)

### significance ----

## But which ILs are actually, significantly different than parent 4024? 

#let's try making our own linear model ----
#(PS- it will compare EVERYTHING by THE FIRST THING, so name your stuff appropriately)

# adaxial stomata
ad.lm <- lm(Stomata~IL,adaxial)
sum.ad.lm <- as.data.frame(summary(ad.lm)$coefficients[,4])

# adaxial pavement
ad.v.lm <- lm(Pavement~IL,adaxial)
sum.ad.v.lm <- as.data.frame(summary(ad.lm)$coefficients[,4])

# abaxial stomata
ab.lm <- lm(Stomata~IL,abaxial)
sum.ab.lm<- as.data.frame(summary(ab.lm)$coefficients[,4])

# abaxial pavement
ab.v.lm <- lm(Pavement~IL,abaxial)
sum.ab.v.lm<- as.data.frame(summary(ab.v.lm)$coefficients[,4])


#the summary shows us 'what we expect' from looking at the bar graph of the stomatal counts (the ILs that look to have way more/less stomata than lyco parent are coming out as 'significantly different' than 4024)

#we still need to adjust for false discovery: adjusted p-value

# adjusted p for adaxial stomata ----
ad.adj.p <- data.frame(value=p.adjust(summary(ad.lm)$coefficients[,4], method="fdr")) 
ad.adj.p$IL=rownames(ad.adj.p) #this makes life easier later

# adjusted p for adaxial pavement
ad.v.adj.p <- data.frame(value=p.adjust(summary(ad.v.lm)$coefficients[,4], method="fdr")) 
ad.v.adj.p$IL=rownames(ad.v.adj.p) #this makes life easier later

# was trying to get the data sets to merge so that the adjusted p values would hang out with their respective observations / sigh 
# holdthephone <- adaxial
# 
# head(holdthephone)
# 
# wowzers <-  merge(x = holdthephone, y = ad.adj.p, by.x = 'IL', by.y = 'IL', by.x, drop = FALSE)
# head(wowzers)


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

# # _________________________ 
# library(multcomp) # trying dunnett test comparison ----
# 
# Group <- factor(c("A","A","B","B","B","C","C","C","D","D","D","E","E","F","F","F"))
# Value <- c(5,5.09901951359278,4.69041575982343,4.58257569495584,4.79583152331272,5,5.09901951359278,4.24264068711928,5.09901951359278,5.19615242270663,4.58257569495584,6.16441400296898,6.85565460040104,7.68114574786861,7.07106781186548,6.48074069840786)
# data <- data.frame(Group, Value)
# 
# fit <- aov(Value ~ Group, data)
# 
# set.seed(20140123)
# Dunnet <- glht(fit, linfct=mcp(Group="Dunnett"))
# summary(Dunnet)
# # https://stats.stackexchange.com/questions/83116/dunnetts-test-in-r-returning-different-values-each-time
# 
# summary(glht(fit, linfct = mcp(Group = "Dunnett")))


# 
# # on my data
# 
# head(adaxial)
# adaxial$IL <- as.factor(adaxial$IL)
# mine <- aov(Pavement ~ IL, adaxial)
# 
# summary(glht(mine, linfct = mcp(IL = "Dunnett")))
# 
# warnings() # the interwebz seems to ignore these. wtf? http://rstudio-pubs-static.s3.amazonaws.com/80007_5a95ad6c52b742409251b78306bd04d5.html
# 
# # works if you pretend the warnings don't freak you out. meanwhile, dunnett's isn't ideal for unequal samples, so TUKEYHSD is better (but really complicated to do correctly in R b/c the standard 'tukey' assumes equal # of samples too /sigh)

# ^ doesn't work yet, so write the chp and fix it in the real paper ???

# __________________

#And again for abaxial stomata
ab.adj.p <- data.frame(value=p.adjust(summary(ab.lm)$coefficients[,4], method="fdr")) 
ab.adj.p$Geno=rownames(ab.adj.p) 

# abaxial pavement
ab.v.adj.p <- data.frame(value=p.adjust(summary(ab.v.lm)$coefficients[,4], method="fdr")) 
ab.v.adj.p$Geno=rownames(ab.v.adj.p)
## saving files of adjusted p's 
# abaxial pavement
ab.v.sigf.p <- as.data.frame(ab.v.adj.p[which(ab.v.adj.p$value<.05),])
write.csv(ab.v.sigf.p,"significant_abaxial_pavement_habILs.csv") 
# abaxial stomatal
ab.sigf.p <- as.data.frame(ab.adj.p[which(ab.adj.p$value<.05),])
write.csv(ab.sigf.p,"significant_abaxial_stomata_ILs_hab.csv") 


head(data1)

setwd("~/Desktop/")

write.csv(data1, file = "hab_stats_R.csv")

#added parent & significance for both data sets in excel, by hand b/c r is awful ><

# _______________________________________________

data3 <- read.csv("~/Desktop/Sinha_Lab_stuff/hab-epidermis/Data/hab_stats_R_useful_categ.csv", head = T) # file back in

head(data3)
#Scatter plot ILs ----
# standardized-to-4024 ad vs ab
head(data1)
# # showing all the stat significant categories for pave & stomata is a bit too busy.
# ggplot(data1, aes( x = norm.ab, y = norm.ad, colour = signif )) +
#   geom_jitter()+
#   geom_point(size=3.5)+
#   theme_bw()+
#   scale_color_manual(name="Color Key", values=c("darkorange", "magenta","darkorchid3","darkseagreen2","deepskyblue2","yellow2", "black", "pink", "green"))+
#   theme(legend.position = "bottom")+
#   theme(legend.direction="horizontal")+
#   ggtitle("Standardized Pavement Density of S. habrochaites ILs")+
#   ylab("Standardized Adaxial Counts")+
#   xlab("Standardized Abaxial Counts")
# 
# this doesn't work and I don't know why... commenting out for now. 10/3/2017

head(data)
 # scatter ad vs ab for Pavement ILs ----
ggplot(data3, aes( x = norm.ab, y = norm.ad, colour = Sig.pav )) +
  geom_jitter()+
  geom_point(size=7)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        legend.text = element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25))+
  ggtitle("Standardized Pavement Density of S. habrochaites ILs")+
  ylab("Standardized Adaxial Counts")+
  xlab("Standardized Abaxial Counts")+
  scale_color_manual(name="", values=c( "deepskyblue2","darkseagreen2","magenta","darkorange", "darkorchid3"),
                     breaks = c("ab","ad","ab.ad","e6203","hab","none"),
                     labels = c("Abaxial", "Adaxial","Both Sides", "Lycopersicum Parent", "Habrochaites Parent", "No Difference"))
  
# chp1_pave_scatter.jpeg ratio 1500 - done 

 # Pavement scatter for poster (without key) final ----
ggplot(data3, aes( x = norm.ab, y = norm.ad, colour = Sig.pav )) +
  geom_jitter()+
  geom_point(size=3.5)+
  theme_bw()+
  scale_color_manual(values=c( "deepskyblue2","darkseagreen2","magenta","darkorange", "darkorchid3"))+
  theme(legend.position = "none")+
  ggtitle("Standardized Pavement Density of S. habrochaites ILs")+
  ylab("Standardized Adaxial Counts")+
  xlab("Standardized Abaxial Counts")+
  

# Stomata scatter plot
ggplot(data3, aes( x = norm.ab.stom, y = norm.ad.stom, colour = Sig.stom )) +
  geom_jitter()+
  geom_point(size=3.5)+
  theme_bw()+
  scale_color_manual(name="Color Key", values=c( "deepskyblue2", "darkseagreen2", "yellow2","magenta","darkorange", "darkorchid3"))+
  theme(legend.position = "bottom")+
  theme(legend.direction="horizontal")+
  ggtitle("Standardized Stomatal Density of S. habrochaites ILs")+
  ylab("Standardized Adaxial Counts")+
  xlab("Standardized Abaxial Counts")

# Stomata scatter plot without key final ----
ggplot(data3, aes( x = norm.ab.stom, y = norm.ad.stom, colour = Sig.stom )) +
  geom_jitter()+
  geom_point(size=3.5)+
  theme_bw()+
  scale_color_manual(name="Color Key", values=c( "deepskyblue2", "darkseagreen2", "yellow2","magenta","darkorange", "darkorchid3"))+
  theme(legend.position = "none",
              plot.title = element_text(lineheight=.8, face="bold", size = 24),
              axis.title.x = element_text(face="bold", size=20),
              axis.title.y = element_text(face="bold", size=20))+
  ggtitle("Standardized Stomatal Density \nof S. habrochaites ILs")+
  ylab("Standardized Adaxial Counts")+
  xlab("Standardized Abaxial Counts")



#separating the data into the leaf sides for simplicity

adaxial <- data[grepl("ad",data$leaf.side),]
head(adaxial)

abaxial <- data[grepl("ab",data$leaf.side),]
head(abaxial)
 # Create stomatal index ----

abaxial$index.ab <- (abaxial$Stomata/abaxial$Pavement)
head(abaxial)
adaxial$index.ad <- (adaxial$Stomata/adaxial$Pavement)
head(adaxial)

adaxial$unique <- c(adaxial$Parent, adaxial$IL, adaxial$Plant.., adaxial$ab.ad) # sept 2017 this doesn't work.. important??

# abaxial

ggplot(abaxial, aes(IL, index.ab, fill=Parent))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("darkorange", "darkblue","magenta"))+
  theme(legend.position = c(.24,.9))+
  theme(legend.direction="horizontal")+
  theme(axis.text.x=element_text(angle=-90,hjust=0))+
  ggtitle("Abaxial Stomatal Index")+
  ylab("Stomata/Pavement")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, .75))

# adaxial
ggplot(adaxial, aes(IL, index.ad, fill=Parent))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.8)+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("darkorange", "darkblue","magenta"))+
  theme(legend.position = c(.24,.9))+
  theme(legend.direction="horizontal")+
  theme(axis.text.x=element_text(angle=-90,hjust=0))+
  ggtitle("Stomatal Adaxial Index")+
  ylab("Stomata/Pavement")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, .2))
# putting that shit together in excel because FUCK R

write.csv(abaxial,"abaxial_pls_work.csv") 
write.csv(adaxial,"adaxial_pls_work.csv")

# together <-  merge(x = abaxial, y = adaxial, by.x = 'IL', by.y = 'IL', by.x, drop = FALSE)
# head(together)
# 
# aggregate(variable ~ model, together, paste, collapse = " ")

# ad and ab side-by-side index: ugly ----
together <- read.csv("~/Desktop/together_pls_work.csv", header = TRUE)
head(together)
ggplot(together, aes(IL, index, fill=Parent))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("darkorange", "darkblue","magenta"))+
  theme(legend.position = c(.24,.93))+
  theme(legend.direction="horizontal")+
  theme(axis.text.x=element_text(angle=-90,hjust=0, vjust = 0.5))+
  ggtitle("Stomatal Index")+
  ylab("Stomata/Pavement")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, .5))

# parental index final ----

# made a separate data set with only hab & lyco to do the parents; then ran ttests on the pairs under "4goodnesssakes_stats.csv" because FOR THE LIFE OF ME, I couldn't get the analyses to work in r. 
tryAgain <- read.csv("~/Desktop/Sinha_Lab_stuff/hab-epidermis/Data/4goodnesssakes.csv")

tryAgain = na.omit(tryAgain)
head(tryAgain)


ggplot(tryAgain, aes(IL, index, fill=Parent))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("darkorange","magenta"))+
  theme(legend.position = c(.2,.92))+
  theme(legend.direction="horizontal",
        plot.title = element_text(lineheight=.8, face="bold", size = 24),
              axis.title.x = element_text(face="bold", size=20),
              axis.title.y = element_text(face="bold", size=20))+
  ggtitle("Parental Lines Stomatal Index")+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = IL, 
                    y = (index+.07),
                    label = In.sign),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  ylab("Stomata/Pavement")+
  xlab("Genotype & Leaf Surface")+
  coord_cartesian(ylim = c(0, .7))+
  scale_x_discrete(labels=c("S. lyco adaxial", "S. lyco abaxial", 
                            "S. hab adaxial", "S. hab abaxial"))




# failed stats on parental index:

# whole data set with index:
data$Index <- (data$Stomata/data$Pavement)

anova(lm(Index~IL*leaf.side, data)) # this works

tuke.index <- TukeyHSD(aov(lm(Index~IL*leaf.side, data))) 

sum.tuke.index <- as.data.frame(tuke.index[3])


# checking on all of them, just for funsies:
tuke.index[3]

# which ones have a p of less than 0.05?
sig.sum.tuke.index <- as.data.frame(sum.tuke.index[which(sum.tuke.index$IL.leaf.side.p.adj<.05),])

# but there's so many comparisons... it's kind of useless... I really just wanted to get this between the two parents and their two sides, but the anova won't work on that =/ 

# let's try averages:

wtff <- data %>%
  group_by(IL, leaf.side) %>% 
  summarise(avg_pave = mean(Pavement), avg_stom = mean(Stomata)) %>% 
  mutate(index.avg = avg_stom/avg_pave)

# this worked, except hab values come up as NA - missing values??? don't know why. going to add them manually. fuck you r.

# hab ab index: 
(wtff[4,5])
wtff[4,5] <- (65.08333/137.66667)

# hab ad index

(wtff[5,5])
wtff[5,5] <- (11.91667/97.41667)

# i mean for fuck's sake! the whole time it was because of NA's?!?!
data = na.omit(data)

together = na.omit(together)

anova(lm(index.avg~IL*leaf.side, wtff)) 

# ERROR: F-TESTS ON AN ESSENTIALLY perfect fit are unreliable

tuke.index <- TukeyHSD(aov(lm(Index~IL*leaf.side, data))) 

sum.tuke.index <- as.data.frame(tuke.index[3])


# checking on all of them, just for funsies:
tuke.index[3]

# which ones have a p of less than 0.05?
sig.sum.tuke.index <- as.data.frame(sum.tuke.index[which(sum.tuke.index$IL.leaf.side.p.adj<.05),])





# Bar graphs ----
#Bar graph showing the stomata ILs and parents with error bars

# bar sig abaxial stomata final ----
ggplot(abaxial, aes(reorder(IL,Stomata, mean), Stomata, fill=Parent))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.8)+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("darkorange", "darkblue","magenta"))+
  theme(legend.position = c(.24,.9))+
  theme(legend.direction="horizontal")+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = IL, 
                    y = (Stomata+5),
                    label = Ab.Stom),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  theme(axis.text.x=element_text(angle=-90,hjust=0, vjust = 0.5),
        plot.title = element_text(lineheight=.8, face="bold", size = 28),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=18))+
  ggtitle("Abaxial Stomatal Density")+
  ylab("Average Abaxial Stomata")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 100))


# abaxial pavement cells final ----

ggplot(abaxial, aes(reorder(IL,Pavement, mean), Pavement, fill=Parent))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("darkorange", "darkblue","magenta"))+
  theme(legend.position = c(.24,.9))+
  theme(legend.direction="horizontal")+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = IL, 
                    y = (Pavement+15),
                    label = Ab.Pav),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  theme(axis.text.x=element_text(angle=-90,hjust=0, vjust = 0.5),
        plot.title = element_text(lineheight=.8, face="bold", size = 28),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=18))+
  ggtitle("Abaxial Pavement Cell Density")+
  ylab("Average Abaxial Pavement Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 210))



# Adaxial stomatal final ----
ggplot(adaxial, aes(reorder(IL,Stomata, mean), Stomata, fill=Parent))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("orange", "dark blue","magenta"))+
  theme(legend.position = c(.24,.9))+
  theme(legend.direction="horizontal")+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = IL, 
                    y = (Stomata+1.3),
                    label = Ad.Stom),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  theme(axis.text.x=element_text(angle=-90,hjust=0, vjust = 0.5),
        plot.title = element_text(lineheight=.8, face="bold", size = 28),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=19))+
  ggtitle("Adaxial Stomatal Density")+
  ylab("Average Adaxial Stomata")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 18))

# adaxial pavement final ----
ggplot(adaxial, aes(reorder(IL,Pavement, mean), Pavement, fill=Parent))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("orange", "dark blue","magenta"))+
  theme(legend.position = c(.24,.9))+
  theme(legend.direction="horizontal")+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = IL, 
                    y = (Pavement+15),
                    label = Ad.Pav),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  theme(axis.text.x=element_text(angle=-90,hjust=0, vjust = 0.5),
        plot.title = element_text(lineheight=.8, face="bold", size = 28),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=18))+
  ggtitle("Adaxial Pavement Cell Density")+
  ylab("Average Adaxial Pavement Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 170))


# just parents for narrative 10/3/2017 ----
head(data)
parents <- data[grepl("a",data$parents_only),]
setwd("~/Desktop/")
write.csv(parents, "parents_trial_1.csv")
# changed data around manually, then saved

# import parent-only file: ----
parents_trial_1 <- read.csv("~/Desktop/parents_trial_1.csv", header = TRUE)
head(parents_trial_1)

# check out data quickly:
parents_trial_1 %>% 
  group_by(IL, leaf.side, cell_type) %>% 
  ggplot(.,aes(x = Parent, y = cell_num))+
  geom_boxplot()+
  facet_grid( cell_type~leaf.side)

# i see no way to get them to plot nicely side-by-side since pave are so much higher # than stom

# split out pave vs stomata: ----
parents_pave <- parents_trial_1[grepl("pave",parents_trial_1$cell_type),]
parents_stom <- parents_trial_1[!grepl("pave",parents_trial_1$cell_type),]




# split for stats b/c tired

pare_pave_ad <- parents_pave[grepl("ad",parents_pave$leaf.side),]
pare_pave_ab <- parents_pave[!grepl("ad",parents_pave$leaf.side),]

pare_stom_ad <- parents_stom[grepl("ad",parents_stom$leaf.side),]
pare_stom_ab <- parents_stom[!grepl("ad",parents_stom$leaf.side),]

e62_stom <- parents_stom[grepl("e6203",parents_stom$IL),]
hab_pave <- parents_pave[!grepl("e6203",parents_pave$IL),]

e62_pave <- parents_pave[grepl("e6203",parents_pave$IL),]
hab_stom <- parents_stom[!grepl("e6203",parents_stom$IL),]

# ______
# run stats on parents alone (this part was silly, I was being inefficient and tired)

compare_means( cell_num ~ IL ,  data = pare_pave_ab, method = "t.test") # significant p = 2e-12
compare_means( cell_num ~ IL ,  data = pare_pave_ad, method = "t.test") # significant p = 5.3e-05
compare_means( cell_num ~ IL ,  data = pare_stom_ab, method = "t.test") # significant p = 7.7e-13

compare_means( cell_num ~ IL ,  data = pare_stom_ad, method = "t.test") # NOT significant p = 0.14

# summary: 
# pave ab & ad hab vs lyc sig
# stom ab sig, ad NS

#_______
# just run a darn tukey: 
tuke_hope <- TukeyHSD(aov(lm(cell_num~IL*leaf.side, parents_pave))) 

sum.tuke.trialb.index_ab <- as.data.frame(tuke_hope[3])
head(sum.tuke.trialb.index_ab)
# which ones have a p of less than 0.05?
sig.sum.tuke.trialb.index_ab <- as.data.frame(sum.tuke.trialb.index_ab[which(sum.tuke.trialb.index_ab$IL.leaf.side.p.adj<.05),])
sig.sum.tuke.trialb.index_ab # print
# output: 
# LA1777:ab-e6203  :ab          72.95833         54.53702         91.37965       0.000000e+00
# LA1777:ad-e6203  :ab          32.70833         14.28702         51.12965       8.319311e-05
# e6203  :ad-LA1777:ab         -73.37500        -91.79632        -54.95368       0.000000e+00
# LA1777:ad-LA1777:ab          -40.25000        -61.52110        -18.97890       2.661349e-05
# LA1777:ad-e6203  :ad          33.12500         14.70368         51.54632       6.687454e-05

# all that for stomata too: 
tuke_hope <- TukeyHSD(aov(lm(cell_num~IL*leaf.side, parents_stom))) 
sum.tuke.trialb.index_ab <- as.data.frame(tuke_hope[3])
sig.sum.tuke.trialb.index_ab <- as.data.frame(sum.tuke.trialb.index_ab[which(sum.tuke.trialb.index_ab$IL.leaf.side.p.adj<.05),])
sig.sum.tuke.trialb.index_ab
# output:
# LA1777:ab-e6203  :ab           33.83333         27.48371         40.18295       0.000000e+00
# e6203  :ad-e6203  :ab         -21.62500        -26.80944        -16.44056       0.000000e+00
# LA1777:ad-e6203  :ab          -19.33333        -25.68295        -12.98371       8.736101e-11
# e6203  :ad-LA1777:ab          -55.45833        -61.80795        -49.10871       0.000000e+00
# LA1777:ad-LA1777:ab           -53.16667        -60.49858        -45.83476       0.000000e+00


# hand-annotated TukeyHSD categories to spreadsheet under "sig", then re-imported and split data to make graphs:

# bar parents pave ----


ggplot(parents_pave, aes(x = reorder(IL, Parent), y = cell_num, fill = leaf.side))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes(y = (cell_num+20),
                   label = sig),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  # stat_summary(fun.data=plot.summary,
  #              geom = "text", 
  #              aes(y = (cell_num-48),
  #                  label = n),
  #              position = position_dodge(.6),
  #              color = "white",
  #              size = 5.6)+
  scale_fill_manual(values= c( "darkgreen", "green3"),
                    breaks = c("ab", "ad"),
                    labels = c("Abaxial","Adaxial"))+
  theme(legend.position = c(.1,.82),
        legend.direction="vertical",
        legend.title=element_blank(),
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        axis.text.x = element_text(face = "italic"))+
  ggtitle("Pavement Cell Density in Two Species\n")+
  ylab("Average Number of Pavement Cells\n")+
  xlab("")+
  coord_cartesian(ylim = c(0, 180))+
  scale_x_discrete(breaks = c("LA1777", "e6203  "),
                   labels = c("S. habrochaites","S. lycopersicum"))
# chp1-parent_pavement.jpeg - done


# bar parents stom ----


ggplot(parents_stom, aes(x = reorder(IL, Parent), y = cell_num, fill = leaf.side))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes(y = (cell_num+8),
                   label = sig),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  # stat_summary(fun.data=plot.summary,
  #              geom = "text", 
  #              aes(y = (cell_num-48),
  #                  label = n),
  #              position = position_dodge(.6),
  #              color = "white",
  #              size = 5.6)+
  scale_fill_manual(values= c( "darkgreen", "green3"),
                    breaks = c("ab", "ad"),
                    labels = c("Abaxial","Adaxial"))+
  theme(legend.position = c(.1,.82),
        legend.direction="vertical",
        legend.title=element_blank(),
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        axis.text.x = element_text(face = "italic"))+
  ggtitle("Stomatal Density in Two Species\n")+
  ylab("Average Number of Stomata\n")+
  xlab("")+
  coord_cartesian(ylim = c(0, 72))+
  scale_x_discrete(breaks = c("LA1777", "e6203  "),
                   labels = c("S. habrochaites","S. lycopersicum"))
# chp1-parent_stom.jpeg - 1500 aspect ratio ; done