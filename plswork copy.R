#Date: 3-28-2016
#Project: Habrochaites IL Pavement & stomatal cell density - just for poster
#Author: Donnelly West - DonWest@UCDavis.edu
#Version: 5.0

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
data <- read.csv("~/Downloads/plswork.csv") # the same file with hand-annotated significance

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
  group_by(Geno, leaf.side) %>% 
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
data1 <-  merge(x = avg_ab_data, y = avg_ad_data, by.x = 'Geno', by.y = 'Geno', by.x, drop = FALSE)
head(data1)
################################################### significance ----

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
sum.ab.v.lm<- as.data.frame(summary(ab.lm)$coefficients[,4])


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


head(data1)

setwd("~/Desktop/")

write.csv(data1, file = "hab_stats_R.csv")

#added parent & significance for both data sets in excel, by hand b/c ><


setwd("~/Desktop/")

data3 <- read.csv("hab_stats_R_useful_categ.csv", head = T) # file back in

head(data3)
#Scatter plot ----
# standardized-to-4024 ad vs ab

# showing all the stat significant categories for pave & stomata is a bit too busy.
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

head(data)
 # ad vs ab for Pavement
ggplot(data3, aes( x = norm.ab, y = norm.ad, colour = Sig.pav )) +
  geom_jitter()+
  geom_point(size=3.5)+
  theme_bw()+
  scale_color_manual(name="Color Key", values=c( "deepskyblue2","darkseagreen2","magenta","darkorange", "darkorchid3"))+
  theme(legend.position = "bottom")+
  theme(legend.direction="horizontal")+
  ggtitle("Standardized Pavement Density of S. habrochaites ILs")+
  ylab("Standardized Adaxial Counts")+
  xlab("Standardized Abaxial Counts")

 # Pavement scatter for poster (without key)
ggplot(data3, aes( x = norm.ab, y = norm.ad, colour = Sig.pav )) +
  geom_jitter()+
  geom_point(size=3.5)+
  theme_bw()+
  scale_color_manual(values=c( "deepskyblue2","darkseagreen2","magenta","darkorange", "darkorchid3"))+
  theme(legend.position = "none")+
  ggtitle("Standardized Pavement Density of S. habrochaites ILs")+
  ylab("Standardized Adaxial Counts")+
  xlab("Standardized Abaxial Counts")

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

adaxial$unique <- c(adaxial$Parent, adaxial$IL, adaxial$Plant.., adaxial$ab.ad)

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
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
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

# what if we just looked at parental indecies? ----
tryAgain <- read.csv("~/Desktop/4goodnesssakes.csv")

ggplot(tryAgain, aes(IL, index, fill=Parent))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("darkorange","magenta"), labels=c("S. habrochaites", "S. lycopersicum"))+
  theme(legend.position = c(.18,.92))+
  theme(legend.direction="vertical",
        plot.title = element_text(lineheight=.8, face="bold", size = 35),
              axis.title.x = element_text(face="bold",vjust = 1, size=25),
              axis.text.x  = element_text(angle=20, vjust = .5, size=20),
              axis.title.y = element_text(face="bold", size=25, vjust=10),
              axis.text.y  = element_text(size=20))+
  ggtitle("Parental Lines Stomatal Index")+
  ylab("Stomata/Pavement")+
  xlab(" ")+
  coord_cartesian(ylim = c(0, .7))+
  scale_x_discrete(labels=c("S. lycopersicum adaxial", "S. lycopersicum abaxial", 
                            "S. habrochaites adaxial", "S. habrochaites abaxial"))+
  theme(legend.title = element_text( size=25, face="bold"))+
  theme(legend.text = element_text( size = 20))


# Bar graphs ----
#Bar graph showing the stomata ILs and parents with error bars

# bar sig abaxial stomata final ----
ggplot(abaxial, aes(reorder(IL,Stomata, mean), Stomata, fill=Parent))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
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





