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


#install & load needed libraries ----
library(lme4)
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5)) # for centered titles
library(dplyr)
library(cowplot)

# get your data - NEWEST DATA 3/7/2016 ----
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
head(data)
avg_by_geno <- data %>% 
  group_by(IL, leaf.side) %>% 
  summarise(avg_pave = mean(Pavement), var_pave = var(Pavement), avg_stom = mean(Stomata), var_stom = var(Stomata)) # these numbers match the original stomatal analyses I did by hand in excel - WHEW!!
avg_by_geno

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

data3 <- read.csv("~/Desktop/Sinha_Lab_stuff/hab-epidermis/Data/hab_stats_R_useful_categ.csv", head = T) # file back in

head(data3)

#Statistics -----
# for stats, see "plswork.R" or "plswork2.R" 

#Scatter plot ILs (done) ----
 # scatter ad vs ab for Pavement ILs (done) ----
q <- ggplot(data3, aes( x = norm.ab, y = norm.ad, colour = Sig.pav )) +
  geom_jitter()+
  geom_point(size=7)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        legend.text = element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25))+
  ggtitle("   Standardized Pavement Density of S. habrochaites ILs\n")+
  ylab("Standardized Adaxial Counts     \n")+
  xlab("\nStandardized Abaxial Counts")+
  scale_color_manual(name="", values=c( "deepskyblue2","darkseagreen2","magenta","darkorange", "darkorchid3"),
                     breaks = c("ab","ad","ab.ad","e6203","hab","none"),
                     labels = c("Abaxial", "Adaxial","Both Sides", "Lycopersicum Parent", "Habrochaites Parent", "No Difference"))
  
# chp1_pave_scatter.jpeg ratio 1500 - done 


# Stomata scatter plot (done) ----
p<-ggplot(data3, aes( x = norm.ab.stom, y = norm.ad.stom, colour = Sig.stom )) +
  geom_jitter()+
  geom_point(size=7)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        legend.text = element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 1)))+
  ggtitle("  Standardized Stomatal Density of S. habrochaites ILs\n")+
  ylab("Standardized Adaxial Counts     \n")+
  xlab("\nStandardized Abaxial Counts")+
  scale_color_manual(name="", values=c(  "deepskyblue2", "darkseagreen2", "yellow2","magenta","darkorange", "darkorchid3"),
                     breaks = c("ab","ad","ab.ad","e6203","hab","none"),
                     labels = c("Abaxial", "Adaxial","Both Sides", "Lycopersicum Parent", "Habrochaites Parent", "No Difference"))

# trying to add letters:
ggdraw(p)+draw_plot_label("A", size = 28)

# chp1_stom_scatter 1200
ggdraw(q)+draw_plot_label("B", size = 28)
#chp1_pave_scatter 1200

#save_plot() - for multigrids
?save_plot

# plot_grid(p, q, labels = "AUTO") - with legend, super awkward
# chp1_stom__ILs_scatter.jpeg ratio 1500 - done 



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

# abaxial

ggplot(abaxial, aes(IL, index.ab, fill=Parent))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.8)+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("darkorange", "darkblue","magenta"))+
  theme(legend.position = c(.24,.9))+
  theme(legend.direction="horizontal")+
  theme(axis.text.x=element_text(angle=-90,hjust=0),
        plot.title = element_text(hjust = 0.5))+
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
  theme(legend.direction="horizontal",
        plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x=element_text(angle=-90,hjust=0))+
  ggtitle("Stomatal Adaxial Index")+
  ylab("Stomata/Pavement")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, .2))

# ad and ab side-by-side index: ugly ----
together <- read.csv("~/Desktop/Sinha_Lab_stuff/hab-epidermis/Data/together_pls_work.csv", header = TRUE) # put data sets together in excel b/c r was being a butt

head(together)

ggplot(together, aes(IL, index, fill=Parent))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.8)+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("darkorange", "darkblue","magenta"))+
  theme(legend.position = c(.24,.93))+
  theme(legend.direction="horizontal")+
  theme(axis.text.x=element_text(angle=-90,hjust=0, vjust = 0.5),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Stomatal Index")+
  ylab("Stomata/Pavement")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, .5))
# chp1_index_together_ILs.png

# parental index final ----

# made a separate data set with only hab & lyco to do the parents; then ran ttests on the pairs under "4goodnesssakes_stats.csv" because FOR THE LIFE OF ME, I couldn't get the analyses to work in r. 
tryAgain <- read.csv("~/Desktop/Sinha_Lab_stuff/hab-epidermis/Data/4goodnesssakes.csv")

head(tryAgain)

# parental index done ----
ggplot(tryAgain, aes(IL, index, fill=Parent))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.5)+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1)+
  scale_fill_manual(name="", values = c("darkorange","magenta"),
                    breaks = c("Hab","Lyco"),
                    labels = c("S. habrochaites", "S. lycopersicum"))+
  theme(legend.position = c(.13,.81),
        legend.direction="vertical",
        legend.title=element_blank(),
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 25, face = "italic"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 1)))+
  ggtitle("Stomatal Index Across Two Species\n")+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = IL, 
                    y = (index+.08),
                    label = In.sign),
               position = position_dodge(.6),
               color = "black",
               size = 10)+
  ylab("Stomata/Pavement\n")+
  xlab("\nGenotype & Leaf Surface")+
  coord_cartesian(ylim = c(0, .7))+
  scale_x_discrete(labels=c("S. lyco adaxial", "S. lyco abaxial", 
                            "S. hab adaxial", "S. hab abaxial"))

# chp1_index__ILs_bar.jpeg ratio 1500 - done 


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
head(data)
# bar sig abaxial stomata final (needs arab_gene help to make clear) ----
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
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = IL, 
                    y = (Stomata + 40),
                    label = arab_gene),
               position = position_dodge(.6),
               color = "grey",
               size = 7.5,
               angle = 90)+
  theme(legend.position = c(.14,.9),
        legend.direction="horizontal",
        legend.title=element_blank(),
        legend.key.size = unit(1.2, "cm"),
        legend.text = element_text(size = 25),
        plot.title = element_text(lineheight=.8,hjust = 0.5, face = "bold", size = 30),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 1)))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, vjust = 0.5))+
  ggtitle("Abaxial Stomatal Density Across Introgressed Lines")+
  ylab("Average number of Abaxial Stomata")+
  xlab("")+
  coord_cartesian(ylim = c(0, 160))

# chp1_AB_stom_bar_ILs.jpeg ratio 1500 - for now

# abaxial pavement cells final (done?) ----

ggplot(abaxial, aes(reorder(IL,Pavement, mean), Pavement, fill=Parent))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.8)+
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
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = IL, 
                    y = (Pavement + 95),
                    label = arab_gene),
               position = position_dodge(.6),
               color = "grey",
               size = 7.5,
               angle = 90)+
  theme(legend.position = c(.14,.9),
        legend.direction="horizontal",
        legend.title=element_blank(),
        legend.key.size = unit(1.2, "cm"),
        legend.text = element_text(size = 25),
        plot.title = element_text(lineheight=.8,hjust = 0.5, face = "bold", size = 30),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 1)))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, vjust = 0.5))+
  ggtitle("Abaxial Pavement Cell Density Across Introgressed Lines")+
  ylab("Average number of \n Abaxial Pavement Cells")+
  xlab("")+
  coord_cartesian(ylim = c(0, 350))

# chp1_AB_pave_ILs_bar.jpeg ratio 1500 - done?

# Adaxial stomatal final (close, if not done) ----
ggplot(adaxial, aes(reorder(IL,Stomata, mean), Stomata, fill=Parent))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.8)+
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
               size = 10)+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = IL, 
                    y = (Stomata + 11),
                    label = arab_gene),
               position = position_dodge(.6),
               color = "grey",
               size = 7.5,
               angle = 90)+
  theme(legend.position = c(.14,.9),
        legend.direction="horizontal",
        legend.title=element_blank(),
        legend.key.size = unit(1.2, "cm"),
        legend.text = element_text(size = 25),
        plot.title = element_text(lineheight=.8,hjust = 0.5, face = "bold", size = 30),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 1)))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, vjust = 0.5))+
  ggtitle("Adaxial Stomatal Density Across Introgressed Lines")+
  ylab("Average number of Adaxial Stomata")+
  xlab("")+
  coord_cartesian(ylim = c(0, 32))
# chp1_ad_stom_ILs_bar.jpeg ratio 1500 - done?



# adaxial pavement final (almost - need to fix la3921) ----
ggplot(adaxial, aes(reorder(IL,Pavement, mean), Pavement, fill=Parent))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.8)+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("orange", "dark blue","magenta"))+
  theme(legend.position = c(.24,.9))+
  theme(legend.direction="horizontal")+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = reorder(IL,Pavement), 
                    y = (Pavement+15),
                    label = Ad.Pav),
               position = position_dodge(.6),
               color = "black",
               size = 8)+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = IL, 
                    y = (Pavement + 69),
                    label = arab_gene),
               position = position_dodge(.6),
               color = "grey",
               size = 7.5,
               angle = 90)+
  theme(legend.position = c(.14,.9),
        legend.direction="horizontal",
        legend.title=element_blank(),
        legend.key.size = unit(1.2, "cm"),
        legend.text = element_text(size = 25),
        plot.title = element_text(lineheight=.8,hjust = 0.5, face = "bold", size = 30),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 1)))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, vjust = 0.5))+
  ggtitle("Adaxial Pavement Cell Density Across Introgressed Lines")+
  ylab("Average number of \n Adaxial Pavement Cells\n")+
  xlab("")+
  coord_cartesian(ylim = c(0, 235))

# chp1_ad_pave_ILs_bar.jpeg ratio 1500 - almost

# just parents for narrative 10/3/2017 ----
# head(data)
# parents <- data[grepl("a",data$parents_only),]
# setwd("~/Desktop/")
# write.csv(parents, "parents_trial_1.csv")
# # changed data around manually, then saved
# dunno what I did ^ to reformat, but luckily I backed it up on evernote:
# import parent-only file: ----
parents_trial_1 <- read.csv("~/Desktop/parents_trial_1.csv", header = TRUE)
head(parents_trial_1)

# check out data quickly:
parents_trial_1 %>%
  group_by(IL, leaf.side, cell_type) %>%
  ggplot(.,aes(x = Parent, y = cell_num))+
  geom_boxplot()+
  facet_grid( cell_type~leaf.side)

# # i see no way to get them to plot nicely side-by-side since pave are so much higher # than stom
# 
# # split out pave vs stomata: ----
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
# from ggpubr

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


a <- ggplot(parents_pave, aes(x = reorder(IL, Parent), y = cell_num, fill = leaf.side))+
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
ggdraw(a)+draw_plot_label("B", size = 30)
# chp1-parent_pavement.png 1300

# bar parents stom ----


b <- ggplot(parents_stom, aes(x = reorder(IL, Parent), y = cell_num, fill = leaf.side))+
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

ggdraw(b)+draw_plot_label("A", size = 30)
#chp1-parent_stom 1300
