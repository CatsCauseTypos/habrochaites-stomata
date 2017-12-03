#Date: 5-3-2017
#Project: Habrochaites IL Pavement & stomatal cell density - checking LA3921
#Author: Donnelly West - DonWest@UCDavis.edu
#Version: 2.2

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

# get your data ----
data <- read.csv("~/Desktop/Sinha Lab stuff/hab-epidermis/Data/sorting_3921_may-03-2017.csv")

# stomatal index ----
data <- data %>% 
  group_by(Geno, leaf.side) %>% 
  mutate(Stomatal_Index = Stomata/Pavement)

# ways to split up the ab/ad column ----

data$leaf.side <- as.factor(substr(data$ab.ad,1,2)) 

data$Plant.number <- as.factor(substr(data$Plant_num,2,2)) #Relevant plant # info

#separating the data into the leaf sides for simplicity ----
adaxial <- data[grepl("ad",data$leaf.side),]
head(adaxial)

abaxial <- data[grepl("ab",data$leaf.side),]

# checking on the different sample times 
avg_by_geno <- data %>% 
  group_by(Geno, leaf.side) %>% 
  summarise(avg_pave = mean(Pavement), var_pave = var(Pavement), avg_stom = mean(Stomata), var_stom = var(Stomata)) # if this stops working, ** YOU HAVE TO UNLOAD PLYR FIRST THEN RE-load DPLYR >< >< >< 

avg_by_geno


# making vectors split by Geno AND side for ttests----

# subset adaxial, new
ad_new <- adaxial[grepl("3921_",adaxial$Geno),]

# vector
ad_new_stom_vector <- ad_new$Stomata
ad_new_pave_vector <- ad_new$Pavement

class(ad_new_stom_vector) # integer - yay!
is.vector(ad_new_stom_vector) # true! yay!!

# subset abaxial, new
ab_new <- abaxial[grepl("3921_",abaxial$Geno),]

# vectors
ab_new_stom_vector <- ab_new$Stomata
ab_new_pave_vector <- ab_new$Pavement

# old ones too
ab_old <- abaxial[grepl("old",abaxial$Geno),]
ad_old <- adaxial[grepl("old",adaxial$Geno),]

ab_o_stom_vector <- ab_old$Stomata
ab_o_pave_vector <- ab_old$Pavement
ad_o_stom_vector <- ad_old$Stomata
ad_o_pave_vector <- ad_old$Pavement

# newest ones too
ab_newer <- abaxial[grepl("Newer",abaxial$Geno),]
ad_newer <- adaxial[grepl("Newer",adaxial$Geno),]

ab_er_stom_vector <- ab_newer$Stomata
ab_er_pave_vector <- ab_newer$Pavement
ad_er_stom_vector <- ad_newer$Stomata
ad_er_pave_vector <- ad_newer$Pavement



################################################### significance ----

## so is the "new" and "old" 3921 data significantly different?

# t-test ab pave ----

t.test(ab_o_pave_vector, ab_new_pave_vector) # different

# output:
# data:  ab_o_pave_vector and ab_new_pave_vector
# t = -4.6397, df = 22.51, p-value = 0.00012
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -56.36918 -21.57526
# sample estimates:
#   mean of x mean of y 
# 97.08333 136.05556 

# do those means match my avg_by_geno?
avg_by_geno #yup

# output:   
#  Geno leaf.side  avg_pave var_pave  avg_stom  var_stom
# <chr>    <fctr>     <dbl>    <dbl>     <dbl>     <dbl>
#   1 3921_        ab 136.05556 464.7614 41.111111 101.28105
# 2 3921_        ad 140.50000 643.7941  9.444444  16.84967
# 3   old        ab  97.08333 536.8106 33.583333  39.35606
# 4   old        ad  80.83333 100.3333  9.583333  11.35606

# t-test ad pave ----
t.test(ad_o_pave_vector, ad_new_pave_vector) # different
# output
# data:  ad_o_pave_vector and ad_new_pave_vector
#t = -8.9821, df = 23.862, p-value = 4.03e-09
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -73.38103 -45.95230
# sample estimates:
#   mean of x mean of y 
# 80.83333 140.50000 

# t-test ab stomata ----
t.test(ab_o_stom_vector, ab_new_stom_vector) # different

# data:  ab_o_stom_vector and ab_new_stom_vector
# t = -2.5224, df = 27.929, p-value = 0.01764
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -13.641659  -1.413896
# sample estimates:
#   mean of x mean of y 
# 33.58333  41.11111 

# t-test ad stomata ----
t.test(ad_o_stom_vector, ad_new_stom_vector) # not different
# data:  ad_o_stom_vector and ad_new_stom_vector
# t = 0.10123, df = 26.651, p-value = 0.9201
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -2.677982  2.955760
# sample estimates:
#   mean of x mean of y 
# 9.583333  9.444444 

#### old vs newer  ----
t.test(ab_o_pave_vector, ab_er_pave_vector) # different
# p-value = 1.619e-06
t.test(ab_o_stom_vector, ab_er_stom_vector) # different
# p-value = 0.0002521
t.test(ad_o_pave_vector, ad_er_pave_vector) # different
# p-value = 1.136e-10
t.test(ad_o_stom_vector, ad_er_stom_vector) # not different
# p-value = 0.1552


## Newer vs New ----
t.test(ad_new_stom_vector, ad_er_stom_vector) # not different =)
# p-value = 0.9643
t.test(ad_new_pave_vector, ad_er_pave_vector) # different, barely
# p-value = 0.04598
t.test(ab_new_stom_vector, ab_er_stom_vector) # not different =)
# p-value = 0.3289
t.test(ab_new_pave_vector, ab_er_pave_vector) # different ><
# p-value = 0.02703


# visualizing the differences ----

# useful labels for later:
labels <- c(ab = "Abaxial", ad = "Adaxial")

# stomata bar graph ----

ggplot(data, aes(Geno, Stomata, fill=leaf.side))+
  facet_grid(~leaf.side, labeller = labeller(leaf.side = labels))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.6)+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.2)+
  stat_summary(fun.data=plot.summary,
               geom = "text",
               aes( x = Geno,
                    y = (Stomata+5),
                    label = sig_stom),
               position = position_dodge(.6),
               color = "black",
               size = 4)+
  scale_fill_manual(name="Color Key", values = c("darkblue","magenta", "green"), guide = FALSE)+
  ggtitle("Stomatal Density in LA3921")+
  ylab("Stomata")+
  xlab("Sampling Times")+
  scale_x_discrete(breaks=c("3921_", "old", "Newer"),
                   labels=c("New LA3921 (n = 6)", "Old LA3921 (n = 4)", "Newer LA3921 (n=6)"))+
  theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=8))






# pavement 

ggplot(data, aes(Geno, Pavement, fill=leaf.side))+
  facet_grid(~leaf.side, labeller = labeller(leaf.side = labels))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.6)+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.2)+
  stat_summary(fun.data=plot.summary,
               geom = "text",
               aes( x = Geno,
                    y = (Pavement+22),
                    label = sig_pave),
               position = position_dodge(.6),
               color = "black",
               size = 4)+
  scale_fill_manual(name="Color Key", values = c("orange","purple"), guide = FALSE)+
  ggtitle("Pavement Density in LA3921")+
  ylab("Pavement")+
  xlab("Sampling Times") +
  scale_x_discrete(breaks=c("3921_", "old", "Newer"),
                   labels=c("New LA3921 (n = 6)", "Old LA3921 (n = 4)", "Newer LA3921 (n=6)"))+
  theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=8))

# scatter plot ----
 labelz <- c("3921_" = "IL3921 New (n=6)", Newer = "IL3921 Newest (n=6)", old = "IL3921 old (n=4)")

# this one is too fancy - i like the simpler one below
# lblz <- c("3921_" = "New", Newer = "Newest", old = "Old")
# ggplot(data, aes(x = Pavement, y = Stomata, color = leaf.side, shape = Geno))+
#   facet_grid(~Geno,labeller = labeller(Geno = labelz) )+
#   geom_jitter()+
#   geom_smooth(method = lm)+
#   ylab("Stomata Counts")+
#   xlab("Pavement Cell Counts")+
#   theme(strip.text.x = element_text(size=8))+
#   scale_color_manual(name="Color Key", 
#                      values = c("darkorchid1", "gold2"),
#                      breaks = c("ab","ad"),
#                      labels = c("Abaxial", "Adaxial"))+
#   scale_shape_discrete(breaks = c("3921_","Newer", "old"),
#                        labels = lblz)+
#    guides(shape = guide_legend(title=NULL))
 


# Scatter plot stom vs pave by sample group----
ggplot(data, aes(x = Pavement, y = Stomata, color = leaf.side))+
  facet_grid(~Geno,labeller = labeller(Geno = labelz) )+
  geom_jitter()+
  geom_smooth(method = lm)+
  ylab("Stomata Counts")+
  xlab("Pavement Cell Counts")+
  theme(strip.text.x = element_text(size=8))+
  scale_color_manual(name="Color Key", 
                     values = c("darkorchid1", "gold2"),
                     breaks = c("ab","ad"),
                     labels = c("Abaxial", "Adaxial"))


  
# stomatal index ----
data <- data %>% 
  group_by(Geno, leaf.side) %>% 
  mutate(Stomatal_Index = Stomata/Pavement)


ggplot(data, aes( x= Geno, y = Stomatal_Index, fill = leaf.side))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.6, position = "dodge")+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.2, position = "dodge")+
  scale_fill_manual(name= NULL, 
                     values = c("darkorchid1", "gold2"),
                     breaks = c("ab","ad"),
                     labels = c("Abaxial", "Adaxial"))+
  ylab("Stomata/Pavement Cells")+
  xlab("Sampling Times")+
  ggtitle("Stomatal Index in IL3921")

# stats on index ----
# make them stupid vectors
ab_old_index_vector <- ab_old$Stomatal_Index
ad_old_index_vector <- ad_old$Stomatal_Index

ab_new_index_vector <- ab_new$Stomatal_Index
ad_new_index_vector <- ad_new$Stomatal_Index

ab_er_index_vector <- ab_newer$Stomatal_Index
ad_er_index_vector <- ad_newer$Stomatal_Index

# start with what SHOULD be different:
# newer data ab vs ad
t.test(ab_er_index_vector, ad_er_index_vector) # different!
# yay

# real comparisons:

# new vs old ad & ab
t.test(ad_new_index_vector, ad_old_index_vector) # different p-value = 0.002725
t.test(ab_new_index_vector, ab_old_index_vector) # not different p-value = 0.1419
# new vs newer ad & ab
t.test(ad_new_index_vector, ad_er_index_vector) # not different p-value = 0.3384
t.test(ab_new_index_vector, ab_er_index_vector) # not different p-value = 0.2405
# old vs newer ab & ad
t.test(ad_old_index_vector, ad_er_index_vector) # different p-value = 0.0005228
t.test(ab_old_index_vector, ab_er_index_vector) # different p-value = 0.01821

ggplot(data, aes( x= Geno, y = Stomatal_Index, fill = leaf.side))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", width=.6, position = "dodge")+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.2, position = "dodge")+
  stat_summary(fun.data=plot.summary,
               geom = "text",
               aes( x = Geno,
                    y = (Stomatal_Index+.03),
                    label = sig_index),
               position = position_dodge(.6),
               color = "black",
               size = 4)+
  scale_fill_manual(name= NULL, 
                    values = c("darkorchid1", "gold2"),
                    breaks = c("ab","ad"),
                    labels = c("Abaxial", "Adaxial"))+
  ylab("Stomata/Pavement Cells")+
  xlab("Sampling Times")+
  ggtitle("Stomatal Index in IL3921")+
  scale_x_discrete(breaks=c("3921_", "old", "Newer"),
                   labels=c("New LA3921 (n = 6)", "Old LA3921 (n = 4)", "Newer LA3921 (n=6)"))+
  theme(axis.text.x  = element_text( vjust=0.5, size=8))
