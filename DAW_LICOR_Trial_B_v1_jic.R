################################################################
# Author:Donnelly West 
# Email: Donwest@UCDdavis.edu
# Date:  12/9/15
# Script Name:  LICOR TrialB
# Version: 1.0
#
# <brief description of script's function>
# Trying to find differences between treatment groups from Trial B in GC418 
#Steven Rowland & Mirko Ledda helped SO MUCH with this code

######################################################################################
#steven's suggestion to fix the error bars
options(stringsAsFactors = FALSE)
#still no error bars - turns out, I had left a bunch of licor file junk in my cv and that was freaking out R

#Function for error bars (thanks Julin!)
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

#ALL OF THE LIBRARIES
library(ggplot2); library(reshape2); library(reshape) ; library(dplyr) ; library(cowplot)

#File time
#(it's actually easier to read this in via import data set - it assigns the classes correctly; dunno why the read in code below makes everything a character, but it does!)
data1=read.csv("~/Downloads/Trial_B_LICOR_R.csv",header=T)

head(data1)

# using sapply because as.numeric kept failing for whatever reason
?sapply

class(data1$Tleaf) #this needs to be numeric
lapply(data1, class) #drat, most are wrong

# fix 'em
data1$Photo <- sapply(data1$Photo, as.numeric) #fixing class for photosynthetic column
data1$Tleaf <- sapply(data1$Tleaf, as.numeric) #fixing class for tleaf
data1$Treat <- as.factor(data1$Treat) #fixing class for treatment
data1$PhiPS2 <- sapply(data1$PhiPS2, as.numeric) #fixing class 
data1$PhiCO2 <- sapply(data1$PhiCO2, as.numeric) #fixing class 
data1$Geno <- as.factor(data1$Geno) #fixing class for geno
## Grabbing the data that matters

# remember: (brackets = row and column -- in that order)

m82=data1[grepl("m82", data1$Geno),] #last comma tells it to take all columns 

controls=data1[grepl("control",data1$Treat),]

penn=data1[grepl("penn", data1$Geno),] #last comma tells it to take all columns 
### I quit and went back to excel to get rid of mistakes and other texts
# all <- data1 %>% 
#   group_by (Treat, Geno) %>% 
#   mutate(avgPhoto = mean(Photo)) %>% 
#   mutate(sdPhoto = sd(Photo)) #%>% 
#   #mutate( n = tally(.)) #still can't seem to get the number of observ per category... dang
# 
# # ok - what if I add a column for wether it's data I want?
# #adding a column T/F if penn or m82
# data1$observ <- ifelse(data1$Geno==c("m82", "penn"), TRUE, FALSE)
# 
# both = data1[grepl("TRUE",data1$observ),] #wtf - why doesn't that work??




# this comes in handy later - let's get the avg temps of leaves:

m82$avgTleaf <- round(m82$Tleaf)
penn$avgTleaf <- round(penn$Tleaf)
data1$avgTleaf <- round(data1$Tleaf)
#just checking
head(controls)

#Up first: box plot!

class(data1$Photo) #photo class needs to be numeric for box plot to work

ggplot(data1,aes(Treat,Photo))+geom_boxplot() #basic ; man, penn can almost compensate for m82 in drought and vice versa in waterlogging

# custom
ggplot(m82,aes(Treat,Photo, fill = Treat)) +
  geom_boxplot() +
  scale_fill_manual(values = c("forestgreen", "firebrick3","dodgerblue3")) +
  xlab(expression("Treatment"))+
  ylab(expression("Photosynthesis (A)"))+
  theme(axis.title.y=element_text(size=20),
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=15),
        strip.text.x=element_text(size=15),
        legend.position="none") # I still like the bar graph below better..


## bar graph of treatment vs photosynth
ggplot(m82, aes(Treat, Photo, fill=Treat)) +
  theme_bw() +
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.6)) +
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.2) +
  theme(legend.position="none") +
  scale_fill_manual(values = c("forestgreen", "firebrick3","dodgerblue3")) +
  ylab("Photosynthesis (A)") +
  xlab("Treatment") +
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.y=element_text(size=10))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.text.x=element_text(size=15))+
  theme(strip.text.x=element_text(size=15))+
  ggtitle("M82 Mean Photosynthesis by Treatment: Trial B")+
  theme(title=element_text(size=18))+
  ylim(0,15)

# for all genotypes
ggplot(data1, aes(Treat, Photo, fill=Treat)) +
  theme_bw() +
  facet_wrap(~Geno)+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.6)) +
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.2) +
  theme(legend.position="none") +
  scale_fill_manual(values = c("forestgreen", "firebrick3","dodgerblue3")) +
  ylab("Photosynthesis (A)") +
  xlab("Treatment") +
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.y=element_text(size=10))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.text.x=element_text(size=15))+
  theme(strip.text.x=element_text(size=15))+
  ggtitle("Mean Photosynthesis by Treatment: Trial B")+
  theme(title=element_text(size=18))+
  ylim(0,15)


################### Mirko & I trying to sort things out with the # of plants per bar in tleaf groups..
# 
# #how many individual plants are in each of those bars?
#  m82 %>% 
#   group_by(Treat,avgTleaf) %>% 
#   tally()
#  
# # so if we use the geo_text thing, it does, in fact, put numbers with the bars, but it wants the same # of bars for the numbers... which is rotten becuase i have 37 obs. but 11 groups of treatment by tleaf avg. humbug. we spent ~1 hour and couldn't figure out an 'easy' way.
# ggplot(m82, aes(avgTleaf, Photo, fill=Treat))+
#    theme_bw()+
#    geom_text(aes(label = Photo), position = position_dodge(width = 0.9), vjust = -0.25)+ # this was an attempt to put # of plants over each bar == fail. Success farther below
#    stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.6), colour="black")+
#    stat_summary(fun.data=plot.summary, geom = "errorbar", width=.2)+
#    theme(legend.position="none")+
#    facet_wrap(~Treat)+
#    ylab("Photosynthesis (A)")+
#    xlab(expression("Mean Leaf Temperature"~(degree*C)))+
#    theme(axis.title.y=element_text(size=20),
#          axis.text.y=element_text(size=15),
#          axis.title.x=element_text(size=20),
#          axis.text.x=element_text(size=15),
#          strip.text.x=element_text(size=15)) +
#    ylim(-5,25) 
#  
# 
# 

### Mirko & I got it to work... mostly Mirko!!!
library(plyr)
?ddply

# dfl <- ddply(m82, .(avgTleaf), mean,na.rm=T) # nope


dfl <- ddply(m82, .(avgTleaf, Treat), summarize, y = mean(Photo)) # this does it = makes a data frame with only avgTleaf, Treatment, and the mean photosyn for that subset
dfl$sd <- ddply(m82, .(avgTleaf, Treat), summarize, y = sd(Photo))$y # standard dev of those means
dfl$n <- ddply(m82, .(avgTleaf, Treat), summarize, y = length(Photo))$y # number of observ within those means

dfl$sd2 <- dfl$sd # to trick R into letting us have values over the bars who have only 1 sample (when SD = NA)
dfl$sd2[is.na(dfl$sd)] <- 0 # continued trickery: assigning 0 to the NA values

se_limits <- aes(ymax = y+sd/sqrt(n),ymin = y-sd/sqrt(n)) # general formula to compute where the bars for standard error sit on the graph
dodge <- position_dodge(width=0.9) # this makes sure everything lines up on the x-axis too

### Actual graph of photo vs treatment w/ leaf temp categories 

ggplot(dfl,aes(x = avgTleaf, y = y, fill = Treat)) + 
  theme_bw() + 
  facet_wrap(~Treat) +
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(se_limits,position=dodge,width=0.3) + # this is the error bar creation 
  geom_text(aes(x=avgTleaf,y=y+sd2/sqrt(n),label=n),position=dodge,vjust=-0.5) + #this is the fancy # of observ per bar 
  ylim(-2,21) +
  theme(legend.position="none") +
  scale_fill_manual(values = c("forestgreen", "firebrick3","dodgerblue3")) +
  ylab(expression("Photosynthesis (A)"))+
  xlab(expression("Rounded Leaf Temperature"~(degree*C)))+
  theme(axis.title.y=element_text(size=20),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=15),
        strip.text.x=element_text(size=15),
        title=element_text(size=18))+
  ggtitle("Mean Photosynthesis by Leaf Temp Split into Treatments")
  



# # Another way to see the inverse corrlation with photo & Tleaf: (all pretty)
# 
# ggplot(m82, aes(Photo, avgTleaf, fill = Treat)) +
#   geom_point() +
#   facet_grid(~Treat) +
#   stat_smooth(method="lm")+
#   xlab(expression("Photosynthetic Rate (A)"))+
#   ylab(expression("Rounded Leaf Temperatures"~(degree*C)))+
#   theme(axis.title.y=element_text(size=20),
#         axis.text.y=element_text(size=15),
#         axis.title.x=element_text(size=20),
#         axis.text.x=element_text(size=15),
#         strip.text.x=element_text(size=13),
#         legend.position="none") +
#   scale_fill_manual(values = c("forestgreen", "firebrick3","dodgerblue3"))+
#   ggtitle("Inverse Relationship between Photosynthesis & Leaf Temp")+
#   theme(plot.title=element_text(face = "bold", size = 18.5))
# 
# 
# class(m82$Trmmol)
# m82$Trmmol <- as.numeric(m82$Trmmol)

# Transpiration in mmol
# need to do the magic to get # of plants on this
ggplot(data1,aes(round_any(Tleaf,1), Trmmol, fill=Treat))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.6), colour="black")+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.2)+
  theme(legend.position="none")+
  facet_wrap(Geno~Treat)+
  ylab(expression("Transpiration (mmol)"))+
  xlab(expression("Mean Leaf Temperature"~(degree*C)))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.y=element_text(size=15))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.text.x=element_text(size=15))+
  theme(strip.text.x=element_text(size=17))

#scatter plot related to photosynthesis
#PhiPS2 is quantum efficiency - photons received going into PHOTOSYS2; PhiCO2 mole of fixed CO2 per mole of photons
m82$PhiPS2 <- as.numeric(m82$PhiPS2)
m82$PhiCO2 <- as.numeric(m82$PhiCO2)
ggplot(m82,aes(PhiPS2,PhiCO2, fill=Treat))+geom_point()+stat_smooth(method="lm") 
#linear, up angle== can stil utilize the photons coming in to fix carbon

# Let's make it pretty:Photosys2
ggplot(m82, aes(PhiPS2, PhiCO2, fill = Treat)) +
  geom_point() +
  stat_smooth(method = "lm") +
  xlab(expression("Quantum Efficiency of PHOTOSYS2"))+
  ylab(expression("mol Fixed CO"[2]~"per mol of Photons"))+
  theme(axis.title.y=element_text(size=20),
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=15),
        strip.text.x=element_text(size=15),
        legend.direction="horizontal",
        legend.position = c(.7, .1))+
  scale_fill_manual(
    name="Color Key",
    values = c("forestgreen", "firebrick3","dodgerblue3"))+
  ggtitle("Photosynthetic Machinery Not at Capacity")+
  theme(plot.title=element_text(face = "bold", size = 22))


controls$PhiPS2 <- as.numeric(controls$PhiPS2)
controls$PhiCO2 <- as.numeric(controls$PhiCO2)
controls$Geno <- as.factor(controls$Geno)

#PARi (par in) vs photo will tell you light response curve which is how you justify the light intensity at which you measured (or it can be how)
ggplot(controls,aes(PhiPS2,PhiCO2, fill=Geno))+geom_point()+stat_smooth(method="lm")+theme(legend.position="none") 

#bar graph for leaf temp
ggplot(data1, aes(Treat, Tleaf, fill = Treat)) +
  stat_summary(fun.data=plot.summary,geom="bar") +
  stat_summary(fun.data=plot.summary,geom="errorbar") +
  facet_wrap(~Geno)+
  theme(axis.text.y=element_text(size=12))+
  ylim(0,30)

## find me
  # bar graph transpiration vs treatment
ggplot(data1,aes(Treat,Trans, fill=Treat)) +
  stat_summary(fun.data=plot.summary,geom="bar") +
  stat_summary(fun.data=plot.summary,geom="errorbar") +
  facet_wrap(~Geno)+ylim(0,0.0045) +
  ggtitle("Trial B Transpiration Rate by Treatment") +
  scale_fill_manual(
    name="Color Key",
    values = c("forestgreen", "firebrick3","dodgerblue3"))+
  theme(axis.text.x=element_text(size=15, angle = 16),
        legend.position="none")+
  xlab(expression("Treatments"))+
  ylab(expression("Transpiration Rate "~(mol/m^{2}~"/s")))


## conductance
ggplot(data1,aes(Treat,Cond, fill=Treat))+stat_summary(fun.data=plot.summary,geom="bar")+stat_summary(fun.data=plot.summary,geom="errorbar")+facet_wrap(~Geno)
#transpiration / conductance match (and probablly should)

 ## internal CO2 with treatment
ggplot(data1,aes(Treat,Ci, fill=Treat))+stat_summary(fun.data=plot.summary,geom="bar")+stat_summary(fun.data=plot.summary,geom="errorbar")+facet_wrap(~Geno)


## photosynthesis vs transpiration

ggplot(data1, aes(Photo, Trans)) + 
  geom_point() +
  facet_grid(avgTleaf~Treat~Geno, margins = TRUE) +
  stat_smooth(method="lm", se=F) # that's a lot of stuff.....

# # with a re-order so temp goes 'up'
# ggplot(transform(m82,
#                  avgTleaf = factor(avgTleaf, levels = c("28", "27", "26", "25", "24")
#                  )
# ), aes(x = Photo, y = Trans)) +
#   geom_point() +
#   facet_grid(avgTleaf~Treat, margins = TRUE) +
#   stat_smooth(method="lm", se=F) # VICTORY
# 
# 
# # Making that monstrosity pretty
# ggplot(transform(m82,
#                  avgTleaf = 
#                    factor(avgTleaf, levels = c("28", "27", "26", "25", "24"))),
#        aes(x = Photo, y = Trans, colour = Treat)) +
#   geom_point() +
#   facet_grid(avgTleaf~Treat, margins = TRUE) +
#   stat_smooth(method="lm", se=F) +
#   scale_colour_manual(values = c("forestgreen", "firebrick3","dodgerblue3", "black"))+
#   xlab(expression("Photosynthetic Rate (A)"))+
#   ylab(expression("Transpiration Rate "~(mol/m^{2}~"/s")))+ 
#   theme(axis.title.y=element_text(size=20),
#         axis.text.y=element_text(size=10.5, angle = 10),
#         axis.title.x=element_text(size=20),
#         axis.text.x=element_text(size=11),
#         strip.text.x=element_text(size=15),
#         legend.position = "none")+
#   ggtitle("Transpiration Trends")+
#   theme(plot.title=element_text(face = "bold", size = 20))
#   
#   
