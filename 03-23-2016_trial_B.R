#Date: 3-23-2016
#Project: Habrochaites Trial B drought counts
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
# import files ----
Trial_b <- read.csv("~/Downloads/Trial_b_R_HAPPY_mar_23_2016.csv")

# right now, the data set is only Adaxial, but both pavement and stomata

head(Trial_b)

# name.the.new.column <- substring function (the column you want, where.it.should.start, where.it.should.stop)

Trial_b$leaf.side <- substr(Trial_b$Slide.Side,2,3) # this is super-useful to split by abaxial/adaxial sides
Trial_b$slide.number <- substr(Trial_b$Slide.Side,1,1) # 

Trial_b$Treatment <- as.factor(Trial_b$Treatment)  

# checking the column classes
sapply(Trial_b, class)


avg_geno <- Trial_b %>% 
  group_by(Geno) %>% 
  summarise(avg_pave = mean(Pavement), var_pave = var(Pavement), avg_stom = mean(Stomata), var_stom = var(Stomata)) # these numbers match the original stomatal analyses I did by hand in excel - WHEW!!



# scatter plot to see data ----

ggplot(Trial_b, aes( x = Pavement, y = Stomata, colour = Treatment, shape = Geno) ) +
  geom_point(size=3.5)+
  theme_bw()+
  scale_color_manual(name="Color Key", values=c("magenta","darkseagreen2"))+
  theme(legend.position = "bottom")+
  theme(legend.direction="horizontal")+
  ggtitle("Adaxial Pavement vs Stomata Cells")+
  ylab("Stomata Cell Counts")+
  xlab("Pavement Cell Counts")


# Bar graph adaxial stomata ----

# got a weird error, so googled it - this fix worked... 
#dev.off()

ggplot(Trial_b, aes(reorder(Geno,Stomata, mean), Stomata, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c("magenta", "darkseagreen2"))+
  theme(legend.position = c(.1,.9))+
  ggtitle("Adaxial Stomatal Density")+
  ylab("Average Adaxial Stomata Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 17))


# bar adaxial pavement ----

ggplot(Trial_b, aes(reorder(Geno,Pavement, mean), Pavement, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c("magenta", "darkseagreen2"))+
  theme(legend.position = c(.1,.9))+
  ggtitle("Adaxial Pavement Density")+
  ylab("Average Adaxial Pavement Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 200))

# stomatal index (stomata per pavement cell)

Trial_b$Index <- (Trial_b$Stomata/Trial_b$Pavement)

ggplot(Trial_b, aes(reorder(Treatment), y = Index, x = Geno, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  scale_fill_manual(values = c("darkgoldenrod3", "darkblue"))+
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 28),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=15))+
  theme(legend.position = c(.15,.9))+
  ggtitle("Stomatal Index")+
  ylab("Adaxial Stomata Cells/Pavement Cells")+
  xlab("Genotypes")+
  theme(axis.text.x = element_text( size = 15))+
  coord_cartesian(ylim = c(0, .11))+
  scale_x_discrete(breaks=c("e6203", "hab"),
                   labels=c("Domesticated", "Wild"))

# ANOVA of index:
tuke.trialb.index <- TukeyHSD(aov(lm(Index~Geno*Treatment, Trial_b))) 
sum.tuke.trialb.index <- as.data.frame(tuke.trialb.index[3])

anova(lm(Index~Geno*Treatment, Trial_b)) 

tuke.trialb.index[1] # if you only want to see Genotypes (but remember, it pools genotypes)
tuke.trialb.index[2] # if you only want to see Treatments (but remember, it pools treatments)
tuke.trialb.index[3]
sig.sum.tuke.trialb.index <- as.data.frame(sum.tuke.trialb.index[which(sum.tuke.trialb.index$Geno.Treatment.p.adj<.05),])


# trying to get significance: 
tuke.trialb.index$sig <- 


## Is there significant difference?

# ANOVA time

# stomata
tuke.trialb <- TukeyHSD(aov(lm(Stomata~Geno*Treatment, Trial_b))) 
sum.tuke.trialb <- as.data.frame(tuke.trialb[3])

anova(lm(Stomata~Geno*Treatment, Trial_b)) 

tuke.trialb[1] # if you only want to see Genotypes (but remember, it pools genotypes)
tuke.trialb[2] # if you only want to see Treatments (but remember, it pools treatments)
tuke.trialb[3]
sig.sum.tuke.trialb <- as.data.frame(sum.tuke.trialb[which(sum.tuke.trialb$Geno.Treatment.p.adj<.05),])

# in Stomata, looks like only the hab/no water to e6203/no water is significantly different

# Pavement

tuke.trialb.pav <- TukeyHSD(aov(lm(Pavement~Geno*Treatment, Trial_b))) 
sum.tuke.trialb.pav <- as.data.frame(tuke.trialb.pav[3])

anova(lm(Pavement~Geno*Treatment, Trial_b)) 

tuke.trialb.pav[1] # if you only want to see Genotypes (but remember, it pools treatments)
tuke.trialb.pav[2] # if you only want to see Treatments (but remember, it pools genotypes)
tuke.trialb.pav[3]
sig.sum.tuke.trialb.pav <- as.data.frame(sum.tuke.trialb.pav[which(sum.tuke.trialb.pav$Geno.Treatment.p.adj<.05),])


# in Pavement, looks like 3 are significantly different:
# hab norm vs e6203 no water, 
# hab norm vs hab no water,
# hab norm vs e6203 norm

# bar graphs w significance ----
ggplot(Trial_b, aes(reorder(Geno,Pavement, mean), Pavement, fill = Treatment))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.5), position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.1, position = position_dodge(.6))+
  stat_summary(fun.data=plot.summary,
               geom = "text", 
               aes( x = Geno, 
                    y = (Pavement+20),
                    label = pav.sig),
                    position = position_dodge(.6),
                    color = "black",
                    size = 8)+
  scale_fill_manual(values = c("goldenrod3", "darkblue"))+
  theme(legend.position = c(.1,.9))+
  ggtitle("Adaxial Pavement Density")+
  ylab("Average Adaxial Pavement Cells")+
  xlab("Genotypes")+
  coord_cartesian(ylim = c(0, 210))

# # part of trouble shooting significance stuff:
# test_lab1 <- c("a", "a", "a", "b")     
# test_lab <- replicate(84, "ab")
# test_list1 <- c(105, 105, 105, 105)
# test_list <- c(1:84)
 
 