#Date: 2-7-2017
#Project: Habrochaites IL Pavement & stomatal cell density - checking LA3921
#Author: Donnelly West - DonWest@UCDavis.edu
#Version: 1.2

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
data <-read.delim("~/Desktop/Sinha Lab stuff/Hab IL stomatal stuff/Data/sorting_3921_feb-7-2017.txt", stringsAsFactors=TRUE)
# old one = 24 rows
# new ones = 12 plants x 3 pics = 36 rows 
nrow(data) # 96 - humm..

class(data$Pavement) # just checking..

# ways to split up the ab/ad column ----

head(data)

# name.the.new.column <- substring function (the column you want, where.it.should.start, where.it.should.stop)

data$leaf.side <- as.factor(substr(data$ab.ad,1,2)) # this is super-useful to split by abaxial/adaxial sides

data$Plant.number <- as.factor(substr(data$Plant_num,2,2)) #Relevant plant # info

# checking the column classes
sapply(data, class)

#separating the data into the leaf sides for simplicity ----
adaxial <- data[grepl("ad",data$leaf.side),]
head(adaxial)

abaxial <- data[grepl("ab",data$leaf.side),]

avg_by_geno <- data %>% 
  group_by(Geno, leaf.side) %>% 
  summarise(avg_pave = mean(Pavement), var_pave = var(Pavement), avg_stom = mean(Stomata), var_stom = var(Stomata)) # so... those look very different... x_x

#take a look
avg_by_geno

# making vectors split by Geno AND side

# subset adaxial, new
ad_new <- adaxial[grepl("LA3921_",adaxial$Geno),]

# vector
ad_new_stom_vector <- ad_new$Stomata
ad_new_pave_vector <- ad_new$Pavement

class(ad_new_stom_vector) # integer - yay!
is.vector(ad_new_stom_vector) # true! yay!!

# subset abaxial, new
ab_new <- abaxial[grepl("LA3921_",abaxial$Geno),]

# vectors
ab_new_stom_vector <- ab_new$Stomata
ab_new_pave_vector <- ab_new$Pavement

# old ones too
ab_old <- abaxial[grepl("LA3921_old",abaxial$Geno),]
ad_old <- adaxial[grepl("LA3921_old",adaxial$Geno),]

ab_o_stom_vector <- ab_old$Stomata
ab_o_pave_vector <- ab_old$Pavement
ad_o_stom_vector <- ad_old$Stomata
ad_o_pave_vector <- ad_old$Pavement

################################################### significance ----

## so is the "new" and "old" 3921 data significantly different?

# F-test to determine if the variances are the same ----
?var.test # http://www.excel-easy.com/examples/f-test.html says to be sure the variance of # 1 is higher than # 2 or you should switch them. based on my calc's of variance in avg_by_geno, old data is always > then new data, so i listed old first.
# trying the example from ^ website
# got code from here: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/var.test.html, but it's basically just the ?var.test

test_1 <- c(26,25,43,34,18,52)
test_2 <- c(23,30,18,25,28)

var.test(test_1, test_2)
# it came up with the same ratio of variances (7.37.. = F, but I don't know where to find the F crit 1 tail)

#output:
# data:  test_1 and test_2
# F = 7.3733, num df = 5, denom df = 4, p-value = 0.07578
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.7873666 54.4728903
# sample estimates:
#   ratio of variances 
# 7.373272 


# let's try an example where they are exactly the same:
test_3 <- c(26,25,43,34,18,52)
var.test(test_1, test_3)

# output:

# F test to compare two variances
# 
# data:  test_1 and test_3
# F = 1, num df = 5, denom df = 5, p-value = 1
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.139931 7.146382
# sample estimates:
#   ratio of variances 
# 1 

### alright, I think I need to pay attention to the p-value output more than anything else to decide if I can 'accept' or 'reject' my null hypoth that they're the same

var.test(ab_o_pave_vector, ab_new_pave_vector)

# output:
# data:  ab_o_pave_vector and ab_new_pave_vector
# F = 0.44276, num df = 11, denom df = 29, p-value = 0.1546
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.1788811 1.3837553
# sample estimates:
#   ratio of variances 
# 0.4427636 



# ttest ----
?t.test
# the standard ttest in R assumes unequal variances, so we should be good to go. (if my interpretation of the F-test results from above is correct)

# following our example from http://www.excel-easy.com/examples/t-test.html

t.test(test_1, test_2)

# output
# data:  test_1 and test_2
# t = 1.4726, df = 6.5433, p-value = 0.1873
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -5.155702 21.555702
# sample estimates:
#   mean of x mean of y 
# 33.0      24.8 

# the means should NOT be different, and they are not. (p > 0.05)

# now for LA3921:
t.test(ab_o_pave_vector, ab_new_pave_vector)

# output:
# data:  ab_o_pave_vector and ab_new_pave_vector
# t = 4.0477, df = 30.432, p-value = 0.0003283
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   18.51602 56.18398
# sample estimates:
#   mean of x mean of y 
# 97.08333  59.73333 


# do those means match my avg_by_geno?
avg_by_geno

# output:   
# Geno leaf.side avg_pave  var_pave  avg_stom  var_stom
# (fctr)    (fctr)    (dbl)     (dbl)     (dbl)     (dbl)
# 1    LA3921_        ab 34.83333  79.67647 12.888889 10.457516
# 2    LA3921_        ad 35.94444  85.82026  4.777778  2.183007
# 3 LA3921_old        ab 97.08333 536.81061 33.583333 39.356061
# 4 LA3921_old        ad 80.83333 100.33333  9.583333 11.356061

# not exactly... the old, ab pave is perfect. the new ab pave is not the same at all
mean(ab_new_pave_vector) #59.73333
mean(ad_new_pave_vector) #53.9
# so it looks like something in the avg_by_geno failed hard-core for the new data... why??

# remember, it's adaxial pavement cell data we're most interested in..
t.test(ad_o_pave_vector, ad_new_pave_vector)
# output
# data:  ad_o_pave_vector and ad_new_pave_vector
# t = 5.0915, df = 39.887, p-value = 8.9e-06
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   16.24118 37.62548
# sample estimates:
#   mean of x mean of y 
# 80.83333  53.90000 

# not equal either

# we should've grown a side-by-side LA4024 - major oversight on my part =/


# visualizing the differences ----

labels <- c(ab = "Abaxial", ad = "Adaxial")

ggplot(data, aes(Geno, Stomata, fill=leaf.side))+
  facet_grid(~leaf.side, labeller = labeller(leaf.side = labels))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("darkblue","magenta"), guide = FALSE)+
  ggtitle("Stomatal Density in LA3921")+
  ylab("Stomata")+
  xlab("Sampling Times")+
  scale_x_discrete(breaks=c("LA3921_", "LA3921_old"),
                   labels=c("New LA3921 (n = 6)", "Old LA3921 (n = 4)"))

ggplot(data, aes(Geno, Pavement, fill=leaf.side))+
  facet_grid(~leaf.side, labeller = labeller(leaf.side = labels))+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  scale_fill_manual(name="Color Key", values = c("orange","purple"), guide = FALSE)+
  ggtitle("Pavement Density in LA3921")+
  ylab("Stomata")+
  xlab("Sampling Times") +
  scale_x_discrete(breaks=c("LA3921_", "LA3921_old"),
                   labels=c("New LA3921 (n = 6)", "Old LA3921 (n = 4)"))


