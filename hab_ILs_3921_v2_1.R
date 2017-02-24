#Date: 2-22-2017
#Project: Habrochaites IL Pavement & stomatal cell density - checking LA3921
#Author: Donnelly West - DonWest@UCDavis.edu
#Version: 2.1

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
data <- read.csv("~/Desktop/Sinha Lab stuff/Hab IL stomatal stuff/Data/sorting_3921_feb-22-2017_v2.csv")

# old one = 24 rows
# new ones = 12 plants x 3 pics = 36 rows 
nrow(data) # 60 - humm..

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
  summarise(avg_pave = mean(Pavement), var_pave = var(Pavement), avg_stom = mean(Stomata), var_stom = var(Stomata)) # this is no longer working
#take a look - it groups everything despite the 'group by' command -- ** YOU HAVE TO UNLOAD PLYR FIRST THEN RE-load DPLYR >< >< >< 
avg_by_geno


# making vectors split by Geno AND side ----

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

################################################### significance ----

## so is the "new" and "old" 3921 data significantly different?

# F-test example  ----
# to determine if the variances are the same
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

# F-test ab pave ----
var.test(ab_o_pave_vector, ab_new_pave_vector)

# output:
# data:  ab_o_pave_vector and ab_new_pave_vector
# F = 1.155, num df = 11, denom df = 17, p-value = 0.7647
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.402498 3.790372
# sample estimates:
#   ratio of variances 
# 1.155024 



# ttest example ----
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
t.test(test_1, test_3)

# data:  test_1 and test_3
# t = 0, df = 10, p-value = 1
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -16.27203  16.27203
# sample estimates:
#   mean of x mean of y 
# 33        33 


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
  scale_x_discrete(breaks=c("3921_", "old"),
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
  scale_x_discrete(breaks=c("3921_", "old"),
                   labels=c("New LA3921 (n = 6)", "Old LA3921 (n = 4)"))


