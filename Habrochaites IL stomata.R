#DAW script for Habrochaites ILs stomata project v1.0 8/12/2015
#this version is extremeley disorganized. please see v2.0 8/22/2015


#making data a shorter handle
hab = habrochaites.stamata.project.all
#checking it worked
View(hab)
#woot

#double checking that we know the column names
colnames(hab)

#this works for quick and dirty plot:
qplot(data = hab, x=avg.ab, y=avg.ad, geom = "point")

#checking the column classes
sapply(hab, class)

#better graph f...
f1 <- ggplot(hab, aes(ab,ad, colour=geno) )
f1+geom_jitter(data=hab, na.rm=TRUE)

#maybe 38 is too many colors... let's try writing the LA#'s
f2 <- ggplot(hab, aes(ab,ad, label=geno, colour=Parent) )
f2 +geom_jitter()+geom_text(aes(label=geno),hjust=.5, vjust=0)+theme(legend.position = "bottom")

#let's look at the data in other ways... normalize to 4024

#From http://stackoverflow.com/questions/15215457/standardize-data-columns-in-r :

#library(dplyr)
#set.seed(1234)
#dat <- data.frame(x = rnorm(10, 30, .2), 
    #              y = runif(10, 3, 5),
    #             z = runif(10, 10, 20))
#dat
#dat %>% mutate_each_(funs(scale),vars=c("y","z")) 

# what is  %>% ??
#it is a "pipe" operator (from functional programming). Instead of writing f(g(x)) it would look nicer if one writes x %>% g %>% f. In other words, dat %>% mutate_each_(funs(scale),vars=c("y","z")) is just mutate_each_(dat,funs(scale),vars=c("y","z")). The operator helps a lot when a chain is very long since f(g(h(i(j(x))))) can be very hard to read.

library(dplyr)

#what does set.seed do??
# From http://stackoverflow.com/questions/13605271/reasons-for-using-the-set-seed-function :
#The need is the possible desire for reproducible results, which may for example come from trying to debug your program, or of course from trying to redo what it does:
  # These two results we will "never" reproduce as I just asked for something "random":
  
  #R> sample(LETTERS, 5)
#[1] "K" "N" "R" "Z" "G"
#R> sample(LETTERS, 5)
#[1] "L" "P" "J" "E" "D"

#These two, however, are identical because I set the seed:
# R> set.seed(42); sample(LETTERS, 5)
#[1] "X" "Z" "G" "T" "O"
#R> set.seed(42); sample(LETTERS, 5)
#[1] "X" "Z" "G" "T" "O"

#After normalizing to parent plant 4024 

qplot(data = hab, x=norm.ab, geom = "bar", binwidth = .05, colour=geno)

e <- ggplot(hab, aes(norm.ab,norm.ad, label=geno, colour=Parent) )
e +geom_jitter()+
  geom_text(aes(label=geno),hjust=.5, vjust=0)+
  theme(legend.position = "bottom")

#normalized to hab...
h <- ggplot(hab, aes(hab.ab,hab.ad, label=geno, colour=Parent) )
h +geom_jitter()+
  geom_text(aes(label=geno),hjust=.5, vjust=0)+
  theme(legend.position = "bottom")
#w/o LA #'s
h1 <- ggplot(hab, aes(norm.ab,norm.ad,colour=Parent) )
h1 +geom_jitter()+
  scale_color_manual(name="Stomatal Density", values=c("orange", "dark blue","magenta"))+
  theme(legend.position = "bottom")


#interesting... 

n <- ggplot(hab, aes(norm.ab,, label=geno) )
n +geom_histogram()#+geom_text(aes(label=geno,),hjust=.5, vjust=0, binwidth=1)
#not sure why the geom histogram doesn't want a "y" but the geom_text "label" requires one...
  
a1 <- ggplot(hab, aes(geno,norm.ad))
a1+
  geom_histogram(position="dodge", stat="identity", binwidth=.05)#+
  geom_text(aes(label=geno),hjust=.5, vjust=0)
#but it's so close together><
  a2 <- ggplot(hab, aes(geno,norm.ad, fill=Parent))
  a2+
    geom_histogram(position="dodge", stat="identity", binwidth=.05)
# colour=Parent worked, but ugly.. fill=Parent = what I meant
  
#stolen from other notes:
#ggplot(data = mymov3, aes( x= as.factor(rating2), y = mpaa, color= as.factor(year))) +
 # geom_jitter() +
  #facet_grid(Comedy2~Animation2) +
  #theme(legend.position = "bottom") +
  #scale_colour_hue(h.start= 5)

#it would be super useful to rank the ILs and parents in order of most to least Ab / Ad 
#stolen code: tapply(TomatoR2CSHL$hyp,TomatoR2CSHL$species, mean,na.rm=T)
ranked<-print(tapply(hab$ab,hab$geno,mean, na.rm=T))
barplot(ranked)
#so this kept them in IL number... if I reverse the order, will it be chaos?
ranked1<-print(tapply(hab$geno,hab$ab,mean, na.rm=T))
#yup. can't do it
#library(gridExtra) - doesn't exist anymore.

#found example, made new file "reordering data notes

#example: 
#colours <- read.table("colours.txt", sep="\t")
#library(lattice)
#histogram(~ V2 | V1, data=colours,  type="count")
#answer:
#colours$V2 = factor(colours$V2, levels=c("low", "medium", "high"))
#histogram(~ V2 | V1, data=colours,  type="count")

library(lattice)
#because they did ^

wtf <- ggplot(hab, aes(geno,norm.ad, fill=Parent))
wtf$norm.ad = factor(wtf$norm.ad, levels=c(1))
wtf+
  geom_histogram(position="dodge", stat="identity", binwidth=.05)

#none of that works

#other example
#mtcars$carb2 <- factor(mtcars$carb, levels=rev(levels(factor(mtcars$carb))))

#ggplot(data=mtcars, aes(y=carb2, x=mpg, colour=hp)) +
 # geom_point()

#hab$norm.ad <- factor(hab$norm.ad, levels=rev(levels(factor(hab$norm.ad))))
#  ^ this changes norm.ad into a factor!! so nothing i tried after this was ever going to work b/c the 'numeric' characteristic was lost

#Ciera helped:
a5 <- ggplot(hab, aes(x=reorder(geno,norm.ab), y=norm.ab, fill=Parent))
a5+
  geom_bar(position="dodge", stat="identity", binwidth=.05)+
  #ylab=("Averaged Adaxial Stomata")+
  scale_fill_manual(name="Abaxial Stomatal Density", values = c("orange", "dark blue","magenta"))+
  theme(legend.position = "bottom")

# ^for some reason, ylabel breaks the whole thing. wtf

#what if we had ab and ad on the same graph? facet?? 

barplot(tapply(hab$geno,list(hab$avg.ad,hab$avg.ab), na.rm=T))

#Steven fixed all of the things: ZOMG so pretty
a6 <- ggplot(hab, aes(x=reorder(geno,norm.ad), y=norm.ad, fill=Parent))
a6+
  geom_bar(position="dodge", stat="identity", binwidth=.05)+
  scale_fill_manual(name="Color Key", values = c("orange", "dark blue","magenta"))+
  theme_bw()+
  theme(legend.position = c(.24,.9))+
  theme(legend.direction="horizontal")+
  theme(axis.text.x=element_text(angle=-90,hjust=-1))+
  ggtitle("Adaxial Stomatal Density")+
  ylab("Normalized Adaxial Stomata")+
  xlab("Genotypes")
  
#abaxial
a7 <- ggplot(hab, aes(x=reorder(geno,norm.ab), y=norm.ab, fill=Parent))
a7+
  geom_bar(position="dodge", stat="identity", binwidth=.05)+
  scale_fill_manual(name="Color Key", values = c("orange", "dark blue","magenta"))+
  theme_bw()+
  theme(legend.position = c(.24,.9))+
  theme(legend.direction="horizontal")+
  theme(axis.text.x=element_text(angle=-90,hjust=-1))+
  ggtitle("Abaxial Stomatal Density")+
  ylab("Normalized Abaxial Stomata")+
  xlab("Genotypes")
#___________________
#Julin's advice on splitting the column

data <- R_HAPPY_FORMAT

#ways to split up the ab/ad data

head(data)

# name.the.new.column <- substring function (the column you want, where.it.should.start, where.it.should.stop)
data$leaf.side <- substr(data$ab.ad,1,2)
data$leaf.number <- substr(data$ab.ad,3,3)

head(data)
#________________



