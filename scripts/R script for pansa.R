# this is an R script
# here you can code and write and do anything 
# these hashs make 'no coding' zones


# OK get your data--it should be saved as a .csv file (can do this in excel)
# make a place on the desktop where your data lives
# I put the data file (Pansa.example.data.csv) in a file on the Desktop called 'pansa r'
rm(list=ls()) #removes list 

# bring the data into R
getwd()
data<-read.csv("pansa r/data/Pansa.example.data.csv") 

# note, these are redundant:
# data<-read.csv("~/Desktop/Pansa Data/pansa r/data/Pansa.example.data.csv")

# to run code, put your cursor at the end of the text and hit Command+Enter

# the above is saying, "bring in this file from this location on my computer". You are telling it to "read the csv file" and once this is in the R environment, we will call this new data-frame "data"
# if the code looks 'blue' below in the R console, then it is attached and it worked! Praise R!

data # inspect the data, see if it is there!
head(data) # show the first few lines of the data
names(data) # show the names of the columns
# colnames(data)[9]="chla" #changed column 9 name
str(data) # what is the data structute, what are these columns?


# we want all dependent variables in the dataframe to be classified as 'num'
# we want all independent variable to be classified as "Factors"
# we can refer to the columns this way data$'column name', which says, in the dataframe 'data' use the column I refer to.


######## (remember you need control + Enter to run script lines) #############

data$Tank<-as.factor(data$Tank) # make the column "data$Tank" as a factor
data$cells.ml<-as.numeric(data$cells.ml) # make the column "data$cells.ml" as numerical

# did it work? 
str(data) # look at data structure again
# yes it worked.

data$CO2 # this shows all the data in the CO2 column
data$ug.chl.a.ml # this shows all the chlorophyll a data

# OK so now we'd like to do the normalizations we want for the data, and we will give these normalization a new name in the dataframe

##### chlorophyll a, make the normalized data as a new column in 'data'
data$chla<-as.numeric(data$ug.chl.a.ml*data$total.blastate.ml/data$surface.area.cm2)
# this is taking the ug chla * total ml in blastate and dividing it by surface area

data$chla # here you go, normalized data! is it in your dataframe?
names(data) # yep!, is it numeric?
str(data) # yep! all good

# Let's do the same for symbiont counts
######## symbiont cells
data$zoox<-as.numeric(data$cells.ml*data$total.blastate.ml/data$surface.area.cm2)
# this is taking the total zoox in a ml * total ml in blastate and dividing it by surface area

# awesome!
# lets make some shorthand so it is easy to 'call' this data

chla<-data$chla
zoox<-data$zoox

# now we don't have to use the clunky reference to "data$" this R now know that these are referenced in the names we just gave them

#############
#############
# Let's explore the data a bit

# how does a plot of all the data look, how is it distributed without thinking of categories or treatments
# we can see a histrogram here
# you can see more colors by searching "colors in R" in google
hist(zoox, col="mediumseagreen")
hist(chla, col="coral")

# do a scatter plot with green and red circles
plot(zoox, col="green")
plot(zoox, col="red")

# what if we want to see a boxpot (this shows the scatter of the data, means)
# we can do this by factors that we care about, for instance 'CO2 treatment'
# you can see some more general formatting here for how to modify axis labels and color
# can also add a title

plot(zoox~data$CO2, col="dodgerblue", ylab = "symbiont cells/cm2", xlab="CO2 treatment", main="Symbiont densities", ylim=c(0,2500000)) # the figure will be called 'zoox.fig' you can see it my calling on the object...

plot(chla~data$CO2, col="coral", ylab = "ug chlorophyll a/cm2", xlab="CO2 treatment", main="Chlorophyll a concentration", ylim=c(0,10))


# ya know what, 2 figures on a slide would be even better-- highlight all the script below here and run it
par(mfrow=c(1,2)) # this says basically make a matrix of 1 row and 2 columns to put plots in
plot(zoox~data$CO2, col="dodgerblue", ylab = "symbiont cells/cm2", xlab="CO2 treatment", main="Symbiont densities", ylim=c(0,2500000))
plot(chla~data$CO2, col="coral", ylab = "ug chlorophyll a/cm2", xlab="CO2 treatment", main="Chlorophyll a concentration", ylim=c(0,10))


# beautiful!
# but maybe you want them to stay squares and not rectangles?
par(mfrow=c(1,2), pty="sq")
plot(zoox~data$CO2, col="dodgerblue", ylab = "symbiont cells/cm2", xlab="CO2 treatment", main="Symbiont densities", ylim=c(0,2500000))
plot(chla~data$CO2, col="coral", ylab = "ug chlorophyll a/cm2", xlab="CO2 treatment", main="Chlorophyll a concentration", ylim=c(0,10))

##### save the figure and export to where you want 
# the width and height here are referenced to pixel size

#### Must run both these calls below, together.
dev.copy(pdf, "figures/chla.zoox.boxplot.pdf", width=8, height=8)
dev.off() # this writes the file
##### 

# the above plot alone are of sufficient quality that you could use them. Box plots are actually better than means and standard error which actually can hide elements of youd data, but let's say you want to go further

install.packages('plotrix') # install this package
library('plotrix') # add it to the library

# calculate means
mean.chla<-aggregate(chla~CO2, data=data, mean) # make a matrix of chla means by CO2 treatment
mean.zoox<-aggregate(zoox~CO2, data=data, mean) 

# wanna see the data? Remember to call upon it
mean.chla # can also click on the dataframe >>>> to the right in your 'global R storage'

##### if you wanted to add more levels to this, you can #####
##### example<-aggregate(chla~CO2+Light+Tank, data=data, mean)  #####

# now calculate SE
chla.SE<-aggregate(chla~data$CO2, data=data, std.error) # calculates the SE
zoox.SE<-aggregate(zoox~data$CO2, data=data, std.error) # calculates the SE

# let's combine all this into a single plca
figure.data<-cbind(mean.chla, mean.zoox[2], chla.SE[2], zoox.SE[2]) # this is saying, cbind( **combine the columns**) all columns and rows of the mean.chla dataframe, but I only want column 2 (i.e., the [2]) from mean.zoox... and other dataframes

# see why I did this (above). The script here shows what would happen if you combined all dataframes
example<-c(mean.chla, mean.zoox[2], chla.SE[2], zoox.SE[2]); example

# Let's give the new dataframe (figure.data) some better column names
colnames(figure.data)<-c("CO2", "chla.mean", "zoox.mean", "chla.SE", "zoox.SE")
figure.data # see the new dataframe here--we can now use this to make some figures


# now this gets a bit complicated, but that's OK--just follow along...

# figure formatting script, this way the figures will look the same according to some specs I like
install.packages('ggplot2') # install this package
library('ggplot2')

Fig.formatting<-(theme_classic()) +
  theme(text=element_text(size=10),
        axis.line=element_blank(),
        legend.text.align = 0,
        legend.text=element_text(size=10),
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        aspect.ratio=1, 
        axis.ticks.length=unit(0.25, "cm"),
        axis.text.y=element_text(
          margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"), colour="black", size=10), 
        axis.text.x=element_text(
          margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"), colour="black", size=8)) +
  theme(legend.key.size = unit(0.4, "cm"))

pd <- position_dodge(0.71) #offset for error bars and columns



########## chlorophyll a figure #############
Fig.chl<-ggplot(figure.data, aes(x=CO2, y=chla.mean, fill=CO2)) +
  geom_bar(colour="black", stat="identity", position = pd, width=0.7) +
  geom_errorbar(aes(ymin=chla.mean-chla.SE, ymax=chla.mean+chla.SE),size=.5, width=0, position=pd) +
  xlab("") +
  scale_y_continuous(expand=c(0,0), limits=c(0, 8)) +
  ylab(expression(paste("Chlorophyll", ~italic("a"+"c"[2]), ~(mu*g~cm^-2), sep=""))) + Fig.formatting; Fig.chl

########## zoox figure #############
Fig.zoox<-ggplot(figure.data, aes(x=CO2, y=zoox.mean, fill=CO2)) +
  geom_bar(colour="black", stat="identity", position = pd, width=0.7) +
  geom_errorbar(aes(ymin=zoox.mean-zoox.SE, ymax=zoox.mean+zoox.SE),size=.5, width=0, position=pd) +
  xlab("") +
  scale_y_continuous(expand=c(0,0), limits=c(0, 2000000)) +
  ylab(expression(paste("Symbiont cells",~(cm^-2), sep=""))) + Fig.formatting; Fig.zoox

#### wanna save them together?
install.packages('gridExtra')
library.packages('grid')
library('gridExtra')
library('grid')

pdf("figures/chla.zoox.mean.pdf", width=8, height=8)
grid.arrange(Fig.chl, Fig.zoox, ncol=1) # arrange the 2 figures in 1 columns
dev.off()

##### two way anova
#... there is  a bit more to it, but let's just see how this could work
mod<-lm(zoox~CO2*Light, data=data) # run the model
anova(mod) # give the ANOVA table
