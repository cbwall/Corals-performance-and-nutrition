#this is script for the master data from pansa/chris coral project
#this is pansa's first r script, pansa is also patient
#please be patient (as is Pansa)

rm(list=ls())
getwd()
library(car)
data=read.csv("data/masterdata.csv")
head(data)
str(data)

data$displacement..ml=as.numeric(data$displacement..ml) #get displacement to be numeric
str(data)

data$chla=as.numeric(data$chla..ug.ml*data$blastate..ml/data$surface.area..cm2)
data$chla
data$chlc2=as.numeric(data$chlc2..ug.ml*data$blastate..ml/data$surface.area..cm2)
data$chlc2
data$cells=as.numeric(data$cells.ml*data$blastate..ml/data$surface.area..cm2)
data$cells
#normalized the data for chla, chlc2, cell counts


chla=data$chla
chlc2=data$chlc2
data$chlorophyll=chla+chlc2 #combined chla and chlc2 into one column
chl=data$chlorophyll
cells=data$cells

#created shortcuts to  reference data


Boxplot(cells~data$treatment, 
     col="lightblue", 
     ylab="symbiont cells/cm2", 
     xlab="Treatment code",
     main="Symbiont Cells by Treatment")

par(mfrow=c(1,3), oma=c(10,4,2,1), pty="sq") #mar is bottom, left, top, right, oma specifies where title
Boxplot(chl~data$temperature,
     col="010203",
     ylab="chlorophyll(ug/cm2)",
     xlab="Temperature")
Boxplot(chl~data$light,
     col="lightyellow",
     ylab="chlorophyll(ug/cm2)",
     xlab="Light")
Boxplot(chl~data$feeding,
     col="334421",
     ylab="chlorophyll(ug/cm2)",
     xlab="Feed")
title("Chlorophyll Concentration", outer=TRUE)

par(mfrow=c(1,3), oma=c(10,4,2,1), pty="sq") #mar is bottom, left, top, right, oma specifies where title
Boxplot(cells~data$temperature,
     col="010203",
     ylab="Cells/cm2",
     xlab="Temperature")
Boxplot(cells~data$light,
     col="lightyellow",
     ylab="Cells/cm2",
     xlab="Light")
Boxplot(cells~data$feeding,
     col="334421",
     ylab="Cells/cm2",
     xlab="Feed")
title("Cell Concentration", outer=TRUE)
max(cells)

max(chl) #this value is abnormal in the context of the plot

      