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

data$chla..ug.ml[24]<-3.704
data$chla..ug.ml[94]<-4.081
data$chla..ug.ml[50]<-3.538
data$chla..ug.ml[53]<-4.579

data$chlc2..ug.ml[24]<-0.648
data$chlc2..ug.ml[94]<-0.655
data$chlc2..ug.ml[50]<-0.696
data$chlc2..ug.ml[53]<-0.616
#changed values of abnormally high data after retesting


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

#below are edits to the data based on recounts 
data2<-data #created a second data set based on the original where revised data will be stored. 
data2$cells.ml[24]<-1727500
data2$cells[24]<-3160670
data2$cells.ml[46]<-2515000
data2$cells[46]<-3539214
data2$cells.ml[70]<-1620000
data2$cells[70]<-2452176
data2$cells.ml[94]<-2358333.33333
data2$cells[94]<-4872019 #changed values of recounted cells in the matrix
cells2<-data2$cells
cells.ml2<-data2$cells.ml


Boxplot(cells2~data2$treatment, 
        col="lightblue", 
        ylab="symbiont cells/cm2", 
        xlab="Treatment code",
        main="Symbiont Cells by Treatment revised") #created plots using second dataset

par(mfrow=c(1,3), oma=c(10,4,2,1), pty="sq") #mar is bottom, left, top, right, oma specifies where title
Boxplot(cells2~data2$temperature,
        col="010203",
        ylab="Cells/cm2",
        xlab="Temperature")
Boxplot(cells2~data2$light,
        col="lightyellow",
        ylab="Cells/cm2",
        xlab="Light")
Boxplot(cells2~data2$feeding,
        col="334421",
        ylab="Cells/cm2",
        xlab="Feed")
title("Cell Concentration revised", outer=TRUE)