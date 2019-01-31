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

data$cells.ml[24]<-1727500
data$cells[24]<-3160670
data$cells.ml[46]<-2515000
data$cells[46]<-3539214
data$cells.ml[70]<-1620000
data$cells[70]<-2452176
data$cells.ml[94]<-2358333.33333
data$cells[94]<-4872019 
#changed values of recounted cells in the matrix

data$chla..ug.ml[24]<-3.704
data$chla..ug.ml[94]<-4.081
data$chla..ug.ml[50]<-3.538
data$chla..ug.ml[53]<-4.579
#changed values of abnormally high data after retesting

data$chlc2..ug.ml[24]<-0.648
data$chlc2..ug.ml[94]<-0.655
data$chlc2..ug.ml[50]<-0.696
data$chlc2..ug.ml[53]<-0.616
#changed values of abnormally high data after retesting


data$chla=as.numeric(data$chla..ug.ml*data$blastate..ml/data$surface.area..cm2)
data$chlc2=as.numeric(data$chlc2..ug.ml*data$blastate..ml/data$surface.area..cm2)
data$cells=as.numeric(data$cells.ml*data$blastate..ml/data$surface.area..cm2)

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

library(plotrix)
library(ggplot2)

#Chlorophyll by treatment
chl.mean<-aggregate(chl~data$treatment + data$symbiont.genotype, data, mean)
colnames(chl.mean)<-c("treatment", "sym", "mean")
chl.SE<-aggregate(chl~data$treatment + data$symbiont.genotype, data, std.error)
colnames(chl.SE)<-c("treatment", "sym", "SE")
chl.df<-cbind(chl.mean, chl.SE[c(0,3)])

fig.chl<-ggplot(chl.df, aes(x=as.factor(treatment), y=mean, fill=sym))+
  geom_bar(position=position_dodge(0.9), stat="identity")+
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=0.2, position=position_dodge(0.9))+ #add error bars
  theme(legend.position="bottom", plot.title=element_text(hjust = 0.5))+
  scale_y_continuous(limits=c(0,16.5))+ #set yaxis 
  labs(title="Chlorophyll Content by Treatment", position="center", x="Treatment code", y="Chlorophyll (ug/cm2)", legend="Symbiont Genotype")+ #add labels
  scale_fill_discrete(name="Symbiont genotype") #add legend label
fig.chl
#make a plot of chlorophyll by treatment

#cells by treatment
cells.mean<-aggregate(cells~data$treatment + data$symbiont.genotype, data, mean)
colnames(cells.mean)<-c("treatment", "sym", "mean")
cells.SE<-aggregate(cells~data$treatment + data$symbiont.genotype, data, std.error)
colnames(cells.SE)<-c("treatment", "sym", "SE")
cells.df<-cbind(cells.mean, cells.SE[c(0,3)])

fig.cells<-ggplot(cells.df, aes(x=as.factor(treatment), y=mean, fill=sym))+
  geom_bar(position=position_dodge(0.9), stat="identity")+
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=0.2, position=position_dodge(0.9))+ #add error bars
  theme(legend.position="bottom", plot.title=element_text(hjust = 0.5))+
  labs(title="Cell Concentration by Treatment", position="center", x="Treatment code", y="Cells", legend="Symbiont Genotype")+ #add labels
  scale_fill_discrete(name="Symbiont genotype") #add legend label
 fig.cells
