library(ggplot2)
library(dplyr)
red=read.csv("Data/winequality-red.csv",sep=";")


ggplot(red,aes(x=quality))+geom_bar()
summary(red)
summary(lm(quality~fixed.acidity+volatile.acidity+citric.acid,data=red))
summary(lm(quality~volatile.acidity,data=red))
summary(lm(quality~density,data=red))
c=matrix(nrow=1599,ncol=11)
c[,1]=red$fixed.acidity
c[,2]=red$volatile.acidity
c[,3]=red$citric.acid
c[,4]=red$residual.sugar
c[,5]=red$chlorides
c[,6]=red$free.sulfur.dioxide
c[,7]=red$total.sulfur.dioxide
c[,8]=red$density
c[,9]=red$pH
c[,10]=red$sulphates
c[,11]=red$alcohol

s=c()
for (i in 1:11) {
  print(summary(lm(red$quality~c[,i])))
}
#Individually, alcohol has the highest Adjusted R-Squared @ 0.2263
#What does this tell us about our connosieurs?
#Volatile Acidity has he second highest @ 0.152

s=summary(lm(quality~volatile.acidity+alcohol,data=red))
int=s$coefficients[1,1]
va=s$coefficients[2,1]
alc=s$coefficients[3,1]
x=rep(int,length(red$alcohol))
s=1:1599
q=x+(red$alcohol*alc)+(red$volatile.acidity*va)
red$predicted=q

#Residual ANalysis and Graph
r=red$quality-red$predicted
redl$res=r
ggplot(redl,aes(x=s,y=res))+geom_point(color="black",alpha=0.8)+
  geom_hline(yintercept = 0,color="red")+
  xlab("Wine #")+ylab("Error from Model")

#Different Graphs for Predicted Vs. Actual 
#(Experimental Mode)
      #Histograms
redl=red %>%
  select("quality","volatile.acidity","alcohol","predicted")
p1=ggplot(redl,aes(x=quality))+
  geom_histogram(bins=6,fill="blue",color="black")+
  xlab("Actual")+ylab("Count")

p2=ggplot(redl,aes(x=q))+
  geom_histogram(bins=6,fill="red",color="black")+
  xlab("Predicted Based on Model")+ylab("Count")

      #Points
ggplot(redl,aes(x=s))+geom_point(aes(y=predicted),color="red",size=0.5)+
  geom_point(aes(y=quality),color="blue",size=0.5)




