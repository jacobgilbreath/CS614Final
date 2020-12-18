library(ggplot2)
library(dplyr)
red=read.csv("Data/winequality-red.csv",sep=";")
redl=red

ggplot(red,aes(x=quality))+geom_bar(fill="red",color="black")
summary(red)
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


for (i in 1:11) {
  print(summary(lm(red$quality~c[,i])))
}
#Individually, alcohol has the highest Adjusted R-Squared @ 0.2263
#What does this tell us about our connosieurs?
#Volatile Acidity has he second highest @ 0.152

AlcSumm=summary(lm(red$quality~red$alcohol))
VolASumm=summary(lm(red$quality~red$volatile.acidity))

AlcPred=AlcSumm$coefficients[1]+AlcSumm$coefficients[2]*red$alcohol
VolPred=VolASumm$coefficients[1]+VolASumm$coefficients[2]*red$volatile.acidity
AlcRes=AlcPred-red$quality
VolRes=VolPred-red$quality
ggplot(red)+geom_line(aes(x=alcohol,y=AlcPred),color="blue",size=1)+
  geom_point(aes(x=alcohol,y=quality))+
  xlab("Alcohol")+ylab("Quality")

ggplot(red)+geom_line(aes(x=volatile.acidity,y=VolPred),color="blue",size=1)+
  geom_point(aes(x=volatile.acidity,y=quality))+
  xlab("Volatile Acidity")+ylab("Quality")

# c1 removal = No change
# c2 removal = Drop (Keep)
# c3 removal = Slight increase
# c4 removal = SLight increase
# c5 removal = Drop (Keep)
# c6 removal = Slight Drop (Keep)
# c7 removal = SLight drop (Keep)
# c8 removal = Slight Increase
# c9 removal = Slight Drop (Keep)
# c10 removal = Larger Drop (Keep)
# c11 removal = Major Drop (Keep)
# BEST MODEL: c2, c5, c6, c7, c9, c10, c11
bestsum=summary(lm(red$quality ~ red$volatile.acidity+red$chlorides+
                     red$total.sulfur.dioxide+red$free.sulfur.dioxide+
                     red$pH+red$sulphates+red$alcohol))

p=c()
for (i in 1:8){
  p[i]=bestsum$coefficients[i]
}
I=rep(p[1],1599)
pred=I+p[2]*c[,2]+p[3]*c[,5]+p[4]*c[,6]+p[5]*c[,7]+
  p[6]*c[,9]+p[7]*c[,10]+p[8]*c[,11]
res=pred-red$quality
red$predicted=pred
red$res=res

#Residual Analysis and Graph
s=1:1599
ggplot(red,aes(x=s,y=res))+geom_point(color="black",alpha=0.8)+
  geom_hline(yintercept = 0,color="red")+
  xlab("Wine #")+ylab("Error from Model")

#Different Graphs for Predicted Vs. Actual 
#(Experimental Mode)
      #Histograms
p1=ggplot(red,aes(x=quality))+
  geom_histogram(bins=6,fill="blue",color="black")+
  xlab("Actual")+ylab("Count")

p2=ggplot(red,aes(x=predicted))+
  geom_histogram(bins=6,fill="red",color="black")+
  xlab("Predicted Based on Model")+ylab("Count")

      #Points
ggplot(red,aes(x=s))+geom_point(aes(y=predicted),color="red",size=abs(res))+
  xlab("Wine #")+ylab("Quality")+scale_size_manual(values=seq(0.1,2,by=0.1))


# Second Research Question
# Focus on Wines That have a quality of 8 and 3
boxplot(red$quality)

nrow(red[red$quality==8|red$quality==3,])
both=redl[red$quality==8|red$quality==3,]


cg=matrix(nrow=28,ncol=11)
cg[,1]=both$fixed.acidity
cg[,2]=both$volatile.acidity
cg[,3]=both$citric.acid
cg[,4]=both$residual.sugar
cg[,5]=both$chlorides
cg[,6]=both$free.sulfur.dioxide
cg[,7]=both$total.sulfur.dioxide
cg[,8]=both$density
cg[,9]=both$pH
cg[,10]=both$sulphates
cg[,11]=both$alcohol

for (i in 1:11) {
  print(summary(lm(both$quality~cg[,i])))
}

summary(lm(both$quality~cg[,1]+cg[,2]+cg[,3]+cg[,4]+cg[,5]+cg[,6]+cg[,7]+
             cg[,8]+cg[,9]+cg[,10]+cg[,11]))

mp=rep(min(cg[,9]),28)
ma=rep(min(cg[,11]),28)
alc=cg[,11]-ma
ph=cg[,9]-mp
sum=summary(lm(both$quality~alc+ph))

pred2=sum$coefficients[1]+sum$coefficients[2]*alc+sum$coefficients[3]*ph
both$pred=pred2
both$Wine_Quality=factor(both$quality)

# Graph Alcohol v PH
xg=seq(2.9,3.9,by=(3.9-2.9)/27)
yg=seq(min(both$alcohol),max(both$alcohol),
       by=(max(both$alcohol)-min(both$alcohol))/27)

b=ggplot(both)+geom_point(aes(x=pH,y=alcohol,color=Wine_Quality))+
  scale_color_manual(values=c("red","blue"))+
  geom_point(aes(x=3.23,y=10),color="darkgreen")+
  geom_line(aes(x=xg,y=yg),color="black",linetype="dashed")+
  xlab("Alcohol Level")+ylab("PH")
b=b+geom_line(aes(x=xgs,y=ygsn),color="cyan")
b=b+geom_line(aes(x=xgs,y=yps),color="orange")

# Finding which prediction was incorrect
res2=both$quality-pred2
d=abs(res2)>=2
d=!d
n=both[d,]

ggplot(both)+
  geom_histogram(aes(x=pred2),binwidth=0.75,fill="red",color="black")+
  xlab("# of Wines")+ylab("Predicted Quality")

great=both[both$quality==8,]
gs=summary(lm(great$pH ~ great$alcohol))
ygs=gs$coefficients[1]+gs$coefficients[2]*both$alcohol
xgs=both$alcohol

greatn=n[n$quality==8,]
gsn=summary(lm(greatn$alcohol ~ greatn$pH))
ygsn=gsn$coefficients[1]+gsn$coefficients[2]*both$pH
xgs=both$pH

poor=both[both$quality==3,]
ps=summary(lm(poor$alcohol ~ poor$pH))
yps=ps$coefficients[1]+ps$coefficients[2]*both$pH

b=b+geom_line(aes(x=xgs,y=ygsn),color="cyan")
b=b+geom_line(aes(x=xgs,y=yps),color="orange")
b=b+geom_line(aes(x=xgs,y=estL),color="yellow")


estI=(gsn$coefficients[1]+ps$coefficients[1])/2
estS=(gsn$coefficients[2]+ps$coefficients[2])/2
estL=estI+estS*both$pH

ggplot(both)+geom_point(aes(x=pH,y=alcohol,color=Wine_Quality))+
  scale_color_manual(values=c("red","blue"))+
  geom_point(aes(x=3.23,y=10),color="darkgreen")+
  geom_line(aes(x=xgs,y=estL),color="darkmagenta",linetype="dashed")+
  xlab("PH Level")+ylab("Alcohol")

max(both$pH)
min(both$pH)
