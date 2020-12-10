library(ggplot2)
red=read.csv("Data/winequality-red.csv",sep=";")


ggplot(red,aes(x=quality))+geom_bar()
summary(red)
summary(lm(quality~fixed.acidity+volatile.acidity+citric.acid,data=red))
summary(lm(quality~volatile.acidity,data=red))
summary(lm(quality~fixed.acidity,data=red))