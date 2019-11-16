setwd("/Users/apple/Documents/Wisc/study/2019Fall/STAT 628/Module3")
topic1=read.csv("topic1_allsumup.csv")
head(topic1)
topic1=topic1[,3:ncol(topic1)]
topic1
model1=lm(topic1$stars~.,data = topic1)
summary(model1)
mycoef1=round(coefficients(model1),4)
a=rep(0,100)
index=1
for (i in na.omit(mycoef1)){
  if (abs(i)>0.01){
    a[index]=i
    index=index+1}
}
  
abs(mycoef1)>0.01


