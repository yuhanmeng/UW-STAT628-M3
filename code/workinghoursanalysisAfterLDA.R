library(ggplot2)
data<-read.csv("workinghoursAfterLDA.csv",header = TRUE,stringsAsFactors = FALSE) 
data$topicindex=factor(data$topicindex)
head(data)
summary(data)

#delete NA working time
data_rmallNA=data[-which(is.na(data$averweektime)==1 | is.na(data$averweekendtime)==1),]
bplotdata=data.frame(
  averworktime=c(data_rmallNA$averweektime,data_rmallNA$averweekendtime),
  cat=c(rep("week",length(data_rmallNA$averweektime)),rep("weekend",length(data_rmallNA$averweekendtime)))
        )
ggplot(bplotdata,aes(y=averworktime,fill=cat))+
  geom_boxplot()+
  theme(axis.title.y=element_text(colour="darkred", size=20),
        axis.title.x=element_text(colour="darkred", size=20),
        axis.text.y = element_text(colour="darkred", size=18),
        axis.text.x = element_blank(),
        legend.key.size=unit(1,'cm'),
        legend.text = element_text(colour="darkred",size=18),
        legend.title = element_blank()) 
t.test(data_rmallNA$averweektime,data_rmallNA$averweekendtime,paired = TRUE)
#cat1=data[which(data5$topicindex==1),]
#par(mfrow=c(1,2),las=1,cex=1)
#par(cex.lab=1.6,cex.axis=1.6)
#boxplot(averweektime~stars,data=cat1,xlab="stars",ylab="Average Workingtime per day")
#boxplot(averweekendtime~stars,data=cat1,xlab="stars",ylab="Total Workingtime per week")
#data[which(data$averweektime==24),]

# method=loess
ggplot(data, aes(y=stars,colour=topicindex)) + 
  ylim(1, 5) +
  geom_smooth(aes(x=averweektime),se=FALSE,size=1.5) +
  labs(x = "Average woring time per day in week", y = "stars") +
  theme(axis.title.y=element_text(colour="darkred", size=20),
        axis.title.x=element_text(colour="darkred", size=20),
        axis.text.y = element_text(colour="darkred", size=18),
        axis.text.x = element_text(colour="darkred", size=18),
        legend.key.size=unit(1,'cm'),
        legend.text = element_text(colour="darkred",size=18),
        legend.title = element_text(colour="darkred", size=18)) +
  geom_point(aes(x=averweektime)) +
  scale_x_continuous(breaks=seq(0, 25, 2))

ggplot(data, aes(y=stars,colour=topicindex)) + 
  ylim(1, 5) +
  geom_smooth(aes(x=averweekendtime),se=FALSE,size=1.5) +
  labs(x = "Average woring time per day in weekend", y = "stars") +
  theme(axis.title.y=element_text(colour="darkred", size=20),
        axis.title.x=element_text(colour="darkred", size=20),
        axis.text.y = element_text(colour="darkred", size=18),
        axis.text.x = element_text(colour="darkred", size=18),
        legend.key.size=unit(1,'cm'),
        legend.text = element_text(colour="darkred",size=18),
        legend.title = element_text(colour="darkred", size=18)) +
  geom_point(aes(x=averweekendtime)) +
  scale_x_continuous(breaks=seq(0, 25, 2))

data.frame(category=c(1,2,3,4,5),
           week=c("4-7","4-7","3-6","5-8","6-9"),
           weekend=c("4-7","5-8","6-9","5-8","5-8"))

#time slot
timeslotweek=matrix(rep(0,24*nrow(data)),ncol = 24)
timeslotweekend=matrix(rep(0,24*nrow(data)),ncol = 24)
for (i in 1:nrow(data)){
  #timeslotweek
  st=c()
  ed=c()
  for (j in 5:9){
    if (is.na(data[i,j])==FALSE){
      s=strsplit(data[i,j],"-")[[1]]
      st=c(st,as.numeric(strsplit(s[1],":")[[1]][1]))
      s=as.numeric(strsplit(s[2],":")[[1]])
      if(s[1]<6) {s[1]=s[1]+24}
      if(s[2]>0) {s[1]=s[1]+1}
      ed=c(ed,s[1])
    }
  }
  if (length(ed)==0){
    timeslotweek[i,]=NA
  }else if(max(ed)<=24) {
    for (k in (min(st)+1):max(ed)){timeslotweek[i,k]=1}
  }else {
    for (k in 1:(max(ed)-24)){timeslotweek[i,k]=1}
    for (k in (min(st)+1):24){timeslotweek[i,k]=1}
  }
  #timeslotweekend
  st=c()
  ed=c()
  for (j in 10:11){
    if (is.na(data[i,j])==FALSE){
      s=strsplit(data[i,j],"-")[[1]]
      st=c(st,as.numeric(strsplit(s[1],":")[[1]][1]))
      s=as.numeric(strsplit(s[2],":")[[1]])
      if(s[1]<6) {s[1]=s[1]+24}
      if(s[2]>0) {s[1]=s[1]+1}
      ed=c(ed,s[1])
    }
  }
  if (length(ed)==0){
    timeslotweekend[i,]=NA
  }else if(max(ed)<=24) {
    for (k in (min(st)+1):max(ed)){timeslotweekend[i,k]=1}
  }else {
    for (k in 1:(max(ed)-24)){timeslotweekend[i,k]=1}
    for (k in (min(st)+1):24){timeslotweekend[i,k]=1}
  }
}
timeslotweek=data.frame(timeslotweek)
timeslotweekend=data.frame(timeslotweekend)
#COMMON EXPERIENCE BY HIGH STAR BUSINESS
for (i in 1:5){
  print(i)
  print("week:")
  print(names(which(apply(timeslotweek[which(data$topicindex==i & data$stars>4),],2,mean,na.rm=TRUE)>=0.5)))
  print("weekend:")
  print(names(which(apply(timeslotweekend[which(data$topicindex==i & data$stars>4),],2,mean,na.rm=TRUE)>=0.5)))
}

#RANDOM FOREST FEATURE SELECTION FOR WEEK
library(randomForest)
library(randomForestExplainer)
for (i in 1:24){timeslotweek[,i]=as.factor(timeslotweek[,i])}
timeslotweek=cbind(data[,c(2,12)],timeslotweek)
timeslotweek=na.omit(timeslotweek)

for (i in 1:5){
  cat=timeslotweek[which(timeslotweek$topicindex==i),-2]
  cat$stars=factor(cat$stars)
  set.seed(24)
  forest <- randomForest(stars~.,data=cat, localImp = TRUE)
  min_depth_frame <- min_depth_distribution(forest)
  plot_min_depth_distribution(min_depth_frame)
  importance_frame <- measure_importance(forest)
  importance_frame=importance_frame[order(importance_frame[,2]),]
  #s=importance_frame[which(importance_frame[,2]<3.5),1]
  s=importance_frame[1:8,1]
  v=levels(s)[as.numeric(s)]
  print(v)
}

cat1=timeslotweek[which(timeslotweek$topicindex==1),-2]
cat2=timeslotweek[which(timeslotweek$topicindex==2),-2]
cat3=timeslotweek[which(timeslotweek$topicindex==3),-2]
cat4=timeslotweek[which(timeslotweek$topicindex==4),-2]
cat5=timeslotweek[which(timeslotweek$topicindex==5),-2]
names(which(lm(stars~X7+X6+X11+X21+X8+X22,data=cat1)$coefficient[-1]>0))
names(which(lm(stars~X23+X17+X16+X22+X24+X12,data=cat2)$coefficient[-1]>0))
names(which(lm(stars~X9+X22+X21+X18+X17+X8,data=cat3)$coefficient[-1]>0))
names(which(lm(stars~X23+X22+X11+X16+X9+X10,data=cat4)$coefficient[-1]>0))
names(which(lm(stars~X11+X16+X17+X21+X10+X22,data=cat5)$coefficient[-1]>0))





#RANDOM FOREST FEATURE SELECTION FOR WEEKEND
for (i in 1:24){timeslotweekend[,i]=as.factor(timeslotweekend[,i])}
timeslotweekend=cbind(data[,c(2,12)],timeslotweekend)
timeslotweekend=na.omit(timeslotweekend)

for (i in 1:5){
  cat=timeslotweekend[which(timeslotweekend$topicindex==i),-2]
  cat$stars=factor(cat$stars)
  set.seed(666)
  forest <- randomForest(stars~.,data=cat, localImp = TRUE)
  min_depth_frame <- min_depth_distribution(forest)
  plot_min_depth_distribution(min_depth_frame)
  importance_frame <- measure_importance(forest)
  importance_frame=importance_frame[order(importance_frame[,2]),]
  #s=importance_frame[which(importance_frame[,2]<3.5),1]
  s=importance_frame[1:6,1]
  v=levels(s)[as.numeric(s)]
  print(v)
}

cat1=timeslotweekend[which(timeslotweekend$topicindex==1),-2]
cat2=timeslotweekend[which(timeslotweekend$topicindex==2),-2]
cat3=timeslotweekend[which(timeslotweekend$topicindex==3),-2]
cat4=timeslotweekend[which(timeslotweekend$topicindex==4),-2]
cat5=timeslotweekend[which(timeslotweekend$topicindex==5),-2]
names(which(lm(stars~X7+X21+X8+X23+X11+X22,data=cat1)$coefficient[-1]>0))
names(which(lm(stars~X12+X23+X22+X24+X17+X16,data=cat2)$coefficient[-1]>0))
names(which(lm(stars~X21+X20+X9+X10+X7+X8,data=cat3)$coefficient[-1]>0))
names(which(lm(stars~X23+X11+X22+X12+X1+X16,data=cat4)$coefficient[-1]>0))
names(which(lm(stars~X16+X17+X23+X11+X24+X10,data=cat5)$coefficient[-1]>0))



data.frame(category=c(1,2,3,4,5),
           week=c("6,12,13,14,15","16,18,19,20","9,13,14,15,16,18","9,10,18,19","10,11,12,13,14,18,19,20"),
           weekend=c("12,13,14","16,18,19,20","9,10,13,14,15,20","18,19","10,17,18,19,20,24"))

