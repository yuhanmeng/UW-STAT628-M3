data<-read.csv("PA_business_cleaning.csv",header = TRUE,stringsAsFactors = FALSE) 
data2<-data[,c("business_id","hours","stars")]
head(data2)
#data4[which(data4$business=="PnfQvtyc5QNcKj7UCY-Riw"),]
businesslist=c()
starslist=c()
averweeklist=c()
averweekendlist=c()
weeklist=data.frame(Mon=c(NA),
                    Tues=c(NA),
                    Wednes=c(NA),
                    Thurs=c(NA),
                    Fri=c(NA),
                    Satur=c(NA),
                    Sun=c(NA))
for (i in 1:nrow(data2)){
  s1=data2$hours[i]
  s2=gsub("[{}\' ]","",s1)
  s3=strsplit(s2,",")[[1]]
  if(length(s3)!=0){
    businesslist=c(businesslist,data2$business_id[i])
    starslist=c(starslist,data2$stars[i])
    dweek=c()
    dweekend=c()
    weeklist[1,]=c(NA,NA,NA,NA,NA,NA,NA)
    for (strtime in strsplit(s3,"day:")){
      week=strtime[1]
      times=strsplit(strtime[2],"-")[[1]]
      weeklist[1,week]=paste(times[1],times[2],sep="-")
      work=-as.numeric(difftime(strptime(times[1], "%H:%M"),strptime(times[2], "%H:%M"),units='hours'))
      if (work<=0){work=work+24}
      if (week=="Satur" | week=="Sun") {
        dweekend=c(dweekend,work)
      }else {dweek=c(dweek,work)}
    }
    weeklist=rbind(weeklist,weeklist[1,])
    averweek=NA
    averweekend=NA
    if (length(dweek)!=0) {
      averweek=mean(dweek)
    }
    if (length(dweekend)!=0) {
      averweekend=mean(dweekend)
    }
    averweeklist=c(averweeklist,averweek)
    averweekendlist=c(averweekendlist,averweekend)
    if(i%%1000==0){print(i)}
  }
}
data3=data.frame(business=businesslist,
                 stars=starslist,
                 averweektime=averweeklist,
                 averweekendtime=averweekendlist
                 )
data4=cbind(data3,weeklist[-1,])

topic<-read.csv("bus_with_topicindex.csv",header = TRUE,stringsAsFactors = FALSE) 
topic=topic[,c("business","topicindex")]
data5=merge(data4, topic, by = "business", all.x = T)

write.csv(data5,"workinghoursAfterLDA.csv",row.names=FALSE)


