data<-read.csv("PA_business_cleaning.csv",header = TRUE,stringsAsFactors = FALSE) 
data2<-data[,c("business_id","hours","stars")]
head(data2)

averworklist=c()
totalworklist=c()
dayslist=c()
for (i in 1:nrow(data2)){
  s1=data2$hours[i]
  s2=gsub("[{}\' ]","",s1)
  s3=strsplit(s2,",")[[1]]
  d=c()
  for (strtime in strsplit(s3,"y:")){
    times=strsplit(strtime[2],"-")[[1]]
    work=-as.numeric(difftime(strptime(times[1], "%H:%M"),strptime(times[2], "%H:%M"),units='hours'))
    if (work<=0){work=work+24}
    d=c(d,work)
  }
  if (length(d)==0) 
  {
    averwork=NA
    totalwork=NA
    days=NA
  }
  else
  {
    averwork=mean(d)
    totalwork=sum(d)
    days=length(d)
  }
  averworklist=c(averworklist,averwork)
  totalworklist=c(totalworklist,totalwork)
  dayslist=c(dayslist,days)
  if(i%%1000==0){print(i)}
}

data3=data.frame(business_id=data2$business_id,
                 stars=data2$stars,
                 averageworkingtime=averworklist,
                 totalworkingtime=totalworklist,
                 days=dayslist);
write.csv(data3,"workinghours.csv",row.names=FALSE)



