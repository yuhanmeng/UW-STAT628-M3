data = read.csv("PA_business_cleaning.csv", header = TRUE,stringsAsFactors = FALSE)
topic<-read.csv("bus_with_topicindex.csv",header = TRUE,stringsAsFactors = FALSE) 
topic=topic[,c("business","topicindex")]
colnames(topic)[1]="business_id"
finaldata=merge(data, topic, by = "business_id", all.x = T)
write.csv(data5,"Finalbusiness.csv",row.names=FALSE)
