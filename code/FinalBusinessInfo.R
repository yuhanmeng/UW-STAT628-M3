data = read.csv("PA_business_cleaning.csv", header = TRUE,stringsAsFactors = FALSE)
topic<-read.csv("bus_with_topicindex.csv",header = TRUE,stringsAsFactors = FALSE) 
topic=topic[,c("business","topicindex")]
colnames(topic)[1]="business_id"
finaldata=merge(data, topic, by = "business_id", all.x = T)
write.csv(finaldata,"Finalbusiness.csv",row.names=FALSE)


topic1_scores = read.csv("topic1_scores.csv", header = TRUE,stringsAsFactors = FALSE)
topic2_scores = read.csv("topic2_scores.csv", header = TRUE,stringsAsFactors = FALSE)
topic3_scores = read.csv("topic3_scores.csv", header = TRUE,stringsAsFactors = FALSE)
topic4_scores = read.csv("topic4_scores.csv", header = TRUE,stringsAsFactors = FALSE)
topic5_scores = read.csv("topic5_scores.csv", header = TRUE,stringsAsFactors = FALSE)
topic1_scores$X=1
topic2_scores$X=2
topic3_scores$X=3
topic4_scores$X=4
topic5_scores$X=5
topic_scores=rbind(topic1_scores,topic2_scores,topic3_scores,topic4_scores,topic5_scores)
write.csv(finaldata,"topic_scores.csv",row.names=FALSE)
