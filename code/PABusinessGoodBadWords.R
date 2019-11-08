library(ggplot2)
data<-read.csv("PABusinessGoodBadWords.csv",header = TRUE,stringsAsFactors = FALSE) 
data$stars=factor(data$stars)
head(data)

ggplot(data, aes(x=bad_num, y=good_num, colour=stars)) + 
  labs(x = "Negative Words", y = "Positive Words") +
  geom_point() 

data=data[which(data$bad_num<100),]
ggplot(data, aes(x=bad_num, y=good_num, colour=stars)) + 
  geom_smooth() +
  labs(x = "Negative Words", y = "Positive Words") +
  theme(axis.title.y=element_text(colour="darkred", size=20),
        axis.title.x=element_text(colour="darkred", size=20),
        axis.text.y = element_text(colour="darkred", size=18),
        axis.text.x = element_text(colour="darkred", size=18),
        legend.key.size=unit(1,'cm'),
        legend.text = element_text(colour="darkred",size=18),
        legend.title = element_text(colour="darkred", size=18)) +
  geom_point() 
