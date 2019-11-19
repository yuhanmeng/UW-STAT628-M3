setwd("/Users/luli/Desktop/wisc/628/module3/guide")
topic1<-read.csv("/Users/luli/Desktop/wisc/628/module3/guide/topic1.csv",header=TRUE)
topic2<-read.csv("/Users/luli/Desktop/wisc/628/module3/guide/topic2.csv",header=TRUE)
topic3<-read.csv("/Users/luli/Desktop/wisc/628/module3/guide/topic3.csv",header=TRUE)
topic4<-read.csv("/Users/luli/Desktop/wisc/628/module3/guide/topic4.csv",header=TRUE)
topic5<-read.csv("/Users/luli/Desktop/wisc/628/module3/guide/topic5.csv",header=TRUE)



colnames(topic1)
ncol(topic1)
names(topic1)
setwd("/Users/luli/Desktop/wisc/628/module3/guide")
write("topic1.csv",file="desc1.txt")
write("Na",file="desc1.txt",append=TRUE)
write("2",file="desc1.txt",append=TRUE)
write.table(cbind(1:ncol(topic1),names(topic1),rep("c",ncol(topic1))),file="desc1.txt",
            row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)

write("topic2.csv",file="desc2.txt")
write("Na",file="desc2.txt",append=TRUE)
write("2",file="desc2.txt",append=TRUE)
write.table(cbind(1:ncol(topic2),names(topic2),rep("c",ncol(topic2))),file="desc2.txt",
            row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)








setwd("/Users/luli/Desktop/wisc/628/module3/guide")
z0 <- read.table("topic1.scr",header=TRUE)
z0<-z0[1:26,]
par(mar=c(5,10,2,1),las=1)
barplot(z0$Score[1:15],names.arg=z0$Variable[1:15],col="orange",horiz=TRUE,xlab="Importance scores",cex.names=0.9)
abline(v=1,col="red",lty=2)
abline(v=5,col="red",lty=2)
attr1<-data.frame(Variable=z0$Variable)
write.table(attr1,file="attr1.txt",col.names=c("Attribute"))



z2 <- read.table("topic2.scr",header=TRUE)
z2<-z2[1:30,]
par(mar=c(5,6,2,1),las=1)
barplot(z2$Score,names.arg=z2$Variable,col="orange",horiz=TRUE,xlab="Importance scores",cex.names=0.5)
abline(v=1,col="red",lty=2)
abline(v=2,col="red",lty=2)
abline(v=3,col="red",lty=2)
barplot(z2$Score[1:23],names.arg=z2$Variable[1:23],col="orange",horiz=TRUE,xlab="Importance scores",cex.names=0.9)
write.table(z2$Variable[1:23],file="attr2.txt",col.names=c("Attribute"))

z3 <- read.table("topic3.scr",header=TRUE)
z3<-z3[1:9,]
barplot(z3$Score,names.arg=z3$Variable,col="orange",horiz=TRUE,xlab="Importance scores",cex.names=0.9)
abline(v=1,col="red",lty=2)
write.table(z3$Variable,file="attr3.txt",col.names=c("Attribute"))


z4 <- read.table("topic4.scr",header=TRUE)
z4<-z4[1:25,]
barplot(z4$Score,names.arg=z4$Variable,col="orange",horiz=TRUE,xlab="Importance scores",cex.names=0.9)
abline(v=1,col="red",lty=2)
write.table(z4$Variable[1:15],file="attr4.txt",col.names=c("Attribute"))


z5 <- read.table("topic5.scr",header=TRUE)
z5<-z5[1:17,]
barplot(z5$Score,names.arg=z5$Variable,col="orange",horiz=TRUE,xlab="Importance scores",cex.names=0.85)
abline(v=2.5,col="red",lty=2)
write.table(z5$Variable[1:10],file="attr5.txt",col.names=c("Attribute"))


### Futher exploring the attributes 


# using box-plot or linear regression  to give recommandation on different attributes 


# In topic1 dataset, we will futher explore the relationship between attributes and 
# stars.

# After checking  the boxplots, regression model and the data distributions under different 
# levels of attributes, there is no significant or reasonable finding in the 
# attirbute of Ambience_romantic, Ambience_touristy, Ambience_intimate,Ambience_trendy,Ambience_upscale,BYOB,
# Music_background_music, Ambience_classy, GoodForMeal_breakfast

par(mar=c(5, 4, 4, 2))
boxplot(stars~RestaurantsPriceRange2,data=topic1,main="Star rating under different price range ")
# when the price range is at the middle level third level, customers tend to give 
# better star rating.


boxplot(stars~Ambience_divey,data=topic1,main="Star rating unde")

# considering the variance and mean of star under different status about 
# ambience_divey, it's better to have the ambience of divey
lm2<-lm(stars~Ambience_divey,data=topic1)
summary(lm2)


boxplot(stars~Ambience_hipster,data=topic1,main="Star rating under different levels of Amibence_hipster",main.cex=0.8)
lm3<-lm(stars~Ambience_hipster,data=topic1)
summary(lm3)
# it's better to have a hipster ambience in your breakfast resturant! Because 
# it will increase your star rating about 0.68 if your ambience stars to be hipster!


boxplot(stars~Ambience_casual,data=topic1,main="Star rating under different levels of Ambience_casual",main.cex=0.8)
lm4<-lm(stars~Ambience_casual,data=topic1)
summary(lm4)
## it's better to have a causual ambience in your breakfast resturant! Because 
# it will increase your star rating about 0.39 if your ambience stars to be casual!

boxplot(stars~RestaurantsDelivery,data=topic1,main="Star rating under different levels of RestaurantsDelivery ",main.cex=0.8)
lm5<-lm(stars~RestaurantsDelivery,data=topic1)
summary(lm5)
## it's better to provide RestaurantsDelivery  in your breakfast resturant! Because 
# it will increase your star rating about 0.30 if you provide returant dilivery !


boxplot(stars~RestaurantsTableService,data=topic1,main="Star rating under different levels of RestaurantsDelivery ")
#It's better to provide RestaurantsDelivery  in your breakfast resturant! It will largely
# increase your star rating!


# In topic2 dataset, we will futher explore the relationship between attributes and 
# stars.

# After checking  the boxplots, regression model and the data distributions under different 
# levels of attributes, there is no significant or reasonable finding in the 
# attirbute of Music_background_music, BusinessParking_garage, Music_karaoke, Music_dj
# Music_jukebox, Music_video, BestNights_tuesday,Music_live, BestNights_wednesday,
# Ambience_casual, Music_no_music, BestNights_monday,WiFi, BestNights_saturday,
# CoatCheck , BikeParking,Corkage .

boxplot(stars~HasTV,data=topic2,main="Star rating under different levels of HasTV")

# It's unexpectable that bar type resturant are not suggested to provide TV as it will decrease your 
# star rating

boxplot(stars~RestaurantsAttire,data=topic2new,main="Star rating under different levels of RestaurantsAttire ")
lm7<-lm(stars~RestaurantsAttire,data=topic2new)
summary(lm7)

# It's better for bar type resturant to have dressy attire, which may because it will 
# attract more guests. And if the busniess change their attire into dressy style, it will roughly 
# increase star rating about 0.18.

boxplot(stars~NoiseLevel,data=topic2new,main="Star rating under different levels of Noiselevel ")
lm8<-lm(stars~NoiseLevel,data=topic2new)
summary(lm8)

# It's better for bar type resturant to be quiet, which may because people in this state
# prefer to quiet atmosphere nor the very loud type bar. And if the busniess stays quiet, it's
# star rating will increade about 0.02. However if their bar is very loud, their star rating
# will decrease about 0.35. And if their their bar is  loud, their star rating
# will decrease about 0.14.

boxplot(stars~Smoking,data=topic2new,main="Star rating under different levels of Smoking ")

# It's better for bar type resturant to be no smoking, as the star rating for no smoking is 
# significantly larger than the star rating under permiting smoking or smoking outdoor.

# In topic3 dataset, we will futher explore the relationship between attributes and 
# stars.

# After checking  the boxplots, regression model and the data distributions under different 
# levels of attributes, there is no significant or reasonable finding in the 
# attirbute of RestaurantsAttire, GoodForMeal_brunch,BusinessParking_garage,Smoking
# BusinessParking_valet, GoodForMeal_lunch and Ambience_divey.

boxplot(stars~RestaurantsDelivery,data=topic3,main="Star rating under different levels of RestaurantsDelivery")

#Dessert type resturants are suggested to provide resturant delivery service as it will 
# increase the star rating.

boxplot(stars~GoodForMeal_dessert,data=topic3,main="Star rating under different levels of GoodForMeal_dessert ")

# It's better for the dessert type resturant to have the GoodForMeal_dessert attribute.



# In topic4 dataset, we will futher explore the relationship between attributes and 
# stars.

# After checking  the boxplots, regression model and the data distributions under different 
# levels of attributes, there is no significant or reasonable finding in the 
# attirbute of Music_background_music,CoatCheck, BusinessAcceptsCreditCards, Ambience_touristy
# wifi, HasTV,RestaurantsTakeOut,HappyHour,NoiseLevel, Music_live, Music_video and Music_jukebox

boxplot(stars~GoodForKids,data=topic4,main="Star rating under different levels of GoodForKids ")

# It's better for the fast food type resturant to have the GoodForMeal_dessert attribute.

boxplot(stars~GoodForMeal_dinner,data=topic4,main="Star rating under different levels of GoodForMeal_dinner ")

# Fast food type may have higher star rating if they have GoodForMeal_dinner label. So dinner is 
# a key time for this type of resturant.

boxplot(stars~BusinessParking_street,data=topic4,main="Star rating under different levels of BusinessParking_street ")

# Fast food type may have higher star rating if their parking palace is on the street. That is 
# maybe it's better for them to open near the street.




# In topic5 dataset, we will futher explore the relationship between attributes and 
# stars.


# After checking  the boxplots, regression model and the data distributions under different 
# levels of attributes, there is no significant or reasonable finding in the 
# attirbute of BusinessAcceptsCreditCards,Ambience_divey,DriveThru,CoatCheck,wifi
# RestaurantsAttire,Music_background_music,BusinessParking_lot and Ambience_upscale. 
#It is hard to define the topic5 beacuse it does not have distinguished features from its' key
# words. So maybe by the same reason, it's hard to give suggestions on this so common type resturants.


