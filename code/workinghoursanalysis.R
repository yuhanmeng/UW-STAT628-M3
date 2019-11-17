library(MASS)
data<-read.csv("workinghours.csv",header = TRUE,stringsAsFactors = FALSE) 
head(data)
data$stars=factor(data$stars)
data$days=factor(data$days)

par(mfrow=c(1,2),las=1,cex=1)
par(cex.lab=1.6,cex.axis=1.6)
boxplot(averageworkingtime~stars,data=data,xlab="stars",ylab="Average Workingtime per day")
boxplot(totalworkingtime~stars,data=data,xlab="stars",ylab="Total Workingtime per week")

ord1=polr(stars~averageworkingtime,data=data)
ctable <- coef(summary(ord1))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable


ord2=polr(stars~totalworkingtime,data=data)
ctable <- coef(summary(ord2))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

