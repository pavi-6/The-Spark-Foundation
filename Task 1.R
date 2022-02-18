#scores=c(21,47,27,75,30,20,88,60,81,25,85,62,41,42,17,95,30,24,67,69,30,54,35,76,86)
#hours=c(2.5,5.1,3.2,8.5,3.5,1.5,9.2,5.5,8.3,2.7,7.7,5.9,4.5,3.3,1.1,8.9,2.5,1.9,6.1,7.4,2.7,4.8,3.8,6.9,7.8)

#IMPORTING DATA
data=read.csv("http://bit.ly/w-data")
head(data) 

#RELATIONSHIP BETWEEN DEPENDENT AND INDEPENDENT VARIABLE
plot(data$Hours,data$Scores,main="Hours vs Scores",col=8)
abline(lm(Scores~Hours,data),col=2)

### SPLITTING THE DATA INTO TRAIN AND TEST ###

install.packages("caTools")
library(caTools)
set.seed(25)

sample_size=floor(0.8*nrow(data))
train_ind=sample(seq_len(nrow(data)),size=sample_size)
train=data[train_ind,]  #data[r,c]
test=data[-train_ind,]
dim(train)
dim(test)

#----------------------------------------

x_train=train[,1]
x_test=test[,1]
y_train=train[,2]
y_test=test[,2]

#FITTING OF THE MODEL
fit=lm(Scores~Hours,train)
summary(fit)

pred=predict(fit,test)
data.frame(pred)
SSR=sum((test$Scores-pred)^2)
SST=sum((test$Scores-mean(test$Scores))^2)
R_square=1-(SSR/SST)
RMSE=sqrt((SSR)/nrow(test))

#EVALUATION METRICS
R_square
RMSE

#LINEAR REGRESSION EQUATION
cat("scores =", summary(fit)$coefficients[1,1], "+", summary(fit)$coefficients[2,1], "* hours", '\n')

#PREDICTED SCORE WHEN A STUDENT STUDIES FOR 9.25 HOURS/DAY:
hour_studied=9.25
score= summary(fit)$coefficients[1,1] + (summary(fit)$coefficients[2,1]*hour_studied)
score