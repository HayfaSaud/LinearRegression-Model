
library("Metrics")
####Data preparation:
#1 import
setwd("D:/PC prices prediction project/")
computers = read.csv("Computers.csv")
str(computers)
computers$X<-NULL
any(is.na(computers))  ##check null values
View(computers)
computers_input<-computers[,c(1,2,3,4)]
colnames<-c("price", "speed", "hd","ram")
summary(computers_input)
plot(computers_input)

#2 training and testing datasets
trainingRowIndex<-sample(1:nrow(computers), 0.8*nrow(computers))
trainingData<-computers[trainingRowIndex,]
testData<-computers[-trainingRowIndex,]
View(testData)


####model building
#1 find the most significant variables and excluse tha variables with high F value 
results <- lm(price~speed + hd + ram , trainingData) #model building
test<-data.frame(33,85,2)
pred1<-predict(results, test)
pred1
summary(results)
plot(results)
#price=b0+ b1speed + b2hd + b3ram + b4screen+err

#2 draw the residuals
with(results, { plot(fitted.values, residuals,ylim=c(-2000,2000) )
  points(c(min(fitted.values),max(fitted.values)),
         c(0,0), type = "l")})  
hist(results$residuals)  
qqnorm(results$residuals, ylab="Residuals") 
qqline(results$residuals) 
confint(results, level = .95)

#3 model evaluation (accuracy)
pred<-predict(results, testData)
pred
rmse<- sqrt(mean(pred-testData$price)^2) # root mean squared error #measure of the standard deviation of the residuals
rmse
mae<-mae(pred, testData$price) #mean absolute error #measure of errors #distance between  
mae
mape<-mape(pred, testData$price) #mean absolute percentage error #measure of accuracy in precentage
mape
plot(pred,testData$price,  #to show the actual vs predicted prices
     xlab="predicted",ylab="actual")
abline(a=0,b=1)


#4 prediction 
speed <- 40
hd <- 350 
ram <- 8
new_obj <- data.frame(speed, hd, ram)
conf_obj <- predict(results,new_obj,level=.95,interval="confidence") #predict on the actual population 
conf_obj   

pred_obj <- predict(results,new_obj,level=.95,interval="prediction") #predict on the predicted population 
pred_obj
