library(MASS)

#importing training data set
dataa <-read.csv("C:\\Users\\M. Yousaf\\Desktop\\UserManual\\trainingData.csv",header = TRUE)

#Importing testing data set
fdataa <- read.csv("C:\\Users\\M. Yousaf\\Desktop\\UserManual\\testing.csv",header = TRUE) 

View(dataa) #viewinig data

head(dataa) #viewing the headers in the data

#Applying the linear model
model1 <- lm(revenue~.,data = dataa)

#Ploting graphs
plot(model1)

#calculating the summary
summary(model1)

#Applying step function to extract important features
step(model1)
AIC(model1)
BIC(model1)
#-------------
library(ISLR)
attach(dataa)
smp_siz = floor(0.70*nrow(dataa))  # creates a value for dividing the data into train and test. In this case the value is defined as 75% of the number of rows in the dataset
smp_siz

# set seed to ensure you always have same random numbers generated
set.seed(123)

# Randomly identifies the rows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
train_ind = sample(seq_len(nrow(dataa)),size = smp_siz)

#creates the training dataset with row numbers stored in train_ind
train =dataa[train_ind,]

# creates the test dataset excluding the row numbers mentioned in train_ind
test=dataa[-train_ind,]  

View(train)
View(test)

#-------------------

library(caTools)
split_model<- sample.split(dataa$revenue,SplitRatio = 0.70)
train <- subset(dataa,split_model== T)
test <- subset(dataa,split_model== F)

x<-data.frame(dataa[1:100])

#-------------------
library(ggplot2)


subset_1=dataa[1:2100,]
View(subset_1)
#for specific
specific=c(2101:3000)
subset_2=dataa[specific,]
View(subset_2)
#---------------
library(caTools)
split_model<- sample.split(dataa$revenue,SplitRatio = 0.70)
train <- subset(dataa,split_model== T)
#take average

View(train)
test <- subset(dataa,split_model== F)
summary(train)
#apply model on all fetures
lmMod <- lm(revenue ~., data=train)
summary(lmMod)
#apply model on step features
lmMod1 <- lm(revenue ~ belongs_to_collection+homepage+budget+popularity+runtime+production_countries+production_companies , data=train) 
View(lmMod1)
head(lmMod1)
summary(lmMod1)
plot(lmMod1)

distPred <- predict(lmMod1,test)  #is pr error a ta ha  -dekh kr bta data load sai hua hau ha
distPred
View(distPred)
AIC(lmMod1)
actuals_preds <- data.frame(cbind(actuals=test$revenue, predicteds=distPred))
View(actuals_preds)
#stacked bar chart----
table1 <- table(test$revenue,distPred)
barplot(table1)
#------------
# make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  

head(actuals_preds)
plot(test$revenue,type = "l",lty=1.8,col="red")
lines(distPred,type = "l",col="green")

rmse <- sqrt(mean(distPred-test$revenue)^2)#,na.rm = T
rmse

#findig percentage error
x <- mean(dataa$revenue)
percentage_error<-(rmse/x)*100
percentage_error
#percenatge accuracy
actutual_prediction_percentage <- 100-percentage_error
actutual_prediction_percentage

#rmse(test$revenue, distPred)

#ACTUAL prediction of test data which is 4398
distPred_test <- predict(lmMod1,fdataa)  
distPred_test
View(distPred_test)
head(distPred_test)
