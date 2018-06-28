#importing the data from csv file 
data<- read.csv("CensusData.csv")

#to remove all the irrelevent data. 
data[data == "?"]= NA
data1 = na.omit(data)

#to view the content of the data frame and to do data exploration 
summary(data1)

head(data1)

str(data1)

dim(data1)

names(data1)

#our prediction value is income which is categorical
#plotting  the boxplot to remove ouliers between dependent varariable and indepependent numerical variable and make the data more rich 

bp1 = boxplot(data1$age ~ data1$Income)
bp2 = boxplot(data1$fnlwgt ~ data1$Income)
bp3 = boxplot(data1$education.num ~ data1$Income)
bp4 = boxplot(data1$hours.per.week ~ data1$Income)

####removing the ouliers which we saw in boxplot
#----we follow the max limit and min limit for boxplot and replace the outlier values with the near values of (min,max) values

#of age 
Mean<- mean(data1$age)
Sd<- sd(data1$age)

minlimit<- Mean-2*Sd
maxlimit<- Mean+2*Sd

for(i in 1:nrow(data1))
{
    if(data1[i,"age"]>= maxlimit)
        data1[i,"age"]<- maxlimit
    
    if(data1[i,"age"]<= minlimit)
      data1[i,"age"]<- minlimit
}

#of fnlwgt
Mean<- mean(data1$fnlwgt)
Sd<- sd(data1$fnlwgt)

minlimit<- Mean-2*Sd
maxlimit<- Mean+2*Sd

for(i in 1:nrow(data1))
{
  if(data1[i,"fnlwgt"]>= maxlimit)
    data1[i,"fnlwgt"]<- maxlimit
  
  if(data1[i,"fnlwgt"]<= minlimit)
    data1[i,"fnlwgt"]<- minlimit
}


#of education.num
Mean<- mean(data1$education.num)
Sd<- sd(data1$education.num)

minlimit<- Mean-2*Sd
maxlimit<- Mean+2*Sd

for(i in 1:nrow(data1))
{
  if(data1[i,"education.num"]>= maxlimit)
    data1[i,"education.num"]<- maxlimit
  
  if(data1[i,"education.num"]<= minlimit)
    data1[i,"education.num"]<- minlimit
}

#of hours.per.week
Mean<- mean(data1$hours.per.week)
Sd<- sd(data1$hours.per.week)

minlimit<- Mean-2*Sd
maxlimit<- Mean+2*Sd

for(i in 1:nrow(data1))
{
  if(data1[i,"hours.per.week"]>= maxlimit)
    data1[i,"hours.per.week"]<- maxlimit
  
  if(data1[i,"hours.per.week"]<= minlimit)
    data1[i,"hours.per.week"]<- minlimit
}

#splitting the data into training and testing data 
# sample the input data with 70% for training and 30% for testing

library(caTools)

sample <- sample.split(data1$Income,SplitRatio=0.70)

#this sample contains true and false values of data depending on the splitting ratio
sample

#assign the splitted data using subset command
train_data <- subset(data1,sample==TRUE) 
View(train_data)

test_data <- subset(data1,sample==FALSE)
View(test_data)

table(data1$Income)
table(train_data$Income)
table(test_data$Income)

#this library is for roc curve
library(ROCR)

#-----logistic regression model
regmod<-glm(Income~.,data = train_data,family="binomial")
summary(regmod)

#-----finding the probability values for the testing data  using losgistic regression model which we have built previously
prob<-predict(regmod,test_data,type ="response")
prob

#------setting the cutoff for probability values
prob1<- ifelse(prob> 0.5,"2","1")
prob1

#---combining both the actual values and predicted values and renaming them and transforming the matrix into a data frame
combined<- cbind(test_data$Income,prob1)
colnames(combined)<- c("actual","predicted")
df_combined<- data.frame(combined)
df_combined


#create confusion matrix to see how mnay cus are correctly predicted and incorrectly predicted
table(df_combined$actual ,df_combined$predicted)

dt_performance<- (6927+1426)/(6927+1426+489+926)
dt_senstivity<- 6927/(6927+489)
dt_specificity<- 1426/(1426+926)

dt_performance
dt_senstivity
dt_specificity

# to plot roc curve
preds<- prediction(as.numeric(df_combined$predicted),as.numeric(df_combined$actual))
perf1<- performance(preds,"tpr","fpr")

plot(perf1)

####decision tree  model 

#rpart --- recursive partitioning decision tree
library(rpart)

#TO BUILD DECISON TREE BY USING RPART PACKAGE(recursive partitioning decision tree)
census_model<- rpart(Income ~ .,data=train_data)
census_model

#to display it in diagram

library(rattle)

library(rpart.plot)

fancyRpartPlot(census_model)

#---predicting the outcome on testing data using the model obtained previously
pred<- predict(census_model,test_data,type = "class" )
pred

#---combining both the actual values and predicted values and renaming them and transforming the matrix into a data frame
combined<- cbind(test_data$Income,pred)
class(combined)
colnames(combined)<- c("actual","predicted")
df_combined<- data.frame(combined)

#Confusion matrix is a technique for summarizing the performance of a classification algorithm 
#create confusion matrix to see how many data points are correctly predicted and incorrectly predicted
table(df_combined$actual ,df_combined$predicted)

dt_performance<- (7077+1222)/(7077+1222+1130+339)
dt_senstivity<- 7077/(7077+339)
dt_specificity<- 1222/(1130+1222)
dt_performance
dt_senstivity
dt_specificity

# to plot roc curve
preds<- prediction(as.numeric(df_combined$predicted),as.numeric(df_combined$actual))
perf2<- performance(preds,"tpr","fpr")

plot(perf2)

####random forest model

library(randomForest)
#creating a random forest for the data 
random_model <- randomForest(Income~.,data=train_data, importance = T)
random_model

#------predicting the test_data based on the rules obtained from random forest model
ran_pred <- predict(random_model,test_data)
ran_pred

#---combining both the actual values and predicted values and renaming them and transforming the matrix into a data frame
combined<- cbind(test_data$Income,ran_pred)
class(combined)
colnames(combined)<- c("actual","predicted")
df_combined<- data.frame(combined)
View(df_combined)

#create confusion matrix to see how mnay cus are correctly predicted and incorrectly predicted
table(df_combined$actual ,df_combined$predicted)

dt_performance<- (6934+1507)/(6934+1507+482+845)
dt_senstivity<- 6934/(6934+482)
dt_specificity<- 1507/(1507+845)
dt_performance
dt_senstivity
dt_specificity

# to plot roc curve
preds<- prediction(as.numeric(ran_pred),as.numeric(test_data$Income))
perf3<- performance(preds,"tpr","fpr")

plot(perf3)


###conclusions 
#when compared between these three models  from confusion matrix and roc curve the random forest model gives highest precision of 86.41482
#so we choose to use random forest model as my final model 
