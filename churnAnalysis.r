################################............CHURN PREDICTION MODEL...........##########################

#remove all objects stored
rm(list=ls(all=T))

#set current working directory
setwd("C:/Users/admin/akansha")
getwd()

#load libraries for data preprocessing and visualisations
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071",
      "Information","MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','class')
#library(ggplot2)           for boxplot visualisations
#library(corrgram)          for correlation plot
#library(DMwR)              for knn imputation
#library(caret)             for partitioning data into train and test
#library(randomForest)      for implementing random forest model
#library(unbalanced)        for rebalancing the dataset
#library(C50)               for implementing decision tree
#library(dummies)           for performing confusion metrics
#library(e1071)             for implementing naive bayes
#library(Information)       for implementing naives and logistic regression
#library(MASS)              for performing chi-square test
#library(rpart)             for implementing cart algorithm
#library(gbm)               for implementing gradient boosting algorithm
#library(ROSE)              Random over sampling in binary classification
#library(sampling)          for performing sampling 
#library(DataCombine)       for combining and cleansing dataset
#library(inTrees)           interpreting tree ensembles extracting,summarising rules
#library(class)             for knn implementattion


#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)
#read data
data1=read.csv("Test_data.csv",header=T,na.strings=c("","","NA"))
data2=read.csv("Train_data.csv",header=T,na.strings=c("","","NA"))

#merging two dataset
churn_data=merge(data1,data2,all.x = TRUE, all.y = TRUE)

################exploring the data ##############
view(churn_data)
head(churn_data)
str(churn_data)

################################################# Explore data analysis #################################

## Univariate Analysis and Variable Consolidation
##Data Manupulation; convert string categories into factor numeric
for(i in 1:ncol(churn_data)){
  if(class(churn_data[,i]) == 'factor'){
    churn_data[,i] = factor(churn_data[,i], 
                            labels=(1:length(levels(factor(churn_data[,i])))))
  }
}

#check for missing values in dataset
sapply(churn_data, function(x) sum(is.na(x)))


########################################## outlier analysis #####################################3
# ## BoxPlots - Distribution and Outlier Check
numeric_index = sapply(churn_data,is.numeric) #selecting only numeric
numeric_data = churn_data[,numeric_index]
cnames=colnames(numeric_data)
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn",group=1), data = subset(churn_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Churn")+
           ggtitle(paste("Box plot of churn  for",cnames[i])))
}
# Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)
gridExtra::grid.arrange(gn10,gn11,gn12,ncol=3)
gridExtra::grid.arrange(gn13,gn14,ncol=2)
gridExtra::grid.arrange(gn15,gn16,ncol=2)

### Remove outliers using boxplot method

df=churn_data  #saving raw data for experiment just to avoid complexity

# # #loop to remove from all variables
#for(i in cnames){
  #print(i)
  #val = churn_data[,i][churn_data[,i] %in% boxplot.stats(churn_data[,i])$out]
  #print(length(val))
  #churn_data = churn_data[which(!churn_data[,i] %in% val),]
#}

#Replace all outliers with NA and impute as missing value % was more than 30%
for(i in cnames){
  val = churn_data[,i][churn_data[,i] %in% boxplot.stats(churn_data[,i])$out]
  #print(length(val))
  churn_data[,i][churn_data[,i] %in% val] = NA
}

missing_val = data.frame(apply(churn_data,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(churn_data)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Miising_perc.csv", row.names = F)
sum(missing_val$Missing_percentage)

# imputing missing values using knn method 
#churn_data=knnImputation(churn_data,k=3)
#Mean Method
#Mean Method
churn_data$account.length[is.na(churn_data$account.length)] = mean(churn_data$account.length, na.rm = T)
churn_data$area.code[is.na(churn_data$area.code)] = mean(churn_data$area.code, na.rm = T)
churn_data$number.vmail.messages[is.na(churn_data$number.vmail.messages)] = mean(churn_data$number.vmail.messages, na.rm = T)
churn_data$total.day.minutes[is.na(churn_data$total.day.minutes)] = mean(churn_data$total.day.minutes, na.rm = T)
churn_data$total.day.calls[is.na(churn_data$total.day.calls)] = mean(churn_data$total.day.calls, na.rm = T)
churn_data$total.day.charge[is.na(churn_data$total.day.charge)] = mean(churn_data$total.day.charge, na.rm = T)
churn_data$total.eve.minutes[is.na(churn_data$total.eve.minutes)] = mean(churn_data$total.eve.minutes, na.rm = T)
churn_data$total.eve.calls[is.na(churn_data$total.eve.calls)] = mean(churn_data$total.eve.calls, na.rm = T)
churn_data$total.eve.charge[is.na(churn_data$total.eve.charge)] = mean(churn_data$total.eve.charge, na.rm = T)
churn_data$total.night.minutes[is.na(churn_data$total.night.minutes)] = mean(churn_data$total.night.minutes, na.rm = T)
churn_data$total.night.calls[is.na(churn_data$total.night.calls)] = mean(churn_data$total.night.calls, na.rm = T)
churn_data$total.night.charge[is.na(churn_data$total.night.charge)] = mean(churn_data$total.night.charge, na.rm = T)
churn_data$total.intl.minutes[is.na(churn_data$total.intl.minutes)] = mean(churn_data$total.intl.minutes, na.rm = T)
churn_data$total.intl.calls[is.na(churn_data$total.intl.calls)] = mean(churn_data$total.intl.calls, na.rm = T)
churn_data$total.intl.charge[is.na(churn_data$total.intl.charge)] = mean(churn_data$total.intl.charge, na.rm = T)
churn_data$number.customer.service.calls[is.na(churn_data$number.customer.service.calls)] = mean(churn_data$number.customer.service.calls, na.rm = T)

#check for missing values in dataset
sapply(churn_data, function(x) sum(is.na(x)))

################################## Feature Selection #################################################
## Correlation Plot 
corrgram(churn_data[,numeric_index], order = F,
         upper.panel=panel.pie,text.panel=panel.txt, main = "Correlation Plot")

## Chi-squared Test of Independence
factor_index = sapply(churn_data,is.factor)
factor_data = churn_data[,factor_index]
for (i in 1:4)
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn,factor_data[,i])),simulate.p.value = TRUE)
}

############################## Dimension Reduction #############################################
churn_data= subset(churn_data, 
                   select = -c(phone.number,state,area.code))

##################################Feature Scaling################################################
#Normality check
qqnorm(churn_data$account.length)
hist(churn_data$account.length)
hist(churn_data$number.vmail.messages)
hist(churn_data$total.day.minutes)
hist(churn_data$total.day.calls)
hist(churn_data$total.day.charge)
hist(churn_data$total.eve.minutes)
hist(churn_data$total.eve.calls)
hist(churn_data$total.eve.charge)
hist(churn_data$total.night.minutes)
hist(churn_data$total.night.calls)
hist(churn_data$total.night.charge)
hist(churn_data$total.intl.minutes)
hist(churn_data$total.intl.calls)
hist(churn_data$total.intl.charge)
hist(churn_data$number.vmail.messages)



#normalisation method
#for(i in cnames){
 #print(i)
  #churn_data[,i] = (churn_data[,i] - min(churn_data[,i]))/(max(churn_data[,i] - min(churn_data[,i])))
#}

## as data is normally distributed so we will go for standardisation method

cnames = c("account.length","number.vmail.messages",
           "total.day.minutes","total.day.calls","total.day.charge",
           "total.eve.minutes","total.eve.calls","total.eve.charge",
           "total.night.minutes","total.night.calls","total.night.charge",
           "total.intl.minutes","total.intl.calls","total.intl.charge",
           "number.customer.service.calls")
#Standardisation
for(i in cnames){
  #print(i)
  churn_data[,i] = (churn_data[,i] - mean(churn_data[,i]))/
    sd(churn_data[,i])
}
head(churn_data)

#############################################Sampling#############################################
# ##Simple Random Sampling
#data_sample = churn_data[sample(nrow(churn_data), 3000, replace = F), ]
# 
# ##Stratified Sampling
#stratas = strata(churn_data, c("international.plan"), size = c(100, 199, 10, 5), method = "srswor")
#stratified_data = getdata(churn_data, stratas)
# 
# ##Systematic sampling
# #Function to generate Kth index
#sys.sample = function(N,n)
#{
  #k = ceiling(N/n)
  #r = sample(1:k, 1)
  #sys.samp = seq(r, r + k*(n-1), k)
}
# 
#lis = sys.sample(5000, 400) #select the repective rows
# 
# #Create index variable in the data
#churn_data$index = 1:5000
# 
# #Extract subset from whole data
#systematic_data = churn_data1[which(churn_data1index %in% lis),]

###################################Model Development#######################################
#Clean the environment
rmExcept("churn_data")

#Divide data into train and test using stratified sampling method
#this code will divide the data into train and test set
#we will be using 80% training and 20%testing split is there
set.seed(1234)
train.index = createDataPartition(churn_data$Churn, p = .80, list = FALSE)
train = churn_data[ train.index,]
test  = churn_data[-train.index,]


####################################################################################################
######  MODEL 1 DECISION TREE CLASSIFIER  
####################################################################################################
##Decision tree for classification
#Develop Model on training data
C50_model = C5.0(Churn ~., train, trials = 80, rules = TRUE)

#Summary of DT model
summary(C50_model)

#write rules into disk
write(capture.output(summary(C50_model)), "c50Rules.txt")

#Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-18], type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(test$Churn, C50_Predictions)
confusionMatrix(ConfMatrix_C50)

#False Negative rate
FNR = 39*102(39+102)

#Accuracy: 95.2%
#FNR: 27.65%


####################################################################################################
######  MODEL 2 RANDOM FOREST
####################################################################################################
RF_model = randomForest(Churn ~ ., train, importance = TRUE, ntree =53)

#Extract rules fromn random forest
#transform rf object to an inTrees' format
#treeList = RF2List(RF_model)  
# 
# #Extract rules
# exec = extractRules(treeList, train[,-20])  # R-executable conditions
# 
# #Visualize some rules
# exec[1:2,]
# 
# #Make rules more readable:
# readableRules = presentRules(exec, colnames(train))
# readableRules[1:2,]
# 
# #Get rule metrics
# ruleMetric = getRuleMetric(exec, train[,-20], train$Churn)  # get rule metrics
# 
# #evaulate few rules
# ruleMetric[1:2,]

#Presdict test data using random forest model
RF_Predictions = predict(RF_model, test[,-18])

##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn, RF_Predictions)
confusionMatrix(ConfMatrix_RF)
#False Negative rate
FNR = 39*100/(39+100)
#Accuracy = 95.7%
#FNR = 27.6%




####################################################################################################
######  MODEL 3 LOGISTIC REGRESSION
####################################################################################################
logit_model = glm(Churn ~ ., data = train, family = "binomial")

#summary of the model
summary(logit_model)

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")

#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)


##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn, logit_Predictions)

##Accuracy=TP+TN/TP+TN+FP+FN
accuracy=(848+20)/(848+20+121+10)

#False Negative rate= FN/FN+TP 
FNR = 121*100/(121+20)

#Accuracy = 86.8%
#FNR = 85.81%

####################################################################################################
######  MODEL 4 KNN IMPLEMENTATION
####################################################################################################

#Predict test data
KNN_Predictions = knn(train[, 1:17], test[, 1:17], train$Churn, k = 17)

#Confusion matrix
Conf_matrix = table(KNN_Predictions, test$Churn)
confusionMatrix(Conf_matrix)

#Accuracy
sum(diag(Conf_matrix))/nrow(test)

#False Negative rate= FN/FN+TP
FNR= 1*100/(25+1)

#Accuracy = 88.28%
#FNR = 17.39%



####################################################################################################
######  MODEL 5 NAIVE BAYES
####################################################################################################

#Develop model
NB_model = naiveBayes(Churn ~ ., data = train)

#predict on test cases #raw
NB_Predictions = predict(NB_model, test[,1:17], type = 'class')

#Look at confusion matrix
Conf_matrix = table(observed = test[,18], predicted = NB_Predictions)
confusionMatrix(Conf_matrix)

#FNR = FN*100/(FN+TP)
FNR= 87*100/(87+54)

#Accuracy: 88.89%
#FNR: 61.70%



##############################################################################################################################
######### Save the model to a file 
##############################################################################################################################
save(RF_model,file="churnmodel.rda")


# the random forest model accuarcy is very high and also false negative rate is very low so this can handle high dimensional spaces 
# and also large number of training examples.
# as compare to knn model it is also good but its computing time is very much and we have to determine the value of parameter k
# where k is number of nearest neighbors and the type of distance to be used







