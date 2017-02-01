# The dependent variable in this problem is the variable sold, 
# which labels if an iPad listed on the eBay site was sold (equal to 1 if it did, and 0 if it did not). 

# The independent variables consist of 9 pieces of product data available at the time the iPad listing is posted, 
# and a unique identifier:
  
# description = The text description of the product provided by the seller.
# biddable = Whether this is an auction (biddable=1) or a sale with a fixed price (biddable=0).
# startprice = The start price (in US Dollars) for the auction (if biddable=1) or the sale price (if biddable=0).
# condition = The condition of the product (new, used, etc.)
# cellular = Whether the iPad has cellular connectivity (cellular=1) or not (cellular=0).
# carrier = The cellular carrier for which the iPad is equipped (if cellular=1); listed as "None" if cellular=0.
# color = The color of the iPad.
# storage = The iPad's storage capacity (in gigabytes).
# productline = The name of the product being sold.
 
# Source: kaggle.com

### Data preparation
data = read.csv("eBayiPadTrain.csv")
data$description = as.character(data$description)
str(data)

# Data prep
levels(data$condition) = c('for parts or not working','refurbished','new','new','refurbished','used')
levels(data$carrier) = c('AT&T','None','Sprint/T-Mobile','Sprint/T-Mobile','Sprint/T-Mobile','Unknown','Verizon')
levels(data$storage) = c('128 GB','16/32/64 GB','16/32/64 GB','16/32/64 GB','Unknown')
levels(data$cellular) = c('No cellular','Cellular','Unknown')
levels(data$productline) = c('iPad 1','iPad 2','iPad 3','iPad 4','iPad 4','iPad Air 1/2','iPad Air 1/2','iPad mini','iPad mini 2','iPad mini3','iPad mini Retina','Unknown')
data$noDescription = as.numeric(data$description=="") # no. of blank reviews
data$charCountDescription = nchar(data$description)  # no. of char in review
data$upperCaseDescription = sapply(gregexpr("[A-Z]",data$description),function(x) sum(x>0)) # number of upper case characters
data$startprice_99end = as.numeric(grepl(pattern = "99$",x = data$startprice))

data$biddable = factor(data$biddable,labels=c('not biddable','biddable'))
#data$sold = factor(data$sold,labels=c('not sold','sold'))
data$noDescription = factor(data$noDescription,labels=c('no description','contains description'))
data$startprice_99end = factor(data$startprice_99end,labels=c('not a 99 ending','99 ending'))
data$description = NULL ## Text in description is not interpreted correctly in non-english OS
write.csv(data,'eBayClean.csv',row.names=F)

######################################################################################################
###########################################################################

# New variables created include
# noDescription: whether there is a description (0,1)
# charCountDescription: Number of characters in the description
# upperCaseDescription: Number of upper case letters in the description
# startprice_99end: Whether the start price has a 99 ending (0,1)

library(ggplot2)
data = read.csv('ebayClean.csv')


## Split the Data
library(caTools)
set.seed(100)
split = sample.split(data$sold,SplitRatio = 0.7)
train = data[split,]
test = data[!split,]

## Explore the Data
names(train)
#startprice
tapply(train$startprice,train$sold,mean)
ggplot(data=train,aes(x=factor(sold),y=startprice,fill=factor(sold)))+
  geom_bar(stat='summary',fun.y='mean')
# biddable
tapply(train$sold,train$biddable,mean)
ggplot(data=train,aes(x=biddable,y=sold,fill=biddable))+
  geom_bar(stat='summary',fun.y='mean')
# condition
tapply(train$sold,train$condition,mean)
ggplot(data=train,aes(x=condition,y=sold,fill=condition))+
  geom_bar(stat='summary',fun.y='mean')
# cellular
tapply(train$sold,train$cellular,mean)
ggplot(data=train,aes(x=cellular,y=sold,fill=cellular))+
  geom_bar(stat='summary',fun.y='mean')
# carrier
tapply(train$sold,train$carrier,mean)
ggplot(data=train,aes(x=carrier,y=sold,fill=carrier))+
  geom_bar(stat='summary',fun.y='mean')
# color
tapply(train$sold,train$color,mean)
ggplot(data=train,aes(x=color,y=sold))+
  geom_bar(stat='summary',fun.y='mean',fill=c('black','gold','grey40','pink','grey100'))
# storage
tapply(train$sold,train$storage,mean)
ggplot(data=train,aes(x=storage,y=sold,fill=storage))+
  geom_bar(stat='summary',fun.y='mean')
# productline
tapply(train$sold,train$productline,mean)
ggplot(data=train,aes(x=productline,y=sold,fill=productline))+
  geom_bar(stat='summary',fun.y='mean')
# productline
tapply(train$sold,train$productline,mean)
ggplot(data=train,aes(x=productline,y=sold,fill=productline))+
  geom_bar(stat='summary',fun.y='mean')
# no description
tapply(train$sold,train$noDescription,mean)
ggplot(data=train,aes(x=noDescription,y=sold,fill=noDescription))+
  geom_bar(stat='summary',fun.y='mean')
# 99 ending
tapply(train$sold,train$startprice_99end,mean)
ggplot(data=train,aes(x=startprice_99end,y=sold,fill=startprice_99end))+
  geom_bar(stat='summary',fun.y='mean')
# upperCaseDescription
tapply(train$upperCaseDescription,train$sold,mean)
ggplot(data=train,aes(x=factor(sold),y=upperCaseDescription,fill=factor(sold)))+
  geom_bar(stat='summary',fun.y='mean')
# charCountDescription description
tapply(train$charCountDescription,train$sold,mean)
ggplot(data=train,aes(x=factor(sold),y=charCountDescription,fill=factor(sold)))+
  geom_bar(stat='summary',fun.y='mean')

### Predictive Model
# One variable
model1 = glm(sold~startprice,data=train,family='binomial')
summary(model1)
# interpretation of coefficient
summary(model1)$coef[2] # coefficient for startprice
exp(summary(model1)$coef[2])
100*(exp(summary(model1)$coef[2])-1) # % change in prob of sale with a $1 increase
# predictions: what is the prob of an iPad priced at $200 selling
predict(model1,newdata=data.frame(startprice=200),type='response') 


# a nominal variable, storage
model2 = glm(sold~storage,data=train,family='binomial')
summary(model2) 

# Interpretation of coefficient. 
# What is the chance of selling a 128 GB iPad (relative to 16/32/64 GB)
summary(model2)$coef[2] # coefficient for storage16/32/64 GB
exp(summary(model2)$coef[2]) # Chance of selling a 16/32/64 GB is 3.06 times that for 128GB
100*(exp(summary(model2)$coef[2])-1) # Chance of selling a 16/32/64 GB is 206.7% better than 128GB

predict(model2,newdata=data.frame(storage='128 GB'),type='response') # or use predict function


model3 = glm(sold~startprice+biddable+condition+cellular+carrier+color+storage+productline+noDescription+upperCaseDescription+startprice_99end,data=train,family='binomial')
summary(model3) # Compare Model 3 with models 1 & 2


### Model Performance
pred = predict(model3,type='response')
ggplot(data=data.frame(pred),aes(x=pred))+
  geom_histogram(fill='steelblue3')
table(as.numeric(pred>0.5)) 
ct = table(train$sold,pred>0.5);ct # classification table
accuracy = sum(ct[1,1],ct[2,2])/nrow(train); accuracy
specificity = ct[1,1]/sum(ct[1,1],ct[1,2]); specificity
sensitivity = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitivity
# accuracy v/s going with majority class
t = table(train$sold)
baseline = max(t[1],t[2])/nrow(train); baseline


# baseline prediction for test sample
baseline = table(test$sold)[1]/nrow(test); baseline
pred = predict(model3,newdata=test,type='response')
ggplot(data=data.frame(pred),aes(x=pred))+
  geom_histogram(fill='steelblue3')
ct = table(test$sold,pred>0.5); ct
accuracy = sum(ct[1,1],ct[2,2])/nrow(test); accuracy
specificity = ct[1,1]/sum(ct[1,1],ct[1,2]); specificity
sensitivity = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitivity


ct = table(test$sold,pred>0.6); ct
accuracy = sum(ct[1,1],ct[2,2])/nrow(test); accuracy
specificity = ct[1,1]/sum(ct[1,1],ct[1,2]); specificity
sensitivity = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitivity

# try a number of cutoffs
j = 1; acc = integer()
for (i in seq(0.10,0.90,0.01)){
  c = table(test$sold,pred>i)
  acc[j] = sum(c[1,1],c[2,2])/nrow(test)
  j=j+1
}
ggplot(data=data.frame(cutoff=seq(0.1,0.9,0.01),accuracy=acc),aes(x=cutoff,y=accuracy))+
  geom_point()

## ROC curve to visualize the impact of different thresholds

library(ROCR)
ROCRpred = prediction(pred,test$sold)
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure
## construct plot
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf) # basic plot
plot(ROCRperf,xlab="1 - Specificity",ylab="Sensitivity") # relabeled axes
plot(ROCRperf,colorize=TRUE) # color coded ROC curve
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),text.adj=c(-0.3,2)) # color coded and annotated ROC curve

# ROC for a baseline model
baselinePred = pred*0
ROCRpred = prediction(baselinePred,test$sold)
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure
## construct plot
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf) # basic plot
plot(ROCRperf,xlab="1 - Specificity",ylab="Sensitivity") # relabeled axes


##############################################
install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)
tree1 = rpart(sold~startprice,data=train, cp=0.001)
prp(tree1)
summary(tree1)



pred=predict(tree1)
ct = table(train$sold,pred>0.5); ct
accuracy = sum(ct[1,1],ct[2,2])/nrow(train); accuracy
