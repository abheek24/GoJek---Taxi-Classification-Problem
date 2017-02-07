install.packages("randomForest")
install.packages("matlab")
install.packages("e1071")
install.packages("caret")
install.packages("deepnet")
install.packages("gbm")
install.packages("ROCR")
install.packages("rms")
install.packages("gbm")
install.packages("aod")
library(randomForest)
library(matlab)
library(e1071)
library(caret)
library(deepnet)
library(gbm)
library(aod) # For chi square test

library(ROCR) # For ROC, AUC calculation
library(rms)
library(dtplyr)
library(tidyverse)

rm(list=ls())

setwd("")
#################You Import your data here#####################################

DT <- read.csv("training_cancellation.csv", stringsAsFactors = TRUE)
training_cancellation <- as.data.frame(DT)
rm(DT)


#####checking the dataset and contents##########
summary(training_cancellation)
str(training_cancellation)
head(training_cancellation)
colnames(training_cancellation)

########Selecting variables which make sense and can be 
###used by checking the contents and print####
keep_var<-c("booking_status",
"booking_date",
"booking_from_long",
"booking_from_lat",
"booking_to_long",
"booking_to_lat",
"original_rate_cost",
"amount_cust_discount",
"is_rush_hour_price" )

training_cancellation<-training_cancellation[keep_var]

str(training_cancellation)
head(training_cancellation)
library(lubridate) 

# Fetching dates and hour of operation 
# Day, date, mon weekday and year are redundant. Since we only have data for one single day.

training_cancellation$date_posix <- as.POSIXct(training_cancellation$booking_date, format = "%m/%d/%Y %H:%M:%S")
training_cancellation$date <- as.Date(training_cancellation$date_posix)
#training_cancellation$date <- as.Date(as.character(training_cancellation$booking_date),format = "%m/%d/%Y %H:%M:%S")
training_cancellation$hour.trip.start = hour(training_cancellation$date_posix)
#training_cancellation$day <- day(training_cancellation$date) 

# training_cancellation$mon <- as.numeric(format(training_cancellation$date, "%m"))
# training_cancellation$year <- as.numeric(format(training_cancellation$date, "%y"))
training_cancellation$weekday <- wday(training_cancellation$date_posix,label=TRUE,abbr=TRUE)


source('distance_geo.R')

# get Haversine formula
training_cancellation$distance.hf <- mapply(gcd.hf, training_cancellation$booking_from_long
                                         ,training_cancellation$booking_from_lat
                                         ,training_cancellation$booking_to_long
                                         ,training_cancellation$booking_to_lat)
# Vincenty inverse formula for ellipsoids (vif)
training_cancellation$distance.vif <- mapply(gcd.vif, training_cancellation$booking_from_long
                                            ,training_cancellation$booking_from_lat
                                            ,training_cancellation$booking_to_long
                                            ,training_cancellation$booking_to_lat)
# Spherical Law of Cosines (slc)
training_cancellation$distance.slc <- mapply(gcd.slc, training_cancellation$booking_from_long
                                            ,training_cancellation$booking_from_lat
                                            ,training_cancellation$booking_to_long
                                            ,training_cancellation$booking_to_lat)
str(training_cancellation)

# To check correlation between quantitative variables
#install.packages("corrplot")
library(corrplot)
M <- cor(training_cancellation[,c(3:8,12,14)])
corrplot(M, method="circle")
table(training_cancellation$hour.trip.start,training_cancellation$booking_status)

training_cancellation_final <- training_cancellation[,c(1,3:6,7:9,12,14:16)]

write.csv(training_cancellation_final,"training_cancellation_final.csv")

table(training_cancellation$hour.trip.start,training_cancellation$booking_status)
plot(training_cancellation$hour.trip.start,training_cancellation$distance.hf)
table(training_cancellation$booking_status,training_cancellation$is_rush_hour_price)
table(training_cancellation$hour.trip.start,training_cancellation$is_rush_hour_price)
str(training_cancellation_final)

library(ggplot2)

head(training_cancellation_final)

table(is.na(training_cancellation_final$booking_status))
table(is.na(training_cancellation_final$original_rate_cost))
table(is.na(training_cancellation_final$is_rush_hour_price))
table(is.na(training_cancellation_final$hour.trip.start))
table(is.na(training_cancellation_final$distance.hf))

# Converting the response variable into a factor 
training_cancellation_final$booking_status_0_1 <- as.factor(ifelse(training_cancellation_final$booking_status == "cancelled",1,0))
# Log transformation of the original rate cost 
#training_cancellation_final$original_rate_cost_log <- log(training_cancellation_final$original_rate_cost)

# Squaring the geodesic distance calculated above
#training_cancellation_final$distance.hf_sq <- training_cancellation_final$distance.hf^2

# Converting the rush hour pricing into a factor 
training_cancellation_final$is_rush_hour_price <-as.factor(training_cancellation_final$is_rush_hour_price)
# Converting the trip origination hours into a factor . 
# The proportions of trips getting cancelled are more between 12-9pm 
training_cancellation_final$hour.trip.start.buck <- ifelse(
          training_cancellation_final$hour.trip.start >= 0 & training_cancellation_final$hour.trip.start <= 5,
          "1) early morning slot",
          ifelse(
          training_cancellation_final$hour.trip.start >= 6 & training_cancellation_final$hour.trip.start <= 8,
            "2) morning slot",
          ifelse(
          training_cancellation_final$hour.trip.start >= 9 & training_cancellation_final$hour.trip.start <= 11,
            "3) 3rd morning slot",
          ifelse(
          training_cancellation_final$hour.trip.start >= 12 & training_cancellation_final$hour.trip.start <= 16,
            "4) afternoon slot",
          ifelse(
          training_cancellation_final$hour.trip.start >= 17 & training_cancellation_final$hour.trip.start <= 21,
            "5) evening slot",
          ifelse(
          training_cancellation_final$hour.trip.start >= 22 & training_cancellation_final$hour.trip.start <= 23,
            "6) late night slot","na"
          ))))))

# Converting the trip origination hours into a factor . this time with lower factors and 
# combining the early morning and morning timings.
# Early mornings and mornings were statistically insignificant during LR model fit.

training_cancellation_final$hour.trip.start.buck2 <- ifelse(
  training_cancellation_final$hour.trip.start >= 0 & training_cancellation_final$hour.trip.start <= 11,
  "1) morning slot",
  ifelse(
      training_cancellation_final$hour.trip.start >= 12 & training_cancellation_final$hour.trip.start <= 16,
        "2) afternoon slot",
        ifelse(
          training_cancellation_final$hour.trip.start >= 17 & training_cancellation_final$hour.trip.start <= 21,
          "3) evening slot",
          ifelse(
            training_cancellation_final$hour.trip.start >= 22 & training_cancellation_final$hour.trip.start <= 23,
            "4) late night slot","na"
          ))))
training_cancellation_final$hour.trip.start.buck <-as.factor(training_cancellation_final$hour.trip.start.buck)
training_cancellation_final$hour.trip.start.buck2 <-as.factor(training_cancellation_final$hour.trip.start.buck2)
str(training_cancellation_final)

keep_var<-c("booking_status_0_1",
            "booking_from_long",
            "booking_from_lat",
            "booking_to_long",
            "booking_to_lat",
            "original_rate_cost",
            "amount_cust_discount",
            "is_rush_hour_price","distance.hf","hour.trip.start.buck2")

#write.csv(training_cancellation_final[keep_var],"training_cancellation_transformed.csv")


############## Import test data #################


DT <- read.csv("test_cancellation.csv", stringsAsFactors = TRUE)
test_cancellation <- as.data.frame(DT)
rm(DT)


#####checking the dataset and contents##########
summary(test_cancellation)
str(test_cancellation)
head(test_cancellation)
colnames(test_cancellation)

########Selecting variables which make sense and can be 
###used by checking the conteants and print####
keep_var<-c("booking_status",
            "booking_date",
            "booking_from_long",
            "booking_from_lat",
            "booking_to_long",
            "booking_to_lat",
            "original_rate_cost",
            "amount_cust_discount",
            "is_rush_hour_price" )

test_cancellation<-test_cancellation[keep_var]

str(test_cancellation)
head(test_cancellation)
library(lubridate) 

test_cancellation$date_posix <- as.POSIXct(test_cancellation$booking_date, format = "%m/%d/%Y %H:%M:%S")
test_cancellation$date <- as.Date(test_cancellation$date_posix)
test_cancellation$hour.trip.start = hour(test_cancellation$date_posix)
test_cancellation$weekday <- wday(test_cancellation$date_posix,label=TRUE,abbr=TRUE)


test_cancellation$distance.hf <- mapply(gcd.hf, test_cancellation$booking_from_long
                                            ,test_cancellation$booking_from_lat
                                            ,test_cancellation$booking_to_long
                                            ,test_cancellation$booking_to_lat)
test_cancellation$distance.vif <- mapply(gcd.vif, test_cancellation$booking_from_long
                                             ,test_cancellation$booking_from_lat
                                             ,test_cancellation$booking_to_long
                                             ,test_cancellation$booking_to_lat)
test_cancellation$distance.slc <- mapply(gcd.slc, test_cancellation$booking_from_long
                                             ,test_cancellation$booking_from_lat
                                             ,test_cancellation$booking_to_long
                                             ,test_cancellation$booking_to_lat)
str(test_cancellation)


#install.packages("corrplot")
#library(corrplot)
M <- cor(test_cancellation[,c(3:8,12,14)])
corrplot(M, method="circle")
table(test_cancellation$hour.trip.start,test_cancellation$booking_status)
plot(test_cancellation$hour.trip.start,test_cancellation$distance.hf)

test_cancellation_final <- test_cancellation[,c(1,3:6,7:9,12,14:16)]

#write.csv(test_cancellation_final,"test_cancellation_final.csv")

table(test_cancellation$hour.trip.start,test_cancellation$booking_status)
plot(test_cancellation$hour.trip.start,test_cancellation$distance.hf)

table(test_cancellation$booking_status,test_cancellation$is_rush_hour_price)

table(test_cancellation$hour.trip.start,test_cancellation$is_rush_hour_price)
str(test_cancellation_final)

library(ggplot2)


table(is.na(test_cancellation_final$booking_status))
table(is.na(test_cancellation_final$original_rate_cost))
table(is.na(test_cancellation_final$is_rush_hour_price))

table(is.na(test_cancellation_final$hour.trip.start))
table(is.na(test_cancellation_final$distance.vif))

test_cancellation_final$booking_status_0_1 <- as.factor(ifelse(test_cancellation_final$booking_status == "cancelled",1,0))
#test_cancellation_final$original_rate_cost_log <- log(test_cancellation_final$original_rate_cost)
#test_cancellation_final$distance.hf_sq <- test_cancellation_final$distance.hf^2

test_cancellation_final$is_rush_hour_price <-as.factor(test_cancellation_final$is_rush_hour_price)
#test_cancellation_final$hour.trip.start <-as.factor(test_cancellation_final$hour.trip.start)
test_cancellation_final$hour.trip.start.buck <- ifelse(
  test_cancellation_final$hour.trip.start >= 0 & test_cancellation_final$hour.trip.start <= 5,
  "1) early morning slot",
  ifelse(
    test_cancellation_final$hour.trip.start >= 6 & test_cancellation_final$hour.trip.start <= 8,
    "2) morning slot",
    ifelse(
      test_cancellation_final$hour.trip.start >= 9 & test_cancellation_final$hour.trip.start <= 11,
      "3) 3rd morning slot",
      ifelse(
        test_cancellation_final$hour.trip.start >= 12 & test_cancellation_final$hour.trip.start <= 16,
        "4) afternoon slot",
        ifelse(
          test_cancellation_final$hour.trip.start >= 17 & test_cancellation_final$hour.trip.start <= 21,
          "5) evening slot",
          ifelse(
            test_cancellation_final$hour.trip.start >= 22 & test_cancellation_final$hour.trip.start <= 23,
            "6) late night slot","na"
          ))))))

test_cancellation_final$hour.trip.start.buck2 <- ifelse(
  test_cancellation_final$hour.trip.start >= 0 & test_cancellation_final$hour.trip.start <= 5,
  "1) morning slot",
  ifelse(
    test_cancellation_final$hour.trip.start >= 6 & test_cancellation_final$hour.trip.start <= 8,
    "1) morning slot",
    ifelse(
      test_cancellation_final$hour.trip.start >= 9 & test_cancellation_final$hour.trip.start <= 11,
      "1) morning slot",
      ifelse(
        test_cancellation_final$hour.trip.start >= 12 & test_cancellation_final$hour.trip.start <= 16,
        "2) afternoon slot",
        ifelse(
          test_cancellation_final$hour.trip.start >= 17 & test_cancellation_final$hour.trip.start <= 21,
          "3) evening slot",
          ifelse(
            test_cancellation_final$hour.trip.start >= 22 & test_cancellation_final$hour.trip.start <= 23,
            "4) late night slot","na"
          ))))))
test_cancellation_final$hour.trip.start.buck <-as.factor(test_cancellation_final$hour.trip.start.buck)
test_cancellation_final$hour.trip.start.buck2 <-as.factor(test_cancellation_final$hour.trip.start.buck2)
str(test_cancellation_final)

#write.csv(test_cancellation_final[keep_var],"test_cancellation_transformed.csv")


########### Clustering Pickup points 

install.packages("flexclust")
library("flexclust")
data("Nclus")

set.seed(1)
dat <- as.data.frame(Nclus)
ind <- sample(nrow(dat), 50)

dat[["train"]] <- TRUE
dat[["train"]][ind] <- FALSE
cl1 = kcca(dat[dat[["train"]]==TRUE, 1:2], k=4, kccaFamily("kmeans"))
cl1    
pred_train <- predict(cl1)
pred_test <- predict(cl1, newdata=dat[dat[["train"]]==FALSE, 1:2])
image(cl1)
points(dat[dat[["train"]]==TRUE, 1:2], col=pred_train, pch=19, cex=0.3)
points(dat[dat[["train"]]==FALSE, 1:2], col=pred_test, pch=22, bg="orange")

#####################################################################

training_cancellation_final$columnid <- c(1:nrow(training_cancellation_final))
training_cancellation_final$Training <- TRUE
test_cancellation_final$columnid <- c(1:nrow(test_cancellation_final))
test_cancellation_final$Training <- FALSE
training_test <- rbind(training_cancellation_final,test_cancellation_final)
cl1 = kcca(training_test[training_test[["Training"]]==TRUE, 2:3], k=4, kccaFamily("kmeans"))
cl1
pred_train <- predict(cl1)
pred_test <- predict(cl1, newdata=training_test[training_test[["Training"]]==FALSE, 2:3])

image(cl1)
points(training_test[training_test[["Training"]]==TRUE, 2:3], col=pred_train, pch=19, cex=0.3)
points(training_test[training_test[["Training"]]==FALSE, 2:3], col=pred_test, pch=22, bg="orange")

pred_train <- as.data.frame(pred_train)
pred_test <- as.data.frame(pred_test)
pred_train$columnid <- c(1:nrow(pred_train))
pred_test$columnid <- c(1:nrow(pred_test))

training_cancellation_final <- training_cancellation_final %>%
  left_join(pred_train,by="columnid")
test_cancellation_final <- test_cancellation_final %>%
  left_join(pred_test,by="columnid")  

training_cancellation_final$kmeans_class <- as.factor(training_cancellation_final$pred_train)  
test_cancellation_final$kmeans_class <- as.factor(test_cancellation_final$pred_test)  

# df <- data.frame(long=training_cancellation_final$booking_from_long, lat=training_cancellation_final$booking_from_lat)
# install.packages("fossil")
# library(fossil)
# # Create transposed data matrix
# reduced <- df[ sample(1:nrow(df), nrow(df)/2 ) , ]
# hc <- hclust(earth.dist(reduced))            # hierarchical clustering, dendrogram
# clust <- cutree(hc, k=5) 
# plot(reduced$long, 
#      reduced$lat, col = clust, pch = 20)
# df <- earth.dist(df)
# km <- kmeans(earth.dist(df),centers=5)
# 
# plot(reduced$long, 
#      reduced$lat, col = km, pch = 20)
# 
# 
# install.packages("fields")
# library(fields)
# threshold.in.km <- 3
# coors <- data.frame(training_cancellation_final$booking_from_long,training_cancellation_final$booking_from_lat)
# #distance matrix
# dist.in.km.matrix <- rdist.earth(coors,miles = F,R=6371)
# #clustering
# fit <- hclust(as.dist(dist.in.km.matrix), method = "single")
# clusters <- cutree(fit,h = threshold.in.km)
# 
# plot(training_cancellation_final$booking_from_long, 
#      training_cancellation_final$booking_from_lat, col = clusters, pch = 20)
# 
# # Or 
# 
# km <- kmeans(as.dist(dist.in.km.matrix),centers=3)
# 
# plot(training_cancellation_final$booking_from_long, 
#      training_cancellation_final$booking_from_lat, col = km, pch = 20)
#####################################################################

keep_var<-c("booking_status_0_1",
            "booking_from_long",
            "booking_from_lat",
            "booking_to_long",
            "booking_to_lat",
            "original_rate_cost",
            "amount_cust_discount",
            "is_rush_hour_price","distance.hf","hour.trip.start.buck2",
            "kmeans_class")


################# Fitting Models without cross validation ######################
############### Cross validation #################
############### Imbalanced Classification Problems #################
## ROSE (Random Over Sampling Examples) package helps us to generate artificial data based on sampling 
## methods and smoothed bootstrap approach
#install.packages("ROSE")
library(ROSE)

table(training_cancellation_final$booking_status_0_1)
prop.table(table(training_cancellation_final$booking_status_0_1))
str(test_cancellation_final)

################### Logistic Regression Models ##############################
lm_training_cancellation <- glm(booking_status_0_1~ ., data=training_cancellation_final[keep_var], family="binomial")
summary(lm_training_cancellation) # estimates

keep_var_logistic<-c("booking_status_0_1",
            "original_rate_cost",
            "amount_cust_discount",
            "is_rush_hour_price",
            "distance.hf",
            "hour.trip.start.buck2","kmeans_class")

lm_training_cancellation <- glm(booking_status_0_1~ ., data=training_cancellation_final[keep_var_logistic], family="binomial")
summary(lm_training_cancellation)

exp(lm_training_cancellation$coefficients)

####confidence intervals for the coefficient estimates
confint(lm_training_cancellation)


#browseURL("https://en.wikipedia.org/wiki/Wald_test")

?wald.test
wald.test(b=coef(lm_training_cancellation),Sigma=vcov(lm_training_cancellation),Terms= 1:4)
#wald.test

#The chi-squared test statistic of 1432.8, 
#with 2 degrees of freedom is associated with a p-value of 0.0
#indicating that the overall effect of Pclass is statistically significant

keep_var_logistic<-c("booking_status_0_1",
                     "original_rate_cost",
                     "amount_cust_discount",
                     "is_rush_hour_price",
                     "distance.hf",
                     "hour.trip.start.buck2","hour.trip.start","kmeans_class")

#####to calculate VIF, so first fit linear regression###
str(training_cancellation_final[keep_var_logistic])
mylm1 <- lm(as.numeric(as.character(booking_status_0_1))~ original_rate_cost+amount_cust_discount
            +as.numeric(as.character(is_rush_hour_price))
            +distance.hf
            +hour.trip.start, data=training_cancellation_final[keep_var_logistic])
summary(mylm1)
vif(mylm1)


#######performance checking on development/training data#####
?predict
?prediction
?performance
prob <- predict(lm_training_cancellation, newdata=training_cancellation_final[keep_var_logistic], type="response") # predicted probabilities
pred <- prediction(prob,training_cancellation_final$booking_status_0_1)
perf <- performance(pred,measure = "tpr",x.measure = "fpr")
plot(perf)

head(perf@x.values )
str(perf)
head(prob)

#AUC - Area under the ROC curve. This is equal to the value of the Wilcoxon-Mann-Whitney test statistic and also 
#the probability that the classifier will score are randomly drawn positive sample higher than a randomly 
#drawn negative sample. 

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc  ####0.607231

#######confusion matrix##
conf_data<-as.data.frame(cbind(prob, training_cancellation_final$booking_status_0_1))
str(conf_data)
table(conf_data$prob>0.5,training_cancellation_final$booking_status_0_1)
m<-table(conf_data$prob>0.5,training_cancellation_final$booking_status_0_1)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])

#########in training KS calculation : using a macro written in R and saved in my desktop
# KS compute function takes the probability/ score associated with each observation and the classification value

source('ks_compute.R')
ks_compute(prob,as.numeric(as.character(training_cancellation_final$booking_status_0_1)))

# cumacc   = # records in each bins, equally distributed
# predresp = mean of all score in each bucket/deciles  
# actresp  = mean of all responses{(response+non response)/total records} in each bucket/deciles
# minscore = minimum score in the decile
# maxscore = maximum score in the decile
# cumrespr = Cumulative response captured in subsequent buckets .i.e. Cumulative_Sum(actresp)/i
# cumresp  = cumacc*cumrespr
# pctresp  = cumresp/#of responders
# KSOPT    = pctresp- {(cumacc-cumresp)*100/nonresp_cnt}


max(ks_compute(prob,as.numeric(as.character(training_cancellation_final$booking_status_0_1)))[10]) #57.25%

e<-ks_compute(prob,as.numeric(as.character(training_cancellation_final$booking_status_0_1)))
# e
# write.csv(e,"Training_KS_logistic_training_cancellation_imarticus.csv")


############performance checking on validation/test data####
prob <- predict(lm_training_cancellation, newdata=test_cancellation_final[keep_var_logistic], type="response") # predicted probabilities
pred <- prediction(prob,test_cancellation_final$booking_status_0_1)
perf <- performance(pred,measure = "tpr",x.measure = "fpr")
plot(perf)

# write.csv(data.frame(prob,test_cancellation_final$booking_status_0_1),"Testing_classification.csv")

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc ####0.5894264

#######confusion matrix##
conf_data<-as.data.frame(cbind(prob, test_cancellation_final$booking_status_0_1))
str(conf_data)
table(conf_data$prob>0.5,test_cancellation_final$booking_status_0_1)
m<-table(conf_data$prob>0.5,test_cancellation_final$booking_status_0_1)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])


#########in Testing KS calculation : using a macro written in R and saved in my desktop

max(ks_compute(prob,as.numeric(test_cancellation_final$booking_status_0_1))[10]) #8.29%



################### Decision Tree Models ##############################
library(rpart)
treeimb <- rpart(booking_status_0_1 ~ ., data = training_cancellation_final[keep_var])
pred.treeimb <- predict(treeimb, newdata = test_cancellation_final[keep_var])
accuracy.meas(test_cancellation_final$booking_status_0_1, pred.treeimb[,2])
roc.curve(test_cancellation_final$booking_status_0_1, pred.treeimb[,2], plotit = T)

################### Random Forest ##############################
colnames(training_cancellation_final[keep_var])
rf = randomForest(booking_status_0_1~. , training_cancellation_final[keep_var], mtry=4, ntree=150)
str(training_cancellation_final)
str(test_cancellation_final)
rf_predictions = predict(rf,test_cancellation_final[keep_var], type="class")
#rf_predictions
table(test_cancellation_final$booking_status_0_1, rf_predictions)
accuracy.meas(test_cancellation_final$booking_status_0_1, rf_predictions)
m<-table(test_cancellation_final$booking_status_0_1, rf_predictions)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])
roc.curve(test_cancellation_final$booking_status_0_1, rf_predictions, plotit = T)
?accuracy.meas

################### GAM ##############################
library(mgcv) 
form2 <- as.formula("booking_status_0_1~s(booking_from_long)+s(booking_from_lat)+s(booking_to_long)+
                                    s(booking_to_lat)+s(original_rate_cost)+s(amount_cust_discount)+
                    is_rush_hour_price+s(distance.hf)+hour.trip.start.buck2")
glogmod <- gam(form2, data=training_cancellation_final[keep_var], family=binomial(link="logit"))
glogmod$converged
summary(glogmod)
pred.glog <- predict(glogmod, newdata=training_cancellation_final[keep_var], type="response")
table(pred.glog>0.5,training_cancellation_final$booking_status_0_1)
m<-table(training_cancellation_final$booking_status_0_1, pred.glog>0.5)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])

################### SVM ##############################
set.seed(123)
tune.out=tune(svm, booking_status_0_1~., data=training_cancellation_final[keep_var], kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)

svmfit=svm(booking_status_0_1~., data=training_cancellation_final[keep_var], kernel="radial",gamma=1,cost=1e5)
# plot(svmfit,training_cancellation_final[keep_var])
summary(svmfit)

###### Predicting Training fit
pred=predict(svmfit,newdata=training_cancellation_final[keep_var])
true=training_cancellation_final[,"booking_status_0_1"]
table(true, pred)
m<-table(training_cancellation_final$booking_status_0_1, pred)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])
accuracy.meas(training_cancellation_final$booking_status_0_1, pred)
roc.curve(training_cancellation_final$booking_status_0_1, pred, plotit = T)

###### Predicting Test fit
pred=predict(svmfit,newdata=test_cancellation_final[keep_var])
true=test_cancellation_final[,"booking_status_0_1"]
table(true, pred)
m<-table(test_cancellation_final$booking_status_0_1, pred)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])
accuracy.meas(test_cancellation_final$booking_status_0_1, pred)
roc.curve(test_cancellation_final$booking_status_0_1, pred, plotit = T)

###### Predicting Test fit from the best model
pred=predict(tune.out$best.model,test_cancellation_final[keep_var])
true=test_cancellation_final[,"booking_status_0_1"]
table(true, pred)
m<-table(test_cancellation_final$booking_status_0_1, pred)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])
# accuracy.meas(test_cancellation_final$booking_status_0_1, pred)
roc.curve(test_cancellation_final$booking_status_0_1, pred, plotit = T)



############# Over/Under sampling #################
table(training_cancellation_final$booking_status_0_1)
table(training_cancellation_final$booking_status_0_1,training_cancellation_final$kmeans_class)
prop.table(table(training_cancellation_final$booking_status_0_1))
?ovun.sample
data_balanced_over <- ovun.sample(booking_status_0_1 ~ ., data = training_cancellation_final[keep_var], method = "over",N = 16500)$data
table(data_balanced_over$booking_status_0_1)

data_balanced_under <- ovun.sample(booking_status_0_1 ~ ., data = training_cancellation_final[keep_var], method = "under", N = 3450, seed = 1)$data
table(data_balanced_under$booking_status_0_1)

data_balanced_both <- ovun.sample(booking_status_0_1 ~ ., data = training_cancellation_final[keep_var], method = "both", p=0.5,N=6000, seed = 1)$data

table(data_balanced_both$booking_status_0_1)

data.rose <- ROSE(booking_status_0_1 ~ ., data = training_cancellation_final[keep_var], seed = 1)$data
table(data.rose$booking_status_0_1)
#build decision tree models
tree.rose <- rpart(booking_status_0_1 ~ ., data = data.rose)
tree.over <- rpart(booking_status_0_1 ~ ., data = data_balanced_over)
tree.under <- rpart(booking_status_0_1 ~ ., data = data_balanced_under)
tree.both <- rpart(booking_status_0_1 ~ ., data = data_balanced_both)

pred.tree.rose <- predict(tree.rose, newdata = test_cancellation_final[keep_var])
pred.tree.over <- predict(tree.over, newdata = test_cancellation_final[keep_var])
pred.tree.under <- predict(tree.under, newdata = test_cancellation_final[keep_var])
pred.tree.both <- predict(tree.both, newdata = test_cancellation_final[keep_var])


#AUC ROSE
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.rose[,2])
#Area under the curve (AUC): 0.563

#AUC Oversampling
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.over[,2])
#Area under the curve (AUC): 0.569

#AUC Undersampling
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.under[,2])
#Area under the curve (AUC): 0.571

#AUC Both
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.both[,2])
#Area under the curve (AUC): 0.567


#build random forest models

rf = randomForest(booking_status_0_1~. , training_cancellation_final[keep_var], mtry=4, ntree=150)
str(training_cancellation_final)
str(test_cancellation_final)
rf_predictions = predict(rf,test_cancellation_final[keep_var], type="class")
#rf_predictions
table(test_cancellation_final$booking_status_0_1, rf_predictions)
accuracy.meas(test_cancellation_final$booking_status_0_1, rf_predictions)
m<-table(test_cancellation_final$booking_status_0_1, rf_predictions)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])
roc.curve(test_cancellation_final$booking_status_0_1, rf_predictions, plotit = T)


tree.rose <- randomForest(booking_status_0_1 ~ ., data = data.rose, mtry=4, ntree=150)
tree.over <- randomForest(booking_status_0_1 ~ ., data = data_balanced_over, mtry=4, ntree=150)
tree.under <- randomForest(booking_status_0_1 ~ ., data = data_balanced_under, mtry=4, ntree=150)
tree.both <- randomForest(booking_status_0_1 ~ ., data = data_balanced_both, mtry=4, ntree=150)

pred.tree.rose <- predict(tree.rose, newdata = test_cancellation_final[keep_var])
pred.tree.over <- predict(tree.over, newdata = test_cancellation_final[keep_var])
pred.tree.under <- predict(tree.under, newdata = test_cancellation_final[keep_var])
pred.tree.both <- predict(tree.both, newdata = test_cancellation_final[keep_var])

table(test_cancellation_final$booking_status_0_1, pred.tree.rose)
m<-table(test_cancellation_final$booking_status_0_1, pred.tree.rose)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])

table(test_cancellation_final$booking_status_0_1, pred.tree.over)
m<-table(test_cancellation_final$booking_status_0_1, pred.tree.over)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])

table(test_cancellation_final$booking_status_0_1, pred.tree.under)
m<-table(test_cancellation_final$booking_status_0_1, pred.tree.under)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])

table(test_cancellation_final$booking_status_0_1, pred.tree.both)
m<-table(test_cancellation_final$booking_status_0_1, pred.tree.both)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])

#AUC ROSE
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.rose)
#Area under the curve (AUC): 0.548

#AUC Oversampling
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.over)
#Area under the curve (AUC): 0.522

#AUC Undersampling
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.under)
#Area under the curve (AUC): 0.560

#AUC Both
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.both)
#Area under the curve (AUC): 0.538
table(test_cancellation_final$booking_status_0_1, pred.tree.rose)
table(rf_predictions)

################### GAM ##############################
library(mgcv) 
form2 <- as.formula("booking_status_0_1~s(booking_from_long)+s(booking_from_lat)+s(booking_to_long)+
                    s(booking_to_lat)+s(original_rate_cost)+s(amount_cust_discount)+
                    is_rush_hour_price+s(distance.hf)+hour.trip.start.buck2")
glogmod <- gam(form2, data=training_cancellation_final[keep_var], family=binomial(link="logit"))
glogmod$converged
summary(glogmod)
pred.glog <- predict(glogmod, newdata=training_cancellation_final[keep_var], type="response")
table(pred.glog>0.5,training_cancellation_final$booking_status_0_1)
m<-table(training_cancellation_final$booking_status_0_1, pred.glog>0.5)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])


tree.rose <- gam(form2, data = data.rose, family=binomial(link="logit"))
tree.over <- gam(form2, data = data_balanced_over, family=binomial(link="logit"))
tree.under <- gam(form2, data = data_balanced_under, family=binomial(link="logit"))
tree.both <- gam(form2, data = data_balanced_both, family=binomial(link="logit"))

pred.tree.rose <- predict(tree.rose, newdata = test_cancellation_final[keep_var], type="response")
pred.tree.over <- predict(tree.over, newdata = test_cancellation_final[keep_var], type="response")
pred.tree.under <- predict(tree.under, newdata = test_cancellation_final[keep_var], type="response")
pred.tree.both <- predict(tree.both, newdata = test_cancellation_final[keep_var], type="response")


m<-table(test_cancellation_final$booking_status_0_1, pred.tree.rose>0.5)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])


m<-table(test_cancellation_final$booking_status_0_1, pred.tree.over>0.5)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])


m<-table(test_cancellation_final$booking_status_0_1, pred.tree.under>0.5)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])


m<-table(test_cancellation_final$booking_status_0_1, pred.tree.both>0.5)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])

#AUC ROSE
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.rose)
#Area under the curve (AUC): 0.548

#AUC Oversampling
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.over)
#Area under the curve (AUC): 0.522

#AUC Undersampling
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.under)
#Area under the curve (AUC): 0.560

#AUC Both
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.both)

#Area under the curve (AUC): 0.538




################### SVM ##############################
# set.seed(123)
# tune.out=tune(svm, booking_status_0_1~., data=training_cancellation_final[keep_var], kernel="radial", 
#               ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
# summary(tune.out)

svmfit=svm(booking_status_0_1~., data=training_cancellation_final[keep_var], kernel="radial",gamma=1,cost=1e5)
plot(svmfit,training_cancellation_final[keep_var])
summary(svmfit)

###### Predicting Training fit
pred=predict(svmfit,newdata=training_cancellation_final[keep_var])
true=training_cancellation_final[,"booking_status_0_1"]
table(true, pred)
m<-table(training_cancellation_final$booking_status_0_1, pred)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])
accuracy.meas(training_cancellation_final$booking_status_0_1, pred)
roc.curve(training_cancellation_final$booking_status_0_1, pred, plotit = T)

###### Predicting Test fit
pred=predict(svmfit,newdata=test_cancellation_final[keep_var])
true=test_cancellation_final[,"booking_status_0_1"]
table(true, pred)
m<-table(test_cancellation_final$booking_status_0_1, pred)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])
accuracy.meas(test_cancellation_final$booking_status_0_1, pred)
roc.curve(test_cancellation_final$booking_status_0_1, pred, plotit = T)


tree.rose <- svm(booking_status_0_1 ~ ., data = data.rose, kernel="radial",gamma=1,cost=1e5)
tree.over <- svm(booking_status_0_1 ~ ., data = data_balanced_over, kernel="radial",gamma=1,cost=1e5)
tree.under <- svm(booking_status_0_1 ~ ., data = data_balanced_under, kernel="radial",gamma=1,cost=1e5)
tree.both <- svm(booking_status_0_1 ~ ., data = data_balanced_both, kernel="radial",gamma=1,cost=1e5)

pred.tree.rose <- predict(tree.rose, newdata = test_cancellation_final[keep_var])
pred.tree.over <- predict(tree.over, newdata = test_cancellation_final[keep_var])
pred.tree.under <- predict(tree.under, newdata = test_cancellation_final[keep_var])
pred.tree.both <- predict(tree.both, newdata = test_cancellation_final[keep_var])

table(test_cancellation_final$booking_status_0_1, pred.tree.rose)
m<-table(test_cancellation_final$booking_status_0_1, pred.tree.rose)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])

table(test_cancellation_final$booking_status_0_1, pred.tree.over)
m<-table(test_cancellation_final$booking_status_0_1, pred.tree.over)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])

table(test_cancellation_final$booking_status_0_1, pred.tree.under)
m<-table(test_cancellation_final$booking_status_0_1, pred.tree.under)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])

table(test_cancellation_final$booking_status_0_1, pred.tree.both)
m<-table(test_cancellation_final$booking_status_0_1, pred.tree.both)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])

#AUC ROSE
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.rose)
#Area under the curve (AUC):  0.512

#AUC Oversampling
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.over)
#Area under the curve (AUC): 0.512

#AUC Undersampling
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.under)
#Area under the curve (AUC): 0.537

#AUC Both
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.both)
#Area under the curve (AUC): 0.522



################### GBM ##############################

keep_var<-c("isCancelled",
            "booking_from_long",
            "booking_from_lat",
            "booking_to_long",
            "booking_to_lat",
            "original_rate_cost",
            "amount_cust_discount",
            "is_rush_hour_price","distance.hf","hour.trip.start.buck2")

yColumn <- 'isCancelled'
training_cancellation_final[,yColumn] <- as.numeric(as.character(training_cancellation_final[,'booking_status_0_1']))==1
test_cancellation_final[,yColumn] <- as.numeric(as.character(test_cancellation_final[,'booking_status_0_1']))==1

training_cancellation_final$isTest <- FALSE
test_cancellation_final$isTest <- TRUE
cancellation_final <- rbind(training_cancellation_final,test_cancellation_final)


modelGBM <- gbm(isCancelled ~ .,
                data=training_cancellation_final[keep_var],
                distribution='bernoulli',
                n.trees=400,
                interaction.depth=3,
                shrinkage=0.05,
                bag.fraction=0.5,
                keep.data=FALSE,
                cv.folds=5)

nTrees <- gbm.perf(modelGBM)
print(nTrees)
#print(summary(modelGBM))
cancellation_final$modelGBM <- predict(modelGBM,newdata=cancellation_final[keep_var],type='response',
                      n.trees=nTrees)
pred.gbmtest <- predict(modelGBM,newdata=test_cancellation_final[keep_var],type='response',
                                       n.trees=nTrees)

# source('GBM_plots.R')
# report(cancellation_final,'modelGBM',"GBM")


table(pred.gbmtest>0.5,test_cancellation_final$booking_status_0_1)
m<-table(test_cancellation_final$booking_status_0_1, pred.gbmtest>0.5)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])

data.rose[,yColumn] <- as.numeric(as.character(data.rose[,'booking_status_0_1']))==1
data_balanced_over[,yColumn] <- as.numeric(as.character(data_balanced_over[,'booking_status_0_1']))==1
data_balanced_under[,yColumn] <- as.numeric(as.character(data_balanced_under[,'booking_status_0_1']))==1
data_balanced_both[,yColumn] <- as.numeric(as.character(data_balanced_both[,'booking_status_0_1']))==1

tree.rose <- gbm(isCancelled ~ .,
                 data=data.rose[keep_var],
                 distribution='bernoulli',
                 n.trees=400,
                 interaction.depth=3,
                 shrinkage=0.05,
                 bag.fraction=0.5,
                 keep.data=FALSE,
                 cv.folds=5)

tree.over <- gbm(isCancelled ~ .,
                 data=data_balanced_over[keep_var],
                 distribution='bernoulli',
                 n.trees=400,
                 interaction.depth=3,
                 shrinkage=0.05,
                 bag.fraction=0.5,
                 keep.data=FALSE,
                 cv.folds=5)

tree.under <- gbm(isCancelled ~ .,
                  data=data_balanced_under[keep_var],
                  distribution='bernoulli',
                  n.trees=400,
                  interaction.depth=3,
                  shrinkage=0.05,
                  bag.fraction=0.5,
                  keep.data=FALSE,
                  cv.folds=5)

tree.both <- gbm(isCancelled ~ .,
                 data=data_balanced_both[keep_var],
                 distribution='bernoulli',
                 n.trees=400,
                 interaction.depth=3,
                 shrinkage=0.05,
                 bag.fraction=0.5,
                 keep.data=FALSE,
                 cv.folds=5)

pred.tree.rose <- predict(tree.rose, newdata = test_cancellation_final[keep_var], type="response",
                          n.trees=gbm.perf(tree.rose))
pred.tree.over <- predict(tree.over, newdata = test_cancellation_final[keep_var], type="response",
                          n.trees=gbm.perf(tree.over))
pred.tree.under <- predict(tree.under, newdata = test_cancellation_final[keep_var], type="response",
                           n.trees=gbm.perf(tree.under))
pred.tree.both <- predict(tree.both, newdata = test_cancellation_final[keep_var], type="response",
                          n.trees=gbm.perf(tree.both))


m<-table(test_cancellation_final$booking_status_0_1, pred.tree.rose>0.5)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])


m<-table(test_cancellation_final$booking_status_0_1, pred.tree.over>0.5)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])


m<-table(test_cancellation_final$booking_status_0_1, pred.tree.under>0.5)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])


m<-table(test_cancellation_final$booking_status_0_1, pred.tree.both>0.5)
(m[1,1]+m[2,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1])

#AUC ROSE
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.rose)
#Area under the curve (AUC): 0.585

#AUC Oversampling
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.over)
#Area under the curve (AUC): 0.599

#AUC Undersampling
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.under)
#Area under the curve (AUC): 0.598

#AUC Both
roc.curve(test_cancellation_final$booking_status_0_1, pred.tree.both)

#Area under the curve (AUC): 0.589





class(training_cancellation_final$booking_status_0_1)
length(rf_predictions)
length(training_cancellation_final$booking_status_0_1)

?prediction

pred <- prediction(rf_predictions,test_cancellation_final$booking_status_0_1)
perf <- performance(pred,measure = "tpr",x.measure = "fpr")
plot(perf)








##### Can use geosphere library function from the same package to calculate geodesic distance#######
########### However this doesn't seem to be working , can try out later ############

# install.packages("geosphere")
# library(geosphere)
# distm (c(106.8453
#          , -6.218911)
#        , c(106.8582
#            ,-6.225292 )
#        , fun = distHaversine)
# ?distm
# ?distHaversine
# matrix1 <- matrix(c(training_cancellation$booking_from_long[1:50]
#               , training_cancellation$booking_from_lat[1:50]), ncol=2)
# matrix2 <- matrix(c(training_cancellation$booking_to_long[1:50]
#                          , training_cancellation$booking_to_lat[1:50]), ncol=2)
# matrix3 <- distm (matrix1, matrix1, fun = distHaversine)

######### Instead include the hand written code to get geodesic distance###########


