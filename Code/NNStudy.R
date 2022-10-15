library(readxl)
fulldata<- read_excel("dataCrashes.xlsx", sheet="Crashes",col_names = TRUE) #3121 obs, 80 variables
fulldata<- fulldata[, c(1, 2, 6, 9, 11, 15, 23, 24, 37:41, 43, 46:48, 50, 51, 53, 61, 75, 76, 78:80)]
library(tidyverse)
library(ggplot2)
attach(fulldata)

#Feedforward Neural Network. Predict the collision severity by non-human factors.
#Remove more columns that will not be used 
fulldata[, c("CASE_ID","POPULATION", "ACCIDENT_YEAR","LATITUDE", "LONGITUDE", "POINT_X", "POINT_Y")]<- NULL
fulldata$ALCOHOL_INVOLVED<- as.numeric(as.factor(ALCOHOL_INVOLVED))
fulldata[is.na(fulldata$ALCOHOL_INVOLVED), ]$ALCOHOL_INVOLVED<- 0
fulldata <- fulldata[complete.cases(fulldata),]
#Change all char to factor to numeric
char_col<- c("CHP_BEAT_TYPE", "INTERSECTION", "WEATHER_1", "PRIMARY_COLL_FACTOR",
             "PCF_VIOL_CATEGORY", "HIT_AND_RUN", "TYPE_OF_COLLISION", "MVIW", "ROAD_SURFACE",
             "ROAD_COND_1", "LIGHTING", "CITY")

fulldata[, char_col] <- fulldata %>% 
  select("CHP_BEAT_TYPE", "INTERSECTION", "WEATHER_1", "PRIMARY_COLL_FACTOR",
         "PCF_VIOL_CATEGORY", "HIT_AND_RUN", "TYPE_OF_COLLISION", "MVIW", "ROAD_SURFACE",
         "ROAD_COND_1", "LIGHTING", "CITY") %>% 
  lapply(as.factor)

fulldata[, char_col] <- fulldata %>% 
  select("CHP_BEAT_TYPE", "INTERSECTION", "WEATHER_1", "PRIMARY_COLL_FACTOR",
         "PCF_VIOL_CATEGORY", "HIT_AND_RUN", "TYPE_OF_COLLISION", "MVIW", "ROAD_SURFACE",
         "ROAD_COND_1", "LIGHTING", "CITY") %>% 
  lapply(as.numeric)


str(fulldata)

fulldata<- data.frame(fulldata)
rmindex1<- as.numeric(rownames(fulldata[fulldata[, 5]==1, ]))
fulldata<- fulldata[-rmindex1, ]


require(neuralnet)
require(nnet)
train<- cbind(fulldata[,-6], class.ind(as.factor(fulldata$COLLISION_SEVERITY)))
names(train)<- c(names(fulldata)[-6], "l1", "l2", "l3", "l4")


n<- names(train)
f<- as.formula(paste("l1+l2+l3+l4~", paste(n[!n %in% c("l1", "l2", "l3", "l4")], collapse = "+")))
nn<- neuralnet(f, data=train, hidden=c(10, 5, 3), threshold=0.04, act.fct = "logistic", linear.output = FALSE, lifesign = "minimal", stepmax=1e7)
plot(nn)


#Check the accuracy on the training set
pr.nn<- compute(nn, train[, 1:18])
pr.nn_<- pr.nn$net.result
head(pr.nn_)
original_values<- max.col(train[, 19:22])
pr.nn_2<- max.col(pr.nn_)
mean(pr.nn_2 == original_values)

#10 fold cross validation
set.seed(500)
k<- 10
outs<- NULL
proportion<- 0.95
for(i in 1:k)
{
  index <- sample(1:nrow(train), round(proportion*nrow(train)))
  train_cv <- train[index, ]
  test_cv <- train[-index, ]
  nn_cv <- neuralnet(f,
                     data = train_cv,
                     hidden = c(10, 5, 3),
                     act.fct = "logistic",threshold=0.04,
                     linear.output = FALSE, stepmax=1e7)
  
  # Compute predictions
  pr.nn <- compute(nn_cv, test_cv[, 1:18])
  # Extract results
  pr.nn_ <- pr.nn$net.result
  # Accuracy (test set)
  original_values <- max.col(test_cv[, 19:22])
  pr.nn_2 <- max.col(pr.nn_)
  outs[i] <- mean(pr.nn_2 == original_values)
}

mean(outs)
