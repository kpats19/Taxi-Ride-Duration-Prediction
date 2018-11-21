rm(list = ls())
library(lubridate)
library(geosphere)
library(h2o)
library(Metrics)
train = read.csv("train_1.csv",header = T)
test = read.csv("test_1.csv",header = T)
train = na.omit(train)
test = na.omit(test)
train = subset(train, select = -c(id,X))
train$month = numeric(nrow(train))
train$month = lubridate::month(train$pickup_datetime)
train$wday = numeric(nrow(train))
train$wday = lubridate::wday(train$pickup_datetime)
train$hour = numeric(nrow(train))
train$hour = lubridate::hour(train$pickup_datetime)
train$distance = numeric(nrow(train))
x = numeric(nrow(train))
for (i in 1:nrow(train))
{
  x[i] = distm(c(train$pickup_longitude[i],train$pickup_latitude[i]),c(train$dropoff_longitude[i],train$dropoff_latitude[i]), fun = distHaversine)
}
train$distance = x

train$speed <- train$distance/train$trip_duration
cond3 <- train$speed > 28
cond4 <- train$speed < 1.5
train <- train[!cond3,]
train <- train[!cond4,]

train$store_and_fwd_flag =  as.numeric(train$store_and_fwd_flag)
train = subset(train, select = -c(pickup_datetime,dropoff_datetime))
train$distance = train$distance/1000
train = na.omit(train)
test = na.omit(test)

longitude = c(train$pickup_longitude,train$dropoff_longitude,test$pickup_longitude,test$dropoff_longitude)
latitude = c(train$pickup_latitude,train$dropoff_latitude,test$pickup_latitude,test$dropoff_latitude)
long_lat = data.frame(longitude,latitude)
pca_long_lat = prcomp(long_lat)
#change it to nrow()
nrtrain = nrow(train)
nrtest = nrow(test)
a = 2*nrow(train)+1
b = 2*nrow(train)+nrow(test)
c= 2*nrow(train)+nrow(test)+1
d = 2*nrow(train)+2*nrow(test)
e = nrtrain+1
f = 2*nrtrain
train_pick_long_pca = pca_long_lat$x[,1][1:nrtrain]
train_drop_long_pca = pca_long_lat$x[,1][e:f]
test_pick_long_pca = pca_long_lat$x[,1][a:b]
test_drop_long_pca = pca_long_lat$x[,1][c:d]

train_pick_lat_pca = pca_long_lat$x[,2][1:nrtrain]
train_drop_lat_pca = pca_long_lat$x[,2][e:f]
test_pick_lat_pca = pca_long_lat$x[,2][a:b]
test_drop_lat_pca = pca_long_lat$x[,2][c:d]


#train = subset(train, select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
#test = subset(test, select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))

train$pick_long_pca = train_pick_long_pca
train$pick_lat_pca = train_pick_lat_pca
train$drop_long_pca = train_drop_long_pca
train$drop_lat_pca = train_drop_lat_pca
train = subset(train, select = -c(speed,pickup_longitude,pickup_latitude,dropoff_longitude, dropoff_latitude))

test$pick_long_pca = test_pick_long_pca
test$pick_lat_pca = test_pick_lat_pca
test$drop_long_pca = test_drop_long_pca
test$drop_lat_pca = test_drop_lat_pca

test$month = lubridate::month(test$pickup_datetime)
test$wday = lubridate::wday(test$pickup_datetime)
test$hour = lubridate::hour(test$pickup_datetime)
test$store_and_fwd_flag = as.numeric(test$store_and_fwd_flag)
x = numeric(nrow(test))
for (i in 1:nrow(test))
{
  x[i] = distm(c(test$pickup_longitude[i],test$pickup_latitude[i]),c(test$dropoff_longitude[i],test$dropoff_latitude[i]), fun = distHaversine)
}
test$distance = x
test$distance = test$distance/1000
test = subset(test,select = -c(X,id,dropoff_datetime,pickup_datetime,pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
#test = subset(test, select = -c(id))
test2 = test[,c(1,2,3,4,9,10,11,12,5,6,7,8)]
test_trip_duration = test2$trip_duration
test2 = subset(test2, select = -c(trip_duration))
#train = subset(train,select = -c(X))
#test = subset(test,select = -c(X))
train$trip_duration = log10(train$trip_duration)
trip_duration = 10^train$trip_duration
#train$trip_duration = 10^(train$trip_duration)
########################################################
localH2O <- h2o.init(nthreads = -1)
train.h2o = as.h2o(train)
test.h2o = as.h2o(test2)
y2 = 4
x2 = c(1,2,3,5,6,7,8,9,10,11)
res =as.numeric(10)
for (i in 1:10)
{
dlmodel = h2o.deeplearning(x = x2,y=y2, training_frame = train.h2o, hidden = c(i+15,1))
#qh2o.performance(dlmodel)
yhat = as.data.frame(h2o.predict(dlmodel,test.h2o))
yhat2 = 10^yhat
#write.csv(yhat2, file = "lol2_2.csv")
#write.csv(yhat, file = "lol_2.csv")
res[i]=rmsle(test_trip_duration,yhat2)
}
index= as.numeric(10)
for (i in 1:10)
  index[i]=i+15;
plot_data=data.frame(x=rnorm(10), y=rnorm(10))
plot_data$x = res;
plot_data$y = index;

ggplot(data=plot_data, aes(x=y, y=x)) +
  geom_line(aes(group=1),size=2, colour="#000099") +  # Blue lines
  geom_point(size=4, colour="#CC0000") +labs(x = "Hidden Layer Size",y="RMSLE",title = "Performance of Neural Network")         # Red dots
###################################################################################

for (i in 1:5)
{
rforestmodel = h2o.randomForest(x=x2, y = y2, training_frame = train.h2o, ntrees = 500, max_depth = i+15, seed = 1122)
h2o.performance(rforestmodel)
yhat_rf = as.data.frame(h2o.predict(rforestmodel, test.h2o))
yhat_rf2 = 10^yhat_rf
#write.csv(yhat_rf2, file = "lol_rf.csv")
res[i]=rmsle(test_trip_duration,yhat_rf2)
}
index= as.numeric(5)
for (i in 1:5)
index[i]=i+15;
plot_data=data.frame(x=rnorm(5), y=rnorm(5))
plot_data$x = res;
plot_data$y = index;

ggplot(data=plot_data, aes(x=y, y=x)) +
  geom_line(aes(group=1),size=2, colour="#000099") +  # Blue lines
  geom_point(size=4, colour="#CC0000") +labs(x = "Tree Depth",y="RMSLE",title = "Performance of Random Forest")         # Red dots


###################################################################################
res =as.numeric(10)
for (i in 1:10)
{
gbmmodel = h2o.gbm(x=x2,y=y2, training_frame = train.h2o, ntrees = 500, max_depth = i+7, learn_rate = 0.02, seed = 1121)
h2o.performance(gbmmodel)
yhat_gbm = as.data.frame(h2o.predict(gbmmodel, test.h2o))
yhat_gbm2 = 10^yhat_gbm
#write.csv(yhat_gbm2, file="lol_gbm2_2.csv")
res[i]=rmsle(test_trip_duration,yhat_gbm2)
}

index= as.numeric(10)
for (i in 1:10)
  index[i]=i+7;
plot_data=data.frame(x=rnorm(10), y=rnorm(10))
plot_data$x = res;
plot_data$y = index;

ggplot(data=plot_data, aes(x=y, y=x)) +
  geom_line(aes(group=1),size=2, colour="#000099") +  # Blue lines
  geom_point(size=4, colour="#CC0000") +labs(x = "Tree Depth",y="RMSLE",title = "Performance of Gradient Boosting Machine")         # Red dots




#*****************************BASELINE*********************************************
rm(list = ls())
library(lubridate)
library(geosphere)
library(h2o)
library(Metrics)
train = read.csv("train_1.csv",header = T)
test = read.csv("test_1.csv",header = T)
train = na.omit(train)
test = na.omit(test)
train = subset(train, select = -c(id,X))
train$month = numeric(nrow(train))
train$month = lubridate::month(train$pickup_datetime)
train$wday = numeric(nrow(train))
train$wday = lubridate::wday(train$pickup_datetime)
train$hour = numeric(nrow(train))
train$hour = lubridate::hour(train$pickup_datetime)
train$distance = numeric(nrow(train))
x = numeric(nrow(train))
for (i in 1:nrow(train))
{
  x[i] = distm(c(train$pickup_longitude[i],train$pickup_latitude[i]),c(train$dropoff_longitude[i],train$dropoff_latitude[i]), fun = distHaversine)
}
train$distance = x

train$speed <- train$distance/train$trip_duration
cond3 <- train$speed > 28
cond4 <- train$speed < 1.5
train <- train[!cond3,]
train <- train[!cond4,]

train$store_and_fwd_flag =  as.numeric(train$store_and_fwd_flag)
train = subset(train, select = -c(pickup_datetime,dropoff_datetime))
train$distance = train$distance/1000
train = na.omit(train)
test = na.omit(test)

x = numeric(nrow(test))
for (i in 1:nrow(test))
{
  x[i] = distm(c(test$pickup_longitude[i],test$pickup_latitude[i]),c(test$dropoff_longitude[i],test$dropoff_latitude[i]), fun = distHaversine)
}
test$distance = x
test$distance = test$distance/1000
test$store_and_fwd_flag = as.numeric(test$store_and_fwd_flag)
test$month = lubridate::month(test$pickup_datetime)
test$wday = lubridate::wday(test$pickup_datetime)
test$hour = lubridate::hour(test$pickup_datetime)


test = subset(test,select = -c(X,id,dropoff_datetime,pickup_datetime))
train = subset (train, select = -c(speed))
#test = subset(test, select = -c(id))
test2 = test[,c(1,2,3,4,5,6,7,8,10,11,12,9)]
test_trip_duration = test2$trip_duration
test2 = subset(test2, select = -c(trip_duration))
#train = subset(train,select = -c(X))
#test = subset(test,select = -c(X))
train$trip_duration = log10(train$trip_duration)
trip_duration = 10^train$trip_duration

train$pickup_longitude = ceiling(train$pickup_longitude)
train$pickup_latitude = ceiling(train$pickup_latitude)
train$dropoff_longitude = ceiling(train$dropoff_longitude)
train$dropoff_latitude = ceiling(train$dropoff_latitude)

test2$pickup_longitude = ceiling(test2$pickup_longitude)
test2$pickup_latitude = ceiling(test2$pickup_latitude)
test2$dropoff_longitude = ceiling(test2$dropoff_longitude)
test2$dropoff_latitude = ceiling(test2$dropoff_latitude)

#train$trip_duration = 10^(train$trip_duration)
########################################################
localH2O <- h2o.init(nthreads = -1)
train.h2o = as.h2o(train)
test.h2o = as.h2o(test2)
y2 = 8
x2 = c(1,2,3,4,5,6,7,9,10,11,12)
res= as.numeric(10)
for (i in 1:10){
  dlmodel = h2o.deeplearning(x = x2,y=y2, training_frame = train.h2o, hidden = c(i+15,1))
  h2o.performance(dlmodel)
  yhat = as.data.frame(h2o.predict(dlmodel,test.h2o))
  yhat2 = 10^yhat
  write.csv(yhat2, file = "lol2_nn.csv")
  #write.csv(yhat, file = "lol_2.csv")
  res[i]= rmsle(test_trip_duration,yhat2)
}
index= as.numeric(10)
for (i in 1:10)
  index[i]=i+15;
plot_data=data.frame(x=rnorm(10), y=rnorm(10))
plot_data$x = res;
plot_data$y = index;

ggplot(data=plot_data, aes(x=y, y=x)) +
  geom_line(aes(group=1),size=2, colour="#000099") +  # Blue lines
  geom_point(size=4, colour="#CC0000") +labs(x = "Hidden Layer Size",y="RMSLE",title = "Performance of Neural Network (Baseline)")         # Red dots

###################################################################################
res= as.numeric(10)

for (i in 1:10){
  rforestmodel = h2o.randomForest(x=x2, y = y2, training_frame = train.h2o, ntrees = 500, max_depth = i+10, seed = 1122)
  h2o.performance(rforestmodel)
  yhat_rf = as.data.frame(h2o.predict(rforestmodel, test.h2o))
  yhat_rf2 = 10^yhat_rf
  #write.csv(yhat_rf2, file = "lol_rf.csv")
  res[5]= rmsle(test_trip_duration,yhat_rf2)
}
index= as.numeric(10)
for (i in 1:10)
  index[i]=i+7;
plot_data=data.frame(x=rnorm(10), y=rnorm(10))
plot_data$x = res;
plot_data$y = index;

ggplot(data=plot_data, aes(x=y, y=x)) +
  geom_line(aes(group=1),size=2, colour="#000099") +  # Blue lines
  geom_point(size=4, colour="#CC0000") +labs(x = "Tree Depth",y="RMSLE",title = "Performance of Random Forest (Baseline)")         # Red dots


###################################################################################

res= as.numeric(10)
for (i in 1:10){
  gbmmodel = h2o.gbm(x=x2,y=y2, training_frame = train.h2o, ntrees = 500, max_depth =i+8, learn_rate = 0.02, seed = 1121)
  h2o.performance(gbmmodel)
  yhat_gbm = as.data.frame(h2o.predict(gbmmodel, test.h2o))
  yhat_gbm2 = 10^yhat_gbm
  #write.csv(yhat_gbm2, file="lol_gbm2_2.csv")
  res[i]= rmsle(test_trip_duration,yhat_gbm2)
}
index= as.numeric(10)
for (i in 1:10)
  index[i]=i+7;
plot_data=data.frame(x=rnorm(10), y=rnorm(10))
plot_data$x = res;
plot_data$y = index;

ggplot(data=plot_data, aes(x=y, y=x)) +
  geom_line(aes(group=1),size=2, colour="#000099") +  # Blue lines
  geom_point(size=4, colour="#CC0000") +labs(x = "Tree Depth",y="RMSLE",title = "Performance of Gradient Boosting Machine(Baseline)")         # Red dots

