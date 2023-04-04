# need to download potential relevant packages for work completed

install.packages("caret", dependencies = TRUE)
install.packages("bst", dependencies = TRUE)
install.packages("plyr", dependencies = TRUE)
install.packages("h2o", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("lattice", dependencies = TRUE)

library("caret")
library("bst")
library("plyr")
library("h2o")
library("ggplot2")
library("lattice")

library("caret")
library("bst")
library("plyr")
library("h2o")
library("ggplot2")
library("lattice")
library("tidyverse")
library("dplyr")
library("shiny")

# ------------------------------------------------

# rm(list = ls())

# set working directory

spotify <-read.csv("songs_normalize.csv", header=TRUE, stringsAsFactors = TRUE) # bringing in renamed Kaggle dataset 
#genre<-unlist(strsplit(as.character(spotify$genre),","))
#genre<-str_trim(genre,side="left")

# input data updates here

s1 <- spotify[,c(6,1:5,7:18)] # brings target to first column

# drop high cardinality categorical columns (artists, track) bc they create too many rows
s2 = s1[,c(1,4:18)]
names(s2)[1] <- "y" 

# create one-hot encoding for factor variables (explicit only)
dummies <- dummyVars(y ~ ., data = s2)
ex <- data.frame(predict(dummies, newdata = s2)) 
names(ex) <- gsub("\\.", "", names(ex))  
s3 <- cbind(s2$y, ex)  
# rename target back y
names(s3)[1] <- "y" 
rm(dummies, ex)

# move factors and 'year' to beginning of data set so preprocessing is easier

s3 = s2[,c(1,3:4,16,5:15)]

#normalize data set

preProcValues <- preProcess(s3[,5:ncol(s3)], method = c("center","scale"))
s3 <- predict(preProcValues, s3)
rm(preProcValues)

#---------------------------------

# utilize h2o for regression modeling

library(h2o)

h2o.init(nthreads=12, max_mem_size="64g")

# split data set into training and test groups
y <- "y"                               
x <- setdiff(names(s3), y)  
s3 = as.h2o(s3)
parts <- h2o.splitFrame(s3, 0.7, seed=99) 
train <- parts[[1]]              
test <- parts[[2]]    

# trying out different models & choosing best

# 1. random forest model
rftrain <- h2o.randomForest(x, y, train)

h2o.performance(rftrain, train)
h2o.performance(rftrain, test)


predrf = h2o.predict(object = rftrain, newdata = test)

# Random forest output:

#> h2o.performance(rftrain, train)
# H2ORegressionMetrics: drf

# MSE:  70.66613
# RMSE:  8.406315
# MAE:  5.790472
# RMSLE:  0.8653482
# Mean Residual Deviance :  70.66613

# > h2o.performance(rftrain, test)
# H2ORegressionMetrics: drf

# MSE:  461.9651
# RMSE:  21.49337
# MAE:  14.88059
# RMSLE:  1.149734
# Mean Residual Deviance :  461.9651


# Review Variable Importances 
h2o.varimp(rftrain)

# 2. deep learning model
dltrain <- h2o.deeplearning(x, y, train)

h2o.performance(dltrain, train)
h2o.performance(dltrain, test)

preddl = h2o.predict(object = dltrain, newdata = test)

# Deep learning output:

# H2ORegressionMetrics: deeplearning

# MSE:  387.7546
# RMSE:  19.69148
# MAE:  13.15723
# RMSLE:  1.144289
# Mean Residual Deviance :  387.7546

# > h2o.performance(dltrain, test)
# H2ORegressionMetrics: deeplearning

# MSE:  465.2248
# RMSE:  21.56907
# MAE:  14.35787
# RMSLE:  1.163528
# Mean Residual Deviance :  465.2248

# Review Variable Importances 
h2o.varimp(dltrain)

# 3. gbm model
gbmtrain <- h2o.gbm(x, y, train)

h2o.performance(gbmtrain, train)
h2o.performance(gbmtrain, test)

predgbm = h2o.predict(object = dltrain, newdata = test)

# gbm output:

# H2ORegressionMetrics: gbm

# MSE:  242.9123
# RMSE:  15.58564
# MAE:  10.51236
# RMSLE:  1.051931
# Mean Residual Deviance :  242.9123

# > h2o.performance(gbmtrain, test)
# H2ORegressionMetrics: gbm

# MSE:  452.3678
# RMSE:  21.26894
# MAE:  14.58913
# RMSLE:  1.149599
# Mean Residual Deviance :  452.3678

# Review Variable Importances 
h2o.varimp(gbmtrain)