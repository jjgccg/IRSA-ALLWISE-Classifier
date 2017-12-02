#########################################
# Benchmarking ML Techniques on ALLWISE #
# Author: Joseph George                 #
#########################################

library(readr)
library(plyr)
library(ggplot2)
library(viridis)
library(stringi)
library(keras)
library(xgboost)

###################
# Data Formatting #
###################

#First dataset contains SIMBAD object names with associated RA and DEC measurements
#Second dataset contains WISE info for each datapoint (W1, W2, W3, W4 info)
simbad <- read.csv("C:/Users/Joe/Documents/Astro_Research_2017/SIMBAD_Matches_1 - matches.csv", header=FALSE, sep=",", stringsAsFactors=FALSE)
allwise_info <-  read.csv("C:/Users/Joe/Documents/Astro_Research_2017/allwise_info.csv", header=FALSE, sep=",", stringsAsFactors=FALSE)

#Attach simbad columns of interest to WISE info
allwise_info$object_name <- simbad$V1
allwise_info$ra <- simbad$V2
allwise_info$dec <- simbad$V3

allwise_dataset <- allwise_info[,c(17,18,19,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]

#Rename some columns
allwise_dataset <- rename(allwise_dataset, c("V1"="w1mpro", "V2"="w1sigmpro", "V3"="w1snr", "V4"="w1rchi2", "V5"="w2mpro",
                                             "V6"="w2sigmpro", "V7"="w2snr", "V8"="w2rchi2", "V9"="w3mpro", "V10"="w3sigmpro",
                                             "V11"="w3snr", "V12"="w3rchi2", "V13"="w4mpro", "V14"="w4sigmpro", "V15"="w4snr",
                                             "V16"="w4rchi2"))

allwise_dataset$object_name <- sapply(allwise_dataset$object_name, function(x) toString(x)) #Change object names to string type to parse
allwise_dataset$object_name <- sapply(allwise_dataset$object_name, function(x) strsplit(x, " ")[[1]][1]) #Remove integer sub-identifiers from objects

#OBJECTIVE: Find highest count categories so we can get these categories only to perform analysis on
unique(allwise_dataset$object_name) #get list of unique object identifiers

#Get unique datasets comprised of TYC, HD, and UCAC2
allwise_subset_tyc <- allwise_dataset[allwise_dataset$object_name == "TYC",]
allwise_subset_hd <- allwise_dataset[allwise_dataset$object_name == "HD",]
allwise_subset_ucac2 <- allwise_dataset[allwise_dataset$object_name == "UCAC2",]

#UPSAMPLING: Sample with replacement to get equal number of HD and UCAC2 as TYC
hd_sample <- allwise_subset_hd[sample(nrow(allwise_subset_hd), replace = TRUE, 7407), ]
ucac2_sample <- allwise_subset_ucac2[sample(nrow(allwise_subset_ucac2), replace = TRUE, 7407), ]
#Create new dataset by stacking TYC, HD, and UCAC2 on top of each other
bind1 <- rbind(allwise_subset_tyc, hd_sample)
allwise_subset <- rbind(bind1, ucac2_sample)
allwise_subset <- allwise_subset[sample(1:nrow(allwise_subset)),] #shuffle this new dataset

#Add training, test, and validation labels to the dataaset
coff <- runif(nrow(allwise_subset))
allwise_subset$train_id <- "valid"
allwise_subset$train_id[coff < .6] <- "train"
allwise_subset$train_id[coff > .8] <- "test"

#We now have a table with labels - save as CSV file
write.csv(allwise_subset, "C:/Users/Joe/Documents/Astro_Research_2017/allwise_subset.csv")

# Get a count of NA values
allwise_subset[allwise_subset == "--"] <- NA
for(i in colnames(allwise_subset))
{
  if(i != "object_name" && i != "train_id")
  {
    print(i)
    print(sum(is.na(allwise_subset[,i]))) 
  }
}

allwise_subset$w4sigmpro <- NULL #delete

#convert the columns to numeric.  they are currently character
allwise_subset$w3sigmpro <- as.numeric(allwise_subset$w3sigmpro)
allwise_subset$w2sigmpro <- as.numeric(allwise_subset$w2sigmpro)
allwise_subset$w1sigmpro <- as.numeric(allwise_subset$w1sigmpro)

#replace NA values with mean for each row
allwise_subset$w3sigmpro[is.na(allwise_subset$w3sigmpro)] <- mean(allwise_subset$w3sigmpro, na.rm = TRUE)
allwise_subset$w2sigmpro[is.na(allwise_subset$w2sigmpro)] <- mean(allwise_subset$w2sigmpro, na.rm = TRUE)
allwise_subset$w1sigmpro[is.na(allwise_subset$w1sigmpro)] <- mean(allwise_subset$w2sigmpro, na.rm = TRUE)

############
# Graphing #
############

qplot(ra, dec, data=allwise_subset, color = factor(object_name), main = "Visualization of Stars in RA and DEC Space", ylab = "Declination", xlab = "Right Ascension")
qplot(w1mpro, w1snr, data=allwise_subset, color = factor(object_name), main = "W1 Magnitude vs. W1 SNR")
qplot(w2mpro, w2snr, data=allwise_subset, color = factor(object_name), main = "W2 Magnitude vs. W2 SNR")
qplot(w3mpro, w3snr, data=allwise_subset, color = factor(object_name), main = "W3 Magnitude vs. W3 SNR")
qplot(w4mpro, w4snr, data=allwise_subset, color = factor(object_name), main = "W4 Magnitude vs. W4 SNR")

##################
# Regularization #
##################

library(glmnet)

#first, make a column of numerical values representing classes
allwise_subset$sclass <- factor(allwise_subset$object_name, labels=(1:length(levels(factor(allwise_subset$object_name)))))
allwise_subset$sclass <- as.numeric(allwise_subset$sclass)

X <- as.matrix(allwise_subset[,4:18])
y <- allwise_subset$sclass

model <- cv.glmnet(X, y, alpha = .9)
coef(model)

#########################
# Gradient Boosted Tree #
#########################

library(xgboost)
X <- model.matrix(~ . - 1, data = allwise_subset[,4:18])
y <- allwise_subset$sclass

X_train <- X[allwise_subset$train_id == "train",]
y_train <- y[allwise_subset$train_id == "train"]
X_valid <- X[allwise_subset$train_id == "valid",]
y_valid <- y[allwise_subset$train_id == "valid"]

data_train <- xgb.DMatrix(data = X_train, label = y_train)
data_valid <- xgb.DMatrix(data = X_valid, label = y_valid)
watchlist <- list(train=data_train, valid=data_valid)

model <- xgb.train(data = data_train,
                   max_depth = 3, eta = 0.4, nthread = 4,
                   nrounds = 1000, objective = "multi:softmax",
                   num_class = 4, watchlist = watchlist,
                   print_every_n = 100)

allwise_subset$object_pred <- predict(model, newdata = X)
tapply(allwise_subset$object_pred == allwise_subset$sclass,
       allwise_subset$train_id, mean)

###################################
# Multinomial Logistic Regression #
###################################

library(nnet)

# MODEL 1
model <- multinom(sclass ~ poly(w1mpro, w1sigmpro, w2mpro, w2sigmpro, degree = 3), data = allwise_subset)
allwise_subset$object_pred <- predict(model, newdata = allwise_subset)
tapply(allwise_subset$object_pred == allwise_subset$sclass, allwise_subset$train_id, mean)

# MODEL 2
model <- multinom(sclass ~ poly(w1mpro, w1sigmpro, w1snr, w1rchi2, w2mpro, w2sigmpro, w2snr, w2rchi2, w3mpro, w3sigmpro, w3rchi2, w4mpro, w4snr, degree = 3), data = allwise_subset, MaxNWts = 1683)
allwise_subset$object_pred <- predict(model, newdata = allwise_subset)
tapply(allwise_subset$object_pred == allwise_subset$sclass, allwise_subset$train_id, mean)

# confusion matrix
table(y = allwise_subset$sclass, y_pred = allwise_subset$object_pred)

#######################
# K-Nearest Neighbors #
#######################

library(FNN)

X <- as.matrix(allwise_subset[,4:18])
y <- allwise_subset$sclass
X_train <- X[allwise_subset$train_id == "train",]
y_train <- y[allwise_subset$train_id == "train"]

allwise_subset$object_pred <- knn(train = X_train, test = X,
                                  cl = y_train, k = 2)
tapply(allwise_subset$object_pred == allwise_subset$sclass, allwise_subset$train_id, mean)

# graph optimal k
rmse <- rep(NA, 25)
for (k in seq_along(rmse)) {
  y_valid_pred <- knn.reg(train = X_train, y = y_train,
                          test = X_valid, k = k)$pred
  rmse[k] <- sqrt( mean((y_valid_pred - y_valid)^2) )
}

qplot(seq_along(rmse), rmse) +
  geom_line() +
  theme_minimal()

###############
# Elastic Net #
###############

X <- as.matrix(allwise_subset[,4:18])
y <- allwise_subset$sclass
X_train <- X[allwise_subset$train_id == "train",]
y_train <- y[allwise_subset$train_id == "train"]

model <- cv.glmnet(X_train, y_train, family = "multinomial")
allwise_subset$object_pred <- predict(model, newx = X, type="class", alpha=.2)

tapply(allwise_subset$object_pred == allwise_subset$sclass, allwise_subset$train_id, mean)

##################
# Neural Network #
##################

X <- as.matrix(allwise_subset[,4:18])
y <- allwise_subset$sclass - 1
X_train <- X[allwise_subset$train_id == "train",]
y_train <- to_categorical(y[allwise_subset$train_id == "train"], num_classes = 3)

model <- keras_model_sequential()
model %>%
  
  layer_dense(units = 128, 
              input_shape = c(15)) %>%
  layer_activation(activation = "selu") %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 3) %>%
  layer_activation(activation = "softmax")

model %>% compile(loss = 'categorical_crossentropy',
                  optimizer = optimizer_sgd(lr = 0.00005, momentum = .9,
                                            nesterov = TRUE),
                  metrics = c('accuracy'))

history <- model %>%
  fit(X_train, y_train, epochs = 10, validation_split = 0.2)

allwise_subset$object_pred <- predict_classes(model,X) + 1
tapply(allwise_subset$object_pred == allwise_subset$sclass, allwise_subset$train_id, mean)

