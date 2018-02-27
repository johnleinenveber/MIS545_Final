setwd("Documents/MIS545/FinalProject")

#install.packages('ISLR')
#install.packages('caTools')
#install.packages('neuralnet')
library(ISLR)
library(caTools)
library(neuralnet)

nslkdd_training_set <- read.csv("NSLKDD/KDDtrain_20Percent_withCols.csv")

nslkdd_training_set$protocol_type <- as.numeric(nslkdd_training_set$protocol_type)
nslkdd_training_set$service <- as.numeric(nslkdd_training_set$service)
nslkdd_training_set$flag <- as.numeric(nslkdd_training_set$flag)
nslkdd_training_set$attack_type <- as.numeric(nslkdd_training_set$attack_type)

nslkdd_training_set$src_bytes <- as.numeric(nslkdd_training_set$src_bytes)
nslkdd_training_set$dst_bytes <- as.numeric(nslkdd_training_set$dst_bytes)
nslkdd_training_set$count <- as.numeric(nslkdd_training_set$count)
nslkdd_training_set$srv_count <- as.numeric(nslkdd_training_set$srv_count)
nslkdd_training_set$dst_host_count <- as.numeric(nslkdd_training_set$dst_host_count)
nslkdd_training_set$dst_host_srv_count <- as.numeric(nslkdd_training_set$dst_host_srv_count)

maxs <- apply(nslkdd_training_set[,1:24], 2, max)
mins <- apply(nslkdd_training_set[,1:24], 2, min)

scaled_data <- as.data.frame(scale(nslkdd_training_set[,1:24], center = mins, scale = maxs - mins))
data <- cbind(scaled_data, nslkdd_training_set$attack_type)

sample_size <- floor(0.7 * nrow(data))
training_index <- sample(nrow(data), size = sample_size, replace = FALSE)
train <- data[training_index,]
test <- data[-training_index,]

feats <- names(scaled_data)
f <- paste(feats, collapse = '+')
f <- paste('nslkdd_training_set$attack_type ~', f)
f <- as.formula(f)

nn <- neuralnet(f, train, linear.output = FALSE)

predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result, round, digits = 0)

table(test$Private, predicted.nn.values$net.result)


neuralnetwork <- neuralnet(f, train, hidden = c(10,10,10), linear.output = FALSE)








