# start run timer
start <- Sys.time()


#libraries
#install.packages('ISLR')
#install.packages('caTools')
#install.packages('neuralnet')
library(ISLR)
library(caTools)
library(neuralnet)

#read in data
nslkdd_training_set <- read.csv("NSLKDD/KDDtrain_20Percent_withCols.csv", fileEncoding="UTF-8-BOM")

#change data types
nslkdd_training_set$protocol_type <- as.integer(as.numeric(nslkdd_training_set$protocol_type))
nslkdd_training_set$service <- as.integer(as.numeric(nslkdd_training_set$service))
nslkdd_training_set$attack_type <- ifelse(nslkdd_training_set$attack_type == "normal", 0, 1)


#scale data
maxs <- apply(nslkdd_training_set[,1:8], 2, max)
mins <- apply(nslkdd_training_set[,1:8], 2, min)
scaled_data <- as.data.frame(scale(nslkdd_training_set[,1:8], center = mins, scale = maxs - mins))

#set nslkdd_training_set$attack_type to variable Attack so we can reference it when we build the formula later
Attack <- as.integer(nslkdd_training_set$attack_type)
data <- cbind(Attack, scaled_data)

#Start with a smaller sample specified by sample_size
sample_size <- 1000
index <- sample(nrow(data), size = sample_size, replace = FALSE)
data <- data[index,]

#sample a training and testing set
sample_size <- floor(0.7 * nrow(data))
training_index <- sample(nrow(data), size = sample_size, replace = FALSE)
train <- data[training_index,]
test <- data[-training_index,]

#set up formula for neuralnet() function
feats <- names(scaled_data)
f <- paste(feats, collapse = '+')
f <- paste('Attack ~', f)
f <- as.formula(f)

#create the neural net using training set
nn <- neuralnet(f, train, hidden = c(5,5), stepmax = 1e+06, linear.output = FALSE)

#predict Output variable using nn with the test set
predicted_nn <- compute(nn,test[2:9])
predicted_nn$net.result <- sapply(predicted_nn$net.result, round, digits = 0)

#confusion matrix
table(test$Attack, predicted_nn$net.result)
plot(nn)

#stop timer and print run time
end <- Sys.time()
end - start
