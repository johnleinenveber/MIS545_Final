
#install.packages('ISLR')
#install.packages('caTools')
#install.packages('neuralnet')
library(ISLR)
library(caTools)
library(neuralnet)

start <- Sys.time()

nslkdd_training_set <- read.csv("NSLKDD/KDDtrain_20Percent_withCols.csv")

nslkdd_training_set$protocol_type <- as.integer(as.numeric(nslkdd_training_set$protocol_type))
nslkdd_training_set$service <- as.integer(as.numeric(nslkdd_training_set$service))
nslkdd_training_set$attack_type <- ifelse(nslkdd_training_set$attack_type == "normal", 0, 1)

maxs <- apply(nslkdd_training_set[,1:8], 2, max)
mins <- apply(nslkdd_training_set[,1:8], 2, min)
scaled_data <- as.data.frame(scale(nslkdd_training_set[,1:8], center = mins, scale = maxs - mins))

Attack <- as.integer(nslkdd_training_set$attack_type)
data <- cbind(Attack, scaled_data)

sample_size <- 10000
index <- sample(nrow(data), size = sample_size, replace = FALSE)
data <- data[index,]

sample_size <- floor(0.7 * nrow(data))
training_index <- sample(nrow(data), size = sample_size, replace = FALSE)
train <- data[training_index,]
test <- data[-training_index,]

feats <- names(scaled_data)
f <- paste(feats, collapse = '+')
f <- paste('Attack ~', f)
f <- as.formula(f)

nn <- neuralnet(f, train, hidden = c(5,5,5), linear.output = FALSE)

predicted_nn <- compute(nn,test[2:9])
predicted_nn$net.result <- sapply(predicted_nn$net.result, round, digits = 0)
table(test$Attack,predicted_nn$net.result)
plot(nn)

end <- Sys.time()
end - start








