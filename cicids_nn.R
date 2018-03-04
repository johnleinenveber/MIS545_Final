# start run timer
start <- Sys.time()

#libraries
#install.packages('ISLR')
#install.packages('caTools')
#install.packages('neuralnet')
#install.packages("mice")
library(ISLR)
library(caTools)
library(neuralnet)
library(mice)

#read in data
NAstrings = c("NaN", "Infinity")
file1 <- read.csv("CICIDS/1Monday.csv", na.strings = NAstrings)
file2 <- read.csv("CICIDS/2Tuesday.csv", na.strings = NAstrings)
file3 <- read.csv("CICIDS/3Wednesday.csv", na.strings = NAstrings)
#file4 <- read.csv("CICIDS/4ThursdayA-WebAttacks.csv", na.strings = NAstrings)
file5 <- read.csv("CICIDS/5ThursdayB-Infilteration.csv", na.strings = NAstrings)
file6 <- read.csv("CICIDS/6FridayA.csv", na.strings = NAstrings)
file7 <- read.csv("CICIDS/7FridayB-PortScan.csv", na.strings = NAstrings)
file8 <- read.csv("CICIDS/8FridayC-DDoS.csv", na.strings = NAstrings)

#the last csv file has one extra column that we need to remove before cbind()
file8 <- subset(file8, select = -c(External.IP))

data <- rbind(file1, file2, file3, file5, file6, file7, file8)
rm(file1, file2, file3,file5, file6, file7, file8)

data = subset(data, select = c(Protocol, Flow.Duration, Total.Fwd.Packets, Total.Backward.Packets, Total.Length.of.Fwd.Packets,
Total.Length.of.Bwd.Packets, Bwd.Header.Length, Fwd.Header.Length, Subflow.Fwd.Packets, Subflow.Fwd.Bytes,
Subflow.Bwd.Packets, Subflow.Bwd.Bytes, act_data_pkt_fwd, Label))

data$Protocol <- as.numeric(as.factor(data$Protocol))

#interested in knowing if the network traffic is normal or not.
data$Label <- ifelse(data$Label == "BENIGN", 0, 1)

#missing and negative values
nrow(data[!complete.cases(data),])
data[data < 0] <- NA
data <- data[complete.cases(data),]

#quickSave <- data
#data <- quickSave

#scale data
maxs <- apply(data[,1:13], 2, max)
mins <- apply(data[,1:13], 2, min)
scaled_data <- as.data.frame(scale(data[,1:13], center = mins, scale = maxs - mins))

#set data$Label to variable Label so we can reference it when we build the formula later. cbind to front of scaled_data
Label <- data$Label
data <- cbind(Label, scaled_data[,1:13])
#rm(scaled_data)

#Start with a smaller sample specified by sample_size
sample_size = 1000
index <- sample(nrow(data), size = sample_size, replace = FALSE)
sampledData <- data[index,]

#sample a training and testing set
sample_size <- floor(0.7 * nrow(sampledData))
training_index <- sample(nrow(sampledData), size = sample_size, replace = FALSE)
train <- sampledData[training_index,]
test <- sampledData[-training_index,]

#set up formula for neuralnet() function
feats <- names(data[,2:14])
f <- paste(feats, collapse = '+')
f <- paste('Label ~', f)
f <- as.formula(f)

#clear some space in memory
#rm(data, scaled_data, Label, NAstrings, maxs, mins, index, feats)
#create the neural net using training set
nn <- neuralnet(f, train, hidden = c(5, 5), stepmax = 1e+06,linear.output = FALSE)

#predict Output variable using nn with the test set
predicted_nn <- compute(nn,test[2:14])
predicted_nn$net.result <- sapply(predicted_nn$net.result, round, digits = 0)

#confusion matrix
results <- data.frame(actual = test$Label, predicted = predicted_nn$net.result)
table(results)

plot(nn)

#TPR
TPR <- sum(test$Label == 0 & predicted_nn$net.result == 0) / sum(test$Label == 0)
TNR <- sum(test$Label == 1 & predicted_nn$net.result == 1) / sum(test$Label == 1)
FPR <- 1 - TNR
FNR <- 1 - TPR

precision <- sum(test$Label == 0 & predicted_nn$net.result == 0) / sum(predicted_nn$net.result == 0)
recall <- TPR

F <- 2 * precision * recall / (precision + recall)

precision
recall
F

#stop timer and print run time
end <- Sys.time()
end - start
