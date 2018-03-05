# start run timer
start <- Sys.time()

#set working directory
#setwd("C:/Users/Owner/Documents/MIS545/FinalProject")
setwd("Documents/MIS545/FinalProject")

#install.packages("e1071")
#install.packages("rpart")
library(e1071)
library(rpart)

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

#select input variables and output variable
data = subset(data, select = c(Protocol, Flow.Duration, Total.Fwd.Packets, Total.Backward.Packets,
                               Bwd.Header.Length, Fwd.Header.Length, Subflow.Fwd.Packets,
                               Subflow.Bwd.Packets, act_data_pkt_fwd, Label))

#only take malicious network traffic
#data$Label <- ifelse(data$Label == "BENIGN", "BENIGN", "ATTACK")
data$Label <- as.numeric(data$Label)

#only select attacks
data <- data[data$Label != 1,]

#missing and negative values
nrow(data[!complete.cases(data),])
data[data < 0] <- NA
data <- data[complete.cases(data),]

data$Protocol <- as.numeric(as.factor(data$Protocol))
data$Label <- as.factor(data$Label)

#set data$Label to variable Label so we can reference it when we build the formula later. cbind to front of scaled_data
#Protocol <- data$Protocol
Label <- data$Label
data <- cbind(Label, data[,2:9])
#rm(scaled_data)

#Start with a smaller sample specified by sample_size
sample_size = 100000
index <- sample(nrow(data), size = sample_size, replace = FALSE)
sampledData <- data[index,]

#sample a training and testing set
sample_size <- floor(0.7 * nrow(sampledData))
training_index <- sample(nrow(sampledData), size = sample_size, replace = FALSE)
train <- sampledData[training_index,]
test <- sampledData[-training_index,]

svm_model <- svm(Label ~ ., data = train)
svm_predict <- predict(svm_model, newdata = test)
results <- data.frame(actual = test$Label, predicted = svm_predict)
table(results)


#TPR
TPR_5 <- sum(test$Label == 5 & svm_predict == 5) / sum(test$Label == 5)
TNR_5 <- sum(test$Label != 5 & svm_predict != 5) / sum(test$Label != 5)
FPR_5 <- 1 - TNR_5
FNR_5 <- 1 - TPR_5

TPR_11 <- sum(test$Label == 11 & svm_predict == 11) / sum(test$Label == 11)
TNR_11 <- sum(test$Label != 11 & svm_predict != 11) / sum(test$Label != 11)
FPR_11 <- 1 - TNR_11
FNR_11 <- 1 - TPR_11

precision_5 <- sum(test$Label == 5 & svm_predict == 5) / sum(svm_predict == 5)
precision_11 <- sum(test$Label == 11 & svm_predict == 11) / sum(svm_predict == 11)

recall_5 <- TPR_5
recall_11 <- TPR_11

F_5 <- 2 * precision_5 * recall_5 / (precision_5 + recall_5)
F_11 <- 2 * precision_11 * recall_11 / (precision_11 + recall_11)

accuracy_5 <- (TPR_5 + TNR_5) / (TPR_5 + TNR_5 + FPR_5 + FNR_5)
accuracy_11 <- (TPR_11 + TNR_11) / (TPR_11 + TNR_11 + FPR_11 + FNR_11)



#stop timer and print run time
end <- Sys.time()
end - start



