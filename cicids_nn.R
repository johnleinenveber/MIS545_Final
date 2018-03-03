#work in progress

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

data$Flow.ID <- as.numeric(data$Flow.ID)
data$Source.IP <- as.numeric(data$Source.IP)
data$Source.Port <- as.numeric(data$Source.Port)
data$Destination.IP <- as.numeric(data$Destination.IP)
data$Destination.Port <- as.numeric(data$Destination.Port)

#Columns only have value of zero.
"
data = subset(data, select = -c(Timestamp, Fwd.PSH.Flags, Bwd.PSH.Flags, Fwd.URG.Flags, Bwd.URG.Flags, CWE.Flag.Count,
Fwd.Avg.Bytes.Bulk, Fwd.Avg.Packets.Bulk, Fwd.Avg.Bulk.Rate, Bwd.Avg.Bytes.Bulk,
Bwd.Avg.Packets.Bulk, Bwd.Avg.Bulk.Rate, Init_Win_bytes_backward, Init_Win_bytes_forward,
Active.Mean, Active.Std, Active.Max, Active.Min, Idle.Mean, Idle.Std, Idle.Max, Idle.Min))
"
data = subset(data, select = c(Protocol, Flow.Duration, Total.Fwd.Packets, Total.Backward.Packets, Total.Length.of.Fwd.Packets,
                               Total.Length.of.Bwd.Packets, Bwd.Header.Length, Fwd.Header.Length, Subflow.Fwd.Packets, Subflow.Fwd.Bytes,
                               Subflow.Bwd.Packets, Subflow.Bwd.Bytes, act_data_pkt_fwd, Label))

#interested in knowing if the network traffic is normal or not.
data$Label <- ifelse(data$Label == "BENIGN", 0, 1)

#missing and negative values
data[data == -1 ] <- NA
nrow(data[!complete.cases(data),])
nrow(data[complete.cases(data),])
data <- data[complete.cases(data),]

#quickSave <- data
#data <- quickSave

#scale data
maxs <- apply(data[,1:62], 2, max)
mins <- apply(data[,1:62], 2, min)
scaled_data <- as.data.frame(scale(data[,1:62], center = mins, scale = maxs - mins))

#set data$Label to variable Label so we can reference it when we build the formula later. cbind to front of scaled_data
Label <- data$Label
data <- cbind(Label, scaled_data[,1:62])
rm(scaled_data)

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
feats <- names(data[,2:63])
f <- paste(feats, collapse = '+')
f <- paste('Label ~', f)
f <- as.formula(f)

#create the neural net using training set
nn <- neuralnet(f, train, hidden = 31, stepmax = 1e+06,linear.output = FALSE)

#predict Output variable using nn with the test set
predicted_nn <- compute(nn,test[2:63])
predicted_nn$net.result <- sapply(predicted_nn$net.result, round, digits = 0)

#confusion matrix
confusionMatrix <- table(test$Label, predicted_nn$net.result)
confusionMatrix
plot(nn)

fileConn <- file("output.txt")
writeLines(table(test$Label, predicted_nn$net.result), fileConn)
close(fileConn)

#stop timer and print run time
end <- Sys.time()
end - start
