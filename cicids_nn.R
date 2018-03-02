# start run timer
start <- Sys.time()

#set working directory
setwd("C:/Users/Owner/Documents/MIS545/FinalProject")

#libraries
#install.packages('ISLR')
#install.packages('caTools')
#install.packages('neuralnet')
library(ISLR)
library(caTools)
library(neuralnet)

#read in data
NAstrings = c("NaN", "Infinity", "")
file1 <- read.csv("CICIDS/1Monday.csv", na.strings = NAstrings)
file2 <- read.csv("CICIDS/2Tuesday.csv", na.strings = NAstrings)
file3 <- read.csv("CICIDS/3Wednesday.csv", na.strings = NAstrings)
file4 <- read.csv("CICIDS/4ThursdayA-WebAttacks.csv", na.strings = NAstrings)
file5 <- read.csv("CICIDS/5ThursdayB-Infilteration.csv", na.strings = NAstrings)
file6 <- read.csv("CICIDS/6FridayA.csv", na.strings = NAstrings)
file7 <- read.csv("CICIDS/7FridayB-PortScan.csv", na.strings = NAstrings)
file8 <- read.csv("CICIDS/8FridayC-DDoS.csv", na.strings = NAstrings)

#the last csv file has one extra column that we need to remove before cbind()
file8 <- subset(file8, select = -c(External.IP))

data <- rbind(file1, file2, file3, file4, file5, file6, file7, file8)
#rm(file1, file2, file3, file4, file5, file6, file7, file8)

data$Flow.ID <- as.numeric(data$Flow.ID)
data$Source.IP <- as.numeric(data$Source.IP)
data$Source.Port <- as.numeric(data$Source.Port)
data$Destination.IP <- as.numeric(data$Destination.IP)
data$Destination.Port <- as.numeric(data$Destination.Port)
data$Label <- as.numeric(data$Label)


#Columns only have value of zero.
data = subset(data, select = -c(Timestamp, Fwd.PSH.Flags, Bwd.PSH.Flags, Fwd.URG.Flags, Bwd.URG.Flags, CWE.Flag.Count,
                             Fwd.Avg.Bytes.Bulk, Fwd.Avg.Packets.Bulk, Fwd.Avg.Bulk.Rate, Bwd.Avg.Bytes.Bulk,
                             Bwd.Avg.Packets.Bulk, Bwd.Avg.Bulk.Rate, Init_Win_bytes_backward, Init_Win_bytes_forward,
                             Active.Mean, Active.Std, Active.Max, Active.Min, Idle.Mean, Idle.Std, Idle.Max, Idle.Min))



#missing values
nrow(data[!complete.cases(data),]) / nrow(data)
data <- data[complete.cases(data),]




#scale data
maxs <- apply(data[,1:16], 2, max)
mins <- apply(data[,1:16], 2, min)
scaled_data <- as.data.frame(scale(data[,1:16], center = mins, scale = maxs - mins))

#set data$Label to variable Label so we can reference it when we build the formula later. cbind to front of scaled_data
Label <- data$Label - 1
data <- cbind(Label, scaled_data)

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
f <- paste('Label ~', f)
f <- as.formula(f)

#create the neural net using training set
nn <- neuralnet(f, train, hidden = 8, stepmax = 1e+06,linear.output = FALSE)

#predict Output variable using nn with the test set
predicted_nn <- compute(nn,test[2:17])
predicted_nn$net.result <- sapply(predicted_nn$net.result, round, digits = 0)

#confusion matrix
table(test$Label, predicted_nn$net.result)
plot(nn)

#stop timer and print run time
end <- Sys.time()
end - start
