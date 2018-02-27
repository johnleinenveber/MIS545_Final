# rbind csv files
setwd("/Users/JohnLeinenveber/Documents/MIS545/FinalProject")

start_time <- Sys.time()
NAstrings = c("NaN", "Infinity")

file1 <- read.csv("CSVs/1Monday.csv", na.strings = NAstrings)
file2 <- read.csv("CSVs/2Tuesday.csv", na.strings = NAstrings)
file3 <- read.csv("CSVs/3Wednesday.csv", na.strings = NAstrings)
file4 <- read.csv("CSVs/4ThursdayA-WebAttacks.csv", na.strings = NAstrings)
file5 <- read.csv("CSVs/5ThursdayB-Infilteration.csv", na.strings = NAstrings)
file6 <- read.csv("CSVs/6FridayA.csv", na.strings = NAstrings)
file7 <- read.csv("CSVs/7FridayB-PortScan.csv", na.strings = NAstrings)
file8 <- read.csv("CSVs/8FridayC-DDoS.csv", na.strings = NAstrings)
file8 <- subset(file8, -c("External IP"))

allData <- rbind(file1, file2, file3, file4, file5, file6, file7, file8)
rm(file1, file2, file3, file4, file5, file6, file7, file8)

#head(allData)
#nrow(allData)

end_time <- Sys.time()
end_time - start_time
