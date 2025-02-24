
data <- read.csv("/Users/kozzze/Desktop/Учеба/BigData/BigData/L2/cars.csv", header=TRUE, stringsAsFactors=FALSE)

data_numeric <- data[, -c(1,2)]

data_numeric[] <- lapply(data_numeric, as.numeric)

max_values <- apply(data_numeric, 2, max, na.rm=TRUE)
min_values <- apply(data_numeric, 2, min, na.rm=TRUE)
mean_values <- apply(data_numeric, 2, mean, na.rm=TRUE)

result <- data.frame(Max=max_values, Min=min_values, Mean=mean_values)
print(result)

#------------------------------------------------------------

count_above_7 <- colSums(data_numeric > 7, na.rm=TRUE)
count_below_3 <- colSums(data_numeric < 3, na.rm=TRUE)

preference_counts <- data.frame(More7 = count_above_7, Low3 = count_below_03)

print(preference_counts)

#------------------------------------------------------------

ranking <- sort(mean_values, decreasing=TRUE)

print(ranking)

#------------------------------------------------------------

barplot(mean_values, main="Средние оценки", col="yellow", las=2)

barplot(sort(min_values, decreasing=TRUE), main="Рейтинг автомобилей", col="red", las=2)

barplot(max_values, horiz=TRUE, main="Средние оценки автомобилей", col="green", las=1)