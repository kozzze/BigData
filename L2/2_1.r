
data <- read.csv("/Users/kozzze/Desktop/Учеба/BigData/BigData/L2/cars.csv", header=TRUE, stringsAsFactors=FALSE)

data_numeric <- data[, -c(1,2)]

data_numeric[] <- lapply(data_numeric, as.numeric)

max_values <- apply(data_numeric, 2, max, na.rm=TRUE)
min_values <- apply(data_numeric, 2, min, na.rm=TRUE)
mean_values <- apply(data_numeric, 2, mean, na.rm=TRUE)

result <- data.frame(Max=max_values, Min=min_values, Mean=mean_values)
print(result)