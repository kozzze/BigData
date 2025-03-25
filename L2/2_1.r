
data <- read.csv("/Users/kozzze/Desktop/Учеба/BigData/BigData/L2/cars.csv", header=TRUE, stringsAsFactors=FALSE)

data_numeric <- data[, -c(1,2)]

data_numeric[] <- lapply(data_numeric, as.numeric)
print(data_numeric)

max_values <- apply(data_numeric, 2, max, na.rm=TRUE)
min_values <- apply(data_numeric, 2, min, na.rm=TRUE)
mean_values <- apply(data_numeric, 2, mean, na.rm=TRUE)

result <- data.frame(Max=max_values, Min=min_values, Mean=mean_values)
print(result)

#------------------------------------------------------------

count_above_7 <- colSums(data_numeric > 7, na.rm=TRUE)
count_below_3 <- colSums(data_numeric < 3, na.rm=TRUE)

preference_counts <- data.frame(More7 = count_above_7, Low3 = count_below_3)

print(preference_counts)

#------------------------------------------------------------

ranking <- sort(mean_values, decreasing=TRUE)

print(ranking)

#------------------------------------------------------------

barplot(mean_values, main="Средние оценки", col="yellow", las=2)

barplot(sort(mean_values, decreasing=TRUE), main="Рейтинг автомобилей", col="red", las=2)

barplot(max_values, horiz=TRUE, main="Средние оценки автомобилей", col="green", las=1)

#____________________________________________________________
# 2_2 найти всех, кто поставил больше 0.7 определённой машине и создать из них новую таблицу.

selected_car <- "E63"
subdataset <- data_numeric[data_numeric[, selected_car] > 7, ]
print("Новая таблица")
print(subdataset)
dim(subdataset)

#_____________________________________________________

# Гистограмма распределения оценок для выбранного автомобиля (E63)
hist(subdataset$E63, main="Гистограмма оценок E63", col="blue", breaks=5)

# Boxplot (ящиковая диаграмма) для всех машин в новом поднаборе
boxplot(subdataset, main="Boxplot оценок", col="orange", las=2)

# Вычисляем средние значения, медианы и стандартные отклонения
mean_values_sub <- apply(subdataset, 2, mean, na.rm=TRUE)
median_values_sub <- apply(subdataset, 2, median, na.rm=TRUE)
sd_values_sub <- apply(subdataset, 2, sd, na.rm=TRUE)
IQR()
# Создаём таблицу с результатами
result_sub <- data.frame(Среднее=mean_values_sub, Медиана=median_values_sub, Отклонение=sd_values_sub,Минимум=min_values,Максимум=max_values)
print(result_sub)