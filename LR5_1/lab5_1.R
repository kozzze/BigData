data <- read.csv("/Users/kozzze/Desktop/Учеба/BigData/BigData/LR5_1/20_Покупатели магазина/Customers.csv")

colnames(data)
numeric_data <- data[, c("Age", "Annual.Income....", "Spending.Score..1.100.", 
                         "Work.Experience", "Family.Size")]
boxplot(data$Age,
        data$Annual.Income..,
        data$Spending.Score..1.100.,
        data$Work.Experience,
        data$Family.Size,
        names = c("Age", "Income", "Score", "Experience", "Family Size"),
        main = "Boxplot по числовым признакам",
        col = "lightgray")

#гаусовская нормализация
normalized <- scale(data[, c("Age", "Annual.Income....", "Spending.Score..1.100.", 
                             "Work.Experience", "Family.Size")])

boxplot(normalized, 
        main = "Boxplot (нормализованные данные)", 
        col = "lightgray", 
        names = c("Age", "Income", "Score", "Experience", "Family Size"))

#dataframe с данными дескриптивного анализа
desc <- data.frame(
  Mean = sapply(numeric_data, mean),
  Median = sapply(numeric_data, median),
  Min = sapply(numeric_data, min),
  Max = sapply(numeric_data, max),
  SD = sapply(numeric_data, sd),
  Q1 = sapply(numeric_data, quantile, probs = 0.25),
  Q3 = sapply(numeric_data, quantile, probs = 0.75)
)
round(desc, 2)

#3

library(factoextra)
fviz_nbclust(normalized, kmeans, method = "wss") +
  labs(subtitle = "Метод локтя")

#метод силуэта
fviz_nbclust(normalized, kmeans, method = "silhouette") +
  labs(subtitle = "Метод силуэта")

#разрыв
gap_stat <- cluster::clusGap(normalized, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

#консенсус‑анализ
library(NbClust)
nb <- NbClust(data = normalized, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")
fviz_nbclust(nb) + labs(subtitle = "Консенсус-анализ")


#4

dist_mat <- dist(normalized)
hc <- hclust(dist_mat, method="ward.D2")
plot(hc, main="Дендрограмма")
rect.hclust(hc, k=3, border="red")
groups_h <- cutree(hc, k=3)
data$cluster_h <- groups_h

plot(hc,
     horiz = TRUE,       
     labels = FALSE,
     xlim   = c(0, 40))    
rect.hclust(hc, k = 3, border = "red", horiz = TRUE)

#5

means_h <- aggregate(
  data[, c("Age",
           "Annual.Income....",
           "Spending.Score..1.100.",
           "Work.Experience",
           "Family.Size")],
  by = list(cluster = data$cluster_h),
  FUN = mean
)

means_h$Annual.Income.... <- means_h$Annual.Income.... / 1000

m2 <- t(as.matrix(means_h[ , -1]))
rownames(m2) <- c("Age","Income (k$)","Score","Experience","Family Size")
colnames(m2) <- c("g1","g2","g3")

barplot(
  m2,
  beside = TRUE,
  names.arg = colnames(m2),
  col = c("tomato","skyblue","seagreen","gold","violet"),
  legend.text= rownames(m2),
  args.legend= list(x = "top", bty = "n", inset = c(0, -0.15)),
  xlab = "Кластер",
  ylab = "Среднее значение"
)
par(mfrow = c(2,3), mar = c(4,4,2,1))
boxplot(Age ~ cluster_h, data = data, main = "Age")
boxplot(Annual.Income.... ~ cluster_h, data = data, main = "Income")
boxplot(Spending.Score..1.100. ~ cluster_h, data = data, main = "Score")
boxplot(Work.Experience ~ cluster_h, data = data, main = "Experience")
boxplot(Family.Size ~ cluster_h, data = data, main = "Family Size")
par(mfrow = c(1,1))


#6

set.seed(123)
km.res <- kmeans(normalized, centers = 3, nstart = 25)
data$cluster_km <- km.res$cluster
library(factoextra)
fviz_cluster(km.res,
             data = normalized,
             geom = "point",
             ellipse.type = "norm",
             main = "k_means k=3")

#7

cols <- c("tomato","skyblue","seagreen")[data$cluster_h]
pairs(
  data[, c("Age",
           "Annual.Income....",
           "Spending.Score..1.100.",
           "Work.Experience",
           "Family.Size")],
  pch = 19,
  col = cols,
  main = "Scatterplot"
)


#8

library(scatterplot3d)

cols3d <- c("tomato","skyblue","seagreen")[data$cluster_h]

s3d <- scatterplot3d(
  x = data$Age,                       
  y = data$Annual.Income....,        
  z = data$Spending.Score..1.100.,   
  color = cols3d,                         
  pch = 16,                            
  xlab = "Age",
  ylab = "Annual Income",
  zlab = "Spending Score",
  main = "3D кластеризация покупателей",
  angle = 55,                             
  scale.y = 1                                
)

legend("topright",
       legend = c("Cluster 1","Cluster 2","Cluster 3"),
       col = c("tomato","skyblue","seagreen"),
       pch = 16,
       inset = 0.05)


#-----------5_2--------



data$cluster_km <- as.factor(data$cluster_km)

set.seed(1234)  
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
trainData <- data[ind == 1, ]
testData <- data[ind == 2, ]
  
library(e1071)
model_nb <- naiveBayes(cluster_km ~ Age + Annual.Income.... + Spending.Score..1.100. + Work.Experience + Family.Size, data = trainData)
pred_nb <- predict(model_nb, testData)
mean(pred_nb == testData$cluster_km)

library(party)
model_tree <- ctree(cluster_km ~ Age + Annual.Income.... + Spending.Score..1.100. + Work.Experience + Family.Size, data = trainData)
plot(model_tree)
pred_tree <- predict(model_tree, testData)
mean(pred_tree == testData$cluster_km)

library(randomForest)
trainData$cluster_km <- as.factor(trainData$cluster_km)
testData$cluster_km <- as.factor(testData$cluster_km)

model_rf <- randomForest(cluster_km ~ Age + Annual.Income.... + Spending.Score..1.100. + Work.Experience + Family.Size, data = trainData, ntree = 100)
pred_rf <- predict(model_rf, testData)
mean(pred_rf == testData$cluster_km)

































