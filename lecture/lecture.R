# Указываем путь к файлу
file_path <- "/Users/kozzze/Desktop/Учеба/BigData/BigData/lecture/assess.dat"  # замени на полный путь, если файл не в рабочей директории

# Считываем таблицу (разделители — пробелы)
data <- read.table(file_path, header = TRUE)

# Проверим структуру
str(data)

# Удалим столбцы NR и NAME, оставим только оценки по тестам
scores <- data[, !(names(data) %in% c("NR", "NAME"))]

# Переименуем столбцы согласно описанию
colnames(scores) <- c(
  "Память на числа",
  "Математические задачи",
  "Прямой диалог",
  "Алгоритмы",
  "Уверенность",
  "Командный дух",
  "Находчивость",
  "Сотрудничество",
  "Признание",
  "Сила убеждения"
)

# Посмотрим на первые строки
head(scores)

# Пример анализа — средние баллы по каждому качеству
colMeans(scores)

# Построим диаграмму среднего уровня по всем качествам
barplot(colMeans(scores), las = 2, col = "skyblue", main = "Средние оценки по качествам", ylab = "Средний балл")




# 1. Загрузка данных
file_path <- "/Users/kozzze/Desktop/Учеба/BigData/BigData/lecture/assess.dat"  # укажи полный путь, если файл не в рабочей папке
data <- read.table(file_path, header = TRUE)

# 2. Отделим только оценки (T1–T10) — переменные 3:12
scores <- data[, 3:12]

# 3. Названия тестов
colnames(scores) <- c(
  "Память на числа",
  "Математика",
  "Прямой диалог",
  "Алгоритмы",
  "Уверенность",
  "Командный дух",
  "Находчивость",
  "Сотрудничество",
  "Признание",
  "Сила убеждения"
)

# 4. Матрица расстояний (Евклидово расстояние)
dist_matrix <- dist(scores)

# 5. Иерархическая кластеризация (метод - complete linkage)
hc <- hclust(dist_matrix, method = "complete")

# 6. Построим дендрограмму
plot(hc, labels = data$NAME, main = "Дендрограмма кандидатов", xlab = "", sub = "", cex = 0.8)

# 7. Разбиение на 3 кластера
groups_3 <- cutree(hc, k = 3)
rect.hclust(hc, k = 3, border = "red")  # нарисуем прямоугольники

# 8. Разбиение на 4 кластера
groups_4 <- cutree(hc, k = 4)
rect.hclust(hc, k = 4, border = "blue")  # ещё раз — для сравнения

# 9. Добавим группы к исходным данным
data$Cluster_3 <- groups_3
data$Cluster_