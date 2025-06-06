data <- read.csv("athlete_events.csv")

# фильтрация по велоспорту
cycling <- subset(data, Sport == "Cycling")

cat("\nУникальных спортсменов в велоспорте:")
print(length(unique(cycling$ID)))

cat("\nРаспределение по полу:")
print(table(cycling$Sex))

cat("\nРаспределение по медалям (включая NA):")
print(table(cycling$Medal, useNA = "ifany"))

cat("\nСтатистика по возрасту:\n")
print(summary(cycling$Age))

cat("\nСтатистика по росту:\n")
print(summary(cycling$Height))

cat("\nСтатистика по весу:\n")
print(summary(cycling$Weight))

hist(cycling$Weight, main = "Распределение веса велогонщиков", xlab = "Вес (кг)", col = "lightblue", breaks = 20)
boxplot(cycling$Height, main = "Boxplot: Рост велогонщиков", col = "lightgreen")

# проверка на нормальность веса у мужчин и женщин 
weight_m <- na.omit(cycling$Weight[cycling$Sex == "M"])
weight_f <- na.omit(cycling$Weight[cycling$Sex == "F"])

set.seed(123)
weight_m_sample <- if(length(weight_m) > 5000) sample(weight_m, 5000) else weight_m

cat("\nShapiro-Wilk тест для веса мужчин (выборка 5000):\n")
print(shapiro.test(weight_m_sample))

qqnorm(weight_m_sample, main = "Q-Q график: Вес мужчин ")
qqline(weight_m_sample, col = "red")

cat("\nShapiro-Wilk тест для веса женщин:\n")
print(shapiro.test(weight_f))

qqnorm(weight_f, main = "Q-Q график: Вес женщин (Cycling)")
qqline(weight_f, col = "red")

cat("\nТест Бартлетта на равенство дисперсий между полами:\n")
print(bartlett.test(Weight ~ Sex, data = cycling))

if(shapiro.test(weight_m_sample)$p.value > 0.05 && shapiro.test(weight_f)$p.value > 0.05){
  cat("\nРезультат t-теста для веса мужчин и женщин в велоспорте:\n")
  print(t.test(weight_m, weight_f, var.equal = TRUE))
} else {
  cat("\nРезультат теста Манна-Уитни для веса мужчин и женщин в велоспорте:\n")
  print(wilcox.test(weight_m, weight_f))
}

# проверка гипотезы о среднем весе у всех велогонщиков
cat("\nПроверка гипотезы о среднем весе = 70 кг:\n")
print(wilcox.test(na.omit(cycling$Weight), mu = 70, conf.int = TRUE, exact = FALSE))

# сравнение веса женщин в велоспорте и легкой атлетике
sport1_f <- na.omit(subset(data, Sport == "Cycling" & Sex == "F")$Weight)
sport2_f <- na.omit(subset(data, Sport == "Athletics" & Sex == "F")$Weight)

cat("\nShapiro-Wilk для веса женщин в велоспорте:\n")
print(shapiro.test(if(length(sport1_f) > 5000) sample(sport1_f, 5000) else sport1_f))

cat("\nShapiro-Wilk для веса женщин в легкой атлетике:\n")
print(shapiro.test(if(length(sport2_f) > 5000) sample(sport2_f, 5000) else sport2_f))

cat("\nТест Бартлетта для равенства дисперсий:\n")
print(bartlett.test(list(sport1_f, sport2_f)))

cat("\nТест Манна-Уитни для сравнения веса женщин:\n")
print(wilcox.test(sport1_f, sport2_f, paired = FALSE, conf.int = TRUE))

# аналогично для мужчин
sport1_m <- na.omit(subset(data, Sport == "Cycling" & Sex == "M")$Weight)
sport2_m <- na.omit(subset(data, Sport == "Athletics" & Sex == "M")$Weight)

cat("\nShapiro-Wilk для веса мужчин в велоспорте:\n")
print(shapiro.test(if(length(sport1_m) > 5000) sample(sport1_m, 5000) else sport1_m))

cat("\nShapiro-Wilk для веса мужчин в легкой атлетике:\n")
print(shapiro.test(if(length(sport2_m) > 5000) sample(sport2_m, 5000) else sport2_m))

cat("\nТест Бартлетта для равенства дисперсий (мужчины):\n")
print(bartlett.test(list(sport1_m, sport2_m)))

cat("\nТест Манна-Уитни для сравнения веса мужчин:\n")
print(wilcox.test(sport1_m, sport2_m, paired = FALSE, conf.int = TRUE))