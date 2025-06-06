#----------7 2 1---------
data(longley)

cat("Количество переменных (столбцов):", ncol(longley), "\n")

cat("Объем выборки (кол-во наблюдений):", nrow(longley), "\n")

cat("\nПростая статистика:\n")
print(summary(longley))

cat("\nКорреляционная матрица:\n")
cor_matrix <- cor(longley)
print(cor_matrix)

library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

pairs(longley, main = "Парные диаграммы рассеяния")

cat("\nПроверка нормальности (Shapiro-Wilk):\n")
normal_tests <- apply(longley, 2, shapiro.test)
for (i in 1:length(normal_tests)) {
  cat(names(normal_tests)[i], ": p-value =", normal_tests[[i]]$p.value, "\n")
}

par(mfrow = c(3, 2))  

for (i in 1:ncol(longley)) {
  hist(longley[[i]], main = paste("Histogram:", colnames(longley)[i]), 
       xlab = "", col = "lightblue", breaks = 10)
}

par(mfrow = c(3, 2))
for (i in 1:ncol(longley)) {
  qqnorm(longley[[i]], main = paste("Q-Q Plot:", colnames(longley)[i]))
  qqline(longley[[i]], col = "red")
}

#-----------7 5-----------
# Чтение и предварительная обработка CSV-таблицы
df <- read.csv("/Users/kozzze/Desktop/Учеба/BigData/BigData/LR7/API_SWE_DS2_en_csv_v2_94006.csv", skip = 4)
names(df) <- trimws(names(df))

# Определяем коды интересующих индикаторов
codes <- c(
  "NY.GDP.MKTP.KD.ZG",       # Рост ВВП (GDP growth)
  "SP.POP.GROW",             # Прирост населения
  "SL.UEM.ADVN.ZS",          # Безработица с высшим образованием
  "SL.UEM.BASC.ZS",          # Безработица с базовым образованием
  "SH.XPD.GHED.GD.ZS",       # Расходы на медицину
  "SP.DYN.LE00.IN",          # Продолжительность жизни
  "SP.DYN.CDRT.IN",          # Уровень смертности
  "SE.TER.CUAT.BA.ZS",       # Уровень высшего образования (общий)
  "SE.TER.CUAT.BA.FE.ZS",    # Высшее образование среди женщин
  "IP.JRN.ARTC.SC",          # Количество научных статей
  "NE.EXP.GNFS.KD.ZG",       # Экспорт (годовой прирост)
  "NV.MNF.TECH.ZS.UN",       # Высокотехнологичное производство (прирост)
  "SE.XPD.TOTL.GD.ZS"        # Расходы на образование
)

# Фильтрация данных для Швеции и нужных индикаторов
df_swe <- df[df$Country.Name == "Sweden" & df$Indicator.Code %in% codes, ]

# Преобразование данных из широкого формата в длинный, а затем в «широкий» датафрейм по годам
library(tidyr)
library(dplyr)
df_long <- df_swe %>%
  select(Indicator.Code, starts_with("X")) %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Year",
               values_to = "Value") %>%
  mutate(Year = as.numeric(sub("X", "", Year))) %>%
  pivot_wider(names_from = Indicator.Code, values_from = Value) %>%
  arrange(Year)

# Фильтрация по годам (1989–2018) для тестов и графиков
sweden_data <- df_long %>% filter(Year >= 1989 & Year <= 2018)

# --------------------- Корреляционные тесты ---------------------

# 1. Корреляция между ростом ВВП и приростом населения
cat("Тест корреляции: рост ВВП против роста населения\n")
gdp_pop_test <- cor.test(sweden_data$`NY.GDP.MKTP.KD.ZG`, sweden_data$`SP.POP.GROW`)
print(gdp_pop_test)

# 2. Корреляция между расходами на медицину и продолжительностью жизни
cat("\nТест корреляции: расходы на здравоохранение и ожидаемая продолжительность жизни\n")
health_life_test <- cor.test(sweden_data$`SH.XPD.GHED.GD.ZS`, sweden_data$`SP.DYN.LE00.IN`)
print(health_life_test)

# 3. Корреляция между расходами на медицину и уровнем смертности
cat("\nТест корреляции: расходы на здравоохранение против смертности\n")
health_mortality_test <- cor.test(sweden_data$`SH.XPD.GHED.GD.ZS`, sweden_data$`SP.DYN.CDRT.IN`)
print(health_mortality_test)

# 4. Корреляция между уровнем высшего образования (общий) и экспортом
cat("\nТест корреляции: высшее образование против роста экспорта\n")
he_export_test <- cor.test(sweden_data$`SE.TER.CUAT.BA.ZS`, sweden_data$`NE.EXP.GNFS.KD.ZG`)
print(he_export_test)

# 5. Корреляция между уровнем высшего образования (общий) и ростом высокотехнологичного производства
cat("\nТест корреляции: высшее образование против роста высокотехнологичного производства\n")
he_hightech_test <- cor.test(sweden_data$`SE.TER.CUAT.BA.ZS`, sweden_data$`NV.MNF.TECH.ZS.UN`)
print(he_hightech_test)

# 6. Корреляция между уровнем высшего образования (общий) и количеством научных статей
cat("\nТест на корреляцию: высшее образование против научных статей\n")
he_articles_test <- cor.test(sweden_data$`SE.TER.CUAT.BA.ZS`, sweden_data$`IP.JRN.ARTC.SC`)
print(he_articles_test)

# --------------------- Графики ---------------------

library(ggplot2)

# График зависимости продолжительности жизни от расходов на медицину
df_long %>%
  filter(!is.na(`SH.XPD.GHED.GD.ZS`), !is.na(`SP.DYN.LE00.IN`)) %>%
  ggplot(aes(x = `SH.XPD.GHED.GD.ZS`, y = `SP.DYN.LE00.IN`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Ожидаемая продолжительность жизни против расходов на здравоохранение",
       x = "Расходы на медицину (% от ВВП)",
       y = "Продолжительность жизни (годы)")

# График зависимости уровня смертности от расходов на медицину
df_long %>%
  filter(!is.na(`SH.XPD.GHED.GD.ZS`), !is.na(`SP.DYN.CDRT.IN`)) %>%
  ggplot(aes(x = `SH.XPD.GHED.GD.ZS`, y = `SP.DYN.CDRT.IN`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Смертность против расходов на здравоохранение",
       x = "Расходы на медицину (% от ВВП)",
       y = "Смертность (на 1000 чел)")

# График зависимости экспорта от уровня высшего образования
df_long %>%
  filter(!is.na(`SE.TER.CUAT.BA.ZS`), !is.na(`NE.EXP.GNFS.KD.ZG`)) %>%
  ggplot(aes(x = `SE.TER.CUAT.BA.ZS`, y = `NE.EXP.GNFS.KD.ZG`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Рост экспорта против высшего образования",
       x = "Уровень высшего образования (%)",
       y = "Экспорт (годовой прирост)")

# График зависимости высокотехнологичного производства от уровня высшего образования
df_long %>%
  filter(!is.na(`SE.TER.CUAT.BA.ZS`), !is.na(`NV.MNF.TECH.ZS.UN`)) %>%
  ggplot(aes(x = `SE.TER.CUAT.BA.ZS`, y = `NV.MNF.TECH.ZS.UN`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Технологическая индустрия против высшего образования",
       x = "Уровень высшего образования (%)",
       y = "Высокотех. производство (прирост)")

# График зависимости количества научных статей от уровня высшего образования
df_long %>%
  filter(!is.na(`SE.TER.CUAT.BA.ZS`), !is.na(`IP.JRN.ARTC.SC`)) %>%
  ggplot(aes(x = `SE.TER.CUAT.BA.ZS`, y = `IP.JRN.ARTC.SC`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Научные статьи против высшего образования",
       x = "Уровень высшего образования (%)",
       y = "Научные статьи")

# График динамики роста ВВП Швеции (1989-2018)
df_long %>%
  filter(Year >= 1989 & Year <= 2018, !is.na(`NY.GDP.MKTP.KD.ZG`)) %>%
  ggplot(aes(x = Year, y = `NY.GDP.MKTP.KD.ZG`)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Рост ВВП Швеции (1989-2018)",
       x = "Год",
       y = "Рост ВВП (%)")

# Создание итоговой таблицы (таблица 7.4) с выбранными показателями
table_7_4 <- df_long %>%
  filter(Year >= 1989 & Year <= 2017) %>%
  select(
    Year,
    GDP = NY.GDP.MKTP.KD.ZG,
    Pop_Growth = SP.POP.GROW,
    Unemp_Adv = SL.UEM.ADVN.ZS,
    Unemp_Base = SL.UEM.BASC.ZS,
    Health = SH.XPD.GHED.GD.ZS,
    Life_Exp = SP.DYN.LE00.IN,
    Death = SP.DYN.CDRT.IN,
    Edu_Total = SE.TER.CUAT.BA.ZS,
    Edu_Women = SE.TER.CUAT.BA.FE.ZS,
    Articles = IP.JRN.ARTC.SC,
    Export = NE.EXP.GNFS.KD.ZG,
    Tech_Industry = NV.MNF.TECH.ZS.UN,
    Edu_Spend = SE.XPD.TOTL.GD.ZS
  )

table_numeric <- table_7_4 %>% select(-Year)

cor_matrix_sweden <- cor(table_numeric, use = "complete.obs")
print(cor_matrix_sweden)

if (!require("corrplot")) install.packages("corrplot", dependencies = TRUE)
library(corrplot)
corrplot(cor_matrix_sweden, 
         method = "color",      
         type = "upper",        
         tl.cex = 0.8,          
         title = "Корреляционная матрица (Sweden Data)",
         mar = c(0, 0, 1, 0))
model_life <- lm(`SP.DYN.LE00.IN` ~ `SH.XPD.GHED.GD.ZS` + `SE.TER.CUAT.BA.ZS`, data = sweden_data)

summary(model_life)

sweden_data <- table_7_4

predicted_life <- predict(model_life, newdata = sweden_data, interval = "prediction", level = 0.95)

sweden_data$predicted_life <- predicted_life[,"fit"]
sweden_data$predicted_lwr  <- predicted_life[,"lwr"]
sweden_data$predicted_upr  <- predicted_life[,"upr"]

last_obs <- sweden_data[which.max(sweden_data$Year), ]
last_forecast <- predict(model_life, newdata = last_obs, interval = "prediction", level = 0.95)
cat(sprintf("Прогноз для года %d: Продолжительность жизни = %.3f (95%% PI: [%.3f, %.3f])\n",
            last_obs$Year, last_forecast[,"fit"], last_forecast[,"lwr"], last_forecast[,"upr"]))
