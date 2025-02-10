brand <- c("Mercedes","BMW","AUDI","Porsche")
weight <- c(1700,1700,1800,1500)
engine <- c(4.0,4.0,3.8,3.2)
kpp <- c("Auto","Robot","Mechanical","Robot")
speed <- c(300,300,280,320)
type <- c("Sedan","Sedan","Universal","Coupe")

auto_data <- data.frame(Brand = brand, Weight = weight, Engine = engine, Transmission = kpp, TopSpeed = speed, CarType = type, stringsAsFactors = FALSE)

eq_engine <- auto_data[auto_data$Engine == 4.0, ]

sort_auto <- eq_engine[order(eq_engine$Brand), ]

count_row <- nrow(auto_data)
count_column <- ncol(auto_data)

print(auto_data)

print("Объем = 4.0")
print(sort_auto)

print(paste("Строк = ",count_row))
print(paste("Столбцов = ",count_column))