
library(rvest)
# уровень жизни стран мира по годам
url_21<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2021')
url_20<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2020')
url_19<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2019')
url_18<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2018')
url_17<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2017')
url_16<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2016')
url_15<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2015')
url_14<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2014')

# функция выбирает все элементы таблицы на странице по селектору 'table'
nodes_21<-html_nodes(url_21, 'table')
nodes_20<-html_nodes(url_20, 'table')
nodes_19<-html_nodes(url_19, 'table')
nodes_18<-html_nodes(url_18, 'table')
nodes_17<-html_nodes(url_17, 'table')
nodes_16<-html_nodes(url_16, 'table')
nodes_15<-html_nodes(url_15, 'table')
nodes_14<-html_nodes(url_14, 'table')

# преобразования HTML-таблицы в датафрейм
df_21<-html_table(nodes_21[[2]])%>%as.data.frame()
df_20<-html_table(nodes_20[[2]])%>%as.data.frame()
df_19<-html_table(nodes_19[[2]])%>%as.data.frame()
df_18<-html_table(nodes_18[[2]])%>%as.data.frame()
df_17<-html_table(nodes_17[[2]])%>%as.data.frame()
df_16<-html_table(nodes_16[[2]])%>%as.data.frame()
df_15<-html_table(nodes_15[[2]])%>%as.data.frame()
df_14<-html_table(nodes_14[[2]])%>%as.data.frame()

rownames(df_21)<-df_21[, 2]
rownames(df_20)<-df_20[, 2]
rownames(df_19)<-df_19[, 2]
rownames(df_18)<-df_18[, 2]
rownames(df_17)<-df_17[, 2]
rownames(df_16)<-df_16[, 2]
rownames(df_15)<-df_15[, 2]
rownames(df_14)<-df_14[, 2]

# выбор столбцов в датафрейме с оценками общего качетсва жизни
df_21<-df_21[, 3:11]
df_20<-df_20[, 3:11]
df_19<-df_19[, 3:11]
df_18<-df_18[, 3:11]
df_17<-df_17[, 3:11]
df_16<-df_16[, 3:11]
df_15<-df_15[, 3:11]
df_14<-df_14[, 3:11]

country<-c("Germany", "Indonesia", "Peru", "Kenya", "France")

# оценка индекса качества жизни
evaluation_of<-'Quality of Life Index'
LIFE<-as.data.frame(
  rbind(
    df_14[country, evaluation_of],
    df_15[country, evaluation_of],
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2014:2021
)
colnames(LIFE)<-country
View(LIFE)

mn<-min(LIFE, na.rm=TRUE)
mx<-max(LIFE, na.rm=TRUE)

plot(2014:2021, LIFE$'Germany', xlab='Года', ylab='Индекс качества жизни', ylim=c(mn-13,mx+13),
     main='Оценка индекса качества жизни',col='blue',type='b',lty=1,pch=1, lwd=2)

lines(2014:2021, LIFE$'Indonesia', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, LIFE$'Peru', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, LIFE$'Kenya', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, LIFE$'France', type='b', col='gold', lty=1, pch=1, lwd=2)
legend('bottomright', cex=0.6,country, fill= c('blue', 'green', 'red', 'purple', 'gold'))

# оценка индекс покупательной способности
evaluation_of<-'Purchasing Power Index'
PAY<-as.data.frame(
  rbind(
    df_14[country, evaluation_of],
    df_15[country, evaluation_of],
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2014:2021
)
colnames(PAY)<-country
View(PAY)

mn<-min(PAY, na.rm=TRUE)
mx<-max(PAY, na.rm=TRUE)

plot( 2014:2021, PAY$'Germany', xlab='Года', ylab='Индекс покупательной способности', ylim=c(mn-13,mx+13), 
      main='Оценка индекса покупательной способности', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, PAY$'Indonesia', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, PAY$'Peru', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, PAY$'Kenya', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, PAY$'France', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

# оценка индекса безопасности
evaluation_of<-'Safety Index'
SI<-as.data.frame(
  rbind(
    df_14[country, evaluation_of],
    df_15[country, evaluation_of],
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2014:2021
)
colnames(SI)<-country
View(SI)

mn<-min(SI, na.rm=TRUE)
mx<-max(SI, na.rm=TRUE)

plot( 2014:2021, SI$'Germany', xlab='Года', ylab='Индекс безопасности', ylim=c(mn-13,mx+13),
      main='Оценка индекса безопасности', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, SI$'Indonesia', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, SI$'Peru', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, SI$'Kenya', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, SI$'France', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

# оценка медицинского обслуживания
evaluation_of<-'Health Care Index'
MED<-as.data.frame(
  rbind(
    df_14[country, evaluation_of],
    df_15[country, evaluation_of],
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2014:2021
)
colnames(MED)<-country
View(MED)

mn<-min(MED, na.rm=TRUE)
mx<-max(MED, na.rm=TRUE)

plot(2014:2021, MED$'Germany', xlab='Года', ylab='Индекс медицинского обслуживания', ylim=c(mn-13,mx+13),
     main='Оценка индекс медицинского обслуживания ', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, MED$'Indonesia', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, MED$'Peru', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, MED$'Kenya', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, MED$'France', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('bottomright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

# оценка индекса прожиточного минимума
evaluation_of<-'Cost of Living Index'
MIN<-as.data.frame(
  rbind(
    df_14[country, evaluation_of],
    df_15[country, evaluation_of],
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2014:2021
)
colnames(MIN)<-country
View(MIN)

mn<-min(MIN, na.rm=TRUE)
mx<-max(MIN, na.rm=TRUE)

plot(2014:2021, MIN$'Germany', xlab='Года', ylab='Индекс прожиточного минимума', ylim=c(mn-13,mx+13),
     main='Оценка индекса прожиточного минимума', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, MIN$'Indonesia', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, MIN$'Peru', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, MIN$'Kenya', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, MIN$'France', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

# оценка отношения цены на жилье к доходу
evaluation_of<-'Property Price to Income Ratio'
PAYR<-as.data.frame(
  rbind(
    df_14[country, evaluation_of],
    df_15[country, evaluation_of],
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2014:2021
)
colnames(PAYR)<-country
View(PAYR)

mn<-min(PAYR, na.rm=TRUE)
mx<-max(PAYR, na.rm=TRUE)
plot( 2014:2021, PAYR$'Germany', xlab='Года', ylab='Отношение цены на жилье к доходу', ylim=c(mn-13,mx+13),
      main='Оценка отношения цены на жилье к доходу', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, PAYR$'Indonesia', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, PAYR$'Peru', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, PAYR$'Kenya', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, PAYR$'France', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

# оценка индекс времени движения на дороге
evaluation_of<-'Traffic Commute Time Index'
TCTI<-as.data.frame(
  rbind(
    df_14[country, evaluation_of],
    df_15[country, evaluation_of],
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2014:2021
)
colnames(TCTI)<-country
View(TCTI)

mn<-min(TCTI, na.rm=TRUE)
mx<-max(TCTI, na.rm=TRUE)

plot( 2014:2021, TCTI$'Germany', xlab='Года', ylab='Индекс времени движения на дороге', ylim=c(mn-13,mx+13), 
      main='Оценка индекса времени движения на дороге', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, TCTI$'Indonesia', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, TCTI$'Peru', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, TCTI$'Kenya', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, TCTI$'France', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

# оценка индекса загрязнения
evaluation_of<-'Pollution Index'
PI<-as.data.frame(
  rbind(
    df_14[country, evaluation_of],
    df_15[country, evaluation_of],
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2014:2021
)
colnames(PI)<-country
View(PI)

mn<-min(PI, na.rm=TRUE)
mx<-max(PI, na.rm=TRUE)

plot( 2014:2021, PI$'Germany', xlab='Года', ylab='Индекс загрязнения', ylim=c(mn-13,mx+13),
      main='Оценка индекса загрязнения', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, PI$'Indonesia', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, PI$'Peru', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, PI$'Kenya', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, PI$'France', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

# оценка климатического индекса
evaluation_of<-'MINmate Index'
CI<-as.data.frame(
  rbind(
    df_16[country, evaluation_of],
    df_17[country, evaluation_of],
    df_18[country, evaluation_of],
    df_19[country, evaluation_of],
    df_20[country, evaluation_of],
    df_21[country, evaluation_of]
  ),
  row.names<-2016:2021
)
colnames(CI)<-country
View(CI)

mn<-min(CI, na.rm=TRUE)
mx<-max(CI, na.rm=TRUE)

plot( 2016:2021, CI$'Germany', xlab='Года', ylab='Климатический индекс', ylim=c(mn-13,mx+13),
      main='Оценка климатического индекса', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2016:2021, CI$'Indonesia', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2016:2021, CI$'Peru', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2016:2021, CI$'Kenya', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2016:2021, CI$'France', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('bottomright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))



# WEB скрэпинг
library(rvest)

page <- read_html("https://www.culture.ru/museums/institutes/location-krasnodarskiy-kray-krasnodar")
cards <- html_nodes(page, ".CHPy6")

titles <- c()
addresses <- c()
links <- c()
descriptions <- c()

for (card in cards) {
  title <- html_node(card, ".p1Gbz") %>% html_text(trim = TRUE)
  address <- html_node(card, ".TxTRy") %>% html_text(trim = TRUE)
  
  href <- html_node(card, "a") %>% html_attr("href")
  full_link <- paste0("https://www.culture.ru", href)
  
  museum_page <- read_html(full_link)
  description <- html_node(museum_page, "meta[name='description']") %>% html_attr("content")
  
  titles <- c(titles, title)
  addresses <- c(addresses, address)
  links <- c(links, full_link)
  descriptions <- c(descriptions, description)
}

df <- data.frame(
  Название = titles,
  Адрес = addresses,
  Ссылка = links,
  Описание = descriptions,
  stringsAsFactors = FALSE
)

View(df)