library(dplyr)
library(lubridate)
daily_new_confirmed_covid_19_cases_per_million_people <- read_csv("daily-new-confirmed-covid-19-cases-per-million-people.csv")
df<-data.frame(daily_new_confirmed_covid_19_cases_per_million_people)
names(df)[names(df) == "Daily.new.confirmed.cases.of.COVID.19.per.million.people..rolling.7.day.average..right.aligned."] <- "Daily_Cases"

df$Day <- as.Date(df$Day)   # Turning the  date column into date format

monthly_cases <- df %>%
  mutate(
    year = year(Day),
    month = month(Day)     
  ) %>%
  group_by(Entity, year, month) %>%
  summarise(
    monthly_cases_per_million = sum(Daily_Cases, na.rm = TRUE),
    .groups = "drop"
  )
#Merging data into monthly cases
Africa_monthly_cases<-monthly_cases[(1:24),]
Asia_monthly_cases<-monthly_cases[(72:95),]
Europe_monthly_cases<-monthly_cases[(143:166),]
North_America_monthly_cases<-monthly_cases[(214:237),]
Oceania_monthly_cases<-monthly_cases[(285:308),]
South_America_monthly_cases<-monthly_cases[(356:379),]
Africa_monthly_cases$Cumulative<-cumsum(Africa_monthly_cases$monthly_cases_per_million)
Asia_monthly_cases$Cumulative<-cumsum(Asia_monthly_cases$monthly_cases_per_million)
Europe_monthly_cases$Cumulative<-cumsum(Europe_monthly_cases$monthly_cases_per_million)
North_America_monthly_cases$Cumulative<-cumsum(North_America_monthly_cases$monthly_cases_per_million)
Oceania_monthly_cases$Cumulative<-cumsum(Oceania_monthly_cases$monthly_cases_per_million)
South_America_monthly_cases$Cumulative<-cumsum(South_America_monthly_cases$monthly_cases_per_million)

#Africa
par(mfrow=c(2,2))
plot(3:10,Africa_monthly_cases$monthly_cases_per_million[Africa_monthly_cases$year==2020 & Africa_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2020", ylab = "Incidence Rate per million", main = "Incidence rates in Africa in 2020", type = "l")
plot(3:10,Africa_monthly_cases$Cumulative[Africa_monthly_cases$year==2020 & Africa_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2020", ylab = "Prevalence Rate per million", main = "Prevalence rates in Africa in 2020", type = "l")
plot(3:10,Africa_monthly_cases$monthly_cases_per_million[Africa_monthly_cases$year==2021 & Africa_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2021", ylab = "Incidence Rate per million", main = "Incidence rates in Africa in 2021", type = "l")
plot(3:10,Africa_monthly_cases$Cumulative[Africa_monthly_cases$year==2021 & Africa_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2021", ylab = "Prevalence Rate per million", main = "Prevalence rates in Africa in 2021", type = "l")
par(mfrow=c(1,1))

#Asia
par(mfrow=c(2,2))
plot(3:10,Asia_monthly_cases$monthly_cases_per_million[Asia_monthly_cases$year==2020 & Asia_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2020", ylab = "Incidence Rate per million", main = "Incidence rates in Asia in 2020", type = "l")
plot(3:10,Asia_monthly_cases$Cumulative[Asia_monthly_cases$year==2020 & Asia_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2020", ylab = "Prevalence Rate per million", main = "Prevalence rates in Asia in 2020", type = "l")
plot(3:10,Asia_monthly_cases$monthly_cases_per_million[Asia_monthly_cases$year==2021 & Asia_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2021", ylab = "Incidence Rate per million", main = "Incidence rates in Asia in 2021", type = "l")
plot(3:10,Asia_monthly_cases$Cumulative[Asia_monthly_cases$year==2021 & Asia_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2021", ylab = "Prevalence Rate per million", main = "Prevalence rates in Asia in 2021", type = "l")
par(mfrow=c(1,1))

#Europe
par(mfrow=c(2,2))
plot(3:10,Europe_monthly_cases$monthly_cases_per_million[Europe_monthly_cases$year==2020 & Europe_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2020", ylab = "Incidence Rate per million", main = "Incidence rates in Europe in 2020", type = "l")
plot(3:10,Europe_monthly_cases$Cumulative[Europe_monthly_cases$year==2020 & Europe_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2020", ylab = "Prevalence Rate per million", main = "Prevalence rates in Europe in 2020", type = "l")
plot(3:10,Europe_monthly_cases$monthly_cases_per_million[Europe_monthly_cases$year==2021 & Europe_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2021", ylab = "Incidence Rate per million", main = "Incidence rates in Europe in 2021", type = "l")
plot(3:10,Europe_monthly_cases$Cumulative[Europe_monthly_cases$year==2021 & Europe_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2021", ylab = "Prevalence Rate per million", main = "Prevalence rates in Europe in 2021", type = "l")
par(mfrow=c(1,1))

#North America
par(mfrow=c(2,2))
plot(3:10,North_America_monthly_cases$monthly_cases_per_million[North_America_monthly_cases$year==2020 & North_America_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2020", ylab = "Incidence Rate per million", main = "Incidence rates in North America in 2020", type = "l")
plot(3:10,North_America_monthly_cases$Cumulative[North_America_monthly_cases$year==2020 & North_America_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2020", ylab = "Prevalence Rate per million", main = "Prevalence rates in North America in 2020", type = "l")
plot(3:10,North_America_monthly_cases$monthly_cases_per_million[North_America_monthly_cases$year==2021 & North_America_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2021", ylab = "Incidence Rate per million", main = "Incidence rates in North America in 2021", type = "l")
plot(3:10,North_America_monthly_cases$Cumulative[North_America_monthly_cases$year==2021 & North_America_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2021", ylab = "Prevalence Rate per million", main = "Prevalence rates in North America in 2021", type = "l")
par(mfrow=c(1,1))

#Oceania
par(mfrow=c(2,2))
plot(3:10,Oceania_monthly_cases$monthly_cases_per_million[Oceania_monthly_cases$year==2020 & Oceania_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2020", ylab = "Incidence Rate per million", main = "Incidence rates in Oceania in 2020", type = "l")
plot(3:10,Oceania_monthly_cases$Cumulative[Oceania_monthly_cases$year==2020 & Oceania_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2020", ylab = "Prevalence Rate per million", main = "Prevalence rates in Oceania in 2020", type = "l")
plot(3:10,Oceania_monthly_cases$monthly_cases_per_million[Oceania_monthly_cases$year==2021 & Oceania_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2021", ylab = "Incidence Rate per million", main = "Incidence rates in Oceania in 2021", type = "l")
plot(3:10,Oceania_monthly_cases$Cumulative[Oceania_monthly_cases$year==2021 & Oceania_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2021", ylab = "Prevalence Rate per million", main = "Prevalence rates in Oceania in 2021", type = "l")
par(mfrow=c(1,1))

#South America
par(mfrow=c(2,2))
plot(3:10,South_America_monthly_cases$monthly_cases_per_million[South_America_monthly_cases$year==2020 & South_America_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2020", ylab = "Incidence Rate per million", main = "Incidence rates in South America in 2020", type = "l")
plot(3:10,South_America_monthly_cases$Cumulative[South_America_monthly_cases$year==2020 & South_America_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2020", ylab = "Prevalence Rate per million", main = "Prevalence rates in South America in 2020", type = "l")
plot(3:10,South_America_monthly_cases$monthly_cases_per_million[South_America_monthly_cases$year==2021 & South_America_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2021", ylab = "Incidence Rate per million", main = "Incidence rates in South America in 2021", type = "l")
plot(3:10,South_America_monthly_cases$Cumulative[South_America_monthly_cases$year==2021 & South_America_monthly_cases$month %in% 3:10], xlab = "Months from March to October, 2021", ylab = "Prevalence Rate per million", main = "Prevalence rates in South America in 2021", type = "l")
par(mfrow=c(1,1))



