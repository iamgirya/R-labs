#1.
library(readxl)

data0 <- read.csv("Лучшие имена.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

#2.
data<-data0[,2:12]
head(data)
summary(data)
summary(data$Даня)

hist(data$`Даня`,main="Гистограмма по столбцу Даня",
     xlab="Баллы", ylab="Частота")
plot(data$Даниил, data$Даня,main="Диаграмма рассеяния",
     xlab = "Даниил", ylab = "Даня")

cor(data$Даниил, data$Даня)
cor(data0[,3:12])

#3.	
#(data$`Олег`,decreasing = TRUE - по убыванию)
data_sorted <- data[order(data$`Олег`),]

#4.
# Создаем subdataset из ФИО у кого Даниилович > 7
high_grade <- subset(data, Даниилович > 7)
dim(high_grade)
hist(high_grade$Даниилович, main="Гистограмма у кого Даниилович > 7",
     xlab="Баллы", ylab="Частота")
boxplot(high_grade$Даниилович, main="Боксплот анализа данных",
        xlab="ФИО у кого Даниилович > 7", ylab= "Значения")

mean(high_grade$Даниилович)
median(high_grade$Даниилович)
