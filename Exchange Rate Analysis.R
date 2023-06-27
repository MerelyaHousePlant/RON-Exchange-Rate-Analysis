setwd("C:/Users/Asus/Desktop/Proiecte Personale R/Macro Cantitativa")
date_proiect <- read.csv("date_cercetare.csv")
library(ggplot2)

windows()
ggplot(date_proiect, aes(x = Anul, y = Nominal.Effective.Exchange.Rate)) +
  geom_line(color = "steelblue", size = 1.5) +
  geom_point(color = "steelblue", size = 3, shape = 21, fill = "white") +
  geom_text(aes(label = sprintf("%.2f", Nominal.Effective.Exchange.Rate)), vjust = -1.5, color = "black") +
  labs(x = "Anii", y = "Valoare de Schimb", title = "Rata efectivă de schimb nominală") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgray"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = unique(date_proiect$Anul))

windows()
ggplot(date_proiect, aes(x = Anul, y = Real.Effective.Exchange.Rate..IPC.)) +
  geom_line(color = "steelblue", size = 1.5) +
  geom_point(color = "steelblue", size = 3, shape = 21, fill = "white") +
  geom_text(aes(label = sprintf("%.2f", Real.Effective.Exchange.Rate..IPC.)), vjust = -1.5, color = "black") +
  labs(x = "Anii", y = "Valoare de Schimb", title = "Rata efectivă de schimb reală") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgray"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = unique(date_proiect$Anul))

windows() 
ggplot(date_proiect, aes(x = Anul)) +
  geom_line(aes(y = Curs.schimb.Euro, color = "Euro"), size = 1.5) +
  geom_point(aes(y = Curs.schimb.Euro, color = "Euro"), size = 3, shape = 21, fill = "white") +
  geom_text(aes(y = Curs.schimb.Euro, label = sprintf("%.2f", Curs.schimb.Euro)), vjust = -1.5, color = "black") +
  geom_line(aes(y = Curs.schimb.USD, color = "USD"), size = 1.5) +
  geom_point(aes(y = Curs.schimb.USD, color = "USD"), size = 3, shape = 21, fill = "white") +
  geom_text(aes(y = Curs.schimb.USD, label = sprintf("%.2f", Curs.schimb.USD)), vjust = -1.5, color = "black") +
  labs(x = "Anii", y = "Valoare de Schimb", title = "Trend-ul cursului de schimb monetar") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgray"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = unique(date_proiect$Anul)) +
  scale_color_manual(values = c("steelblue", "palegreen3"), labels = c("Euro", "USD")) +
  guides(color = guide_legend(title = "Monedă"))

library(moments)
deviatia_standard <- sd(date_proiect$Nominal.Effective.Exchange.Rate)
coeficient_aplatizare <- kurtosis(date_proiect$Nominal.Effective.Exchange.Rate)
coeficient_asimetrie <- skewness(date_proiect$Nominal.Effective.Exchange.Rate)
distrib_quantile<- quantile(date_proiect$Nominal.Effective.Exchange.Rate)
deviatia_standard <- sd(date_proiect$PIB.Miliarde.)
coeficient_aplatizare <-kurtosis(date_proiect$PIB.Miliarde.)
coeficient_asimetrie <-skewness(date_proiect$PIB.Miliarde.)
distrib_quantile<- quantile(date_proiect$PIB.Miliarde.)
deviatia_standard <- sd(date_proiect$Rata.Inflatiei)
coeficient_aplatizare <-kurtosis(date_proiect$Rata.Inflatiei)
coeficient_asimetrie <-skewness(date_proiect$Rata.Inflatiei)
distrib_quantile<- quantile(date_proiect$Rata.Inflatiei)

deviatia_standard
coeficient_aplatizare
coeficient_asimetrie
distrib_quantile

sd(date_proiect$Curs.schimb.USD)
mean(date_proiect$Curs.schimb.Euro)
mean(date_proiect$Curs.schimb.USD)
summary(date_proiect$Curs.schimb.Euro)
summary(date_proiect$Curs.schimb.USD)
range_Euro <- max(date_proiect$Curs.schimb.Euro) - min(date_proiect$Curs.schimb.Euro)
range_USD <- 4.16 - min(date_proiect$Curs.schimb.USD)
hist(date_proiect$Nominal.Effective.Exchange.Rate)
hist(date_proiect$IPC)
hist(date_proiect$PIB.Miliarde.)
cor(date_proiect[-1])
shapiro.test(date_proiect$IPC)
shapiro.test(date_proiect$Real.Effective.Exchange.Rate..IPC.)
shapiro.test(date_proiect$PIB.Miliarde.)

plot(date_proiect$Real.Effective.Exchange.Rate..IPC. ~ date_proiect$PIB.Miliarde., data = date_proiect)
date_analizate <- lm(Nominal.Effective.Exchange.Rate~PIB.Miliarde.+Rata.Inflatiei, data=date_proiect)
summary(date_analizate)
library(ggiraph)
library(ggiraphExtra)
windows()
ggPredict(date_analizate,colorAsFactor = TRUE,interactive=TRUE)
windows()
par(mfrow=c(1,6))
boxplot_NEER <- boxplot(date_proiect$Nominal.Effective.Exchange.Rate, main="NEER", col="steelblue")
boxplot_REER <- boxplot(date_proiect$Real.Effective.Exchange.Rate..IPC., main="REER", col="darkviolet")
boxplot_EURO <- boxplot(date_proiect$Curs.schimb.Euro, main="EURO", col="aquamarine")
boxplot_DOLAR <- boxplot(date_proiect$Curs.schimb.USD, main="DOLAR", col="palegreen3")
boxplot_INFLATIE <- boxplot(date_proiect$Rata.Inflatiei, main="Rata Inflatie", col="deeppink3")
boxplot_PIB <- boxplot(date_proiect$PIB.Miliarde., main="PIB", col="darkgoldenrod1")

boxplot_NEER$out
boxplot_REER$out
boxplot_EURO$out
date_proiect[date_proiect$Nominal.Effective.Exchange.Rate == boxplot_NEER$out,]
date_proiect[date_proiect$Real.Effective.Exchange.Rate..IPC. == boxplot_REER$out,]
date_proiect[date_proiect$Curs.schimb.Euro == boxplot_EURO$out,]