library(ggplot2)

getwd()
setwd("C:/Users/nick/Documents/GitHub/Math489/CurveFitting")

#attaching data
austin <- read.csv("ausmodeldata.csv")
austintest <- read.csv("austestdata.csv")
austintest$Status <- "Actual"
austin$Status <- "Actual"

#Q1

#plot of data from 1850 to 1950
ggplot(data = austin, aes(x = Date, y = Population)) +
  geom_point() +
  geom_line() +
  ggtitle("Austin Population")

#plot of data with linear fit
ggplot(data = austin, aes(x = Date, y = Population)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y~x) +
  ggtitle("Austin Population With linear model")

ggsave("austestlin.jpeg")

#creating linear model for data 1850-1950
lmfit <- lm(Population ~ Date, data = austin)

#make a dataframe to put in the predictions
auslm <- data.frame(Date = c(1960,1970,1980,1990,2000,2010))

auslm$Population <- predict(lmfit, newdata = auslm)

auslm$Status <- "Predicted"

auslmtotal <- rbind(auslm, austintest, austin)

#ploting predictions against actaul data
ggplot(data = auslmtotal, aes(x = Date, y = Population, color = Status)) +
  geom_line() +
  geom_point() +
  ggtitle("Austin population with linear model")
ggsave("auscolorlm.jpeg")

#Q2
#plot of model data with quad fit on it
ggplot(data = austin, aes(x = Date, y = Population)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y~ x + I(x^2)) +
  ggtitle("Austin Population With quadraic model")

ggsave("austestquad.jpeg")

#creating model for quadraic fit
quadfit <- lm(Population ~ Date + I(Date^2), data = austin)

#predicting using model
ausquad <- data.frame(Date = c(1960,1970,1980,1990,2000,2010))

ausquad$Population <- predict(quadfit, newdata = ausquad)

ausquad$Status <- "Predicted"

ausquadtotal <- rbind(ausquad, austintest, austin)

#ploting predicted v. actaul data
ggplot(data = ausquadtotal, aes(x = Date, y = Population, color = Status)) +
  geom_line() +
  geom_point() +
  ggtitle("Austin population with quadardic model")
ggsave("auscolorquad.jpeg")

#Q3
#plot of ln pop v. ln time since founding (1839)
#creating dataframes with ln time since founding
austinln <- austin
austinln$Date <- log(austinln$Date - 1839)
austinln$Population <- log(austinln$Population)

austintestln <- austintest
austintestln$Date <- log(austintestln$Date -1839)
austintestln$Population <- log(austintestln$Population)

#ploting austinln data
ggplot(data = austinln, aes(x = Date, y = Population)) +
  geom_point() +
  geom_line() +
  ggtitle("Austin ln(Population) v. ln(years since founding)")

#creating model for powerlaw fit
powerfit <- lm(Population ~ Date, data = austinln)

#predicting powerlaw fit
auspower <- data.frame(Date = log(c(1960,1970,1980,1990,2000,2010) - 1850))

auspower$Population <- predict(powerfit, newdata = auspower)

auspower$Status <- "Predicted"

auspowertotal <- rbind(auspower, austinln, austintestln)

#ploting predictions v. actaul data
ggplot(data = auspowertotal, aes(x = Date, y = Population, color = Status)) +
  geom_line() +
  geom_point() +
  ggtitle("Austin log(population) v. log(date since founding) with power-law fit")
ggsave("auscolorpower.jpeg")

#Q4
#creating dataframes with ln pop
austinpopln <- austin
austintestpopln <- austintest

austinpopln$Population <- log(austinpopln$Population)
austintestpopln$Population <- log(austintestpopln$Population)

#plot ln population v. time since founding
ggplot(data = austin, aes(x = Date, y = log(Population))) +
  geom_point() +
  geom_line() +
  ggtitle("Austin Log(Population)")

#bulid model for exponential fit
expfit <- lm(log(Population) ~ Date, data = austin)

#making predictions for each model
ausexp <- data.frame(Date = c(1960,1970,1980,1990,2000,2010))

ausexp$Population <- predict(expfit, newdata = ausexp)
ausexp$Status <- "Predicted"

ausexptotal <- rbind(ausexp, austinpopln, austintestpopln)

#ploting exponential fit
ggplot(data = ausexptotal, aes(x = Date, y = Population, color = Status)) +
  geom_line() +
  geom_point() +
  ggtitle("Austin population with exponential model")
ggsave("auscolorexp.jpeg")