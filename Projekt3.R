dax <- read.csv("C:/Users/Filip/OneDrive/EFiD/Projekt3-2/Projekt3/^dax.csv")
px <- read.csv("C:/Users/Filip/OneDrive/EFiD/Projekt3-2/Projekt3/^px.csv")
wig <- read.csv("C:/Users/Filip/OneDrive/EFiD/Projekt3-2/Projekt3/wig20_d.csv")

install.packages("data.table")
install.packages("urca")
install.packages("forecast")
install.packages("zoo")
install.packages("aTSA")
install.packages("rugarch")
install.packages("fGarch")
install.packages("vars")
library(rugarch)
library(aTSA)
library(zoo)
library(forecast)
library(urca)
library(data.table)
library(fGarch)
library(vars)

#Stopy logarytmiczne

drop = c('Otwarcie', 'Najwyzszy', 'Najnizszy', 'Wolumen')
dax = dax[,!(names(dax) %in% drop)]
px = px[,!(names(px) %in% drop)]
wig = wig[,!(names(wig) %in% drop)]

names(dax)[2] = 'Zamkniecie_dax'
names(px)[2] = 'Zamkniecie_px'
names(wig)[2] = 'Zamkniecie_wig'

merged = merge(dax, px, by = 'Data', all.x = TRUE)
merged = merge(merged, wig, by = 'Data', all.x = TRUE)


for (i in 2:4)
{
  merged[,i] <- na.approx(merged[,i], na.rm=FALSE)
}

stopa_log<- merged
for(j in 2:ncol(stopa_log)){
  for(i in 1:nrow(stopa_log)){
    stopa_log[i,j]<-log(merged[i+1,j]/merged[i,j])
  }
}

stopa_log <- stopa_log[complete.cases(stopa_log),]

plot(stopa_log$Zamkniecie_dax, type = "l", col = "green")
lines(stopa_log$Zamkniecie_px, col = "red")
lines(stopa_log$Zamkniecie_wig, col = "blue")

#1
daxAdf <- ur.df(stopa_log$Zamkniecie_dax)
pxAdf <- ur.df(stopa_log$Zamkniecie_px)
wigAdf <- ur.df(stopa_log$Zamkniecie_wig)

summary(daxAdf)
#stacjo
summary(pxAdf)
#stacjo
summary(wigAdf)
#stacjo

#wybór rzędu opóźnien

VARselect(stopa_log[2:length(stopa_log[1,])])
# 10, 2 ,1

#opoznienie = 1
var_model1 <- VAR(stopa_log[2:4], p = 1)

#badanie autokorelacji testem  Ljunga-Boxa
#H0: Brak autokorelacji wiec chce pvalue wieksze niz 0.05
model1_res_dax <-var_model1$varresult$Zamkniecie_dax$residuals
Box.test(model1_res_dax, type=c("Ljung-Box"))
# > 0.05, ok

model1_res_px <-var_model1$varresult$Zamkniecie_px$residuals
Box.test(model1_res_px, type=c("Ljung-Box"))
# > 0.05, ok

model1_res_dax <-var_model1$varresult$Zamkniecie_dax$residuals
Box.test(model1_res_dax, type=c("Ljung-Box"))
# > 0.05, ok

#opoznienie = 10
var_model10 <- VAR(stopa_log[2:4], p = 10)

#badanie autokorelacji testem  Ljunga-Boxa
#H0: Brak autokorelacji wiec chce pvalue wieksze niz 0.05
model10_res_dax <-var_model10$varresult$Zamkniecie_dax$residuals
Box.test(model10_res_dax, type=c("Ljung-Box"))
# > 0.05, ok

model10_res_px <-var_model10$varresult$Zamkniecie_px$residuals
Box.test(model10_res_px, type=c("Ljung-Box"))
# > 0.05, ok

model10_res_dax <-var_model10$varresult$Zamkniecie_dax$residuals
Box.test(model10_res_dax, type=c("Ljung-Box"))
# > 0.05, ok

# zarowno opoznienie 1 jak i 10 maja ok korelacje ale wybieramy 10 bo dwa kryteria inf go zasugerowaly

summary(var_model10)

# opoznienie 10 jest git bo przy najwyzszych opoznieniach wspolczynniki sa istiotne
# natomiast opoznienie =1 jest do dupy bo nie ma tam istotnosci 

# 3

archEfekt <- arch.test(var_model10)
archEfekt
# pvalue < 0.05 - efekt arch wystepuje

#garch

garchFit(formula = ~ garch(1, 1), data = var_model10$varresult$Zamkniecie_dax, cond.dist = c("norm")) 

#ModelSpec <- ugarchspec(mean.var_model10$varresult$Zamkniecie_dax = list(armaOrder=c(0,0)), variance.var_model10$varresult$Zamkniecie_dax = list(var_model10$varresult$Zamkniecie_dax = "sGARCH", garchOrder = c(1,1)), distribution.var_model10$varresult$Zamkniecie_dax = "norm")

