##### PACKAGES #################################################################

library(tseries)
library(tidyverse)
library(readxl) # to use read_xlsx
library(plyr) # to use  ddply
library(forecast) # to use hw
library(lmtest) # to use coeftest
library(tidyr) # to use crossing


options(scipen=999)


##### A. Descriptive and visual analysis already made on excel #################

##### B. Create Train (Treino) and Holdout #####################################

# Last 2 months of sales (vendas): Holdout

vendas = read_xlsx(path = ".../Vendas_ASN_depto2_previsao.xlsx")

head(vendas)
min(vendas$Data)
max(vendas$Data)

vendas$separa = case_when(
    vendas$Ano == 2021 ~ "holdout",
    vendas$Ano == 2018 ~ "eliminate",
    vendas$Ano < 2021 ~ "train"
  )

vendas_treino = filter(vendas, separa == "train")
vendas_holdout = filter(vendas, separa == "holdout")



##### C. Verify white noise pattern ############################################

names(vendas_treino)

vendas_treino_soma <- ddply(vendas_treino,
                            .(Data, X_Dia_da_semana, X_Primeira_semana_mes, 
                              X_Ultima_semana_mes,
                              X_Ultimo_mes, Dia,Semana, Mes, Ano, Secao),
                            summarise,
                            Vendas_soma=sum(Vendas))

head(vendas_treino_soma)
table(vendas_treino_soma$Secao)

ggplot(data = vendas_treino_soma, aes(x = Data, y = Vendas_soma)) +
  geom_line() + labs(x = "Data", y = "Total de Vendas - Depto 2") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))



# using Ljung-Box test
Box.test(vendas_treino_soma$Vendas_soma, lag=10, type="Ljung-Box")
# p_value < 0.05  => reject null hypothesis => series is not white noise



##### D. Stationarity evaluation ###############################################

adf.test(vendas_treino_soma$Vendas_soma, k=25) # stationary
# p_valor > 0.05  => accept null hypothesis =>  series is not stationary

vendas_treino_soma_semana <- ddply(vendas_treino_soma,
                                   .(Ano, Semana),
                                   summarise,
                                   Vendas_soma=sum(Vendas_soma))
adf.test(vendas_treino_soma_semana$Vendas_soma, k=25) # not stationary
# p_valor > 0.05  => accept null hypothesis =>  series is not stationary

vendas_treino_soma_mes <- ddply(vendas_treino_soma,
                                .(Ano, Mes),
                                summarise,
                                Vendas_soma=sum(Vendas_soma))
adf.test(vendas_treino_soma_mes$Vendas_soma, k=5) # not stationary
# p_valor > 0.05  => accept null hypothesis =>  series is not stationary

##### E. Identifying components ################################################

# day

par(mfrow=c(3,1))
plot(vendas_treino_soma$Vendas_soma, type = "l")
acf(vendas_treino_soma$Vendas_soma, 35, main="ACF")
pacf(vendas_treino_soma$Vendas_soma, 35, main="PACF")
# decaimento de senóide na PACF depois do lag 4  
# decaimento de senóide na ACF depois do lag 6  
# parece ARMA(4,6)


# week

par(mfrow=c(3,1))
plot(vendas_treino_soma_semana$Vendas_soma, type = "l")
acf(diff(vendas_treino_soma_semana$Vendas_soma, lag = 1), 25, main="ACF") 
# using lag 1 as it is already not stationary
pacf(diff(vendas_treino_soma_semana$Vendas_soma, lag = 1), 25, main="PACF")
# probably has weekly / monthly seasonality (has lags over CI)
# down sine wave PACF after lag 3
# down sine wave ACF after lag 2
# ARMA(3,2)?

par(mfrow=c(2,1))
acf(diff(diff(vendas_treino_soma_semana$Vendas_soma, lag = 1), lag = 3), 25, 
    main="ACF lag 4")
pacf(diff(diff(vendas_treino_soma_semana$Vendas_soma, lag = 1), lag = 3), 25, 
     main="PACF lag 4")
# looks like there is stil seasonal behavior


# month

par(mfrow=c(3,1))
plot(vendas_treino_soma_mes$Vendas_soma, type = "l")
acf(vendas_treino_soma_mes$Vendas_soma, 24, main="ACF")
pacf(vendas_treino_soma_mes$Vendas_soma, 24, main="PACF")
# no lags over CI, but goes up on lag 12 PACF
# no lags after lag 1






##### F1. Exponential smoothing#################################################

# Section 2: no tendency (adf.test) but with seasonality (plots)
# apply Additive Winters model
# (This model wont be enough to capture annual seasonality)

depto4_add = hw(msts(vendas_treino_soma$Vendas_soma, seasonal.periods=c(7)),
                seasonal = "additive", h = 21)
summary(depto4_add)

# residues plot
par(mfrow=c(1,1))
plot(depto4_add$residuals) 

# check if it is white noise
checkresiduals(depto4_add)
# p_valor < 0.05  => reject null hypothesis => series is not white noise

# forecast
autoplot(depto4_add) + autolayer(fitted(depto4_add))
accuracy(depto4_add)




##### F2. Fit ARIMAX model #####################################################
# SARIMAX(p,d,q).(P,D,Q)s + Xreg
# - p = based on PACF daily series
# - d = based on stationarity (or not) daily series
# - q = based on ACF daily series
# - P = based on PACF daily series (repeated peaks)
# - D = based on stationarity (or not) daily series lag1
# - Q = based on ACF daily model (repeated peaks)
# - s = langht of seasonal cycle of daily series
# - Xreg = explain week and annual pattern

# Depto 2
# suggested parameters: (1:4,0:1,1:6).(0:1,0:1,0:1)7 + Xreg week and month

m1= crossing(var1 = 1:4, var2 = 0:1, var3 = 1:6,
         var4 = 0:1, var5 = 0:1, var6 = 0:1,
         var7 = 7, MAE = 0, Sig = 0) 

excluir = c("xreg1", "xreg2", "xreg3", "intercept") # used in the loop

for (i in 384:384)
{
  
# adjust model and show it
depto4_mod = 
  try(
  Arima(vendas_treino_soma$Vendas_soma, 
                   order=c(m1$var1[i], m1$var2[i], m1$var3[i]), 
                   seasonal=list(order=c(m1$var4[i], m1$var5[i], m1$var6[i]), 
                                 period=7), 
                   xreg = cbind(vendas_treino_soma$X_Ultimo_mes,
                            vendas_treino_soma$X_Primeira_semana_mes,
                            vendas_treino_soma$X_Ultima_semana_mes))
  , silent = TRUE)

# write MAE 
m1$MAE[i] = 
  try(
  summary(depto4_mod)[1,3]
  , silent = TRUE)

# show coefficients
print(
  try(coeftest(depto4_mod), silent = TRUE)
)

# count p values < 0.05 at SARIMA
valores_p = coeftest(depto4_mod)
valores_p = 
  coeftest(depto4_mod)[!(row.names(coeftest(depto4_mod)) %in% excluir),4]

# specify if all SARIMA components are statistically significant
m1$Sig[i] = 
  try(
    sum(valores_p <= 0.05, na.rm = TRUE) == sum(m1$var1[i], m1$var3[i], 
                                                m1$var4[i], m1$var6[i]) 
   , silent = TRUE)

print(c(i, "/", nrow(m1)))

}

write.table(m1, ".../m2.csv", dec = ",")
            


##### MANUAL TESTING ########################################################### 

depto4_teste = 
    Arima(vendas_treino_soma$Vendas_soma, 
          order=c(2,0,1), 
          seasonal=c(0,1,1, period=7), 
          xreg = cbind(vendas_treino_soma$X_Ultimo_mes,
                       vendas_treino_soma$X_Primeira_semana_mes #,
                      # vendas_treino_soma$X_Ultima_semana_mes
                       ))
summary(depto4_teste)
coeftest(depto4_teste)

autoplot(ts(vendas_treino_soma$Vendas_soma, start = 1, end = 4386)) 
+ autolayer(fitted(depto4_teste))

##### H. Residues ############################################################## 

tsdiag(depto4_teste, gof.lag = 50)
shapiro.test(depto4_teste$residuals)
mean(depto4_teste$residuals)
sd(depto4_teste$residuals)
checkresiduals(depto4_teste)

##### I. Quality evaluation: error comparison # ##### ##### 

vendas_holdout_soma <- ddply(vendas_holdout,
                               .(Data, xreg1 = X_Ultimo_mes, 
                                 xreg2 = X_Primeira_semana_mes,
                                 Dia, Semana, Mes, Ano),
                               summarise,
                               Vendas_soma=sum(Vendas))

x = vendas_treino_soma
names(x)[names(x) == "Vendas_soma"] <- "a"
names(x)[names(x) == "X_Ultimo_mes"] <- "xreg1"
names(x)[names(x) == "X_Primeira_semana_mes"] <- "xreg2"           
        
y = forecast(depto4_teste, x$a, xreg = as.matrix(x[,c(5,3)]))

accuracy(y, as.matrix(vendas_holdout_soma[,c(2,3,8)]))



##### J. Forecasting

vendas_consolida = filter(vendas, separa != "eliminar")

vendas_consolida_soma <- ddply(vendas_consolida,
                             .(Data, xreg1 = X_Ultimo_mes, 
                               xreg2 = X_Primeira_semana_mes,
                               Dia, Semana, Mes, Ano),
                             summarise,
                             Vendas_soma=sum(Vendas))

depto4_final = 
  Arima(vendas_consolida_soma$Vendas_soma, 
        order=c(2,0,1), 
        seasonal=c(1,1,1, period=7), 
        xreg = cbind(vendas_consolida_soma$xreg1,
                     vendas_consolida_soma$xreg2  ))
summary(depto4_final)

futuro_xreg = matrix(c(rep(0, 21), rep(1,7), rep(0, 14)), ncol = 2,
                     dimnames = list(NULL,c("xreg1","xreg2")))
 
pred = forecast(depto4_final, xreg = futuro_xreg, h = 21)

autoplot(ts(vendas_consolida_soma$Vendas_soma, start = 1, end = 790)) + 
  autolayer(fitted(depto4_final)) +
  autolayer(pred$mean) + 
  autolayer(pred$lower) + 
  autolayer(pred$upper)

z = cbind(pred$mean, pred$lower, pred$upper)
write.table(z, "C.../z.csv", dec = ",")
