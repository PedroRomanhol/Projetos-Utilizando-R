##PACOTES##
install.packages('readxl')
install.packages('lmtest')
install.packages('ggplot')
install.packages('TSA')
install.packages('MASS')
install.packages('forecast')

library(readxl)
library(ggplot2)
library(TSA)
library(MASS)
library(forecast)
library(quadprog) 
library(lmtest)
library(tseries)
library(dplyr)
require(patchwork)


# LISTA 1

##LEITURA DE DADOS##
dados = read_excel("C:/Users/pedro/OneDrive/?rea de Trabalho/UFOP/2022-2/S?ries Temporais/Precip_Uberlandia_2015-2017.xlsx")

##TRANSFORMANDO EM SERIE TEMPORAL##
z = ts(dados[,3],start = c(2015, 1), frequency = 12)
plot.ts(z) #(a) Gráfico da série temporal.
#(b) A série parece ser estacionária, pois flutua em torno de 200 mm de precipitação.

##DIFEREN?AS
d1z= diff(z, lag = 1, differences = 1)
plot.ts(d1z)

d2z= diff(z, lag = 1, differences = 2)
plot.ts(d2z)
#(c) A série da diferença 1 parece ser estacionária, pois flutua em torno de 0 mm de precipitação.
#(c) A série da diferença 2 parece ser estacionária, pois flutua em torno de 0 mm de precipitação.


meses = c(rep(seq(1, 12), 3))
boxplot(z ~ meses)
#(d) A série aparenta SER sazonal
#(e) A sazonalidade da série parece ser aditiva.

# LISTA 2
  #a

# Alisamento exponencial
HoltWinters(z, beta=FALSE, gamma=FALSE, l.start=91.4)

## Modelo apenas com nível.
fit1 <- HoltWinters(z, beta=FALSE, gamma=FALSE)
fit1
fit1$SSE
fit1$fitted[1:10,]
plot(fit1)

  #Previsão
prev1 <- forecast(fit1, h=12)
prev1
plot(prev1)

  #Resíduos
plot.ts(prev1$residuals)
hist(prev1$residuals)
shapiro.test(prev1$residuals) #p-valor > 0.1, aceita-se H0: os dados seguem uma distribui??o normal

# Modelo com nível e tendência
fit2 <- HoltWinters(z, gamma=FALSE)
fit2
fit2$SSE
fit2$fitted[1:10,]
plot(fit2)

#Previsão
prev2 <- forecast(fit2, h=12)
prev2
plot(prev2)

#Resíduos
plot.ts(prev2$residuals)
hist(prev2$residuals)
shapiro.test(prev2$residuals) #p-valor > 0.1, aceita-se H0: os dados seguem uma distribuição normal


# Ajuste com nível, tendência e sazonalidade aditiva
fit3<- HoltWinters(z)
fit3$SSE
prev3 <- forecast(fit3, h=12)
plot(prev3)
plot(fit3)

#Resíduos
plot.ts(prev3$residuals)
hist(prev3$residuals)
shapiro.test(prev3$residuals) #p-valor > 0.1, aceita-se H0: os dados seguem uma distribuição normal

# Ajuste com nível, tendência e sazonalidade multiplicativa
fit4<- HoltWinters(z,seasonal="multiplicative")
fit4$SSE
prev4 <- forecast(fit4, h=12)
plot(prev4)
plot.ts(prev4$residuals)

#Tab erro quadrático
tab = as.data.frame(cbind(c(fit1$SSE, fit2$SSE, fit3$SSE)))

  #b
### Modelo de Auto-Regressão
par(mfrow = c(1, 2))
acf(z)
pacf(z)

ar(dados[, 3], aic = TRUE, method = 'yule-walker')
modelo_arima = arima(z, c(9, 0, 0))
modelo_arima

### Análise de Reséduos do Modelo de Auto-Regressão
acf(modelo_arima$residuals)
pacf(modelo_arima$residuals)
Box.test(modelo_arima$residuals, type = 'Ljung-Box')
shapiro.test(modelo_arima$residuals)

#Lista 3
  #Relembrando a base
z = ts(dados[,3],start = c(2015, 1), frequency = 12)
plot.ts(z)

#Estacionarizar variância
boxcox(z ~ 1)

y = log(z) #logaritmica
w = sqrt(z) #raíz
par(mfrow = c(1,2))
plot.ts(y)
plot.ts(w)

par(mfrow=c(1,2))
acf(y,lag.max =80)
pacf(y,lag.max =80)

#Estacionarizar média
adf.test(y) #indentificamos que a série não é estacioníria

nsdiffs(y) #sozonalidade

w<- diff(diff(y, lag = 12, differences = 1),lag = 1, differences = 1)
# w<- diff(y,lag = 1, differences = 1) # para séries não sazonais
adf.test(w)

par(mfrow=c(1,1))
plot.ts(w)

#Sazonalidade
meses = c(rep(seq(1, 12), 3))
boxplot(z ~ meses, ylab = 'Precipita??o em mm', xlab = 'Meses',
        main = 'Box-Plot mensal para precipita??o em Uberl?ndia entre 2015 ? 2017')

#Vamos dar uma olhada nas funções de autocorrelação e auto-correlação parcial novamente:

par(mfrow=c(1,2))
acf(z,lag.max =80)
pacf(z,lag.max =80)

#### Vamos dar uma olhada no periodograma:
par(mfrow=c(1,1))
periodogram(z,log='no',plot=TRUE,ylab="Periodogram", xlab="Frequency",lwd=2) 

nsdiffs(z) #sozonalidade

#Agora, vamos ajustar um SARIMA(1,1,1)(2,0,2)

modelo1 <- arima(y, order=c(p=1, d=1, q=1),seasonal = list(order = c(1, 1, 0), period = 12))  
modelo1 

resi<- modelo1$residuals # Aqui temos os resíduos do modelo

#Diagnostico
  #Indepedência dos resíduos
Box.test(resi,type="Ljung-Box") #A hipótese nula diz que os resíduos são independentes.
tsdiag(modelo1,gof.lag=10)

qqnorm(resi, main = 'Normal Q-Q Plot')
qqline(resi)
shapiro.test(resi)


#Agora, construiremos os intervalos preditivos.
  #Para tanto, usaremos a aproximação pela normal:
    #previsão +- 2*desvio-padr?o.  

h<- 12  # Previsões para os 12 primeiros meses no futuro:

previsoes<- predict(modelo1,n.ahead=h)

adj<- ts(c(y,as.numeric(previsoes$pred)),start = c(2015, 1), frequency = 12) # valores preditos para o período observado

#Gráfico com os valores preditos, previsões a intervalos preditivos:

plot.ts(adj,ylim=c(min(y,min(previsoes$pred-2*previsoes$se)),max(y,max(previsoes$pred+2*previsoes$se))))
lines(previsoes$pred,col="red")
lines(previsoes$pred+2*previsoes$se,col="blue",lty=3)
lines(previsoes$pred-2*previsoes$se,col="blue",lty=3)

#Por fim, vamos usar uma função do R que serve para ajustar um modelo SARIMA automaticamente para compararmos com o modelo que obtemos:

auto_fit <- auto.arima(y)
auto_fit