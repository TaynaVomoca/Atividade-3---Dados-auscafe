#########################
#Pacotes
#########################

#########################
#instalação
#########################
install.packages("dplyr")
install.packages("fpp2")
install.packages("forecast")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("Metrics")
install.packages("tsibble")
install.packages("tsibbledata")
install.packages("TTR")

###########################
#Carregamento
###########################
library(dplyr)
library(fpp2)
library(forecast)
library(ggplot2)
library(lubridate)
library(Metrics)
library(tsibble)
library(tsibbledata)
library(TTR)
############################

# 1)
data("auscafe")

# 2)
autoplot(auscafe)

# 3)
auscafe %>% decompose() %>% autoplot()

# 4)
auscafe_treino <- window(auscafe, end = c(2009, 12))
auscafe_teste <- window(auscafe, start = c(2010, 1))

# 5)
auscafe_naive_teste <- naive(auscafe_teste)

# Vendo os valores previstos:
auscafe_naive_teste

# Calculandoo o erro:
mape(auscafe_naive_teste$fitted[2:93], auscafe_teste[2:93])*100 #começa no 2 pq o naive desconsidera o primeiro

#gráfico:
autoplot(auscafe_naive_teste)

# 6)

auscafe_snaive_teste <- snaive(auscafe)

# cálculo do erro 
mape(auscafe_snaive_teste$fitted[13:93], auscafe[13:93])*100

#gráfico
autoplot(auscafe_snaive_teste)

# 7)
# escolha da janela (ecolher jaanela com menor MAPE)
janela <- 2 #O 2 é o melhor pq deu resultado menor
media_movel_treino <- SMA(auscafe, janela)
mape(auscafe[janela:333], media_movel_treino[janela:333])*100

#previsões na amostra de teste
auscafe_media_movel <- SMA(auscafe, janela)

#erro
mape(auscafe_teste[janela:93], auscafe_media_movel[janela:93])*100

# 8)
#treino (encontrando alfa)
aes_treino <- ses(auscafe_treino)
summary(aes_treino)

alfa_aes <- 0.4936  

prev_aes_teste <- ses(auscafe_teste, alpha = alfa_aes)

#erro 
mape(prev_aes_teste$ fitted, auscafe)*100

#Vendo os valores previstos:
prev_aes_teste

#gráfico
autoplot(prev_aes_teste)

# 9)

#Encontrando alfa e bata
holt_treino <- holt(auscafe_treino)
summary(holt_treino)

alfa_holt <- 0.4033
beta_holt <- 1e-04

#previsoes na amostra de teste
prev_holt_teste <- holt(auscafe_teste, alpha = alfa_holt, beta = beta_holt)

#erro
mape(prev_holt_teste$fitted, auscafe)*100

#Vendo os valores previstos:
prev_holt_teste

#grafico
autoplot(prev_holt_teste)

# 10)
#encontrando os valores de alfa, beta e gama
hw_treino <- hw(auscafe_treino)
summary(hw_treino)

alfa_hw <- 0.4854
beta_hw <- 0.0192 
gama_hw <- 0.2423


#Previsoes na amostra de teste
prev_hw_teste <- hw(auscafe_teste, alpha = alfa_hw, beta = beta_hw, gamma = gama_hw)

#erro
mape(prev_hw_teste$fitted, auscafe_teste)*100

#Vendo os valores previstos:
prev_hw_teste

#grafico
autoplot(prev_hw_teste)

#11)
mape_tabela <- data.frame(
  Metodo = c("Naive", "Naive Sazonal", "AES", "Holt", "Holt-Winters"),
  MAPE = c(
    mape(auscafe_naive_teste$fitted[2:93], auscafe_teste[2:93]) * 100,
    mape(auscafe_snaive_teste$fitted[13:93], auscafe[13:93]) * 100,
    mape(prev_aes_teste$fitted, auscafe) * 100,
    mape(prev_holt_teste$fitted, auscafe) * 100,
    mape(prev_hw_teste$fitted, auscafe_teste) * 100
  )
)
mape_tabela <- mape_tabela[order(mape_tabela$MAPE), ]
print(mape_tabela)


#12)
melhor_metodo <- mape_tabela[1, "Metodo"]
print(paste("O melhor método é:", melhor_metodo))

