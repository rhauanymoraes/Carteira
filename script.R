#Rhauany Catharine Moraes


rm(list=ls())

setwd("C:/Users/rhauany.moraes/Desktop/FINANÇAS")

#install.packages("quantmod")
library(quantmod)
library(ggplot2)

#Série de preços das ações da Vale
vale <- getSymbols("VALE3.SA", src = "yahoo", from = "2014-01-01", to = "2020-03-31", auto.assign = FALSE)

#Série de preços das ações Banco do Brasil
bb <- getSymbols("BBAS3.SA", src = "yahoo", from = "2014-01-01", to = "2020-03-31", auto.assign = FALSE)

#Retorno mensal de ambas as ações
vale_ret_mes <- monthlyReturn(vale[,6],type="log")
bb_ret_mes <- monthlyReturn(bb[,6], type = "log")

#Retirando os NA's
vale_ret_mes <- na.omit(vale_ret_mes)
bb_ret_mes <- na.omit(bb_ret_mes)

#Desvio-padrão de ambos os retornos mensais
sd(vale_ret_mes$monthly.returns)
sd(bb_ret_mes$monthly.returns)

summary(vale)
summary(bb)


#Variância de ambos os retornos mensais
var(vale_ret_mes$monthly.returns)
var(bb_ret_mes$monthly.returns)


#Correlação e covariança com os retornos mensais
cov(vale_ret_mes$monthly.returns,bb_ret_mes$monthly.returns)
cor(vale_ret_mes$monthly.returns,bb_ret_mes$monthly.returns)


#Expectativa dos retornos mensais
mean(vale_ret_mes$monthly.returns)
mean(bb_ret_mes$monthly.returns)


#Ativo livre de risco
selic <- c(((0.035+1)^(1/12))-1)

#IBC-BR
ibc <- read.csv("ibc.csv", sep=";", header = FALSE, stringsAsFactors = FALSE, dec = ",")
names(ibc) <- c("data","IBC","variação")

#Covariância dos retornos mensais com o IBC-Br
cov(vale_ret_mes$monthly.returns,ibc[,2])
cov(bb_ret_mes$monthly.returns, ibc[,2])

#Correlação dos retornos mensais com o IBC-Br
cor(vale_ret_mes$monthly.returns,ibc[,2])
cor(bb_ret_mes$monthly.returns,ibc[,2])

# Baixando as planilhas de sharpe ratio

portfólio_xy <- read.csv("portfolio_xy.csv", header = TRUE, sep = ";", dec = ",")
portfólio_r <- read.csv("portfolio_r.csv", header = TRUE, sep = ";", dec = ",")

#-------------------------------------------------X------------------------------------------------

#Gráficos

library("showtext")
font_add_google("Montserrat")
showtext.auto()

#Gráfico dos preços das ações

png("bb_price.png", width = 1000, height = 500)

ggplot(bb, aes(x = index(bb), y = bb[,6])) + 
  geom_line(color="#4bc8cd", size = .7) + 
  ggtitle("Preço das ações do Banco do Brasil") + 
  theme_minimal(base_size = 13, base_family = "Montserrat") +
  theme(
        panel.background = element_blank())+
  xlab("Data") + 
  ylab("Preço (R$)") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_breaks = "6 month", date_labels = "%m/%y")

dev.off()

png("vale_price.png", width = 1000, height = 500)

ggplot(vale, aes(x = index(vale), y = vale[,6])) + 
  geom_line(color="#dd51be", size = .5) + 
  ggtitle("Preço das ações da Vale") + 
  theme_minimal(base_size = 13, base_family = "Montserrat") +
  theme(
    panel.background = element_blank())+
  xlab("Data") + 
  ylab("Preço (R$)") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_breaks = "6 month", date_labels = "%m/%y")

dev.off()


#Gráfico dos retornos mensais

png("bb_retorno.png", width = 1000, height = 500)

ggplot(bb_ret_mes, aes(x = index(bb_ret_mes), y = bb_ret_mes[,1])) + 
  geom_line(color="#4bc8cd", size = 1) + 
  ggtitle("Retorno mensal das ações do Banco do Brasil") + 
  theme_minimal(base_size = 13, base_family = "Montserrat") +
  theme(
    panel.background = element_blank())+
  xlab("Data") + 
  ylab("Retorno") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_breaks = "6 month", date_labels = "%m/%y")

dev.off()


png("vale_retorno.png", width = 1000, height = 500)

ggplot(vale_ret_mes, aes(x = index(vale_ret_mes), y = vale_ret_mes[,1])) + 
  geom_line(color="#dd51be", size=1) + 
  ggtitle("Retorno mensal das ações da Vale") + 
  theme_minimal(base_size = 13, base_family = "Montserrat") +
  theme(
    panel.background = element_blank())+
  xlab("Data") + 
  ylab("Retorno") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_breaks = "6 month", date_labels = "%m/%y")

dev.off()

retornos <- merge(bb_ret_mes, vale_ret_mes)

png("retornos.png", width = 1000, height = 500)

ggplot(retornos, aes(x = index(retornos))) + 
  geom_line(aes(y = retornos[,1], color="Retorno mensal BB")) + 
  geom_line(aes(y = retornos[,2], color="Retorno mensal Vale")) +
  theme_minimal(base_size = 13, base_family = "Montserrat") +
  theme(panel.background = element_blank())+
  xlab("Data") + 
  ylab("") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_breaks = "6 month", date_labels = "%m/%y") 

dev.off()



#Comparando com o IBC-Br

bb_ret_mes$ibc <- c(ibc$variação)

png("ibc_bb.png", width = 1000, height = 500)

ggplot(bb_ret_mes, aes(x = index(bb_ret_mes))) + 
  geom_line(aes(y = bb_ret_mes[,1], color="Retorno mensal BB")) + 
  geom_line(aes(y = bb_ret_mes[,2], color="IBC-Br")) +
  theme_minimal(base_size = 13, base_family = "Montserrat") +
  theme(panel.background = element_blank())+
  xlab("Data") + 
  ylab("") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_breaks = "6 month", date_labels = "%m/%y") 

dev.off()


vale_ret_mes$ibc <- c(ibc$variação)

png("ibc_vale.png", width = 1000, height = 500)

ggplot(vale_ret_mes, aes(x = index(vale_ret_mes))) + 
  geom_line(aes(y = vale_ret_mes[,1], color="Retorno mensal Vale")) + 
  geom_line(aes(y = vale_ret_mes[,2], color="IBC-Br")) +
  theme_minimal(base_size = 13, base_family = "Montserrat") +
  theme(panel.background = element_blank())+
  xlab("Data") + 
  ylab("") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_breaks = "6 month", date_labels = "%m/%y")

dev.off()

#Portfólio R

png("port_r.png", width = 1000, height = 500)

ggplot(portfólio_r, aes(x = portfólio_r[,2], y = portfólio_r[,3])) + 
  geom_line(color="#dd51be", size = 1) + 
  ggtitle("Trade-off Retorno X Volatilidade (r,z)") + 
  theme_minimal(base_size = 13, base_family = "Montserrat") +
  theme(
    panel.background = element_blank())+
  xlab("Risco") + 
  ylab("Retorno Esperado Potfólio r,z") + 
  theme(plot.title = element_text(hjust = 0.5)) 

dev.off()

#Portfólio xy

png("port_xy.png", width = 1000, height = 500)

ggplot(portfólio_xy, aes(x = portfólio_xy[,2], y = portfólio_xy[,3])) + 
  geom_point(color="#dd51be", size=5) + 
  ggtitle("Trade-off Retorno X Volatilidade (x,y)") + 
  theme_minimal(base_size = 13, base_family = "Montserrat") +
  theme(
    panel.background = element_blank())+
  xlab("Risco") + 
  ylab("Retorno Esperado Potfólio x,y") + 
  theme(plot.title = element_text(hjust = 0.5)) 

dev.off()


tudo <- c(portfólio_r,portfólio_xy)
tudo <- as.data.frame(tudo)

png("intercepto.png", width = 1000, height = 500)

ggplot(tudo) + 
  geom_point(aes(x = tudo [,6], y = tudo [,7], color = "Portfólio x,y")) + 
  geom_line(aes(x = tudo[,2], y = tudo [,3], color = "Portfólio z")) +
  theme_minimal(base_size = 13, base_family = "Montserrat") +
  theme(panel.background = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Risco", y = "Retorno Esperado do Portfólio", title = "Trade-off Retorno X Volatilidade") +
  scale_y_continuous("Retorno Esperado do Portfólio", breaks = c(.0, 0.003,
                                                                 0.0035,
                                                                 .004, .0045,
                                                                 .005,.0055,
                                                                 .006, .0065,.007 ))

dev.off()
  


