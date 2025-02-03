########################### PRVI DIO - PONAVLJANJE ########################

install.packages("forecast")
library(forecast)
str(nottem)
# Time-Series [1:240] from 1920 to 1940: 40.6 40.8 44.4 46.7 54.1 58.5 57.7 56.4 54.3 50.5 ...

#Jednostavan rastav signala pomoću naredba stl():
rez1 <- stl(nottem,s.window =50)
plot(rez1)
#Odnos trenda prema čitavom signalu:
plot(nottem, col="gray", ylab="Prosjecna temperatura", xlab="Mjeseci")
lines(rez1$time.series[,2], col="red", ylab="Trend")

#Još jedan način rastava signala:
rez2 <- decompose(nottem, type="additive")
plot(rez2)
plot(nottem, ylab="Prosjecna temperatura", xlab="Mjeseci")
lines(rez2$trend, col="red", ylab="Trend")

#AirPassengers:
head(AirPassengers)
#Jan Feb Mar Apr May Jun
#1949 112 118 132 129 121 135

str(AirPassengers)
#Time-Series [1:144] from 1949 to 1961: 112 118 132 129 121 135 148 148 136 119 ...

#Jednostavan rastav signala pomoću naredba stl():
plot(AirPassengers)
rez3<-stl(AirPassengers,s.window=7)
rez3
plot(rez3)

sez<-rez3$time.series[,"seasonal"]
plot(sez)

#AR MODEL
#Autoregresivan model (AR) je onaj model u kojem neko buduće
#stanje varijable želimo opisati kao linearnom kombinacijom nekih
#prethodnih stanja te iste varijable

ar1<-predict(ar(window(sez,c(1950,1),c(1956,12)),order.max=15), n.ahead=90)  #podskup od pocetka50-te te do kraja 55-te
plot(sez, xlim=c(1949,1970), ylim=c(-100,100))
lines(ar1$pred, col="red")


#ARIMA MODEL
#ARIMA modeli su najopćenitiji oblik modela jer njime možemo
#izraziti i AR i MA i ARMA model.
#Obično pišemo ARIMA(p,d,q) gdje p označava broj parametara AR
#modela, q broj parametara MA modela, a d stupanj diferencijacije

arima1 <- predict(arima(window(sez,c(1950,4),c(1956,12)), order=c(10,0,20)),n.ahead=100)
plot(sez, xlim=c(1949,1970), ylim=c(-100,100))
lines(arima1$pred, col="red")

#MA model -> ARIMA(0,0,q) ------  MA model koristi prethodne pogreške predviđanja.
ma1 <- predict(arima(window(sez,c(1950,5),c(1955,12)), order=c(0,0,20)),n.ahead=50)
plot(sez, xlim=c(1949,1960), ylim=c(-100,100))
lines(ma1$pred, col="red")

#AUTO ARIMA MODEL
aa1<- predict(auto.arima(window(sez, c(1950,4),c(1956,12))), n.ahead=100)
plot(sez, xlim=c(1949,1970), ylim=c(-100,100))
lines(aa1$pred, col="red")




################## DRUGI DIO - SAMOSTALNI RAD #############################

#Modelirajte signal koji se sastoji od trenda-ciklusa i sezonske komponente.
b <- rnorm(300,100,50)
head(b)
#[1]  87.14074 103.20732  88.93703  93.90453 126.96426
#[6]  97.86599

#Modeliramo podatke koji ce npr predstavljat broj ljudi zarazenih od neke bolesti
zarazeni <- ts(b, frequency=12, start=c(1990,1))
head(zarazeni)
#Jan       Feb       Mar       Apr       May
#1990  87.14074 103.20732  88.93703  93.90453 126.96426
#Jun
#1990  97.86599
plot(zarazeni)

#Dodajmo šum i kreirajmo novi signal:
sum_b <- jitter(b, factor = 25, amount=50)
head(sum_zarazeni)
#[1]  98.91610 151.91008  39.71545  70.66303  83.31780
#[6] 134.99257

sum_zarazeni <-ts(sum_b, frequency =12, start=c(1990,1))
head(sum_zarazeni)
#           Jan       Feb       Mar       Apr       May
#1990 110.65422  88.11941  96.93217  94.25294 161.34080
#Jun
#1990  96.19571

plot(sum_zarazeni)


#USPOREDIMO RASTAV JEDNOG I DRUGOG SIGNALA

#podaci bez suma (stl)
rezultat1_stl <- stl(zarazeni, s.window=7)
plot(rezultat1_stl)

#podaci sa sumom (stl)
rezultati2_stl <-stl(sum_zarazeni, s.window=7)
plot(rezultati2_stl)

#podaci bez suma (decompose)
rezultati1_dec <- decompose(zarazeni, type="additive")
plot(rezultati1_dec, ylab="Podatak za zarazne", xlab="Mjeseci")

#podaci sa sumom (decompose)
rezultati2_dec <- decompose(sum_zarazeni, type="additive")
plot(rezultati2_dec, ylab="Podatak za zarazne", xlab="Mjeseci")

#Usporedimo predvidjanja nekog ARIMA modela na jednom i drugom signalu
#bez suma
plot(zarazeni)
ar1<- predict(arima(window(zarazeni,1990,2000), order=c(10,2,5)), n.ahead=20)
lines(ar1$pred, col="red")


#sa sumom
plot(sum_zarazeni)
ar2<- predict(arima(window(sum_zarazeni,1990,2000), order=c(10,2,5)), n.ahead=20)
lines(ar2$pred, col="red")
