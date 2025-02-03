######### PRVI DIO - PONAVLJANJE #########

#Freeny:
freeny[1,]
#y lag.quarterly.revenue price.index
#1962.25 8.79236               8.79636     4.70997
#income.level market.potential
#1962.25       5.8211          12.9699

summary(freeny)
#y         lag.quarterly.revenue  price.index     income.level   market.potential
#Min.   :8.791   Min.   :8.791         Min.   :4.278   Min.   :5.821   Min.   :12.97   
#1st Qu.:9.045   1st Qu.:9.020         1st Qu.:4.392   1st Qu.:5.948   1st Qu.:13.01   
#Median :9.314   Median :9.284         Median :4.510   Median :6.061   Median :13.07   
#Mean   :9.306   Mean   :9.281         Mean   :4.496   Mean   :6.039   Mean   :13.07   
#3rd Qu.:9.591   3rd Qu.:9.561         3rd Qu.:4.605   3rd Qu.:6.139   3rd Qu.:13.12   
#Max.   :9.794   Max.   :9.775         Max.   :4.710   Max.   :6.200   Max.   :13.17

#Izračunati korelaciju između neka 2 vremenska niza:
cor(freeny$y, freeny$price.index)
#[1] -0.9895118
cor(freeny$y, freeny$income.level)
#[1] 0.9839382
cor(freeny$y,freeny$market.potential)
#[1] 0.9965928

#Napraviti regresijsku analizu:
reg_analiza_1 <- lm(freeny$price.index~freeny$income.level)  #lm koristimo za linearno modeliranje, freeny$y je odzivna varijabla(ono što zelimo predvidjeti)
plot(freeny$income.level, freeny$price.index, col=c("pink","purple"))
abline(reg_analiza_1, lwd=2)
summary(reg_analiza_1)
freeny$y
#Call:
#  lm(formula = freeny$y ~ freeny$price.index)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.06666 -0.03696 -0.01259  0.03773  0.08382 

#Coefficients:
#  Estimate Std. Error t value
#(Intercept)         19.8359     0.2528   78.46
#freeny$price.index  -2.3419     0.0562  -41.67
#Pr(>|t|)    
#(Intercept)          <2e-16 ***
#  freeny$price.index   <2e-16 ***
#  ---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.0462 on 37 degrees of freedom
#Multiple R-squared:  0.9791,	Adjusted R-squared:  0.9786 
#F-statistic:  1736 on 1 and 37 DF,  p-value: < 2.2e-16

#Predviđanje:
install.packages("forecast")
library(forecast)
fit <- lm(price.index~income.level, data=freeny)  #definiranje linearnog modela
f <- forecast(fit, h=5, level=c(80,95), newdata=freeny$income.level)
#forecast se koristi za generiranje prognoza
#h=5 : generiramo 5 vremenskih koraka unaprijed
#level = c(80,95) : nivo pouzdanosti postavllja na 80% i 95%
#prognoze zelimo generirati za freeny$income.level
plot(f,xlab="price.index", ylab="income.level")




######### DRUGI DIO - SAMOSTALNI RAD ###################
data(package = .packages(all.available = TRUE)) 
summary(airmiles)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#412    1580    6431   10528   17532   30514 

summary(beaver2)
#        day             time           temp           activ     
#Min.   :307.0   Min.   :   0   Min.   :36.58   Min.   :0.00  
#1st Qu.:307.0   1st Qu.:1128   1st Qu.:37.15   1st Qu.:0.00  
#Median :307.0   Median :1535   Median :37.73   Median :1.00  
#Mean   :307.1   Mean   :1446   Mean   :37.60   Mean   :0.62  
#3rd Qu.:307.0   3rd Qu.:1942   3rd Qu.:37.98   3rd Qu.:1.00  
#Max.   :308.0   Max.   :2350   Max.   :38.35   Max.   :1.00 

length(beaver2$time)
#[1] 100

#stvaramo skup podataka za prognozu:
s <- 1+sin(seq(-1.5,1.5,length.out=100))
scenarij=data.frame(income=s)
scenarij
plot(s,type="l")


fit2<- lm(time~activ, data=beaver2)
f2<- forecast(fit2, newdata=scenarij$income, level=c(80,95))
plot(f2)
lines(fitted(f2), col="pink")
summary(beaver2)
