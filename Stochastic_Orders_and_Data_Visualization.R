#ZADATAK 1 - WOMEN
women

#Statističke informacije o podacima:
summary(women)

#height         weight     
#Min.   :58.0   Min.   :115.0  
#1st Qu.:61.5   1st Qu.:124.5  
#Median :65.0   Median :135.0  
#Mean   :65.0   Mean   :136.7  
#3rd Qu.:68.5   3rd Qu.:148.0  
#Max.   :72.0   Max.   :164.0 

attach(women)
mean(height)
#[1] 65
median(height)
#[1] 65
range(height)
#[1] 58 72
min(height)
#[1] 58
max(height)
#[1] 72
quantile(height)
#0%  25%  50%  75% 100% 
#58.0 61.5 65.0 68.5 72.0 
quantile(height,c(.1,.5,.7))
#10%  50%  70% 
#59.4 65.0 67.8
var(height)
#[1] 20
sd(height)
#[1] 4.472136

mean(weight)
#[1] 136.7333
median(weight)
#[1] 135
range(weight)
#[1] 115 164
min(weight)
#[1] 115
max(weight)
#[1] 164
quantile(weight)
#0%   25%   50%   75%  100% 
#115.0 124.5 135.0 148.0 164.0
quantile(weight,c(.1,.5,.7))
#10%   50%   70% 
#118.2 135.0 145.2
var(weight)
#[1] 240.2095
sd(weight)
#[1] 15.49869

detach(women)


#Osnovni prikaz podataka:
hist(women$height, main="Histogram parametra visina.")
hist(women$weight, main="Histogram parametra težina.")

plot(density(women$height), main="Razdioba gustoće vjerojatnosti parametra visina.")
plot(density(women$weight), main="Razdioba gustoće vjerojatnosti parametra težina.")

pie(table(women$height))
pie(table(women$weight))

barplot(table(women$height))
barplot(table(women$weight))


#Informacije o odnosu među podatcima
cov(women$height,women$weight)
#[1] 69
cor(women$height,women$weight)
#[1] 0.9954948


#Prikaz podataka:
with(women, plot(height,weight, col=height))  #veza između visine i težine

pairs(women)

install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(women$height, women$weight,color=women$height, pch=women$height)  #boja i oblik variraju u odnosu na visinu

library(lattice)
cloud(height~weight*height |weight , data=women,
      main="3D prikaz u ovisnosti o težini.")

levelplot(height~height*weight,data=women, col.regions=rainbow)

install.packages("rgl")
library(rgl)
plot3d(women$height, women$weight)

library("MASS")
parcoord(women, col=women$height)
parcoord(women, col=women$weight)

parallelplot(women, col=women$height)
parallelplot(women, col=women$weight)

install.packages(("ggplot2"))
library(ggplot2)
qplot(women$height, women$weight, data=women,
      color=height)  # odnos visine i težine



#ZADATAK 2 - ToothGrowth
ToothGrowth
names(ToothGrowth)
#[1] "len"  "supp" "dose"

#Statističke informacije o podacima:
summary(ToothGrowth)

#len        supp         dose      
#Min.   : 4.20   OJ:30   Min.   :0.500  
#1st Qu.:13.07   VC:30   1st Qu.:0.500  
#Median :19.25           Median :1.000  
#Mean   :18.81           Mean   :1.167  
#3rd Qu.:25.27           3rd Qu.:2.000  
#Max.   :33.90           Max.   :2.000 

attach(ToothGrowth)
mean(len)
#[1] 18.81333
median(len)
#[1] 19.25
range(len)
#[1]  4.2 33.9
min(len)
#[1] 4.2
max(len)
#[1] 33.9
quantile(len)
#0%    25%    50%    75%   100% 
#4.200 13.075 19.250 25.275 33.900 
quantile(len,c(.1,.5,.7))
#10%   50%   70% 
#8.11 19.25 23.87
var(len)
#[1] 58.51202
sd(len)
#[1] 7.649315

mean(dose)
#[1] 1.166667
median(dose)
#[1] 1
range(dose)
#[1] 0.5 2.0
min(dose)
#[1] 0.5
max(dose)
#[1] 2
quantile(dose)
#  0%  25%  50%  75% 100% 
#0.5  0.5  1.0  2.0  2.0 
quantile(dose,c(.1,.5,.7))
#10%   50%   70% 
#0.5   1.0   2.0
var(dose)
#[1] 0.3954802
sd(dose)
#[1] 0.6288722
detach(ToothGrowth)


#Osnovni prikaz podataka:
hist(ToothGrowth$len, main="Histogram parametra len.")
hist(ToothGrowth$dose, main="Histogram parametra dose.")

plot(density(ToothGrowth$len), main="Razdioba gustoće vjerojatnosti parametra len.")
plot(density(ToothGrowth$dose), main="Razdioba gustoće vjerojatnosti parametra dose.")

pie(table(ToothGrowth$len))
pie(table(ToothGrowth$dose))
pie(table(ToothGrowth$supp))

barplot(table(ToothGrowth$len))
barplot(table(ToothGrowth$dose))
barplot(table(ToothGrowth$supp))


#Informacije o odnosu među podatcima
cov(ToothGrowth$len,ToothGrowth$dose)
#[1] 3.861299
cor(ToothGrowth$len,ToothGrowth$dose)
#[1] 0.8026913


#Prikaz podataka:
with(ToothGrowth, plot(len,dose, col=len))

pairs(ToothGrowth)

scatterplot3d(ToothGrowth$len, ToothGrowth$dose,color=ToothGrowth$len, pch=ToothGrowth$len)

cloud(len~dose*dose |len , data=ToothGrowth,
      main="3D prikaz.")

levelplot(len~len*dose,data=ToothGrowth, col.regions=rainbow)

plot3d(ToothGrowth$len, ToothGrowth$supp, ToothGrowth$dose)

parallelplot(~ToothGrowth | ToothGrowth$len, data=ToothGrowth)
parallelplot(~ToothGrowth | ToothGrowth$dose, data=ToothGrowth)
parallelplot(~ToothGrowth | ToothGrowth$supp, data=ToothGrowth)

qplot(ToothGrowth$len, ToothGrowth$dose, data=ToothGrowth,
      color=len)

qplot(ToothGrowth$len, ToothGrowth$supp, data=ToothGrowth,
      color=len)



#ZADATAK 3 - Funkcije u R - u
statistic_info <- function(a){
  print(var(a[[1]]))
  print(sd(a[[1]]))
  print(max(a[[1]]))
  print(min(a[[1]]))
}
statistic_info(women)
statistic_info(ToothGrowth)

prikaz <- function(b){
  hist(b[[1]], main="Histogram prvog parametra.")
  readline(prompt="Pritisni [Enter] za nastavak")
  plot(density(b[[1]]), main="Razdioba")
  pie(b[[1]])
}

prikaz(women)




