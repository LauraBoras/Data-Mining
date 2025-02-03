library(scatterplot3d)
library(rgl)  


################# PRVI DIO ##########################

set.seed(123) # radi ponovljivosti
n <- 150
m <- 15*n

theta <- seq(0, 2*pi, length=n)
phi <- seq(0, 2*pi, length=m)
a <-1
b<-2
c<-3

#Prvi skup podataka
x<- a*sinh(theta)*cos(phi)+rnorm(m, sd=0.5)
y<-b*sinh(theta)*sin(phi)+rnorm(m,sd=0.5)
z<-c*sinh(theta)+rnorm(m, sd=10)
podaci1<-cbind(x,y,z)
plot(podaci1)
scatterplot3d(x,y,z)


#Drugi skup podataka
x2 <- a*sinh(theta)*cos(phi)+rnorm(m, sd=10)
y2<-b*sinh(theta)*sin(phi)+rnorm(m,sd=7)
z2<-c*sinh(theta)+rnorm(m, sd=15)
u<- sqrt(sin(phi)*theta)+rnorm(m,sd=5)
v <- sqrt(cosh(phi)+theta) +rnorm(m,sd=3)
podaci2<- cbind(x2,y2,z2,u,v)





################ DRUGI DIO ############################

#Projekcija podataka na glavne osi:

?princomp


##PODATCI 1
glavni_comp<-princomp(podaci1)
glavni_comp

#Standard deviations:
#  Comp.1    Comp.2    Comp.3 
#191.05948 108.69344  54.36771 
# 3  variables and  2250 observations.

glavni_projekcije <- glavni_comp$scores  #Projekcije na svojstvene vektore
glavni_projekcije


#pca1~pca2
xlim<-c(0,1)
ylim<-c(0,1)

plot(glavni_projekcije[,1], glavni_projekcije[,2], main="Projekcije na prve 2 glavne osi PCA.", xlab="PCA os 1", ylab="PCA os 2")
segments(xlim[1],ylim[1],xlim[2],ylim[1], col="purple",lwd=8)
segments(xlim[1], ylim[1], xlim[1], ylim[2], col="pink", lwd=8)


#pca2~pca3
xlim<- c(0,1)
ylim<- c(0,1)
plot(glavni_projekcije[,2], glavni_projekcije[,3], main="Projekcije na druge 2 glavne osi PCA.", xlab="PCA os 2", ylab="PCA os 3")
segments(xlim[1],ylim[1],xlim[2],ylim[1], col="purple",lwd=8)
segments(xlim[1], ylim[1], xlim[1], ylim[2], col="pink", lwd=8)



##PODATCI 2
glavni_comp2<-princomp(na.omit(podaci2))

glavni_osi2 <- glavni_comp2$loadings  #osi (svojstvene vrijednosti)
glavni_projekcije2 <- glavni_comp2$scores  #projekcije na svojstvene vektore

glavni_osi2
#Comp.1 Comp.2 Comp.3 Comp.4 Comp.5
#SS loadings       1.0    1.0    1.0    1.0    1.0
#Proportion Var    0.2    0.2    0.2    0.2    0.2
#Cumulative Var    0.2    0.4    0.6    0.8    1.0

#pca1~pca2
plot(glavni_projekcije2[,1], glavni_projekcije2[,2], main="Projekcije na os1 i os2.", xlab="PCA os 1", ylab="PCA os 2")
segments(xlim[1],ylim[1],xlim[2],ylim[1], col="purple",lwd=8)
segments(xlim[1], ylim[1], xlim[1], ylim[2], col="pink", lwd=8)

#pca2~pca3
plot(glavni_projekcije2[,2], glavni_projekcije2[,3], main="Projekcije na os2 i os3.", xlab="PCA os 2", ylab="PCA os 3")
segments(xlim[1],ylim[1],xlim[2],ylim[1], col="purple",lwd=8)
segments(xlim[1], ylim[1], xlim[1], ylim[2], col="pink", lwd=8)

#pca3~pca4
plot(glavni_projekcije2[,3], glavni_projekcije2[,4], main="Projekcije na os3 i os4.", xlab="PCA os 3", ylab="PCA os 4")
segments(xlim[1],ylim[1],xlim[2],ylim[1], col="purple",lwd=8)
segments(xlim[1], ylim[1], xlim[1], ylim[2], col="pink", lwd=8)

#pca4~pca5
plot(glavni_projekcije2[,4], glavni_projekcije2[,5], main="Projekcije na os4 i os5.", xlab="PCA os 4", ylab="PCA os 5")
segments(xlim[1],ylim[1],xlim[2],ylim[1], col="purple",lwd=8)
segments(xlim[1], ylim[1], xlim[1], ylim[2], col="pink", lwd=8)



###PODATCI 3

#scaler je vrijednost koja pokazuje da li bi varijable trebale biti skalirane
#da bi imale jedinicnu varijaciju prije nego sto se analiza provede


podatci3 <- podatci1 + 1
prcomp(podatci3, scale=TRUE)   #scaler=TRUE oznacava da se podaci skaliraju prije izvodjenja PCA
#Rotation (n x k) = (3 x 3):
#  PC1          PC2         PC3
#x1  0.70592225 -0.058386416 -0.70587874
#y1 -0.04053767 -0.998293498  0.04203319
#z1 -0.70712833 -0.001057481 -0.70708444


podatci3 <- podatci1 + 1
prcomp(podatci3, scale=FALSE)  ##scaler=TRUE oznacava da se podaci ne skaliraju prije izvodjenja PCA
#Rotation (n x k) = (3 x 3):
#  PC1          PC2         PC3
#x1  0.94020472 -0.052098045 -0.33660196
#y1 -0.04818217 -0.998638670  0.01998213
#z1 -0.33718477 -0.002569077 -0.94143499


#center je vrijednost koja pokazuje jesu li varijable pomaknute kako bi bile centrirane na nula
podatci3 <- podatci1 + 1
prcomp(podatci3, center=TRUE) #provodi analizu glavnih komponenti na podacima podatci3, pri čemu ih prethodno sredi tako da se od svake vrijednosti značajke oduzme srednja vrijednost te značajke.
#Rotation (n x k) = (3 x 3):
#  PC1          PC2         PC3
#x1  0.94020472 -0.052098045 -0.33660196
#y1 -0.04818217 -0.998638670  0.01998213
#z1 -0.33718477 -0.002569077 -0.94143499

podatci3 <- podatci1 + 1
prcomp(podatci3, center=FALSE)
#Rotation (n x k) = (3 x 3):
#  PC1        PC2        PC3
#x1 -0.5103478 0.59217330 -0.6235992
#y1  0.3497087 0.80538542  0.4786002
#z1  0.7856519 0.02617444 -0.6181148



podatci3 <- podatci1 + 1
prcomp(podatci3, cor=TRUE) # izracunat ce se matrica korelacije između značajki prije nego što se izvrši analiza glavnih komponenti (PCA)


podatci3 <- podatci1 + 1
prcomp(podatci3, cor=FALSE) # PCA se provodi na standardiziranim podacima, što znači da će se svaka značajka skalirati 
#na svoju standardnu devijaciju, ali neće se uzimati u obzir međusobne korelacije između značajki.




#princomp može biti korisna kada želimo brzo provesti PCA bez potrebe za podešavanjem opcija poput centriranja i skaliranja podataka. Također, 
#može biti korisno u situacijama kada želimo samo osnovne informacije o glavnim komponentama i varijancama, bez potrebe za dodatnim detaljima. 

podatci3 <- podatci1 + 1
princomp(podatci3)

#Call:
#  princomp(x = podatci3)

#Standard deviations:
#  Comp.1   Comp.2   Comp.3 
#8.148850 7.713243 2.575884 

#3  variables and  4000 observations.
