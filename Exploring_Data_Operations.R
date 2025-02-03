rm(list=ls())

#SIMULIRANJE PRIKUPLJENIH PODATAKA

osobe <- data.frame("dan"= rep(rep(c("ponedjeljak","utorak","srijeda","cetvrtak","petak","subota","nedjelja"),each=12),4),
                    "ime"=rep(rep(rep(c("Ana","Klara","Sara"),each=4),7),4),
                    "lokacija"=rep(rep(rep(c("Split","Omis","Makarska","Zadar"),each=1),7),4),
                    "tjedan"=rep(c("1.","2.","3.","4."),each=84))

osobe$Normalna_distribucija <-rnorm(336)
names(osobe)
dim(osobe)

#RAD S PODACIMA

subset(osobe, ime=="Sara" & tjedan=="1.")
subset(osobe, lokacija=="Split" & (ime=="Sara" | ime=="Klara"))
osobe[osobe$ime=="Sara", "dan"]

#Pridodavanje okvira u putanju koju pretražuje ljuska R-a
attach(osobe)
osobe[ime=="Sara", "dan"]   #naredba attach nam omogućuje da jednostavnije pritupimo podacima
#Isključivanje okvira iz putanje
detach(osobe)


#ISKUŠAJTE NEKE OD NAREDBI I KOMENTIRAJTE ŠTO RADE
head(osobe[,1:3])     #vraća vrijednosti prvog, drugog i treceg stupca za prvih 6 redaka
tail(osobe[,-(1:3)])  #vraća vrijednosti četvrtog i petog stupca za posljednjih 6 redaka
summary(osobe[,2:4])  #vraća statističku analizu za sve podatke iz drugog, trećeg i četvrtog stupca
min(osobe[,5])        #vraća najmanju vrijednost iz petog stupca
max(osobe[,5])        #vraća najveću vrijednost iz petog stupca
which.min(osobe[,5])  #vraća indeks (odnosno broj retka) na kojem se nalazi najmanja vrijednost iz petog stupca
sapply(osobe[,1:3],class)  #vraća klasu kojoj pripadaju podaci prvog, druggo i trećeg stupca
edit(osobe)        #omogućava nam uređivanje podataka u tablici


#OSNOVE CRTANJA U R-U
x=rnorm(100)
y=rnorm(100)
plot(x,y)
?png
?pdf
pdf("grafički_prikaz.pdf")                               #otvaramo pdf uređivač
plot(x,y,xlab="x-os",ylab="y-os",main="Ovisnost x o y")  #crtamo graf
dev.off()                                                #zatvaramo pdf uređivač
getwd()     #ispis putanje                                             #provjera radnog direktorija u kojem je spremljen graf



#ZADATAK

#Nacrtajmo crveni graf ovisnosti variajble x o variajbli y tako da točke budu povezane punom linijom i
#spremimo ga kao .pdf datoteku.
#Koristite se parametrima type i color2 da biste to postigli.

pdf("grafički_prikaz2.pdf")                               #otvaramo pdf uređivač
plot(x,y,xlab="x-os",ylab="y-os",main="Ovisnost x o y", col="red", type="s" )  #crtamo graf
dev.off() 

#Isprobavanje naredbi
x=seq(-pi,pi,length=50)
y=seq(-pi,pi,length=50)
f=outer(x,y,function(x,y)(x^2+y^2))   #primjenjujemo funkciju f na svaki elemnt vanjskog produkta vektora x i vektora y
contour(x,y,f)  #prikaz kontura funkcije f u xy ravnini
image(f)        
persp(x,y,f)    #3D prikaz

f2=outer(x,y,function(x,y)(-x^2+y^2))
persp(x,y,f2)
contour(x,y,f2)


