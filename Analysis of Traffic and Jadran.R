########################################################################################################
########################################## PRIPREMA ZA UZI #############################################
########################################################################################################


######################################    JADRAN    #########################################################


#Učitavanje podataka:
radni_dir<-"C:/Users/Administrator/Desktop/PRIPREMA_RUDARENJE/Jadran"
load(paste(radni_dir, "LonLat_sep.RData", sep="/"))
radni_dir<-"C:/Users/Administrator/Desktop/PRIPREMA_RUDARENJE/Jadran"
load(paste(radni_dir, "wind.RData", sep="/"))


#################### GRUPIRANJE:
#U zemljopisnom prostoru (širina i dužina) pronađite k grupa. Broj grupa (k) 
#možete proizvoljno odabrati ili pronaći nekom metodom grupiranja.

#Kreirajmo okvir:
prostor <- data.frame(cbind(lon[1,],lat[1,]))
names(prostor)<-c("lon", "lat")

library(fpc)
grupe<-pamk(prostor)
grupe$nc   #[1] 2

#Dodajmo stupac "grupa"
prostor$grupa<-grupe$pamobject$clustering

#Prikažimo sada točke obojane ovisno o grupi kojoj pripadaju:
plot(prostor$lon, prostor$lat, col=prostor$grupa)


################## RAZVRSTAVANJE:
#Koristeći se informacijom o grupama iz prethodnog zadataka, 
#razvrstajte podatke u te grupe nekom metodom razvrstavanja.

#osigurajmo da je varijabla po kojoj razvrsavamo kategorička:
prostor$grupa<-factor(prostor$grupa)

#Podjela na train i test:
set.seed(123)
uzorak<-sample(2, nrow(prostor), replace=TRUE, prob=c(0.7,0.3))
train<-prostor[uzorak==1,]
test<-prostor[uzorak==2,]

library(C50)
stablo<-C5.0(grupa~., data=train)
plot(stablo)

evaluacija<-predict(stablo, newdata = test)
table(test$grupa, evaluacija)
#evaluacija
#   1   2
#1 145   4
#2   1 176



#################### PREDVIĐANJE:
#Odaberite neku točku u prostoru i pokušajte predvidjeti njenu vrijednost na 
#temelju okolnih točaka u prostoru.

#Odaberimo točku:
plot(lon,lat)
which(lon==16 & lat==43)  #[1] 392

#Pronađimo okolne točke:
okolne<-1:1106
okolne<-okolne[-392]
okolne<-okolne[sqrt((lon[okolne]-lon[392])^2+(lat[okolne]-lat[392])^2)<0.2]
plot(lon,lat)
points(lon[392],lat[392],col="red",lwd=3)
points(lon[okolne], lat[okolne], col="purple",lwd=3)

#Predvidimo lon i lat odabrane točke pomoću okolnih:
a<-mean(lon[okolne])
a==lon[392]   #[1] TRUE

b<-mean(lat[okolne])
b==lat[392]    #[1] TRUE












##################################   PROMETNICE   #################################################

#Učitavanje podataka:
library(arrow)
load("C:/Users/Administrator/Desktop/PRIPREMA_RUDARENJE/Prometnice/prometnice.Rda")
prometnice<-aa
vozila<-read.csv("C:/Users/Administrator/Desktop/PRIPREMA_RUDARENJE/Prometnice/vozila_s10.csv")



######### GRUPIRANJE:
#[Prethodno izračunajte duljinu svaki prometnice] Grupirajte prometnice u ovisnosti o nekom 
#parametru (primjerice - gustoći prometa)
#Prikažite to grupiranje.

#Radi jednostavnosti smanjit ćemo skup podataka:
vozila<-vozila[1:1000,]

#Za svaku prometnicu odredimo broj vozila na traci:
broj_vozila<- unlist(lapply(unique(vozila$lane),function(x)  dim(vozila[vozila$lane==x,])[1]))

#Stvorimo novi okvir:
trake<-data.frame(cbind(unlist(unique(vozila$lane)), broj_vozila))
names(trake)<-c("id_trake", "broj_vozila")

#Dodajmo duljinu prometnice:
trake$duljina<-unlist(lapply(trake$id_trake, function(x) prometnice[prometnice$id==x,][1,3]))
#Buduči da nisu sve trake iz vozila u prometnicama, izbacimo one s nepotpunim podacima:
trake<-na.omit(trake)

#Dodajmo i stupac gustoca, no potrebno je osigurati da su duljina i broj vozila numeričke varijable:
str(trake)
trake$broj_vozila<-as.numeric(trake$broj_vozila)

trake$gustoca<-trake$broj_vozila/trake$duljina

#Sada kada smo pripremili podatke možemo preći na grupiranje:
library(fpc)
grupe2<-pamk(trake$gustoca)
grupe2$nc   #[1] 2

trake$grupa<-grupe2$pamobject$clustering
plot(trake$broj_vozila, trake$duljina, col=trake$grupa)



############ RAZVRSTAVANJE:
#Razvrstajte prometnice u nekoliko različitih kategorija, ovisno o duljini prometnice. 
#Usporedi kako to razvrstavanje odgovara gustoći prometa?

#Podjela na train i test:
set.seed(123)
uzorak2<-sample(2, nrow(trake), replace=TRUE, prob=c(0.7,0.3))
train2<-trake[uzorak2==1,]
test2<-trake[uzorak2==2,]


library(rpart)
stablo2<-rpart(duljina~broj_vozila+gustoca, data=train2)
eval2<-predict(stablo2, newdata=test2)
table(test2$duljina, eval2)

library(rpart.plot)
prp(stablo2)

layout(1)
plot(eval2, test2$gustoca, col=eval2)


############## PREDVIĐANJE:
#Odaberite neku prometnicu. Na temelju drugih prometnica (susjeda ili prometnica sa sličnim parametrima) 
#pokušajte predvidjeti broj vozila na toj prometnici u danom trenutku u vremenu.

#Odaberimo sve prometnice koje se javljaju u trenutku 0:
vozila2<-vozila[vozila$time==0,]

#Neka smo odabrali prvu prometnicu iz tog skupa:
vozila2$lane[6]   #[1] "128676689#5_0"

#Ponovimo postupak od prije, napravimo novi okvir no sada pomocu podataka iz vozila2:

#Za svaku prometnicu odredimo broj vozila na traci:
broj_vozila2<- unlist(lapply(unique(vozila2$lane),function(x)  dim(vozila2[vozila2$lane==x,])[1]))
#Stvorimo novi okvir:
trake2<-data.frame(cbind(unlist(unique(vozila2$lane)), broj_vozila2))
names(trake2)<-c("id_trake", "broj_vozila")
#Dodajmo duljinu prometnice:
trake2$duljina<-unlist(lapply(trake2$id_trake, function(x) prometnice[prometnice$id==x,][1,3]))
#Buduči da nisu sve trake iz vozila u prometnicama, izbacimo one s nepotpunim podacima:
trake2<-na.omit(trake)
#Dodajmo i stupac gustoca, no potrebno je osigurati da su duljina i broj vozila numeričke varijable:
str(trake2)
trake2$broj_vozila<-as.numeric(trake2$broj_vozila)
trake2$gustoca<-trake2$broj_vozila/trake2$duljina

#Predviđanje:
treniranje<-trake2[-6,]
testiranje<-trake2[6,]

library(nnet)
nn<-nnet(broj_vozila~duljina+gustoca, data=treniranje,range=0.1, size=3, decay=5e-1, maxit=100)
predict(nn, testiranje)  #----> 0.9870304
testiranje$broj_vozila   #----> 1

