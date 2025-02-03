#Promotrimo
assign("a", 1)
1 -> b
c(1,2,3)
x <- c(4,5,6)
(y <- c(7,8,9))



x[length(x)+2]
# kada od vektora zatražimo vrijednost na indeksu koji prelazi opseg/duljinu vektora vrati nam NA
#NA (Not Available) je vrijednost koja se koristi kada podatci nisu dostupni, nedostaju ili su nepouzdani.


#Demonstracija vektorizacije funkcije:
factorial(x)


#Pretvorba i iznuda vrste vrijednosti
imena <- c("Ivana", "Ana", "Marija")
mode(imena)
brojevi1 <- c(1,2,3)
mode(brojevi1)
mode(brojevi1) <- "character"  #Naredba kojom je učinjena pretvorba vrste vrijednosti
brojevi1
mode(brojevi1)
brojevi2 <- c(1,2,"3")  #Naredba kojom je izvršena iznuda vrste vrijednosti
mode(brojevi2)


#Grupiranje i repliciranje niza
niz <- seq(from=5, to=15, by=0.7)
predmeti <- c('Matematika', 'Hrvatski', 'Likovni', 'Tjelesni')
predmeti <- rep(predmeti, each=1, times=3)

polozeni <- c('polozen','polozen', 'nije polozen')
polozeni <- rep(polozeni, each=1, times=4)
polozeni


#Repliciranje faktora
rep("a", 10)


#Tablice
r <-  c('m', 'f', 'f', 'm', 'm', 'm', 'f', 'f', 'm', 'f')
glas <- factor(c("podržava", "snažno podržava", "ne podržava", "ne podržava",
                 "podržava", "ne podržava", "podržava", "ne podržava",
                 "snažno podržava", "podržava"))
t <- table(r,glas)
t
prop.table(t, 1)   #relativna frekvencija
margin.table(t,2) #granična frekvencija

#Naredba sweep
sweep(t, 1, margin.table(t, 1), "/") 


#Uzorkovanje niza
l <- c(1:100)
sl <- sample(1:100,7)  #slucajnim odabirom
pozicija <- l[5]       #pozicijskim indeksiranjem
slijed <- l[3:7]      #slijednim indeksiranjem
logickom <- l[l<mean(l)]  #logickom maskom


#Rijecnik

#indeksiranje imenom
rijecnik <- c(Ana = 18, Maja=20, Klara=24) 
rijecnik

#indeksiranje praznionom
v = vector("numeric", 5)
v
v[]=4

#Odabir elementa
rijecnik["Ana"]
rijecnik[["Ana"]]

rijecnik[-1]
