#INSTALIRANJE PAKETA:
install.packages('fpc')
install.packages('cluster')
library(cluster)
library(fpc)




############### PRVI DIO - PONAVLJANJE #################################



#Prvo uklanjamo stupac informacija o vrsti pa zatim primjenjujemo algoritam
iris2 <- iris
iris2$Species <- NULL

############################################################# BIRAJUCI PARAMETAR K:

#################################### S 3:
rezultat1 <- kmeans(iris2, 3)

print(rezultat1)

#K-means clustering with 3 clusters of sizes 38, 50, 62

#Cluster means:
#  Sepal.Length Sepal.Width Petal.Length Petal.Width
#1     6.850000    3.073684     5.742105    2.071053
#2     5.006000    3.428000     1.462000    0.246000
#3     5.901613    2.748387     4.393548    1.433871

#Clustering vector:
#[1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#[48] 2 2 2 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#[95] 3 3 3 3 3 3 1 3 1 1 1 1 3 1 1 1 1 1 1 3 3 1 1 1 1 3 1 3 1 3 1 1 3 3 1 1 1 1 1 3 1 1 1 1 3 1 1
#[142] 1 3 1 1 1 3 1 1 3

#Within cluster sum of squares by cluster:
#  [1] 23.87947 15.15100 39.82097
#(between_SS / total_SS =  88.4 %)

#Available components:
  
 # [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"   
#[7] "size"         "iter"         "ifault"

names(rezultat1)
#[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"        
#[8] "iter"         "ifault" 

rezultat1$cluster
#[1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#[48] 2 2 2 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#[95] 3 3 3 3 3 3 1 3 1 1 1 1 3 1 1 1 1 1 1 3 3 1 1 1 1 3 1 3 1 3 1 1 3 3 1 1 1 1 1 3 1 1 1 1 3 1 1
#[142] 1 3 1 1 1 3 1 1 3

rezultat1$centers
#Sepal.Length Sepal.Width Petal.Length Petal.Width
#1     6.850000    3.073684     5.742105    2.071053
#2     5.006000    3.428000     1.462000    0.246000
#3     5.901613    2.748387     4.393548    1.433871

rezultat1$totss  # suma kvadrata udaljenosti između svake točke i centroida njezinog klastera.
#[1] 681.3706

rezultat1$withinss  # suma kvadrata unutar klastera za svaki pojedinačni klaster.
#[1]   23.87947 15.15100 39.82097

rezultat1$tot.withinss  #suma rezultata rezultat1$withinss
#[1] 78.85144

rezultat1$betweenss
#[1] 602.5192

rezultat1$size  #broj podataka o svakom klasteru
#[1] 38 50 62

rezultat1$iter  # broj iteracija koje je algoritam proveo za klasteriranje podataka.
#[1] 2

rezultat1$ifault  #informacija o bilo kojoj detektiranoj grešci ili problema koji je algoritam mogao susresti tijekom izvođenja.
#[1] 0

table(iris$Species, rezultat1$cluster) #Matrica konfuzije
#           1  2  3
#setosa      0 50  0
#versicolor  2  0 48
#virginica  36  0 14


plot(iris2[c("Sepal.Length","Sepal.Width")],col=rezultat1$cluster)


######################## S 2:
rezultat2 <- kmeans(iris2, 2)

rezultat2$cluster
#[1]  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2
#[52] 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 1 2 2 2
#[103] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

table(iris$Species, rezultat2$cluster) #Matrica konfuzije
#            1  2
#  setosa     50  0
#versicolor  3 47
#virginica   0 50

plot(iris2[c("Sepal.Length","Sepal.Width")],col=rezultat2$cluster)

########################### S 4:

rezultat3<-kmeans(iris2, 4)
rezultat3$cluster
#[1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 4
#[52] 4 4 3 4 3 4 3 4 3 3 3 3 4 3 4 3 3 4 3 4 3 4 4 4 4 4 4 4 3 3 3 3 4 3 4 4 4 3 3 3 4 3 3 3 3 3 4 3 3 2 4
#[103] 2 4 2 2 3 2 2 2 4 4 2 4 4 4 4 2 2 4 2 4 2 4 2 2 4 4 2 2 2 2 2 4 4 2 2 4 4 2 2 2 4 2 2 2 4 4 4 4

table(iris$Species, rezultat3$cluster) #Matrica konfuzije
#           1  2  3  4
#  setosa     50  0  0  0
#versicolor  0  0 27 23
#virginica   0 27  1 22

plot(iris2[c("Sepal.Length","Sepal.Width")],col=rezultat3$cluster)


########################################################## NE BIRAJUCI PARAMETAR K:
rezultat4<-pamk(iris2)
names(rezultat4)
#[1] "pamobject" "nc"        "crit" 

rezultat4$pamobject
#Medoids:
#ID Sepal.Length Sepal.Width Petal.Length Petal.Width
#[1,]   8          5.0         3.4          1.5         0.2
#[2,] 127          6.2         2.8          4.8         1.8
#Clustering vector:
#[1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#[49] 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#[97] 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#[145] 2 2 2 2 2 2
#Objective function:
#  build      swap 
#0.9901187 0.8622026 

#Available components:
#[1] "medoids"    "id.med"     "clustering" "objective"  "isolation"  "clusinfo"   "silinfo"   
#[8] "diss"       "call"       "data" 

rezultat4$nc  #Broj grupa koje je pronašao
#[1] 2

rezultat4$crit  #vrijednost kriterija koji je korišten za određivanje optimalnog broja klastera.
#[1] 0.0000000 0.6857882 0.5528190 0.4896972 0.4867481 0.4703951 0.3390116 0.3318516 0.2918520 0.2918482

table(rezultat4$pamobject$clustering, iris$Species)  #Uočimo da algoritam nije razdvojio versicolor i virginica u odvojene grupe
#setosa versicolor virginica
#1     50          1         0
#2      0         49        50

layout(matrix(c(1,2),1,2)) # za crtanje 2 grafa jedan pored drugog
plot(rezultat4$pamobject)


############################# KORISTECI CENTORIDE I MEDOIDE KAO CENTRE GRUPE

########## CENTROIDI
plot(iris2[c("Sepal.Length","Sepal.Width")],col=rezultat1$cluster)
points(rezultat1$centers, col="purple")

############## MEDOIDI
rezultat5 <- pam(iris2, 3)  
table(rezultat5$clustering, iris$Species)   #Matrica konfuzije
#setosa versicolor virginica
#1     50          0         0
#2      0         48        14
#3      0          2        36

layout(matrix(c(1,2),1,2))         
plot(rezultat5)

rezultat6 <- clara(iris2, 3)              #naredbom clara izvlacimo vise uzoraka i na svakom koristimo pam
table(rezultat6$clustering, iris$Species) #matrica konfuzije
#setosa versicolor virginica
#1     50          0         0
#2      0         48        13
#3      0          2        37

layout(matrix(c(1,2),1,2))         
plot(rezultat6)
#manje su otporni na outliere od centroida


################################################################## HIJERARHIJSKO GRUPIRANJE

############ ODOZGO - Kreće od jedne grupe u kojoj pripadaju svi elementi početnog skupa
layout(1)  #vracamo da nam se ispisuje samo 1 graf
library(stats)
idx <- c(1:10,51:60,101:110)      # Radi jednostavnosti uzimamo samo neke elemente
hg <- hclust(dist(iris2[idx,]), method="average")
plot(hg, hang = -1, labels=iris$Species[idx])
rect.hclust(hg, k=3)                     #rezanje na 3 grupe

plot(hg, hang = -1, labels=iris$Species[idx])
rect.hclust(hg, k=4)                     #rezanje na 4 grupe

############ ODOZDO - Kreće od onoliko grupa koliko ima elemenata (svaki element je vlastita grupa)
dg <- diana(dist(iris2[idx,]))
plot(dg, labels=iris$Species[idx], which = 2)
rect.hclust(dg, k=3)                 #rezanje na 3 grupe


################################################################ DBSCAN

gg2 <- dbscan(iris2, eps=0.6, MinPts=20)
table(gg2$cluster, iris$Species)        #matrica zabune
#setosa versicolor virginica
#0      2          7        20
#1     48          0         0
#2      0         43        15
#3      0          0        15

plotcluster(iris2, gg2$cluster)      #rezultat naredbe je projekcija koja najbolje prikazuje odijeljenost grupa
plot(gg2, iris2)
#DBSCAN osjetljiv na izbor parametra i broja tocaka koji se u toj okolini moraju nalaziti





################################ DRUGI DIO -SAMOSTALAN RAD ############################################
N <- 25
g1 <- cbind(rnorm(N, mean=12), rnorm(N, mean=16))
g2 <- cbind(rnorm(N, mean=16), rnorm(N, mean=15))
g3 <- cbind(rnorm(N, mean=15), rnorm(N, mean=23))
g4 <- cbind(rnorm(N, mean=15), rnorm(N, mean=20))
g5 <- cbind(rnorm(N, mean=19), rnorm(N, mean=29))
g6 <- cbind(rnorm(N, mean=26), rnorm(N, mean=31))
g7 <- cbind(rnorm(N, mean=23), rnorm(N, mean=28))
g8 <- cbind(rnorm(N, mean=25), rnorm(N, mean=25))
tocke <- rbind(g1,g2,g3,g4,g5,g6,g7,g8)

tocke

####### Grupirajte nakupine jednim od algoritama iz obitelji k-centara
layout(1)
tocke_kc<-kmeans(tocke,4)
plot(tocke, col = tocke_kc$cluster)
points(tocke_kc$centers,col="purple",pch=16)

######## Nakupine više centara:

### Hijerarhijsko grupiranje
hg2 <- hclust(dist(tocke))  #izvršili hijerarhijsko grupiranje
centri<-cutree(hg2,k=5)
print(centri)
#[1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#[49] 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 1 2 1 2 1 1 1 2 2 1 2 1 2
#[97] 2 1 2 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
#[145] 4 4 4 4 4 4 4 5 5 4 5 5 4 5 5 4 5 5 5 5 3 5 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5
#[193] 5 5 5 5 5 5 5 5



################### U 2D prostoru napravite barem 5 nakupina različite gustoće (koristite kod iz prethodnog zadataka)
#gušće nakupine
prvaa<-cbind(rnorm(50,10),rnorm(50,15))
drugaa<-cbind(rnorm(40,5),rnorm(40,5))
trecaa<-cbind(rnorm(40,25),rnorm(40,10))
#rjeđe nakupine
cetvrtaa<-cbind(rnorm(5,2),rnorm(5,3))
petaa<-cbind(rnorm(5,7),rnorm(5,8))
tocke2<-rbind(prvaa,drugaa,trecaa,cetvrtaa,petaa)
tocke2

plot(NA, xlim=range(1:40), ylim=range(1:30))
points(prvaa, col='green')
points(drugaa, col='red')
points(trecaa, col='purple')
points(cetvrtaa, col='blue')
points(petaa, col="black")

#Kad se ne preklapaju:
gg2 <- dbscan(tocke2,eps=5,MinPts=10)
plotcluster(tocke2,gg2$cluster)

pam4<-pamk(tocke2)
layout(matrix(c(1,2),1,2)) # za crtanje 2 grafa jedan pored drugog
plot(pam4$pamobject)


#Kad se preklapaju
prvaaa<-cbind(rnorm(100,7,5),rnorm(100,7,5))
drugaaa<-cbind(rnorm(20,10),rnorm(20,7))
trecaaa<-cbind(rnorm(10,5),rnorm(10,5))


plot(NA, xlim=range(-1:20), ylim=range(1:30))
points(prvaaa, col="red")
points(drugaaa, col='green')
points(trecaaa, col='blue')



layout(1)
tocke3<-rbind(prvaaa,drugaaa,trecaaa)
gg3 <- dbscan(tocke3,eps=3,MinPts=5)
plotcluster(tocke3,gg3$cluster)
#Kod gušćih nakupina dbscan ih većinom stavi u jednu grupu

pam1<- pamk(tocke3)
layout(matrix(c(1,2),1,2)) # za crtanje 2 grafa jedan pored drugog
plot(pam1$pamobject)
