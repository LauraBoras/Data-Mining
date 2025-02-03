install.packages("Java")
install.packages("partykit")
library(partykit)
library("RWeka")
install.packages("RWeka")#nemam instalirano pa instaliram
library("RWeka")
library("C50")
install.packages("C50")#instaliram ga

library("tree")
install.packages("tree")#instaliram ga

library("rpart")



####################### PRVI DIO_ PONAVLJANJE ################

pogresno_razvrstani_treniranje<-c()
pogresno_razvrstani_testiranje<-c()
#u ova dva vektora unosit ćemo broj pogrešno razvrstanih
#elemenata na skupovima za treniranje i testiranje



skup<-sample(2,nrow(iris),rep=TRUE,prob=c(0.8,0.2))
skup_za_treniranje<-iris[skup==1,]
skup_za_testiranje<-iris[skup==2,]


perunika_stablo_jedan<-J48(Species~.,data=skup_za_treniranje)
perunika_stablo_jedan

#J48 pruned tree
#------------------
  
#Petal.Width <= 0.6: setosa (39.0)
#Petal.Width > 0.6
#|   Petal.Length <= 4.7: versicolor (35.0/1.0)
#|   Petal.Length > 4.7
#|   |   Petal.Length <= 5
#|   |   |   Sepal.Width <= 3: virginica (8.0/1.0)
#|   |   |   Sepal.Width > 3: versicolor (2.0)
#|   |   Petal.Length > 5: virginica (38.0)

#Number of Leaves  : 	5

#Size of the tree : 	9

plot(perunika_stablo_jedan)

#evaluacija
eval_stablo_jedan_treniranje<-evaluate_Weka_classifier(perunika_stablo_jedan,newdata=skup_za_treniranje)
eval_stablo_jedan_treniranje

#=== Summary ===
  
#  Correctly Classified Instances         120               98.3607 %
#Incorrectly Classified Instances         2                1.6393 %
#Kappa statistic                          0.9753
#Mean absolute error                      0.0202
#Root mean squared error                  0.1004
#Relative absolute error                  4.5604 %
#Root relative squared error             21.3563 %
#Total Number of Instances              122     

#=== Confusion Matrix ===
  
#  a  b  c   <-- classified as
#39  0  0 |  a = setosa
#0 36  1 |  b = versicolor
#0  1 45 |  c = virginica

pogresno_razvrstani_treniranje["1"]<-2

eval_stablo_jedan_testiranje<-evaluate_Weka_classifier(perunika_stablo_jedan,newdata=skup_za_testiranje)
eval_stablo_jedan_testiranje

#=== Summary ===
  
#  Correctly Classified Instances          25               89.2857 %
#Incorrectly Classified Instances         3               10.7143 %
#Kappa statistic                          0.8337
#Mean absolute error                      0.0753
#Root mean squared error                  0.2466
#Relative absolute error                 18.3487 %
#Root relative squared error             54.6876 %
#Total Number of Instances               28     

#=== Confusion Matrix ===
  
#  a  b  c   <-- classified as
#11  0  0 |  a = setosa
#0 10  3 |  b = versicolor
#0  0  4 |  c = virginica


pogresno_razvrstani_testiranje["1"]<-3



#######2.stablo
perunika_stablo_dva<-C5.0(Species ~ .,data=skup_za_treniranje)
perunika_stablo_dva


#Call:
#  C5.0.formula(formula = Species ~ ., data
 #              = skup_za_treniranje)

#Classification Tree
#Number of samples: 122 
#Number of predictors: 4 

#Tree size: 5 

#Non-standard options: attempt to group attributes

plot(perunika_stablo_dva)
eval_stablo_dva_treniranje<-predict(perunika_stablo_dva,newdata=skup_za_treniranje,type="class")
eval_stablo_dva_treniranje
table(eval_stablo_dva_treniranje,skup_za_treniranje$Species)
pogresno_razvrstani_treniranje["2"]<-2

eval_stablo_dva_testiranje<-predict(perunika_stablo_dva,newdata=skup_za_testiranje,type="class")
eval_stablo_dva_testiranje
table(eval_stablo_dva_testiranje,skup_za_testiranje$Species)
pogresno_razvrstani_testiranje["2"]<-6



#########3.stablo
perunika_stablo_tri<-tree(Species~.,data=skup_za_treniranje)
plot(perunika_stablo_tri)
summary(perunika_stablo_tri)

#Classification tree:
#  tree(formula = Species ~ ., data = skup_za_treniranje)
#Variables actually used in tree construction:
#  [1] "Petal.Length" "Sepal.Length" "Sepal.Width" 
#Number of terminal nodes:  6 
#Residual mean deviance:  0.1012 = 11.73 / 116 
#Misclassification error rate: 0.02459 = 3 / 122 

pogresno_razvrstani_treniranje["3"]<-3 #misclassification error

perunika_stablo_tri_testiranje<-tree(Species~.,data=skup_za_testiranje)
summary(perunika_stablo_tri_testiranje)
#Classification tree:
#  tree(formula = Species ~ ., data = skup_za_testiranje)
#Variables actually used in tree construction:
#  [1] "Petal.Length"
#Number of terminal nodes:  3 
#Residual mean deviance:  0.2002 = 5.004 / 25 
#Misclassification error rate: 0.03571 = 1 / 28 

pogresno_razvrstani_testiranje["3"]<-1 #misclassification error



#######4.stablo
install.packages("party")
library(party)
zavisnosti <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
perunika_stablo_cetiri <- ctree(zavisnosti, data=skup_za_treniranje)
plot(perunika_stablo_cetiri)
summary(perunika_stablo_cetiri)
# Length      Class       Mode 
# 1 BinaryTree         S4 

table(predict(perunika_stablo_cetiri), skup_za_treniranje$Species)
# setosa versicolor virginica
# setosa         39          0         0
# versicolor      0         36         4
# virginica       0          1        42
pogresno_razvrstani_treniranje["4"]<-5

perunika_stablo_cetiri_testiranje <- ctree(zavisnosti, data=skup_za_testiranje)
summary(perunika_stablo_cetiri_testiranje)
# Length      Class       Mode 
# 1 BinaryTree         S4 
table(predict(perunika_stablo_cetiri_testiranje), skup_za_testiranje$Species)
# setosa versicolor virginica
# setosa          11          0         0
# versicolor      0         13         4
# virginica       0          0        0
pogresno_razvrstani_testiranje["4"]<-4


##########5.stablo
library(rpart)
perunika_stablo_pet<-rpart(zavisnosti, data=skup_za_treniranje,control=rpart.control(minsplit=10))
table(predict(perunika_stablo_pet,type="class"),skup_za_treniranje$Species)
# setosa versicolor virginica
# setosa         39          0         0
# versicolor      0         34         1
# virginica       0          3        45
pogresno_razvrstani_treniranje["5"]<-4

perunika_stablo_pet_testiranje<-rpart(zavisnosti, data=skup_za_testiranje,control=rpart.control(minsplit=10))
table(predict(perunika_stablo_pet_testiranje,type="class"),skup_za_testiranje$Species)
#setosa versicolor virginica
#setosa         11          0         0
#versicolor      0         12         0
#virginica       0          1         4

pogresno_razvrstani_testiranje["5"]<-1

pogresno_razvrstani_treniranje
#1 2 3 4 5 
#2 2 3 5 4 

pogresno_razvrstani_testiranje
#1 2 3 4 5 
#3 6 1 4 1 

#najveću pogrešku na skupu za treniranje ima 4.stablo
#najveću pogrešku na skupu za testiranje ima 2.stablo


#### Stabla poredana po pogreški izvršavanja od najmanjih do najvećih
pogresno_razvrstani_treniranje[order(unlist(pogresno_razvrstani_treniranje))]
#1 2 3 5 4 
#2 2 3 4 5 

pogresno_razvrstani_testiranje[order(unlist(pogresno_razvrstani_testiranje))]
#3 5 1 4 2 
#1 1 3 4 6 


####################### DRUGI DIO - SAMOSTALNI RAD ##################################3

## VOLCANO
str(volcano)
#Radi se o numeričkim podacima pa se ne može obaviti razvrstavanje


##MTCARS
str(mtcars)
#'data.frame':	32 obs. of  11 variables:
#  $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
#$ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
#$ disp: num  160 160 108 258 360 ...
#$ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
#$ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
#$ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
#$ qsec: num  16.5 17 18.6 19.4 17 ...
#$ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
#$ am  : num  1 1 1 0 0 0 0 0 0 0 ...
#$ gear: num  4 4 4 3 3 3 3 4 4 4 ...
#$ carb: num  4 4 1 1 2 1 4 2 2 4 ...

#dataframe s numeričkim vrijednostima


mtcars_stablo<-J48(as.factor(vs)~.,data=mtcars)
plot(mtcars_stablo)
summary(mtcars_stablo)
#=== Summary ===
  
#  Correctly Classified Instances          31               96.875  %
#Incorrectly Classified Instances         1                3.125  %
#Kappa statistic                          0.936 
#Mean absolute error                      0.0592
#Root mean squared error                  0.1721
#Relative absolute error                 12.0189 %
#Root relative squared error             34.6834 %
#Total Number of Instances               32     

#=== Confusion Matrix ===
  
#  a  b   <-- classified as
#18  0 |  a = 0
#1 13 |  b = 1



##INFERT
str(infert)
#'data.frame':	248 obs. of  8 variables:
#  $ education     : Factor w/ 3 levels "0-5yrs","6-11yrs",..: 1 1 1 1 2 2 2 2 2 2 ...
#$ age           : num  26 42 39 34 35 36 23 32 21 28 ...
#$ parity        : num  6 1 6 4 3 4 1 2 1 2 ...
#$ induced       : num  1 1 2 2 1 2 0 0 0 0 ...
#$ case          : num  1 1 1 1 1 1 1 1 1 1 ...
#$ spontaneous   : num  2 0 0 0 1 1 0 0 1 0 ...
#$ stratum       : int  1 2 3 4 5 6 7 8 9 10 ...
#$ pooled.stratum: num  3 1 4 2 32 36 6 22 5 19 ...

infert[1:2,]
odabrani<-sample(2,nrow(infert),replace=TRUE,prob=c(0.8,0.2))
treniranje<-infert[odabrani==1,]
testiranje<-infert[odabrani==2,]
infert_stablo<-J48(education~.,data=treniranje)
plot(infert_stablo)
summary(infert_stablo)

#=== Summary ===
  
#  Correctly Classified Instances         193              100      %
#Incorrectly Classified Instances         0                0      %
#Kappa statistic                          1     
#Mean absolute error                      0     
#Root mean squared error                  0     
#Relative absolute error                  0      %
#Root relative squared error              0      %
#Total Number of Instances              193     

#=== Confusion Matrix ===
  
#  a  b  c   <-- classified as
#10  0  0 |  a = 0-5yrs
#0 97  0 |  b = 6-11yrs
#0  0 86 |  c = 12+ yrs

evaluacija_infert<-evaluate_Weka_classifier(infert_stablo,newdata=testiranje)
evaluacija_infert

#=== Summary ===
  
#  Correctly Classified Instances          55              100      %
#Incorrectly Classified Instances         0                0      %
#Kappa statistic                          1     
#Mean absolute error                      0     
#Root mean squared error                  0     
#Relative absolute error                  0      %
#Root relative squared error              0      %
#Total Number of Instances               55     

#=== Confusion Matrix ===
  
#  a  b  c   <-- classified as
#2  0  0 |  a = 0-5yrs
#0 23  0 |  b = 6-11yrs
#0  0 30 |  c = 12+ yrs

###WOMEN
str(women)
#'data.frame':	15 obs. of  2 variables:
#$ height: num  58 59 60 61 62 63 64 65 66 67 ...
#$ weight: num  115 117 120 123 126 129 132 135 139 142 ...

#Nema smisla razvrstavanje u kategorije 




############### TH.data
install.packages("TH.data")
library(TH.data)
data(package="TH.data")

#Data sets in package ‘TH.data’:
  
#  GBSG2                   German Breast Cancer Study Group 2
#GlaucomaM               Glaucoma Database
#Westbc                  Breast Cancer Gene Expression
#birds                   Habitat Suitability for Breeding Bird
#Communities
#bodyfat                 Prediction of Body Fat by Skinfold Thickness,
#Circumferences, and Bone Breadths
#geyser                  Old Faithful Geyser Data
#mammoexp                Mammography Experience Study
#mn6.9                   I.Q. and attitude towards science
#sphase                  S-phase Fraction of Tumor Cells
#wpbc                    Wisconsin Prognostic Breast Cancer Data

str(TH.data::GBSG2)
GBSG2_c4.5_stablo<-J48(horTh~.,data=TH.data::GBSG2)
plot(GBSG2_c4.5_stablo)
summary(GBSG2_c4.5_stablo)


str(TH.data::GlaucomaM)
GlaucomaM_c4.5_stablo<-J48(Class~.,data=TH.data::GlaucomaM)
plot(GlaucomaM_c4.5_stablo)
summary(GlaucomaM_c4.5_stablo)

str(TH.data::Westbc)
#ovaj skup podatak je lista, i sastoji se od num varijable i dataframea

str(TH.data::birds)
#skup podataka je dataframe koji se sastoji od numerickih varijabli



str(TH.data::bodyfat) 
#dataframe koji se sasatoji od numerickih varijabli


str(TH.data::geyser) 
#dataframe koji se sasatoji od numerickih varijabli i jedne klase


str(TH.data::mammoexp)
mammoexp_c4.5_stablo<-J48(SYMPT~.,data=TH.data::mammoexp)
plot(mammoexp_c4.5_stablo)
summary(mammoexp_c4.5_stablo)


str(TH.data::mn6.9)
mn6.9_c4.5_stablo<-J48(group~.,data=TH.data::mn6.9)
plot(mn6.9_c4.5_stablo)
summary(mn6.9_c4.5_stablo)


str(TH.data::sphase)
sphase_c4.5_stablo<-J48(as.factor(event)~.,data=TH.data::sphase)
plot(sphase_c4.5_stablo)
summary(sphase_c4.5_stablo)



str(TH.data::wpbc)
wpbc_c4.5_stablo<-J48(status~.,data=TH.data::wpbc)
plot(wpbc_c4.5_stablo)
summary(wpbc_c4.5_stablo)

summary(TH.data::wpbc)

######## Regresijsko stablo nad nekim skupom
#predvidanje kontinuirane varijable mean_perimeter iz skupa podataka wpbc iz TH.data

infert[1:5,]
zavisnosti_infert<-infert$education~infert$age+infert$parity+infert$induced+infert$case+infert$spontaneous+infert$stratum+infert$pooled.stratum
infert_regresijsko_stablo<-rpart(zavisnosti_infert,data=treniranje,control=rpart.control(minsplit=10))
attributes(infert_regresijsko_stablo)


############ Barem jednom stablu odrežite barem jedan čvor i usporedite pogrešku na skupu za treniranje i na
# skupu za testiranje
#odabrala sam 5.stablo
perunika_stablo_pet_rezano<-prune(perunika_stablo_pet,cp=0.002)
plot(perunika_stablo_pet_rezano)
table(predict(perunika_stablo_pet_rezano,type="class"),skup_za_treniranje$Species)

#              setosa versicolor virginica
#setosa         42          0         0
#versicolor      0         33         0
#virginica       0          3        41

#na skupu za treniranje imamo 3 pogrešno razvrstana objekta

perunika_stablo_pet_rezano_testiranje<-prune(perunika_stablo_pet_testiranje,cp=0.002)
plot(perunika_stablo_pet_rezano_testiranje)
table(predict(perunika_stablo_pet_rezano_testiranje,type="class"),skup_za_testiranje$Species)
#             setosa versicolor virginica
#setosa          8          0         0
#versicolor      0         14         1
#virginica       0          0         8

#na skupu za testiranje imamo 1 pogrešno razvrstan objekt