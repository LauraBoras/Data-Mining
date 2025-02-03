#################################3 DRUGI DIO - SAMOSTALAN RAD ##################33
install.packages("nnet")
library(nnet)

install.packages("neuralnet")
library(neuralnet)

install.packages("NeuralNetTools")
library(NeuralNetTools)

########### MTCARS ###########

auti<-mtcars
auti
auti$cyl<- as.factor(auti$cyl)  #ciljnu varijablu pretvaramo u factor

skup1 <- sample(2, nrow(auti), replace= TRUE, prob=c(0.7,0.3))
skup_za_treniranje1 <- auti[skup1==1,]
skup_za_testiranje1 <- auti[skup1==2,]
cyl_testiranje <- skup_za_testiranje1$cyl
skup_za_testiranje1$cyl <- NULL

########### NNET

nn1_model1 <- nnet(cyl~.,data=skup_za_treniranje1, size = 2, range=0.1, decay = 5e-4, maxit=200)

# size - broj neurona u skrivenom sloju neuronske mreže
#range - početni raspon težina neuronske mreže
#decay - faktor degradacije koji se koristi za kontrolu prenaučenosti
#maxit - maksimalan broj iteracija

# weights:  31
#initial  value 29.961081 
#iter  10 value 25.336659
#iter  20 value 25.333378
#iter  30 value 25.296559
#iter  40 value 21.252437
#iter  50 value 21.041746
#iter  60 value 20.997252
#iter  70 value 20.989613
#iter  80 value 20.989329
#iter  90 value 19.677993
#iter 100 value 6.420157
#iter 110 value 6.361936
#iter 120 value 6.316321
#iter 130 value 5.823423
#iter 140 value 1.198673
#iter 150 value 0.611780
#iter 160 value 0.495203
#iter 170 value 0.400469
#iter 180 value 0.287162
#iter 190 value 0.194447
#iter 200 value 0.157050
#final  value 0.157050 
#stopped after 200 iterations

table(skup_za_treniranje1$cyl, predict(nn1_model1, skup_za_treniranje1, type="class"))
#   4  6  8
#4  9  0  0
#6  0  4  0
#8  0  0 12

table(cyl_testiranje, predict(nn1_model1, skup_za_testiranje1, type="class"))
#  4 6 8
#4 2 0 0
#6 0 1 2
#8 0 0 2

######### NEURALNET

nn2_model1<- neuralnet(cyl~., data=skup_za_treniranje1, hidden = 100, act.fct="tanh")
table(skup_za_treniranje1$cyl, max.col(compute(nn2_model1, skup_za_treniranje1)$net.result))

#  1  2  3
#4  9  0  0
#6  0  4  0
#8  0  0 12

table(cyl_testiranje, max.col(compute (nn2_model1, skup_za_testiranje1)$net.result ))
#  1 2 3
#4 2 0 0
#6 0 1 2
#8 0 2 0

plot(nn2_model1, rep=1)




############### INFERT #########

zene <- sample(2, nrow(infert), replace=TRUE, prob=c(0.8,0.2))
skup_za_treniranje2 <- infert[zene==1,]
skup_za_testiranje2 <- infert[zene==2,]

klase_testiranje_zene <- skup_za_testiranje2$education
klase_testiranje_zene
skup_za_testiranje2$education<- NULL


### NNET
nn1_model2 <- nnet(education~.,data=skup_za_treniranje2, size=1, range=0.1, decay =5e-4,maxit=200)
# weights:  14
#initial  value 253.132793 
#iter  10 value 176.534984
#iter  20 value 176.532236
#iter  30 value 67.867833
#iter  40 value 36.529823
#iter  50 value 36.228456
#iter  60 value 35.711986
#iter  70 value 35.595046
#iter  80 value 35.142163
#iter  90 value 31.168470
#iter 100 value 5.199554
#iter 110 value 1.693151
#iter 120 value 1.682368
#iter 130 value 1.681285
#iter 140 value 1.681149
#iter 140 value 1.681149
#final  value 1.681149 
#converged

table(predict(nn1_model2, skup_za_treniranje2, type="class"), skup_za_treniranje2$education)
#           0-5yrs 6-11yrs 12+ yrs
#0-5yrs      11       0       0
#12+ yrs      0       0      94
#6-11yrs      0      99       0

table(predict(nn1_model2, skup_za_testiranje2, type="class"), klase_testiranje_zene)
#          0-5yrs 6-11yrs 12+ yrs
#0-5yrs       1       0       0
#12+ yrs      0       0      22
#6-11yrs      0      21       0



######### NEURAL NET 

nn2_model2 <- neuralnet(education~., data=skup_za_treniranje2, hidden =10, act.fct="tanh")

table(skup_za_treniranje2$education, max.col(compute(nn2_model2, skup_za_treniranje2)$net.result))
#         1  2  3
#0-5yrs  11  0  0
#6-11yrs  0  0 99
#12+ yrs  0 94  0

table(klase_testiranje_zene, max.col(compute(nn2_model2, skup_za_testiranje2)$net.result))
#         1  2  3
#0-5yrs   1  0  0
#6-11yrs  0  0 21
#12+ yrs  0 22  0

plot(nn2_model2, rep=1)



                                             