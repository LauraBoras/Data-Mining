(A <- matrix(1:25, 5, 5))
(B <- matrix(1:25, 5, 5, byrow = T))
(x <- seq(2, 10, by=2))

#Provjerimo što rade naredbe:
diag(x) #vraća kvadratnu matricu kojoj su na dijagonali upravo vrijednosti iz x-a, a ostalo su nule
diag(A) #vraća dijagonalu matrice A
diag(c(5)) #vraća jediničnu 5*5 matricu
diag(c(5,5,5))  #vraća kvadratnu matricu 3*3 s peticama na dijagonali, a ostalo su nule
diag(diag(A)) #vraća kvadratnu maricu kojoj su na dijagonali elementi dijagonale matrice A, a ostalo su nule

install.packages("MASS")
solve(A, x)   #rješava Ax = b
solve(A)      #vraća inverz matrice
ginv(A)

svd(A)     #singularna dekompozicija matrice
eigen(A)   #eačuna svojstvene vrijednosti i svojstvene vektore
qr(A)     #razbija matricu na Q i R matricu
chol(A)   #vraca Cholesky dekompoziciju matrice (L+L transporirano gdje je L donjetrokutasta)

#Još neke operacije s matricama:
cbind(A,B) #konkatenacija dviju matrica po stupcima
rbind(A,B,diag(x))  #konkatenacija po retcima
rowMeans(A)  #vraća niz srednjih vrijednosti matrice A po retcima
rowSums(A)   #vraća niz suma matrice A po retcima
colMeans(A)  #vraća niz srednjih vrijednosti matrice B po stupcima
colSums(A)   #vraća sumu matrice B po stupcima


#Matrica udaljenosti:
B[upper.tri(B)]<-t(B)[upper.tri(B)]
diag(B)<-0
B

#Alternativno rješenje:
A[lower.tri(A)]<-t(A)[lower.tri(A)]
diag(A)<-0
A

#Višedimenzionalno polje:
polje <- array(1:80, c(4,5,4))
polje
dim(polje)

polje2 <- polje[c(2,4),1:3,2:3]
polje2
dim(polje2)   #vraća : [1] 2 3 2


#Lista:
razred <- list("Ana", "Marija", "Klara", "Marta")
razred

lista <- list(ime = "Laura", prezime = "Boras", tel_br = 12345, likes=rep(50,7))
lista

l <- list(razred, lista)
l



