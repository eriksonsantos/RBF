rm(list = ls())

source('treinaRBF.r')
source('saidaRBF.r')

data(iris)

xseq <- sample(100)

xall <- as.matrix(iris[xseq,1:4])

yall <- as.matrix((1 * (iris$Species[xseq] == 'versicolor') -0.5)*2)
xin <- as.matrix(xall[1:70,])

yd <- as.matrix(yall[1:70,])

yd <- as.matrix(yall[1:70,])

xinteste <- as.matrix(xall[(71:100),])
yteste <- as.matrix(yall[(71:100),])

for (i in 1:10) {
  modRBF <- treinaRBF(xin,yd,10)
  w <- modRBF[[3]]
  H <- modRBF[[4]]
  
  Yhat<- YRBF(xin,modRBF)
  yt <- (1*(Yhat >=0)-0.5)*2
  acuraciaTrain[i]<- t(yd - yt)%*%(yd -yt)/(4*70)

  Yhat_teste<- YRBF(xinteste,modRBF)
 
  ytst <- (1*(Yhat_teste >=0)-0.5)*2
  acuraciaTeste[i] <- t(yteste - ytst)%*%(yteste -ytst)/(4*30) 
}




