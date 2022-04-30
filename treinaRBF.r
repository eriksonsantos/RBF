treinaRBF <- function(xin,yin,p)
{
  
  pdfnvar <- function(x,m,K,n){
    if (n == 1){
      
      r <- sqrt(as.numeric(K))
      px <- (1/(sqrt(2*pi*r*r))) * exp(-0.5*((x-m)/(r))^2)
    }else{
      px <- ((1/sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m) %*% (solve(K)) %*% (x-m)))
      return(px)
    }
  }
  
  N <- dim(xin)[1]
  n <- dim(xin)[2]
  
  xin <- as.matrix(xin)
  yin <- as.matrix(yin)
  
  #xclust <- kmeans(xin,p)
  
  #m<- as.matrix(xclust$centers)
  xlist <- seq(1,N,1)
  centros <- matrix(ncol = n)
  raios <- matrix(nrow=1)
  
  for (i in 1:p) {
    iseq <- matrix(sample(xlist,2))
    xc1 <- matrix(xin[iseq[1],], ncol=n)
    xc2 <- matrix(xin[iseq[2],],ncol=n)
    
    pontos <- rbind(xc1,xc2)
    raio <- matrix(ncol = n)
    for(k in 1:n){
      aux <- rbind(pontos[1,k],pontos[2,k])
      raio[k] <- dist(aux,method = "euclidean")
    }
   # raio <- dist(pontos, method = "euclidean")
    
    centro <- colMeans(pontos)
     if(is.na(centros[1]) != TRUE ) {
       centros <- rbind(centros,centro)
     }else{
       centros <- centro
     }
  
    if(is.na(raios[1]) != TRUE ) {
      raios <- rbind(raios,raio)
    }else{
      raios <- raio
    }
   
   # raios[i] <-  as.numeric(raio)
  }
  
  
  
  
#  covlist<- list()
  
#  for(i in 1:p){
#    ici <- which(xclust$cluster == i)
#    xci <- xin[ici,]
#    if(n==1)
#      covi <- var(xci)
#    else covi <- cov(xci)
#    covlist[[i]] <- covi
    
 # }
  
  H <- matrix(nrow = N,ncol=p)
  
  for(j in 1:N){
    for(i in 1:p){
      mi <- matrix(centros[i,])
      raio <- raios[i]
      raio <- diag(n)*raio
      xentrada <- matrix(xin[j,])
      
     # covi <- covlist[i]
     # covi <- matrix(unlist(covlist[i]), ncol = n, byrow = T) + 0.001* diag(n)
      
      H[j,i] <- pdfnvar(xentrada,mi,raio,n)
      
    }
  }
  
  Haug <- cbind(1,H)
  W <- (solve(t(Haug) %*% Haug) %*% t(Haug)) %*% yin
  
  return(list(centros,raios,W,H))
 
}

