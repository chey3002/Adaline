and=matrix( 
  c( 1, 1, 1,
     1,0,0,
     0, 1,0,
     0,0,0), # the data elements 
  nrow=4,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)
bin=matrix( 
  c( 0,0, 1,1,
     0, 1,0,2,
     0, 1, 1,3,
     1,0,0,4,
     1,0, 1,5,
     1, 1,0,6,
     1, 1, 1,7), # the data elements 
  nrow=7,              # number of rows 
  ncol=4,              # number of columns 
  byrow = TRUE)
bin2=matrix( 
  c( 0,0,0, 1,1,
     0,0, 1,0,2,
     0,0, 1, 1,3,
     0,1,0,0,4,
     0,1,0, 1,5,
     0,1, 1,0,6,
     0,1, 1, 1,7,
     1,0, 0, 0,8,
     1,0, 0, 1,9,
     1,0, 1, 0,10,
     1,0, 1, 1,11,
     1,1,0,0,12,
     1,1,0, 1,13,
     1,1, 1,0,14,
     1,1, 1, 1,15), # the data elements 
  nrow=15,              # number of rows 
  ncol=5,              # number of columns 
  byrow = TRUE)

or=matrix( 
  c( 1, 1, 1,
     1,0, 1,
     0, 1, 1,
     0,0,0), # the data elements 
  nrow=4,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)
xor=matrix( 
  c( 1, 1, 1,
     1,0,0,
     0, 1,0,
     0,0, 1), # the data elements 
  nrow=4,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)
toys=matrix( 
  c( -1  ,-1  ,0,
     -5  ,-2.5,0,
     -7.5, 7.5,0,
     10  , 7.5, 1,
     -2.5,12.5,0,
     5  ,10  , 1,
     5  , 5  , 1), # the data elements 
  nrow=7,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)
PerceptronBin <- function(data) {
  gama=0.01
  teta=0
  w=runif(ncol(data)-1,0,1)
  misclassfied= TRUE
  k=0
  E=c()
  while (misclassfied) {
    k=k+1
    Ep=c()
    for (i in 1:(nrow(data)-2)) {
      y=sum(c(w*data[i,1:(ncol(data)-1)],teta))
      dif=data[i,ncol(data)]-y
      w=w+(data[i,1:(ncol(data)-1)]*gama*dif)
      Ep=c(Ep,dif^2)
    }
    E=c(E,sum(Ep)/2)
    
    
    if(k==1000){
      misclassfied= FALSE
    }
    
    if(!misclassfied){
      print(w)
      print("Test")
      
      for (i in 1:nrow(data)) {
        y=sum(c(w*data[i,1:(ncol(data)-1)],teta))
        print(y)    
      } 
    }
    
  }
  plot(1:length(E),E)
  w=(c(w,teta))
  return(w)
}
plotTheLine <- function(data, title,a1,b1) {#y=a+b*x
  plot(data,pch= ifelse(data[, 3] == 0, 18, 19),col = ifelse(data[, 3] == 0, "blue", "red"),xlab = "X", ylab = "Y",main=title)
  abline(a=a1,b=b1)
}

w=PerceptronBin(and)
print("Pesos Bin: ")
print(w)

w=PerceptronBin(bin)
print("Pesos Bin: ")
print(w)

w=PerceptronBin(bin2)
print("Pesos Bin2: ")
print(w)
#Solo puede solucionar problemas que se solucionan linealmente