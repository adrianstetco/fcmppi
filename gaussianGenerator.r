#GaussianGenerator, Adrian Stetco, University of Manchester, 2016
#Generates clouds of Gaussians
#Uses mvtnorm library, install it with "install.packages("mvtnorm")" 

library(mvtnorm)
gaussianGenerator <- function(kcenters, dim, variance, points){   
  j<-1
  c<- matrix(0,kcenters,dim) 
  x <- rmvnorm(n=kcenters*1000, 
               mean=rep(0, dim), 
               sigma=diag(variance,dim))
  sampl<-sample(nrow(x), kcenters, replace=FALSE)  
  for (i in 1:nrow(x))  
    {if (i %in% sampl) {  
      c[j,]<- x[i,]  
      j<-j+1   
    }
  } 
  
  samplesCNr<-(points-kcenters)/kcenters
  sampleData<- matrix(0, points, dim) 
  sampleData[1:kcenters,]<-c[1:kcenters,] 
  k<-kcenters+1
  for( i in 1:kcenters){
   y <- rmvnorm(n=samplesCNr, mean=c[i,], sigma=diag(10,dim)) #runif(1, 5.0, 7.5)
    for(j in 1:samplesCNr){
      sampleData[k,]<- y[j,]
      k<-k+1
    }              
  }
  return(sampleData)
}

#for testing, uncomment the following lines
#g<-gaussianGenerator(5,2,4000, 1000)
#plot(g)


