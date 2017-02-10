# Adrian Stetco, Phd Student, University of Manchester, 2015
# We use the FCM implementation from e1071 library
# Please note, the FCMPP code is not optimized while the one in e1071 is, this code 
# is for demonstration purposes

#install with install.packages(package_name) if not available
library(mvtnorm)
library(e1071)

# Euclidean distance function
euclid <- function(x,y) {invisible(sqrt(sum((x - y) ^ 2)))}

fcmpp <- function(X, k, pov) {
  centers<-matrix(X[sample(nrow(X),size=1,replace=FALSE),],1, byrow=T) 
  dis<-rep(0, k)
  for (i in 1:nrow(X)){dis[i]<-euclid(centers[1,], X[i,])}
  for(j in 2:k){
    centers<-rbind(centers,X[sample(1:nrow(X),1, prob=dis^pov/sum(dis^pov),1),])
    for (i in 1:nrow(X)){
      aux<-euclid(centers[j,], X[i,])
      if (aux<dis[i]) dis[i]<-aux
    }
  }
  return(centers)
}

# Main program
fuzzycmeans<- function(data, k, m, seeding = 0,  pov=k){
  data<-as.matrix(data)
  if (seeding==0){    
    c<-data[sample(nrow(data),size=k,replace=FALSE),]
    points(c[,1],c[,2], col="red", cex=2, pch=18)
    res<-cmeans(x=data, centers=c, m=m)
    return(res)
  }else if (seeding==1){   
    c=fcmpp(data,k, pov)
    points(c[,1], c[,2], col="blue",cex=2, pch=18)
    res<-cmeans(x=data, centers=c, m=m)
    return(res)
  }}

#TEST
g<-gaussianGenerator(10,2,5000, 1000)
plot(g,main="FCM centers in red, FCM++ centers in blue");
fcm<-fuzzycmeans(data=g,k=10,m=2,seeding=0) #FCM call
fcmpp<-fuzzycmeans(data=g,k=10,m=2,seeding=1, pov=5) #FCM++ call


#the lower the better
print(paste("FCM iterations=", fcm$iter))
print(paste("FCM++ iterations=", fcmpp$iter))

#the lower the better
print(paste("FCM Xie Beni Validity index=",fclustIndex(fcm, g, "xie.beni")))
print(paste("FCM++ Xie Beni Validity index=",fclustIndex(fcmpp, g, "xie.beni")))

