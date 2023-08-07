###############################################################################
## Further improvements of finite sample approximation of 
## central limit theorems for envelopment estimators
## Author: Shirong Zhao
## The programming codes used in this paper involve 
## some earlier codes from Paul Wilson
## All rights reserved. 
## It is free for academic use only with adequate citation and acknowledgments.
## For any other use, contact the authors.
###############################################################################

### data generation
data.vrs <- function(np=3,nq=1,n=100) {
  lambda=rhalfnorm(n,theta=sqrt(pi/2))+1
  if (np==1){
    x=runif(n)
    y=(x^0.4)/lambda
  } else if(np==2){
    x1=runif(n)
    x2=runif(n)
    x=cbind(x1,x2)
    y=(x1^0.4)*(x2^0.2)/lambda
  } else if(np==3){
    x1=runif(n)
    x2=runif(n)
    x3=runif(n)
    x=cbind(x1,x2,x3)
    y=(x1^0.4)*(x2^0.2)*(x3^0.1)/lambda
  } else if (np==3){
    x1=runif(n)
    x2=runif(n)
    x3=runif(n)
    x=cbind(x1,x2,x3)
    y=(x1^0.4)*(x2^0.2)*(x3^0.1)/lambda
  } else if(np==4){
    x1=runif(n)
    x2=runif(n)
    x3=runif(n)
    x4=runif(n)
    x=cbind(x1,x2,x3,x4)
    y=(x1^0.4)*(x2^0.2)*(x3^0.1)*(x4^0.15)/lambda
  } else if(np==5){
    x1=runif(n)
    x2=runif(n)
    x3=runif(n)
    x4=runif(n)
    x5=runif(n)
    x=cbind(x1,x2,x3,x4,x5)
    y=(x1^0.4)*(x2^0.2)*(x3^0.1)*(x4^0.15)*(x5^0.05)/lambda
  } else if(np==7){
    x1=runif(n)
    x2=runif(n)
    x3=runif(n)
    x4=runif(n)
    x5=runif(n)
    x6=runif(n)
    x7=runif(n)
    x=cbind(x1,x2,x3,x4,x5,x6,x7)
    y=(x1^0.05)*(x2^0.1)*(x3^0.15)*(x4^0.2)*(x5^0.125)*(x6^0.075)*(x7^0.025)/lambda
  }
  x=matrix(t(x),nrow=np)
  y=matrix(y,nrow=1)
  #
  lambda.s=mean(lambda)
  sig.s=sd(lambda)
  res=list(x=x,y=y,lambda=lambda,lambda.s=lambda.s,sig.s=sig.s)
  return(res)
}