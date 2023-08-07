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
rm(list=ls())
require(fdrtool)
require(FEAR)

source("./Functions/data.vrs.R")
source("./Functions/coverage.vrs1.R")
source("./Functions/coverage.vrs2.R")

#### Set Seed ####
if (exists(".Random.seed")) {
  save.seed=.Random.seed
  flag.seed=TRUE
} else {
  flag.seed=FALSE
}
set.seed(900000)

#######################################
np=4
nq=1
########################################
# compute the true mean efficiency 
lambda.t=sqrt(2/pi) + 1
sig.t = sqrt((pi-2)/pi)
lambda.t
sig.t
N=c(10,20,50,100,200,300,500,1000)
M=2000
# for data sharpening
res=matrix(0,nrow=length(N),ncol=9)
sig=matrix(0,nrow=length(N),ncol=8)
tm=matrix(0,nrow=length(N),ncol=5)
# for non-data sharpening
res0=matrix(0,nrow=length(N),ncol=9)
tm0=matrix(0,nrow=length(N),ncol=5)

t1=proc.time()
for (i in 1:length(N)) {
  for (m in 1:M) {
    data.m=data.vrs(np=np,nq=nq,n=N[i])
    x=data.m$x
    y=data.m$y
    sig.s=data.m$sig.s
    lambda.s=data.m$lambda.s
    
    if (np+nq<4) {
      tt=coverage.vrs1(x,y,L=20)
    } else {
      tt=coverage.vrs2(x,y,L=20)
    }
    
    ii=c(1,4,7)
    jj=c(2,5,8)
    kk=c(3,6,9)

    
    res[i,ii]=ifelse(tt$bounds1[1:3,1]<lambda.t & lambda.t<tt$bounds1[1:3,2],
                     res[i,ii]+1,res[i,ii])
    res[i,jj]=ifelse(tt$bounds2[1:3,1]<lambda.t & lambda.t<tt$bounds2[1:3,2],
                     res[i,jj]+1,res[i,jj])
    res[i,kk]=ifelse(tt$bounds3[1:3,1]<lambda.t & lambda.t<tt$bounds3[1:3,2],
                     res[i,kk]+1,res[i,kk])
    #
    res0[i,ii]=ifelse(tt$bounds10[1:3,1]<lambda.t & lambda.t<tt$bounds10[1:3,2],
                      res0[i,ii]+1,res0[i,ii])
    res0[i,jj]=ifelse(tt$bounds20[1:3,1]<lambda.t & lambda.t<tt$bounds20[1:3,2],
                      res0[i,jj]+1,res0[i,jj])
    res0[i,kk]=ifelse(tt$bounds30[1:3,1]<lambda.t & lambda.t<tt$bounds30[1:3,2],
                      res0[i,kk]+1,res0[i,kk])
    #
    sig[i,]=sig[i,]+tt$sig
    #
    tm[i,]=tm[i,]+tt$estimate
    tm0[i,]=tm0[i,]+tt$estimate0
    #
    if (m %% 200==0){
      cat("Iteration:", m,"/",M,'of',N[i],'Observations',"\n")
      t2=proc.time()
      cat("Elapsed Time in Hour",(t2[[3]]-t1[[3]])/3600,"\n")
    }
  }
}

#
res/M
sig/M
tm/M
#
res0/M
tm0/M

### Save the Data For the Coverages

outfile=paste("./Data/coverage-p",np,"q",nq,".csv",sep="")
write.csv(res0/M,row.names=FALSE,file=outfile)


outfile=paste("./Data/coverage-sharpening-p",np,"q",nq,".csv",sep="")
write.csv(res/M,row.names=FALSE,file=outfile)



### construct the Table ###
res1=res/M
tex=formatC(N,width=4,digits=0,format="f")
tex=paste(np,"&",nq,"&",tex)
for (k in 1:ncol(res1)) {
  if ( (k-1) %% 3==0) {
    tex=paste(tex,"&","&")
  } 
  tex = paste(tex,"&",formatC(res1[,k],width=5,digits = 3,format = "f"))
}
tex = paste(tex,"\\\\")
outfile=paste("./Output/coverage-sharpening-p",np,"q",nq,".tex",sep="")
write(tex,file=outfile)


res2=sig/M
tex=formatC(N,width=4,digits=0,format="f")
tex=paste(np,"&",nq,"&",tex)
for (k in 1:ncol(res2)) {
  tex = paste(tex,"&",formatC(res2[,k],width=6,digits = 4,format = "f"))
}
tex = paste(tex,"\\\\")
outfile=paste("./Output/sigma-p",np,"q",nq,".tex",sep="")
write(tex,file=outfile)


res3=tm/M
tex=formatC(N,width=4,digits=0,format="f")
tex=paste(np,"&",nq,"&",tex)
for (k in 1:ncol(res3)) {
  tex = paste(tex,"&",formatC(res3[,k],width=6,digits = 4,format = "f"))
}
tex = paste(tex,"\\\\")
outfile=paste("./Output/mu-sharpening-p",np,"q",nq,".tex",sep="")
write(tex,file=outfile)


### construct the Table ###
res1=res0/M
tex=formatC(N,width=4,digits=0,format="f")
tex=paste(np,"&",nq,"&",tex)
for (k in 1:ncol(res1)) {
  if ( (k-1) %% 3==0) {
    tex=paste(tex,"&","&")
  } 
  tex = paste(tex,"&",formatC(res1[,k],width=5,digits = 3,format = "f"))
}
tex = paste(tex,"\\\\")
outfile=paste("./Output/coverage-p",np,"q",nq,".tex",sep="")
write(tex,file=outfile)



res3=tm0/M
tex=formatC(N,width=4,digits=0,format="f")
tex=paste(np,"&",nq,"&",tex)
for (k in 1:ncol(res3)) {
  tex = paste(tex,"&",formatC(res3[,k],width=6,digits = 4,format = "f"))
}
tex = paste(tex,"\\\\")
outfile=paste("./Output/mu-p",np,"q",nq,".tex",sep="")
write(tex,file=outfile)
