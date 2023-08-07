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


#### For VRS and p+q<=4 ####
coverage.vrs1 <- function(x,y,L=20) {
 
  np=nrow(x)
  nq=nrow(y)
  n=ncol(x)
  na=floor(n/2)
  nb=n-na
  kappa=2/(np+nq+1)
  bc.fac=1/(2**kappa - 1)
  #
  tau0=n^(-0.75*kappa)
  # evaluate efficiency using VRS-DEA, Output Direction
  d0=1/FEAR::dea(XOBS=x,YOBS=y,XREF=x,YREF=y,RTS=1,ORIENTATION=2)
  # using data sharpening method in nsz2021 to define y.tilde
  ii=which(d0<=1+tau0)
  epison=runif(length(ii),1,1+tau0)
  #
  d=d0
  d[ii]=d0[ii]*epison
  # compute bias corrections via generalized jackknife:
  # involving some earlier codes from Paul Wilson
  tbar0=rep(0,n)
  ind=c(1:n)
  for (j in 1:L) {
    if (j==1) {
      ind1=c(1:n)
      x.b=x
      y.b=y
    } else {
      ind1=sample(ind,size=n)
      x.b[,1:n]=x[,ind1]
      y.b[,1:n]=y[,ind1]
    }
    #####
    da0=1/FEAR::dea(XOBS=matrix(x.b[,1:na],nrow=np),
                    YOBS=matrix(y.b[,1:na],nrow=nq),
                    XREF=matrix(x.b[,1:na],nrow=np),
                    YREF=matrix(y.b[,1:na],nrow=nq),
                    RTS=1,ORIENTATION=2)
    db0=1/FEAR::dea(XOBS=matrix(x.b[,(na+1):n],nrow=np),
                    YOBS=matrix(y.b[,(na+1):n],nrow=nq),
                    XREF=matrix(x.b[,(na+1):n],nrow=np),
                    YREF=matrix(y.b[,(na+1):n],nrow=nq),
                    RTS=1,ORIENTATION=2)
    #
    tbar0[ind1[1:na]]=tbar0[ind1[1:na]] +
      da0 - d0[ind1[1:na]]
    tbar0[ind1[(na+1):n]]=tbar0[ind1[(na+1):n]] +
      db0 - d0[ind1[(na+1):n]]
  }
  # tbar/tbar0 contains the bias for eff of each obs
  tbar0=(1/L)*bc.fac*tbar0
  tbar=tbar0
  tbar[ii]=tbar0[ii]*epison
  # mean bias
  mu.bias=mean(tbar) 
  mu.bias0=mean(tbar0) 
  # d.bc/d.bc0 is bias-corrected eff for each obs
  d.bc=d-tbar
  d.bc0=d0-tbar0
  # mean eff and bias-corrected mean eff
  mu=mean(d)
  mu.bc=mu-mu.bias
  #
  mu0=mean(d0)
  mu.bc0=mu0-mu.bias0
  # variance
  var=var(d) # ksw2015 original method
  var.bc=var(d.bc) # our proposed method
  var.bc.sz=var+mu.bias^2 # sz2020 method
  #
  var0=var(d0) # ksw2015 original method
  var.bc0=var(d.bc0) # our proposed method
  var.bc.sz0=var0+mu.bias0^2 # sz2020 correction method
  # ksw2015 original method
  sig1=sqrt(var)
  sig10=sqrt(var0)
  # sz2020 correction method
  sig2=sqrt(var.bc.sz)
  sig20=sqrt(var.bc.sz0)
  # method proposed by us
  sig3=sqrt(var.bc)
  sig30=sqrt(var.bc0)
  # for data sharpening
  ts1=sig1/sqrt(n)
  ts2=sig2/sqrt(n)
  ts3=sig3/sqrt(n)
  ts4=sig.t/sqrt(n)
  # for non-data sharpening
  ts10=sig10/sqrt(n)
  ts20=sig20/sqrt(n)
  ts30=sig30/sqrt(n)
  ts40=sig.t/sqrt(n)
  # using true sample standard deviation
  ts5=sig.s/sqrt(n)
  #
  crit=qnorm(p=c(0.95,0.975,0.995,0.05,0.025,0.005))
  bounds1=matrix((mu.bc-ts1*crit),nrow=3,ncol=2)
  bounds2=matrix((mu.bc-ts2*crit),nrow=3,ncol=2)
  bounds3=matrix((mu.bc-ts3*crit),nrow=3,ncol=2)
  bounds4=matrix((mu.bc-ts4*crit),nrow=3,ncol=2)
  #
  bounds10=matrix((mu.bc0-ts10*crit),nrow=3,ncol=2)
  bounds20=matrix((mu.bc0-ts20*crit),nrow=3,ncol=2)
  bounds30=matrix((mu.bc0-ts30*crit),nrow=3,ncol=2)
  bounds40=matrix((mu.bc0-ts40*crit),nrow=3,ncol=2)
  #
  bounds5=matrix((lambda.s-ts5*crit),nrow=3,ncol=2)
  # make a list of results to return to calling routine and then quit:
  res=list(bounds1=bounds1,bounds2=bounds2,
           bounds3=bounds3,bounds4=bounds4,
           bounds10=bounds10,bounds20=bounds20,
           bounds30=bounds30,bounds40=bounds40,
           bounds5=bounds5,
           sig=c(sig10,sig20,sig30,sig1,sig2,sig3,sig.s,sig.t),
           estimate=c(mu,mu.bias,mu.bc,lambda.s,lambda.t),
           estimate0=c(mu0,mu.bias0,mu.bc0,lambda.s,lambda.t)
  )
  return(res)
}