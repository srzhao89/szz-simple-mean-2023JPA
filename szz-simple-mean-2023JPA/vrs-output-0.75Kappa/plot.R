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
setEPS()
postscript(file = "./Figures/vrs-75.eps")

par(mfrow = c(3, 2),oma = c(2,1,0,1),cex=0.55,mar=c(5,4,3.5,4))

# p=1, q=1
np=1
nq=1
outfile=paste("./Data/coverage-p",np,"q",nq,".csv",sep="")
df1=read.csv(outfile)
outfile=paste("./Data/coverage-sharpening-p",np,"q",nq,".csv",sep="")
df2=read.csv(outfile)

sol1=df1$V4
sol2=df1$V5
sol3=df1$V6

sol4=df2$V4
sol5=df2$V5
sol6=df2$V6

x=c(log10(10),log10(20),log10(50),log10(100),log10(200),log10(300),log10(500),log10(1000))
plot(x,sol1, type="l",lty=1,lwd=2,col = 1, xaxt='n',ylim=c(0,1),
     xlab="Sample size", ylab = "Coverage", 
     main = bquote(p==1~","~q==1))
lines(x,sol2, type="l",lty=2,lwd=2,col = 2)
lines(x,sol3, type="l",lty=7,lwd=2,col = 3)
lines(x,sol4, type="l",lty=4,lwd=2,col = 4)
lines(x,sol5, type="l",lty=5,lwd=2,col = 5)
lines(x,sol6, type="l",lty=6,lwd=2,col = 6)
abline(0.95,0,lty=2,lwd=1)
axis(side=1,at=c(log10(10),log10(20),log10(50),log10(100),log10(200),log10(300),log10(500),log10(1000)),
     labels=c("10","20","50","100","200","300","500","1000"),
     cex.axis = 0.82)

axis(side=2,at=c(0.2,0.4,0.6,0.8,1.0),
     labels=c("0.2","0.4","0.6","0.8","1.0"))


# p=2, q=1
np=2
nq=1
outfile=paste("./Data/coverage-p",np,"q",nq,".csv",sep="")
df1=read.csv(outfile)
outfile=paste("./Data/coverage-sharpening-p",np,"q",nq,".csv",sep="")
df2=read.csv(outfile)

sol1=df1$V4
sol2=df1$V5
sol3=df1$V6

sol4=df2$V4
sol5=df2$V5
sol6=df2$V6

plot(x,sol1, type="l",lty=1,lwd=2,col = 1, xaxt='n',
     xlab="Sample size", ylab = "Coverage", ylim=c(0,1),
     main = bquote(p==2~","~q==1))
lines(x,sol2, type="l",lty=2,lwd=2,col = 2)
lines(x,sol3, type="l",lty=7,lwd=2,col = 3)
lines(x,sol4, type="l",lty=4,lwd=2,col = 4)
lines(x,sol5, type="l",lty=5,lwd=2,col = 5)
lines(x,sol6, type="l",lty=6,lwd=2,col = 6)
abline(0.95,0,lty=2,lwd=1)
axis(side=1,at=c(log10(10),log10(20),log10(50),log10(100),log10(200),log10(300),log10(500),log10(1000)),
     labels=c("10","20","50","100","200","300","500","1000"),
     cex.axis = 0.82)

axis(side=2,at=c(0.2,0.4,0.6,0.8,1.0),
     labels=c("0.2","0.4","0.6","0.8","1.0"))


# p=3, q=1
np=3
nq=1
outfile=paste("./Data/coverage-p",np,"q",nq,".csv",sep="")
df1=read.csv(outfile)
outfile=paste("./Data/coverage-sharpening-p",np,"q",nq,".csv",sep="")
df2=read.csv(outfile)

sol1=df1$V4
sol2=df1$V5
sol3=df1$V6

sol4=df2$V4
sol5=df2$V5
sol6=df2$V6

plot(x,sol1, type="l",lty=1,lwd=2,col = 1, xaxt='n',ylim=c(0,1),
     xlab="Sample size", ylab = "Coverage", 
     main = bquote(p==3~","~q==1))
lines(x,sol2, type="l",lty=2,lwd=2,col = 2)
lines(x,sol3, type="l",lty=7,lwd=2,col = 3)
lines(x,sol4, type="l",lty=4,lwd=2,col = 4)
lines(x,sol5, type="l",lty=5,lwd=2,col = 5)
lines(x,sol6, type="l",lty=6,lwd=2,col = 6)
abline(0.95,0,lty=2,lwd=1)
axis(side=1,at=c(log10(10),log10(20),log10(50),log10(100),log10(200),log10(300),log10(500),log10(1000)),
     labels=c("10","20","50","100","200","300","500","1000"),
     cex.axis = 0.82)

axis(side=2,at=c(0.2,0.4,0.6,0.8,1.0),
     labels=c("0.2","0.4","0.6","0.8","1.0"))


# p=4, q=1
np=4
nq=1
outfile=paste("./Data/coverage-p",np,"q",nq,".csv",sep="")
df1=read.csv(outfile)
outfile=paste("./Data/coverage-sharpening-p",np,"q",nq,".csv",sep="")
df2=read.csv(outfile)

sol1=df1$V4
sol2=df1$V5
sol3=df1$V6

sol4=df2$V4
sol5=df2$V5
sol6=df2$V6


plot(x,sol1, type="l",lty=1,lwd=2,col = 1, xaxt='n',ylim=c(0,1),
     xlab="Sample size", ylab = "Coverage", 
     main = bquote(p==4~","~q==1))
lines(x,sol2, type="l",lty=2,lwd=2,col = 2)
lines(x,sol3, type="l",lty=7,lwd=2,col = 3)
lines(x,sol4, type="l",lty=4,lwd=2,col = 4)
lines(x,sol5, type="l",lty=5,lwd=2,col = 5)
lines(x,sol6, type="l",lty=6,lwd=2,col = 6)
abline(0.95,0,lty=2,lwd=1)
axis(side=1,at=c(log10(10),log10(20),log10(50),log10(100),log10(200),log10(300),log10(500),log10(1000)),
     labels=c("10","20","50","100","200","300","500","1000"),
     cex.axis = 0.82)

axis(side=2,at=c(0.2,0.4,0.6,0.8,1.0),
     labels=c("0.2","0.4","0.6","0.8","1.0"))


# p=5, q=1
np=5
nq=1
outfile=paste("./Data/coverage-p",np,"q",nq,".csv",sep="")
df1=read.csv(outfile)
outfile=paste("./Data/coverage-sharpening-p",np,"q",nq,".csv",sep="")
df2=read.csv(outfile)

sol1=df1$V4
sol2=df1$V5
sol3=df1$V6

sol4=df2$V4
sol5=df2$V5
sol6=df2$V6


plot(x,sol1, type="l",lty=1,lwd=2,col = 1, xaxt='n',ylim=c(0,1),
     xlab="Sample size", ylab = "Coverage", 
     main = bquote(p==5~","~q==1))
lines(x,sol2, type="l",lty=2,lwd=2,col = 2)
lines(x,sol3, type="l",lty=7,lwd=2,col = 3)
lines(x,sol4, type="l",lty=4,lwd=2,col = 4)
lines(x,sol5, type="l",lty=5,lwd=2,col = 5)
lines(x,sol6, type="l",lty=6,lwd=2,col = 6)
abline(0.95,0,lty=2,lwd=1)
axis(side=1,at=c(log10(10),log10(20),log10(50),log10(100),log10(200),log10(300),log10(500),log10(1000)),
     labels=c("10","20","50","100","200","300","500","1000"),
     cex.axis = 0.82)


axis(side=2,at=c(0.2,0.4,0.6,0.8,1.0),
     labels=c("0.2","0.4","0.6","0.8","1.0"))


# p=7, q=1
np=7
nq=1
outfile=paste("./Data/coverage-p",np,"q",nq,".csv",sep="")
df1=read.csv(outfile)
outfile=paste("./Data/coverage-sharpening-p",np,"q",nq,".csv",sep="")
df2=read.csv(outfile)

sol1=df1$V4
sol2=df1$V5
sol3=df1$V6

sol4=df2$V4
sol5=df2$V5
sol6=df2$V6


plot(x,sol1, type="l",lty=1,lwd=2,col = 1, xaxt='n',ylim=c(0,1),
     xlab="Sample size", ylab = "Coverage", 
     main = bquote(p==7~","~q==1))
lines(x,sol2, type="l",lty=2,lwd=2,col = 2)
lines(x,sol3, type="l",lty=7,lwd=2,col = 3)
lines(x,sol4, type="l",lty=4,lwd=2,col = 4)
lines(x,sol5, type="l",lty=5,lwd=2,col = 5)
lines(x,sol6, type="l",lty=6,lwd=2,col = 6)
abline(0.95,0,lty=2,lwd=1,col = 1)
axis(side=1,at=c(log10(10),log10(20),log10(50),log10(100),log10(200),log10(300),log10(500),log10(1000)),
     labels=c("10","20","50","100","200","300","500","1000"),
     cex.axis = 0.82)


axis(side=2,at=c(0.2,0.4,0.6,0.8,1.0),
     labels=c("0.2","0.4","0.6","0.8","1.0"))



par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom',legend = c("Sol1","Sol2","Sol3","Sol4","Sol5","Sol6","0.95"), col = c(1,2,3,4,5,6,1), 
       lty=c(1,2,7,4,5,6,2),lwd =c(2,2,2,2,2,2,1),xpd = TRUE,cex=1.8, ncol=7,bty = 'n')


dev.off()

