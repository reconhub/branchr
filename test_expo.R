Nrep <- 1e2
R <- matrix(NA,50,Nrep)
f <- function(x,theta){
  sum(-dexp(x,rate=1/theta,log=T))
}

for (k in 1:50){
  for (i in 1:Nrep){
    x <- rexp(k,rate = 1/2)
    R[k,i] <- optimize(f=f,x=x,interval=c(0,5))$minimum
  }
}  
plot(apply(R,1,mean))
lines(c(0,50),c(2,2))


Nrep <- 1e3
R <- matrix(NA,50,Nrep)
f <- function(x,theta){
  sum(-dnbinom(x = x,size = 1, mu = theta,log=T))
}
for (k in 1:50){
  for (i in 1:Nrep){
    x <- rnbinom(n = k,size = .1 ,mu = 2)
    R[k,i] <- optimize(f=f,x=x,interval=c(0,5))$minimum
  }
}  
plot(apply(R,1,mean))
lines(c(0,50),c(2,2))