# bias exponential distribution
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

# bias negBin distribution
Nrep <- 1e2
R <- matrix(NA,50,Nrep)
f <- function(x,theta){
  sum(-dnbinom(x = x,size = .1, mu = theta,log=T))
}
for (k in 1:50){
  for (i in 1:Nrep){
    x <- rnbinom(n = k,size = .1 ,mu = 2)
    R[k,i] <- optimize(f=f,x=x,interval=c(0,5))$minimum
  }
}
plot(apply(R,1,mean))
lines(c(0,50),c(2,2))

# bias outbreak size (taner-blabla) distribution (with poisson offspring)
Nrep <- 1e2
R <- matrix(NA,50,Nrep)
f <- function(z,R){
  s <- 1
  - sum(((z-s-1)*log( s*z)+(z-s)*log(R)+ (-z*R) - lgamma(z-s +1)) )
}
for (k in 1:50){
  for (i in 1:Nrep){

    Sim_I <- simulate_poisson(.8,k,1,1,1e3)
    y_obs <- Sim_I$observed_size

    R[k,i] <- optimize(f=f,z=y_obs,interval=c(0,1))$minimum
  }
}
save.image('test_outbreakSize.RData')
load('test_outbreakSize.RData')
plot(apply(R,1,median),ylim=c(0,1))
lines(c(0,50),c(.8,.8))
