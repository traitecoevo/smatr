robust.factor<-function(data,q)
{
   fac   <- NULL
   S     <- huber.M(data)
   means <- S$loc
   datac <- sweep(data,2,means,"-")
 
   # get matrix square root of S$cov:
   apu   <- eigen(S$cov)
   L     <- apu$values
   P     <- apu$vectors
   z     <- datac %*% P%*%(diag(L^(-1/2)))%*%t(P)
   r     <- sqrt(diag(z%*%t(z)))

   fac[1]<- mean(alpha.fun(r,2,q=pchisq(3,2))^2)/8
   fac[2]<- mean(gamma.fun(r,2,q=pchisq(3,2))^2)/2
   fac
}


gamma.fun<-function(r,k,q) 
{
  c<-sqrt(qchisq(q,k))
  sig<-pchisq(c,k+2)+(c/k)*(1-q)
  c<-sqrt(c)
  eta<-NULL
  eta[r<=c]<-1 
  eta[r>c]<-c*(k-1)/(r[r>c]*k)
  eta<-mean(eta)

  gamma<-NULL
  gamma[r<=c]<-(r[r<=c])/eta
  gamma[r>c]<-c/eta 
  gamma
}
