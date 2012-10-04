alpha.fun<-function(r,k,q) 
{
  c<-qchisq(q,k)
  sig<-pchisq(c,k+2)+(c/k)*(1-q)

  c<-sqrt(c)
  eta<-NULL
  eta[r<=c]<-(r[r^2<=c^2])^2/(2*sig^2) 
  eta[r>c]<-c^2/(4*sig^2) 
  eta <- mean(eta)

  alpha<-NULL
  alpha[r<=c]<-(r[r<=c])^2/(eta*sig^2)
  alpha[r>c]<-c^2/(eta*sig^2)
  alpha
}
