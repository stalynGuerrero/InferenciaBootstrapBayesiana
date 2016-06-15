## Simular datos para un diseño MAS
SimMASGamma<-function(N,shepe,rate,sigma){
x=rgamma(N,shepe,rate)
y=200 + 4 * x + rnorm(N, 0, sigma)
print(cor(x,y))
return(data.frame(X=x,Y=y))
}


## Simular datos No Lineal
SimGammaNoL<-function(N,shepe,rate,sigma){
  x=rgamma(N,shepe,rate)
  y= 1000+2.45*exp(.1+0.2*x) + rnorm(N, 0, sigma)
  print(cor(x,y))
  return(data.frame(X=x,Y=y))
}
