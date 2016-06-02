## Simular datos para un diseño MAS
SimMASGamma<-function(N,shepe,rate,sigma){
x=rgamma(N,4,1)
y=20000 + 4 * x + rnorm(N, 0, sigma)
print(cor(x,y))
return(data.frame(X=x,Y=y))
}
