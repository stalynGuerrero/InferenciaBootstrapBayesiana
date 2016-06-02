# rm(list = ls())
# dirpath <- "C:/Users/guerr/Dropbox/articulos/Artículo Proporción bayesiana/Estimación del total BB/Algoritmo prueba"
# setwd(dirpath)
set.seed(1) 

N=3000 
sigma=87
x=rgamma(N,4,1);tx=sum(x)
y=20000 + 4 * x + rnorm(N, 0, sigma)
ty=sum(y)  
cor(x,y)

# Estimador de Horvitz-Thompson (HT)
HT<-function(y,n){
  require(TeachingSampling)
  # Selecci'on de una muestra de tamaño n
  ty=sum(y)
  sel <- sample(N,n)
  y.s <- y[sel]
  # Estimaci'on de HT 
  ty.mas = E.SI(N, n, y.s)[1,2]
  sdpi = E.SI(N, n, y.s)[2,2]
  CV =   10*E.SI(N, n, y.s)[3,2]
  # intervalo de confianza al 95%  para el total   
  Li=ty.mas-qnorm(0.975)*sdpi
  Ls=ty.mas+qnorm(0.975)*sdpi

c(cont     = ifelse(ty>Li & ty<Ls,1,0), 
  Longitud = Ls-Li,                     # Longitud del intervalo
  hat.ty    = ty.mas,                   # Estimador de HT
  Sesgo    = ty.mas-ty,                 # Sesgo de la estimaci'on
  CV       = CV)                        # Coeficiente de variaci'on estimado
}

# Medidas de Calidad de la estimaci'on
Medida.Calidad <- function(Dat,ty){
  round( data.frame(Coverage.100 = 100*mean(Dat[,"cont"]),
                    Longitud.Relative.1000 = 1000*mean((Dat[,"Longitud"]/2)/Dat[,"hat.ty"]),
                    Sesgo.Relative.1000 = 1000*mean(Dat[,"Sesgo"]/ty),
                    CV.1000 = mean(Dat[,"CV"])),3)
}

n = 50 
dat.HT <- t(replicate(1000,HT(y,n)))
Resul<-data.frame(rho=0.1,Estim="HT-MAS",n.muestra=paste0("n= ",n), Medida.Calidad(dat.HT,ty))


n = 400 
dat.HT <- t(replicate(1000,HT(y,n)))
Resul<- data.frame(rbind(
  data.frame(rho=0.1,Estim="HT-MAS",n.muestra=paste0("n= ",n), Medida.Calidad(dat.HT,ty)),Resul))

n = 1000 
dat.HT <- t(replicate(1000,HT(y,n)))
Resul<- data.frame(rbind(
  data.frame(rho=0.1,Estim="HT-MAS",n.muestra=paste0("n= ",n), Medida.Calidad(dat.HT,ty)),Resul))

resul.ante<-read.table("Salida de simulacion/Resul.simulacion.txt",sep = "\t",dec = ".",header = T)

Resul<-rbind(resul.ante,Resul)
write.table(Resul,"Salida de simulacion/Resul.simulacion.txt",sep = "\t",dec = ".",row.names = FALSE)
