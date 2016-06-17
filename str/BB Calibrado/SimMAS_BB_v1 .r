#############################################################################
rm(list = ls())
options(digits = 10)
dirPath<-"/home/sguerrero/Documentos/Dropbox/articulos/Artículo Proporción bayesiana/InferenciaBootstrapBayesiana/Algoritmo prueba/InferenciaBootstrapBayesiana"
setwd(dirPath)
#############################################################################
require(TeachingSampling)
require(hdrcde)
require(cubature)
source("str/Funciones Comunes/SimMASGamma.r")
source("str/Funciones Comunes/VeroBoot.r")
source("str/Funciones Comunes/Medida.Calidad.r")
#############################################################################
# Definir función para la simulación
SimHT <- function(Pob, n, apriori = "unif", k) {
  N <- nrow(Pob)
  ty <- sum(Pob[, "Y"])
  sel <- sample(N, n)
  ys <- Pob[sel, "Y"]
  Vero <- VeroBoot(ys, N, n, ty)

  if (apriori == "gamma") {
    Aprio = dgamma(Vero$x, ty ^ 2 / (k), ty / (k))
  }
  if (apriori == "unif") {
    Aprio = dunif(Vero$x, min = 0, max = 10 ^ 10)
  }
  if (apriori == "normal") {
    Aprio = dnorm(Vero$x, mean = ty, sd = k)
  }
  post   = Vero$y * Aprio
  Fxpost<-approxfun(Vero$x,post)

  q0.001<-as.numeric(quantile(Vero$x,probs = 0.1))
  q0.999<-as.numeric(quantile(Vero$x,probs = 0.9))
  rejilla<-seq(q0.001,q0.999,by=1)
  muesb  = sample(rejilla, 1000, prob =Fxpost(rejilla), replace = T)

  # par(mfrow=c(1,3))
  # plot(Vero,type = "l",main = "Vero")
  # abline(v=ty)
  # plot(x = rejilla,y=Fxpost(rejilla),type = "l",main = "Pos")
  # abline(v=ty)
  # points(x =muesb,Fxpost(muesb),pch=20)
  # hist(muesb)
  CV = 1000 * sd(muesb) / mean(muesb)
  IC = hdr(muesb, 95)$hdr # intervalo de credibilidad

  c(cont     = ifelse(ty > IC[1] & ty < IC[2], 1, 0),
    Longitud = IC[2] - IC[1],    # Longitud del intervalo
    hat.ty   = mean(muesb),      # Estimador de calibration
    Sesgo    = mean(muesb) - ty, # Sesgo de la estimaci'on
    CV       = CV )              # Coeficiente de variaci'on estimado
}
#############################################################################
# Inicializar las variables
N=20000
shape=4
rate=1
#############################################################################
# Crear escenario
n=c(50,400,1000)
sigma=c(74,13.7,3.6)
Escenarios<-expand.grid(n=n,sigma=sigma)
RsultMAS1<- data.frame(Coverage.100=NA,
                      Longitud.Relative.1000=NA,
                      Sesgo.Relative.1000=NA,
                      CV.1000=NA)
RsultMAS2<-RsultMAS1
RsultMAS3<-RsultMAS1
RsultMAS4<-RsultMAS1
RsultMAS5<-RsultMAS1
#############################################################################
for (i in 1:9) {
  set.seed(1)
  Pob<-SimMASGamma(N,shape,rate,Escenarios[i,"sigma"])
  ResulSimU   <-t(replicate(50,SimHT(Pob,Escenarios[i,"n"])))
  ResulSimN_N <-t(replicate(50,SimHT(Pob,Escenarios[i,"n"],apriori = "normal",k=1000)))
  ResulSimG_N <-t(replicate(50,SimHT(Pob,Escenarios[i,"n"],apriori = "gamma",k=10000)))
  ResulSimN   <-t(replicate(50,SimHT(Pob,Escenarios[i,"n"],apriori = "normal",k=10)))
  ResulSimG   <-t(replicate(50,SimHT(Pob,Escenarios[i,"n"],apriori = "gamma",k=100)))
  ty<-sum(Pob[,"Y"])
  print(RsultMAS1[i,]<-Medida.Calidad(ResulSimU,ty))
  print(RsultMAS2[i,]<-Medida.Calidad(ResulSimN_N,ty))
  print(RsultMAS3[i,]<-Medida.Calidad(ResulSimG_N,ty))
  print(RsultMAS4[i,]<-Medida.Calidad(ResulSimN,ty))
  print(RsultMAS5[i,]<-Medida.Calidad(ResulSimG,ty))
  }

RsultUNF<-cbind(Escenarios,RsultMAS1)
RsultNOR_N<-cbind(Escenarios,RsultMAS2)
RsultGAM_N<-cbind(Escenarios,RsultMAS3)
RsultNOR<-cbind(Escenarios,RsultMAS4)
RsultGAM<-cbind(Escenarios,RsultMAS5)

write.table(RsultUNF,"output/RsultUNF.txt",sep = "\t",dec = ".",row.names = FALSE)
write.table(RsultNOR_N,"output/RsultNOR_N.txt",sep = "\t",dec = ".",row.names = FALSE)
write.table(RsultGAM_N,"output/RsultGAM_N.txt",sep = "\t",dec = ".",row.names = FALSE)
write.table(RsultNOR,"output/RsultNOR.txt",sep = "\t",dec = ".",row.names = FALSE)
write.table(RsultGAM,"output/RsultGAM.txt",sep = "\t",dec = ".",row.names = FALSE)