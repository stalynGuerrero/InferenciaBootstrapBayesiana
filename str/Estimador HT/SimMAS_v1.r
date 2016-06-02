#############################################################################
rm(list = ls())
dirPath<-"/home/sguerrero/Documentos/Dropbox/articulos/Artículo Proporción bayesiana/InferenciaBootstrapBayesiana/Algoritmo prueba/InferenciaBootstrapBayesiana"
setwd(dirPath)
#############################################################################
require(TeachingSampling)
source("str/Funciones Comunes/SimMASGamma.r")
source("str/Estimador HT/E.HT.r")
source("str/Funciones Comunes/Medida.Calidad.r")
#############################################################################
# Definir función para la simulación
SimHT<-function(Pob,n){
  N<-nrow(Pob)
  ty<-sum(Pob[,"Y"])
  sel<-sample(N,n)
  ys<-Pob[sel,"Y"]
  E.HT(ys,N,n,ty)
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
RsultMAS<- data.frame(Coverage.100=NA,
                      Longitud.Relative.1000=NA,
                      Sesgo.Relative.1000=NA,
                      CV.1000=NA)
#############################################################################
for (i in 1:9) {
  set.seed(1)
  Pob<-SimMASGamma(N,shape,rate,Escenarios[1,"sigma"])

  ResulSim<-t(replicate(1000,SimHT(Pob,Escenarios[1,"n"])))
  ty<-sum(Pob[,"Y"])
  RsultMAS[i,]<-Medida.Calidad(ResulSim,ty)
}

RsultMAS<-cbind(Escenarios,RsultMAS)

write.table(RsultMAS,"output/ResulMAS.txt",sep = "\t",dec = ".",row.names = FALSE)
