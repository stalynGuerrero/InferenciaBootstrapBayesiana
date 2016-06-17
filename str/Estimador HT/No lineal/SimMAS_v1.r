#############################################################################
rm(list = ls())
dirPath<-"/home/sguerrero/Documentos/Dropbox/articulos/Artículo Proporción bayesiana/InferenciaBootstrapBayesiana/InferenciaBootstrapBayesiana"
setwd(dirPath)
#############################################################################
require(TeachingSampling)
source("str/Estimador HT/E.HT.r")
source("str/Funciones Comunes/Medida.Calidad.r")
Pob<-read.table("PobBB.txt",header = T)
names(Pob)<-c("Y","X")
Pob<-na.omit(Pob)

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
# Crear escenario
n=c(50,400,1000)
RsultMAS<- data.frame(Coverage.100=NA,
                      Longitud.Relative.1000=NA,
                      Sesgo.Relative.1000=NA,
                      CV.1000=NA)
#############################################################################
j=0
for (i in n) {
  j=j+1
  set.seed(2)
  ResulSim<-t(replicate(1000,SimHT(Pob,i)))
  ty<-sum(Pob[,"Y"])
  RsultMAS[j,]<-Medida.Calidad(ResulSim,ty)
}

RsultMAS<-cbind(n,RsultMAS)
RsultMAS
write.table(RsultMAS,"output/No_Lin/ResulMAS.txt",sep = "\t",dec = ".",row.names = FALSE)
