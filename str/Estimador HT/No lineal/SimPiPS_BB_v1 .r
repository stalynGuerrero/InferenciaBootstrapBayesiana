#############################################################################
rm(list = ls())
options(digits = 10)
dirPath<-"/home/sguerrero/Documentos/Dropbox/articulos/Artículo Proporción bayesiana/InferenciaBootstrapBayesiana/InferenciaBootstrapBayesiana"
setwd(dirPath)
#############################################################################
require(TeachingSampling)
require(hdrcde)
require(cubature)
source("str/Funciones Comunes/VeroBootPiPS.r")
source("str/Funciones Comunes/Medida.Calidad.r")
Pob<-read.table("PobBB.txt",header = T)
names(Pob)<-c("Y","X")
Pob<-na.omit(Pob)
#############################################################################
# Definir función para la simulación
SimHT <- function(Pob, n, apriori = "unif", k) {
  ty<-sum(Pob[,"Y"])
  sel<-S.piPS(n,Pob[,"X"])
  pik<-sel[,"Pik.s"]
  sel<-sel[,"samp"]
  ys<-Pob[sel,"Y"]
  Vero <- VeroBootPiPS(ys,pik,ty)

  if (apriori == "gamma") {
    Aprio = dgamma(Vero$x, ty ^ 2 / (k), ty / (k))
    nombre<-paste0("Vero,",k,apriori)
  }
  if (apriori == "unif") {
    Aprio = dunif(Vero$x, min = 0, max = 10 ^ 10)
    nombre<-paste0("Vero,",apriori)
  }
  if (apriori == "normal") {
    Aprio = dnorm(Vero$x, mean = ty, sd = k)
    nombre<-paste0("Vero,",k,apriori)
  }

  # par(mfrow=c(1,2))
  # plot(Vero,type = "l",main = nombre)
  # abline(v=ty)

  post   =  Vero$y * Aprio
  if(sum(post)==0){post=dunif(Vero$x, min = min(Vero$x), max = max(Vero$x))}
  Vero$x <- Vero$x[post>0]
  Vero$y <- Vero$y[post>0]
  post   <- post[post>0]
  Fxpost <- approxfun(Vero$x,post)

  q0.001<-as.numeric(quantile(Vero$x,probs = 0.05))
  q0.999<-as.numeric(quantile(Vero$x,probs = 0.95))
  rejilla<-seq(q0.001,q0.999,by=1)
  muesb  = sample(rejilla, 1000, prob =Fxpost(rejilla), replace = T)


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
# Crear escenario
n=c(50,400,1000)
RsultMAS1<- data.frame(Coverage.100=NA,
                      Longitud.Relative.1000=NA,
                      Sesgo.Relative.1000=NA,
                      CV.1000=NA)
RsultMAS2<-RsultMAS1
RsultMAS3<-RsultMAS1
RsultMAS4<-RsultMAS1
RsultMAS5<-RsultMAS1
#############################################################################
j=0
for (i in n) {
  j=j+1
  set.seed(1)
  ResulSimU   <-t(replicate(10,SimHT(Pob,i)))
  ResulSimN_N <-t(replicate(10,SimHT(Pob,i,apriori = "normal",k=1000)))
  ResulSimG_N <-t(replicate(10,SimHT(Pob,i,apriori = "gamma",k=10000)))
  ResulSimN   <-t(replicate(10,SimHT(Pob,i,apriori = "normal",k=10)))
  ResulSimG   <-t(replicate(10,SimHT(Pob,i,apriori = "gamma",k=100)))
  ty<-sum(Pob[,"Y"])
  print(RsultMAS1[j,]<-Medida.Calidad(ResulSimU,ty))
  print(RsultMAS2[j,]<-Medida.Calidad(ResulSimN_N,ty))
  print(RsultMAS3[j,]<-Medida.Calidad(ResulSimG_N,ty))
  print(RsultMAS4[j,]<-Medida.Calidad(ResulSimN,ty))
  print(RsultMAS5[j,]<-Medida.Calidad(ResulSimG,ty))
  }

RsultUNF<-cbind(n,RsultMAS1)
RsultNOR_N<-cbind(n,RsultMAS2)
RsultGAM_N<-cbind(n,RsultMAS3)
RsultNOR<-cbind(n,RsultMAS4)
RsultGAM<-cbind(n,RsultMAS5)

write.table(RsultUNF,"output/No_Lin/RsultUNFPiPS.txt",sep = "\t",dec = ".",row.names = FALSE)
write.table(RsultNOR_N,"output/No_Lin/RsultNOR_NPiPS.txt",sep = "\t",dec = ".",row.names = FALSE)
write.table(RsultGAM_N,"output/No_Lin/RsultGAM_NPiPS.txt",sep = "\t",dec = ".",row.names = FALSE)
write.table(RsultNOR,"output/No_Lin/RsultNORPiPS.txt",sep = "\t",dec = ".",row.names = FALSE)
write.table(RsultGAM,"output/No_Lin/RsultGAMPiPS.txt",sep = "\t",dec = ".",row.names = FALSE)

setwd("/home/sguerrero/Documentos/Dropbox/articulos/Artículo Proporción bayesiana/InferenciaBootstrapBayesiana/InferenciaBootstrapBayesiana/output/No_Lin")
#############################################################################
x<-dir()
for(i in 1:length(x)){
  if(i==1){
    xx= cbind(diseno=gsub(".txt","",x[i]),read.csv(x[i],sep="\t"))
  }else{
    xx=rbind(xx, cbind(diseno=gsub(".txt","",x[i]),read.csv(x[i],sep="\t")))
  }
}
