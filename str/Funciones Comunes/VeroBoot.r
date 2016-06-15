require(MASS)
require(KernSmooth)
source("str/Estimador HT/E.HT.r")
VeroBoot<-function(ys,N,n,ty){

  E.HT2<-function(ys,N,n,ty){
    sel <-sample(n,n,replace = T)
    E.HT(ys[sel],N,n,ty)["hat.ty"]
  }
  tyBoot <- as.numeric(replicate(500,E.HT2(ys,N,n,ty)))
  tyhat  <- as.numeric(E.HT(ys,N,n,ty)["hat.ty"])
  h1=bandwidth.nrd(tyBoot)
  bkde((2*tyhat-tyBoot),kernel = "normal",bandwidth = h1,gridsize = 50000)

  }