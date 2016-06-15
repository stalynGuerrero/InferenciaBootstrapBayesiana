require(MASS)
require(KernSmooth)
source("str/Estimador HT/E.PiPS.r")
VeroBootPiPS<-function(ys,pik,ty){

    E.PiPS2<-function(ys,pik,ty){
    n<-length(ys)
    sel <-sample(n,n,replace = T)
    E.PiPS(ys[sel],pik[sel],ty)["hat.ty"]
  }
  tyBoot <- as.numeric(replicate(500,E.PiPS2(ys,pik,ty)))
  tyhat  <- as.numeric(E.PiPS(ys,pik,ty)["hat.ty"])
  h1=bandwidth.nrd(tyBoot)
  bkde((2*tyhat-tyBoot),kernel = "normal",bandwidth = h1,gridsize = 50000)

  }