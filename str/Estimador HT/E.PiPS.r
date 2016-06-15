E.PiPS<-function(ys,pik,ty){
  # Estimaci'on de HT
  ty.pi = E.piPS(ys,Pik = pik)["Estimation","y"]
  sdpi = E.piPS(ys,Pik = pik)["Standard Error","y"]
  CV =   10*E.piPS(ys,Pik = pik)["CVE","y"]
  # intervalo de confianza al 95%  para el total
  Li=ty.pi-qnorm(0.975)*sdpi
  Ls=ty.pi+qnorm(0.975)*sdpi

  c(cont     = ifelse(ty>Li & ty<Ls,1,0),
    Longitud = Ls-Li,                     # Longitud del intervalo
    hat.ty    = ty.pi,                   # Estimador de HT
    Sesgo    = ty.pi-ty,                 # Sesgo de la estimaci'on
    CV       = CV)                        # Coeficiente de variaci'on estimado
}
