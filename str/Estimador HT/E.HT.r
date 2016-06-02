# Estimador de Horvitz-Thompson (HT)
E.HT<-function(ys,N,n,ty){
  # Estimaci'on de HT
  ty.mas = E.SI(N, n, ys)["Estimation","y"]
  sdpi   = E.SI(N, n, ys)["Standard Error","y"]
  CV     = 10*E.SI(N, n, ys)["CVE","y"]
  # intervalo de confianza al 95%  para el total
  Li=ty.mas-qnorm(0.975)*sdpi
  Ls=ty.mas+qnorm(0.975)*sdpi

  c(cont     = ifelse(ty>Li & ty<Ls,1,0),
    Longitud = Ls-Li,                     # Longitud del intervalo
    hat.ty    = ty.mas,                   # Estimador de HT
    Sesgo    = ty.mas-ty,                 # Sesgo de la estimaci'on
    CV       = CV)                        # Coeficiente de variaci'on estimado
}
