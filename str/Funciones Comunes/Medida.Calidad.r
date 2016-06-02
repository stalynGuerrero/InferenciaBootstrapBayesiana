# Medidas de Calidad de la estimaci'on
Medida.Calidad <- function(Dat,ty){
  round( data.frame(Coverage.100 = 100*mean(Dat[,"cont"]),
                    Longitud.Relative.1000 = 1000*mean((Dat[,"Longitud"]/2)/Dat[,"hat.ty"]),
                    Sesgo.Relative.1000 = 1000*mean(Dat[,"Sesgo"]/ty),
                    CV.1000 = mean(Dat[,"CV"])),3)
}