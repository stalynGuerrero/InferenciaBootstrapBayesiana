require(RCurl)
library(foreign)

URL01 <-getURL("https://raw.githubusercontent.com/stalynGuerrero/InferenciaBootstrapBayesiana/master/BaseEj.txt")

Datos01 <- read.csv(textConnection(URL01))

################## Funciones ###############
for()
tapply()
by()
do.call()
aggregate()
ddplyr()
summarise()
group_by()

#####################################
## Resumen de datos
#####################################
summary(Datos01)
################################################################
####    Obtener el resumen de resultados por catégorias     ####
################################################################

##################################
###    Solución  empleando for ###
##################################

## Solución 1
R01 <- data.frame()

for(ii in unique(Datos01[,c("ORG_TYPE")])){
x <- subset(Datos01, ORG_TYPE == ii)
x01 <- summary(x[,c("SEX","NUMBER_OF_PATIENTS")])
x01 <- as.data.frame(x01)
R01 <- rbind(R01,x01)

}

## Solución 2
R02 <- data.frame()

for(ii in unique(Datos01[,c("ORG_TYPE")])){
x <- subset(Datos01, ORG_TYPE == ii)

x02 <- data.frame(id = ii,
  SinDefinir = sum(x$SEX == "ALL"),
  Hombre = sum(x$SEX == "MALE"),
  Mujer = sum(x$SEX == "FEMALE"),
  Min = min(x$NUMBER_OF_PATIENTS),
  Max = max(x$NUMBER_OF_PATIENTS))

R02 <- rbind(R02,x02)  
}

#################################################
## Repetir el ejecicio con la variable "ONS_CODE"
#################################################

E01 <-  data.frame()
for(ii in unique(Datos01[,c("ONS_CODE")])){
  x <- subset(Datos01, ONS_CODE == ii)
  
  x02 <- data.frame(id = ii,
    SinDefinir = sum(x$SEX == "ALL"),
    Hombre = sum(x$SEX == "MALE"),
    Mujer = sum(x$SEX == "FEMALE"),
    Min = min(x$NUMBER_OF_PATIENTS),
    Max = max(x$NUMBER_OF_PATIENTS))
  
  E01 <- rbind(E01,x02)  
}

#####################################
###    Solución  empleando tapply ###
#####################################

## Solución 3

R03 <- tapply(X = Datos01$SEX,
          INDEX = Datos01[,c("ORG_TYPE")], 
            FUN = function(x){
               data.frame(  SinDefinir = sum(x == "ALL"),
                            Hombre = sum(x == "MALE"),
                             Mujer = sum(x == "FEMALE"))
       })
 

R03 <- do.call("rbind",R03)

## Solución 4

R04 <- tapply(X = Datos01$NUMBER_OF_PATIENTS ,
          INDEX = Datos01[,c("ORG_TYPE")], 
            FUN = function(x){summary(x)})

R04 <- do.call("rbind",R04)

##################################
###    Solución  empleando by ###
##################################
## Solución 5

R05 <- by(INDICES = Datos01[,"ORG_TYPE"],
             data = Datos01[,"NUMBER_OF_PATIENTS"],
              FUN = summary)

R05 <- do.call("rbind",R05)

## Solución 6

R06 <- by(INDICES = Datos01[,"ORG_TYPE"],
          data = Datos01[,"SEX"],
          FUN = table)

R06 <- do.call("rbind",R06)

#################################################
## Repetir el ejecicio con la variable "ONS_CODE"
#################################################

E02 <- by(INDICES = Datos01[,"ONS_CODE"],
          data = Datos01[,"SEX"],
          FUN = table)

E02 <- do.call("rbind",E02)

##########################################
###    Solución  empleando aggregate   ###
##########################################

## Solución 7
R07 <- aggregate(Datos01$SEX,
          list(Region = Datos01[,"ORG_TYPE"]),
          table)

## Solución 8
R08 <- aggregate(Datos01[,c("NUMBER_OF_PATIENTS")],
          list(Region = Datos01[,"ORG_TYPE"]),
          summary)

## Solución 9
R09 <- aggregate(Datos01[,c("NUMBER_OF_PATIENTS")],
          list(Region = Datos01[,"ORG_TYPE"],
               SEX = Datos01[,"SEX"]),
               summary)

##########################################
###    Solución  empleando ddplyr   ###
##########################################
require(plyr)

ddply(Datos01, .(ORG_TYPE), summarise, 
      N = n(),
      Media = mean(NUMBER_OF_PATIENTS),
      Min = min(NUMBER_OF_PATIENTS),
      Max = max(NUMBER_OF_PATIENTS) )

ddply(Datos01, .(ORG_TYPE, SEX), summarise, 
      N = n(),
      Media = mean(NUMBER_OF_PATIENTS),
      Min = min(NUMBER_OF_PATIENTS),
      Max = max(NUMBER_OF_PATIENTS) )

ddply(Datos01, .(SEX, ORG_TYPE), summarise, 
      N = n(),
      Media = mean(NUMBER_OF_PATIENTS),
      Min = min(NUMBER_OF_PATIENTS),
      Max = max(NUMBER_OF_PATIENTS) )

##########################################
###    Solución  empleando dplyr   ###
##########################################
require(dplyr)

R10 <- Datos01 %>% group_by(SEX, ORG_TYPE) %>%
           summarise(N = n(),
                 Media = mean(NUMBER_OF_PATIENTS))

R10

Datos01 %>% group_by(SEX, ORG_TYPE) %>% summarise( 
      N = n(),
      Media = mean(NUMBER_OF_PATIENTS),
      Min = min(NUMBER_OF_PATIENTS),
      Max = max(NUMBER_OF_PATIENTS) )
################################################
################################################
set.seed(01)
Datos01$Epsilon <- runif(nrow(Datos01))


Datos02 <- by(INDICES = Datos01[,"ORG_TYPE"],
                 data = Datos01,
                  FUN = function(x)subset(x,Epsilon < 0.02))

Datos02 <- do.call("rbind", Datos02)
Datos02[,"Epsilon"] <- NULL

################################################
E05 <- Datos02 %>% group_by(SEX, ORG_TYPE) %>% summarise( 
  n = n()) 

E05 <- merge(R10,E05)

 Datos02 %>% group_by(SEX, ORG_TYPE) %>% summarise( 
  Medhat = sum(NUMBER_OF_PATIENTS/0.02)/n()) %>% 
   merge(E05)
