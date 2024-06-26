# Trabajo de Software Skills
#Grupo 7
# Arámbulo Zapata, Walter Javier
# Francia, Juan
# Mulatillo , Mervy
#......................................
# Modelo Logístico - Cliente default
#......................................

# Importancia del modelo: Relacionado directamente con la rentabilidad
# de la empresa, puesto que mientras mejor se conozca el comportamiento
# de un cliente, mejores estratégicas se pueden tomar.

#Cambiar la dirección del directorio
setwd("C:/Users/Walter/Documents/UNI/Trabajo R UNI")

#===============================================================================
# 1.- Importar la data desde Excel
#===============================================================================
install.packages("readxl")
install.packages("dplyr")

library(readxl)
Base01 <- read_excel("Base01.xlsx", 
                     sheet = "Hoja1", skip = 4)

#===============================================================================
# 2.- Visualizar la base de datos
#===============================================================================

#Cabecera de data
head(df_buro)

#View(Base01)

str(Base01) #Para ver la estructura de la data y los tipos de variables

# Variables consideradas
#NUM,ID_DEUDOR,FLG_MALO,CNT_ENT_SLD,EDAD,SEXO,INGR_GESTION,LIN_TARJ_PCT_LIN_CRED

#Medidas de tendencia central de las variables
summary(Base01)

#===============================================================================
# 3.- Tratamiento de datos faltante
#===============================================================================
#Para ver si hay datos faltantes
sapply(Base01, function(x) sum(is.na(x)))

#Pudimos apreciar que se tenían 14758 valores pérdidos.
#Valores NA que serán reemplazado por 0
Base01[is.na(Base01)] <-0

#Comprobamos si los NA fueron reemplazados
sapply(Base01, function(x) sum(is.na(x)))

#Ahora ya no hay valores perdidos

#===============================================================================
# 4.- Eliminar columnas
#===============================================================================
#Se elimino la columna Nº y ID
library(dplyr)      #Manipulación de dataframes
MBase01 <- select(Base01,-1,-2)

#Renombrar Variables
names (MBase01) = c("FLG_MALO","CNT_ENT_SLD","CNT_MES_EXP","EDAD","SEXO","INGR_GESTION","LIN_TARJ","PCT_LIN_CRE")
names (MBase01)[1]= "FLG"
names (MBase01)[2]= "SLD_E"
names (MBase01)[3]= "SLD_M"
names (MBase01)[4]= "EDAD"
names (MBase01)[5]= "SEXO"
names (MBase01)[6]= "ING"
names (MBase01)[7]= "TARJETA"
names (MBase01)[8]= "LINEA"

#===============================================================================
# 5.- Análisis exploratorio
#===============================================================================
install.packages("ggcorrplot")
library(ggcorrplot)
nv <- sapply(MBase01,is.numeric)
cormat <- cor(MBase01[,nv])
ggcorrplot::ggcorrplot(cormat, title = "Correlación de variables numéricas", lab = T)

#Se puede apreciar que existe correlaciòn fuerte positiva entre las variables LIN_TARJ e INGR_GESTION

#===============================================================================
# 6.- Estimación del modelo de probabilidad de defauld
#===============================================================================
logit01 <- glm(FLG~SLD_E+SLD_M+EDAD+SEXO+ING+TARJETA+LINEA, family=binomial(link = "logit"), data=MBase01)

summary(logit01) # Analizando el modelo se puede observar que todos los coeficientes son significativos

# Interpretabilidad de los coeficientes
# Relación con la variable target (FGL_MALO)

# 1.- SLD_E: Relación positiva, entendiendo que a más IFI en las que se tenga deuda, 
# mayor será la probabilidad de caer en default

# 2.- SLD_M: Relación negativa, entendiendo mientras el periodo de ahorro se acerca al año, 
# menor será la probabilidad de caer en default.

# 3.- EDAD: Relación negativa, entendiendo que a mayor edad del cliente, 
# menor será la probabilidad de caer en default.

# 4.- SEXO: Relación positiva, entendiendo que si el cliente es mujer será una mejor pagadora, 
# y con menor probabilidad de caer en default

# 5.- ING: Relación negativo, entendiendo que a mayor ingreso 
#(mayor solvencia tendrá el cliente para hacer frente a la deuda), 
# menor será la probabilidad de caer en default

# 6.- TARJETA: Relación negativo, entendiendo que mientras mayor sea la linea disponible de crédito, 
# menor será la probabilidad de caer en default

# 7.- LINEA: Relación positiva, entendiendo que a mayor uso de la tarjeta, 
# mayor será la probabilidad de caer en default


#Guardar resultados en formato html (se guarda en el directorio especificado)
install.packages("stargazer")
library(stargazer)

stargazer(logit01, type="html", out="logit.htm")

#Odds Ratio
install.packages("mfx")
library(mfx)

logitor(FLG~SLD_E+SLD_M+EDAD+SEXO+ING+TARJETA+LINEA, data=MBase01)
logitor(formula = FLG~SLD_E+SLD_M+EDAD+SEXO+ING+TARJETA+LINEA, data = MBase01)

logit.or = exp(coef(logit01))
logit.or
v_em=logit.or-1
v_em

#Betas de las variables son significativos

#Guardar resultados del modelo en formato html (ODDS RATIO) 
stargazer(v_em, type="html",  out="vem.htm")
stargazer(logit01, type="html", coef=list(logit.or), p.auto=FALSE, out="logitor.htm")

# Guardar resumen del modelo en formato html
l_objetos <- list(estimado = logit01,
                   or= logit.or,
                  emarginales = v_em)

print(l_objetos)
stargazer(l_objetos, type="html",  out="objetos.htm")

#Probabilidad de defauld si el cliente es hombre
#Hombre sexo=1
invlogit = function (x) {1/(1+exp(-x))}
invlogit(coef(logit01)[1]+
           coef(logit01)[2]*mean(MBase01$SLD_E)+
           coef(logit01)[3]*mean(MBase01$SLD_M)+
           coef(logit01)[4]*mean(MBase01$EDAD)+
           coef(logit01)[5]*1+
           coef(logit01)[6]*mean(MBase01$ING)+
           coef(logit01)[7]*mean(MBase01$TARJETA)+
           coef(logit01)[8]*mean(MBase01$LINEA))

#Probabilidad de defauld si el cliente es mujer
#Mujer sexo=0
invlogit = function (x) {1/(1+exp(-x))}
invlogit(coef(logit01)[1]+
           coef(logit01)[2]*mean(MBase01$SLD_E)+
           coef(logit01)[3]*mean(MBase01$SLD_M)+
           coef(logit01)[4]*mean(MBase01$EDAD)+
           coef(logit01)[5]*0+
           coef(logit01)[6]*mean(MBase01$ING)+
           coef(logit01)[7]*mean(MBase01$TARJETA)+
           coef(logit01)[8]*mean(MBase01$LINEA))

#===============================================================================
# 7.- Análisis de la capacidad predictiva del modelo
#===============================================================================
#Punto de corte
c<-seq(0.01,0.3,by=0.001)
sens<-c()
spec<-c()
for (i in 1:length(c)){
  y.pred<-ifelse(logit01$fitted.values > c[i], yes = 1, no = 0) 
  spec[i]<-prop.table(table(MBase01$FLG,y.pred),1)[1]
  sens[i]<-prop.table(table(MBase01$FLG,y.pred),1)[4]
}

o.cut<-mean(c[which(round(spec,1.5)==round(sens,1.5))],na.rm = T)
plot(c,sens,type="l",col=2,main=c("Especificidad vs Sensibilidad"),ylab=c("Especificidad/Sensibilidad"))
lines(c,spec,col=3)

#Se observa el punto donde se cruzan :

abline(v=o.cut)

#El punto se corte es de 0.1105
#Es decir que el 11.05% de los clientes se encontraran en la posición de corte.

# sensibilidad y especificidad
y.pred<-ifelse(logit01$fitted.values > o.cut, yes = 1, no = 0) 
matriz_confusion <-table(MBase01$FLG, y.pred,
                         dnn = c("observaciones", "predicciones"))

matriz_confusion

prop.table(matriz_confusion,1)

# Verdaderos positivos y verdaderos negativos
# El modelo predice u(n 65.7% la probabilidad de que un cliente 
# no caiga en default (Sea bueno) y un 66.10% de probabilidad 
# de que caiga en default (sea malo).

#Curva ROC
install.packages("pROC")
library(pROC)

roc<- roc(MBase01$FLG,logit01$fitted.values)

roc

plot(roc,main=c("Curva ROC"))

tabla<-table(MBase01$FLG, y.pred)
tabla

#Interpretación
# El área bajo la curva es 0.7207, por ello se comenta que el modelo es
# medio aceptable para explicar y predecir si un cliente caera en default.
# Lo óptimo es que sea superior al 0.90.
