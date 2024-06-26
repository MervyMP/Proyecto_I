# Trabajo de Software Skills
#Grupo 7

#......................................
# Modelo Logístico - Cliente default
#......................................

# Importancia del modelo: Relacionado directamente con la rentabilidad
# de la empresa, puesto que mientras mejor se conozca el comportamiento
# de un cliente, mejores estratégicas se pueden tomar.

#========================================
# 1.- Instalar packages a utilizar
#========================================
install.packages("readxl")
install.packages("dplyr")
install.packages("ggcorrplot")
install.packages("ggpubr")
install.packages("stargazer")
install.packages("mfx")
install.packages("pROC")

install.packages("tidyverse")
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rminer")
install.packages("nnet")
install.packages("ggplot2")
install.packages("Amelia")
install.packages("caTools")
install.packages("dummies")
install.packages("repr")

#===============================
# 2.- Llamar las librerias a usar
#===============================
library(readxl)     #Importar data de excel
library(dplyr)      #Manipulación de dataframes
library(ggcorrplot) #Para graficar la correlación de las variables
library(stargazer)  #Para guardar en en formato html
library(mfx)        #Para los efectos marginales del modelo
library(pROC)       #Para la función de la curva ROC

library(readxl)    #Importar data de excel
  library(tidyverse)  
library(dplyr)    #Manipulación de dataframes
  library(caret)      #Clasificación y entrenamiento de regresión
library(rpart)      #Partición y preprocesamiento de data
library(rpart.plot) #Árboles de decisión y partición recursiva
library(nnet)       #Redes Neuronales
library(ggplot2)    #Visualización de datos "Gramática de los gráficos"
library(ggplot)     
library(ggcorrplot) #Para graficar la correlación de las variables
  library(Amelia)     #Para datos faltantes en el data frame
library(caTools)    #Rendimiento de las estadística de la ventana móvil
library(dummies)    #Para convertir factores en variables dummy
library(ggpubr)
library(repr)
library(stargazer)  #Para guardar en en formato html
  library(mfx)        #Para los efectos marginales del modelo
  library(pROC)       #Para la función de la curva ROC

###################################################
#=================================================
# 3.- Cargar data para el modelo
#=================================================

#Opción 1
#Para abrir la ventana de windows y buscar el archivo
file.choose()
ruta_excel <- "C:\\Users\\MERVY\\Downloads\\PE. Econometría Aplicada\\Módulo 1\\Trabajo Sofware Skills\\ModeloBuro.xlsx"

#Para ver cuántas hojas tiene el excel
excel_sheets(ruta_excel)
df <- read_xlsx(ruta_excel)
Data=df


#Opción 2
#Elegir el directorio  donde se aloja el archivo
df_buro <- read_xlsx("ModeloBuro.xlsx")


########################################################
#===================================
# 4.- Exploración de la data
#===================================
df_buro

#Cabecera de data
head(df_buro)

#================================================
#4.1.- Variables consideradas
#NUM,ID_DEUDOR,FLG_MALO,CNT_ENT_SLD,EDAD,SEXO,INGR_GESTION,LIN_TARJ_PCT_LIN_CRED


#4.2.- Estadísticas descriptivas
str(df_buro)      #Para ver la estructura de la data y los tipos de variables
?str

summary(df_buro)

#================================================
#5.- Limpieza de la data
#================================================
#5.1.- Tratamiento de datos faltante
       #Para ver si hay datos faltantes
sapply(df_buro, function(x) sum(is.na(x)))

#Pudimos apreciar que se tenían 14758 valores pérdidos.
#Valores NA que serán reemplazado por 0
df_buro[is.na(df_buro)] <-0

#Comprobemos si los NA fueron reemplazados
sapply(df_buro, function(x) sum(is.na(x)))

#Ahora ya no hay valores pérdidos

#================================================
#5.2.- Eliminar columnas
       #Se elimino la columna NUM y ID_DEUDOR
df_Mburo <- select(df_buro,-1,-2)


#================================================
# 5.5. Renombrando variables para facilidad de uso
names (df_Mburo) = c("FLG_MALO","CNT_ENT_SLD","CNT_MES_EXP","EDAD","SEXO","INGR_GESTION","LIN_TARJ","PCT_LIN_CRE")
names (df_Mburo)[1]= "FLG"
names (df_Mburo)[2]= "SLD_E"
names (df_Mburo)[3]= "SLD_M"
names (df_Mburo)[4]= "EDAD"
names (df_Mburo)[5]= "SEXO"
names (df_Mburo)[6]= "ING"
names (df_Mburo)[7]= "TARJETA"
names (df_Mburo)[8]= "LINEA"


#================================================
#5.6.- Convertir las variables de tipo doble a tipo numérico
#y guardarlas en una variable como marco de datos
df_Mburo$SEXO <- as.factor(df_Mburo$SEXO)

#df_MburoN <-df_Mburo[,c("FLG_M,"SEXO")]
#df_MburoN <-data.frame(scale(df_MburoN))

summary(df_Mburo)



#####################################################
#=====================================
#6.- Análisis Exploratorio
#=====================================

#===========================================================
#6.1.- Gráfico de correlación entre las variables numéricas

#Opción 1
nv <- sapply(df_Mburo,is.numeric)

cormat <- cor(df_Mburo[,nv])

ggcorrplot::ggcorrplot(cormat, title = "Correlación de variables numéricas")

ggcorrplot::ggcorrplot(cormat, title = "Correlación de variables numéricas", lab = T)

                       
#Se puede apreciar que existe correlaciòn fuerte positiva entre las variables LIN_TARJ e INGR_GESTION

#Opción 2
pairs(df_Mburo)

#Opción 3
plot(df_Mburo)

#===========================
#6.2.- Gráficas de variables

##1.- SLD_E
     # Es la cantidad de empresas en las que el cliente tiene saldo. 
boxplot(df_Mburo$SLD_E ~ df_Mburo$FLG, col = "red",main = "Número de empresas con saldo y FLG")

##2.- SLD_M
    # Es la cantidad de empresas en las que el cliente tiene saldo.
boxplot(df_Mburo$SLD_M ~ df_Mburo$FLG, col = "red",main = "Meses con saldo en los últimos 12 meses y FLG")

##3.- EDAD
# Es la cantidad de empresas en las que el cliente tiene saldo
#ggplot(df_Mburo=df_Mburo, aes(x=EDAD))+
  geom_histogram(bins = 50, fill="purple", col="blue", alpha=0.3)+
  scale_x_continuous(breaks = seq(min(0), max(90), by=5), na.value = TRUE)
  
boxplot(df_Mburo$EDAD ~ df_Mburo$FLG, col = "red",main = "Edad y FLG")

##4.- SEXO
# Es la cantidad de empresas en las que el cliente tiene saldo.
#plot1 <- ggplot(df_Mburo = df_Mburo, aes(x=factor(SEXO), fill =factor(FLG)))+
                                         geom_bar()+
                                           ylab("Observaciones")+
                                           scale_x_discrete(labels = c('Female','Male'))+
                                           xlab("")
plot


##5.- ING
# Es la cantidad de ingresos de gestión.
boxplot(df_Mburo$ING ~ df_Mburo$FLG, col = "red",main = "ING y FLG")

##6.- TAREJTA
# Es la cantidad de empresas en las que el cliente tiene saldo.
boxplot(df_Mburo$TARJETA ~ df_Mburo$FLG, col = "red",main = "TARJETA y FLG")

##7.- LINEA
# Es la cantidad de empresas en las que el cliente tiene saldo.
boxplot(df_Mburo$LINEA ~ df_Mburo$FLG, col = "red",main = "LINEA y FLG")


#======================================
#7.- MODELO LOGISTICO
#======================================

#====================
#7.1.- Modelo Logit01
logit <- glm (FLG ~ SLD_E + SLD_M + EDAD + SEXO + ING +TARJETA + LINEA, family = binomial(link = "logit"),
                data=df_Mburo)


summary(logit)
# Analizando el modelo se puede observar que todos los coeficientes son significativos


# Para visualizar los datos del modelo en un mejor formato
stargazer(logit, type="html", out="logit.htm")


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

  # 1.- LINEA: Relación positiva, entendiendo que a mayor uso de la tarjeta, 
              # mayor será la probabilidad de caer en default


#=================
#7.2.- ODDS RATIO

#MODO 1
#Estimación de los ratios ODDS 

cbind(Estimate=round(coef(logit),8), or=round(exp(coef(logit)),8))

#MODO 2
logitor(FLG ~ SLD_E + SLD_M + EDAD + SEXO + ING +TARJETA + LINEA,data=df_Mburo)


#Betas de las variables son significativos


#=============================
#7.3.- Efectos marginales - ratios exponenciales

#  Para guardarlo en formato html
logit.or = exp(coef(logit))
logit.or

# Para visualizar los datos en un formato html
stargazer(logit, type="html", coef=list(logit.or), p.auto=FALSE, out="logitor.htm")


#===================================
#Resumen del modelo
#===================================
logit.or = exp(coef(logit))
logit.or
v_em=logit.or-1
v_em
stargazer(v_em, type="html",  out="vem.htm")
stargazer(logit01, type="html", coef=list(logit.or), p.auto=FALSE, out="logitor.htm")
l_objetos <- list(estimado = logit,
                  or= logit.or,
                  emarginales = v_em)


p
print(l_objetos)
stargazer(l_objetos, type="html",  out="objetos.htm")


#==========================================
#8.- CAPACIDAD PREDICTIVA DEL MODELO
#===========================================

#8.1.-Punto corte
c<-seq(0.01,0.3,by=0.001)
sens<-c()
spec<-c()
for (i in 1:length(c)){
  y.pred<-ifelse(logit$fitted.values > c[i], yes = 1, no = 0) 
  spec[i]<-prop.table(table(df_Mburo$FLG,y.pred),1)[1]
  sens[i]<-prop.table(table(df_Mburo$FLG,y.pred),1)[4]
}

o.cut<-mean(c[which(round(spec,1.5)==round(sens,1.5))],na.rm = T)
plot(c,sens,type="l",col=2,main=c("Especificidad vs Sensibilidad"),ylab=c("Especificidad/Sensibilidad"))
lines(c,spec,col=3)

#Se observa el punto donde se cruzan :

abline(v=o.cut)

#El punto se corte es de 0.1105
#Es decir que el 11.05% de los clientes se encontraran en la posición de corte.



#=====================
#Matriz de confunsión
#======================
y.pred<-ifelse(logit$fitted.values > o.cut, yes = 1, no = 0) 
matriz_confusion <-table(df_Mburo$FLG, y.pred,
                         dnn = c("observaciones", "predicciones"))

matriz_confusion

prop.table(matriz_confusion,1)

# Verdaderos positivos y verdaderos negativos
# El modelo predice u(n 65.7% la probabilidad de que un cliente 
# no caiga en default (Sea bueno) y un 66.10% de probabilidad 
# de que caiga en default (sea malo).


#========================
# Curva Roc
#========================
roc <- roc(df_Mburo$FLG,logit$fitted.values)

roc

plot(roc,main=c("Curva ROC"))

#Interpretación
# El área bajo la curva es 0.7207, por ello se comenta que el modelo es
# medio aceptable para explicar y predecir si un cliente caera en default.
# Lo óptimo es que sea superior al 0.90.







